{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Cookies
import HTTP

import Control.Applicative ((<$>), (<*>))
import Control.Concurrent (forkIO, newEmptyMVar, putMVar, takeMVar, myThreadId)
import Control.Exception (finally, bracketOnError, handle, throw, SomeException)
import Control.Monad (forever, mzero, join, liftM)
import Control.Monad.Error (runErrorT)
import Control.Monad.Trans (liftIO)
import Crypto.Random (createEntropyPool, CPRG(..), SystemRNG)
import qualified Data.Aeson as Aeson
import Data.Aeson (parseJSON)
import qualified Data.ByteString.UTF8 as BU
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.UTF8 as BLU
import Data.Certificate.X509 (X509)
import qualified Data.ConfigFile as CF
import Data.List (find)
import Data.Time.Clock (getCurrentTime)
import Network (PortID(..), listenOn, connectTo, accept)
import qualified Network.Curl as Curl
import qualified Network.HTTP.Cookie as HTTP
import qualified Network.HTTP.Types.URI as Query
import qualified Network.BSD as BSD
import qualified Network.Socket as Socket
import qualified Network.TLS as TLS
import qualified Network.TLS.Extra as TLS
import qualified Network.URI as URI
import System.IO (Handle, stdin, hClose)
import System.IO.Unsafe (unsafeInterleaveIO)
import qualified System.Log.Logger as Log

import Prelude hiding (log)

-- These are JSON responses that come from Google.
data AccessToken = AccessToken {accessToken :: String, expiresIn :: Integer, tokenType :: String}
instance Aeson.FromJSON AccessToken where
  parseJSON (Aeson.Object v) = AccessToken <$>
                               v Aeson..: "access_token" <*>
                               v Aeson..: "expires_in" <*>
                               v Aeson..: "token_type"
  parseJSON _ = mzero

data UserInfo = UserInfo {userEmail :: String}
instance Aeson.FromJSON UserInfo where
  parseJSON (Aeson.Object v) = UserInfo <$>
                               v Aeson..: "email"
  parseJSON _ = mzero

data Config = Config { cfDomain :: String
                     , cfContact :: String
                     , cfURL :: String
                     , cfClientID :: String
                     , cfClientSecret :: String
                     , cfAuthTokenKey :: String
                     , cfCertificates :: [(X509, Maybe TLS.PrivateKey)]
                     , cfAuthorizedEmails :: [String]
                     }

main = do
  -- Make sure we have all necessary config options. Read them from stdin.
  -- TODO: Reading these from stdin means that forgetting to provide
  -- them will result in what looks like a running daemon, but one
  -- that's not listening on any ports.
  Log.updateGlobalLogger "sproxy" (Log.setLevel Log.DEBUG)
  config' <- getConfig
  case config' of
    Left err -> log $ show err
    Right config -> do
      -- Immediately fork a new thread for accepting connections since
      -- the main thread is special and expensive to communicate with.
      wait <- newEmptyMVar
      forkIO (handle handleError (listen (PortNumber 443) (serve config)) `finally` (putMVar wait ()))
      takeMVar wait
 where handleError :: SomeException -> IO ()
       handleError e = log $ show e
       getConfig = runErrorT $ do
         cf <- join $ liftIO $ CF.readhandle CF.emptyCP stdin
         domain <- CF.get cf "DEFAULT" "domain"
         contact <- CF.get cf "DEFAULT" "contact"
         url <- CF.get cf "DEFAULT" "url"
         clientID <- CF.get cf "DEFAULT" "client_id"
         clientSecret <- CF.get cf "DEFAULT" "client_secret"
         authTokenKey <- CF.get cf "DEFAULT" "auth_token_key"
         sslKeyFile <- CF.get cf "DEFAULT" "ssl_key"
         sslCertFile <- CF.get cf "DEFAULT" "ssl_cert"
         sslExtraCertFiles <- words `fmap` CF.get cf "DEFAULT" "ssl_extra_certs"
         sslKey <- liftIO $ TLS.fileReadPrivateKey sslKeyFile
         sslCert <- liftIO $ TLS.fileReadCertificate sslCertFile
         sslExtraCerts <- liftIO $ mapM TLS.fileReadCertificate sslExtraCertFiles
         -- We'll need more complex rules for access control, but for now let's just use a list.
         authorizedEmails <- words `fmap` CF.get cf "DEFAULT" "authorized_emails"
         let certs = (sslCert, Just sslKey) : (map (\x -> (x, Nothing)) sslExtraCerts)
         return $ Config domain contact url clientID clientSecret authTokenKey certs authorizedEmails

serve :: Config -> Handle -> IO ()
serve cf h = do
  rng <- cprgCreate `liftM` createEntropyPool :: IO SystemRNG
  let params = TLS.defaultParamsServer { TLS.pCertificates = cfCertificates cf
                                       , TLS.pCiphers = TLS.ciphersuite_all
                                       , TLS.pUseSecureRenegotiation = True
                                       }
  ctx <- TLS.contextNewOnHandle h params rng
  TLS.handshake ctx
  input <- tlsGetContents ctx
  serve' ctx input
  TLS.bye ctx
 where serve' c input =
         -- TODO: Clean this up. The logic is really messy.
         case oneRequest input of
           (Nothing, _) -> return () -- no more requests
           (Just request@(_, url, headers, _), rest) -> do
             -- TODO: Don't loop for more input on Connection: close header.
             log $ show request
             -- Check if this is an authorization response.
             case URI.parseURIReference $ BU.toString url of
               Nothing -> internalServerError c "Failed to parse request URI" >> serve' c rest
               Just uri -> do
                 let query = Query.parseQuery $ BU.fromString $ URI.uriQuery uri
                 -- This isn't a perfect test, but it's perfect for testing.
                 case (lookup "state" query, lookup "code" query) of
                   (Just (Just _), Just (Just code)) -> do
                     tokenRes <- post "https://accounts.google.com/o/oauth2/token" ["code=" ++ BU.toString code, "client_id=" ++ cfClientID cf, "client_secret=" ++ cfClientSecret cf, "redirect_uri=" ++ cfURL cf, "grant_type=authorization_code"]
                     case tokenRes of
                       Left err -> internalServerError c err >> serve' c rest
                       Right response -> do
                         case Aeson.decode $ BLU.fromString $ Curl.respBody response of
                           Nothing -> internalServerError c "Received an invalid response from Google's authentication server." >> serve' c rest
                           Just token -> do
                             infoRes <- get $ "https://www.googleapis.com/oauth2/v1/userinfo?access_token=" ++ accessToken token
                             case infoRes of
                               Left err -> internalServerError c err >> serve' c rest
                               Right info -> do
                                 case Aeson.decode $ BLU.fromString $ Curl.respBody info of
                                   Nothing -> internalServerError c "Received an invalid user info response from Google's authentication server." >> serve' c rest
                                   Just userInfo -> do
                                     clientToken <- authToken (cfAuthTokenKey cf) (userEmail userInfo)
                                     TLS.sendData c $ BLU.fromString $ "HTTP/1.1 302 Found\r\nSet-Cookie: " ++ setCookie (HTTP.MkCookie (cfDomain cf) "gauth" (show clientToken) Nothing Nothing Nothing) authShelfLife ++ "\r\nLocation: " ++ cfURL cf ++ "\r\nContent-Length: 0\r\n\r\n"
                                     serve' c rest
                   _ -> do
                     -- Check for an auth cookie.
                     let (_, cookies) = processCookieHeaders (cfDomain cf) headers
                     case find (\x -> HTTP.ckName x == "gauth") cookies of
                       Nothing -> redirectForAuth c >> serve' c rest
                       Just authCookie -> do
                         auth <- validAuth (cfAuthTokenKey cf) (HTTP.ckValue authCookie)
                         case auth of
                           Nothing -> redirectForAuth c >> serve' c rest
                           Just token -> do
                             requestWithEmail c request (cfAuthorizedEmails cf) (authEmail token) (cfContact cf)
                             serve' c rest
       redirectForAuth c = do
         let authURL = "https://accounts.google.com/o/oauth2/auth?scope=https%3A%2F%2Fwww.googleapis.com%2Fauth%2Fuserinfo.email+https%3A%2F%2Fwww.googleapis.com%2Fauth%2Fuserinfo.email&state=%2Fprofile&redirect_uri=" ++ cfURL cf ++ "&response_type=code&client_id=" ++ cfClientID cf ++ "&approval_prompt=force"
         TLS.sendData c $ BLU.fromString $ "HTTP/1.1 302 Found\r\nLocation: " ++ authURL ++ "\r\nContent-Length: 0\r\n\r\n"

-- Check our access control list for this user's request and forward it to the backend if allowed.
requestWithEmail :: TLS.Context -> Request -> [String] -> String -> String -> IO ()
requestWithEmail c (method, url, headers, body) authorizedEmails email _ | email `elem` authorizedEmails = do
  -- TODO: Make the backend address configurable.
  -- TODO: Reuse connections to the backend server.
  h <- connectTo "127.0.0.1" $ PortNumber 8080
  BL.hPutStr h $ rawRequest (method, url, headers ++ [("From", BU.fromString email)], body)
  input <- BL.hGetContents h
  case oneResponse input of
    (Nothing, _) -> return () -- no more responses
    (Just response, _) -> TLS.sendData c $ rawResponse response
  hClose h
-- TODO: Send out a page that allows the user to request authorization.
requestWithEmail c _ _ _ _ = TLS.sendData c "HTTP/1.1 403 Forbidden\r\nContent-Length: 0\r\n\r\n"

log s = do
  tid <- myThreadId
  t <- getCurrentTime
  Log.debugM "sproxy" $ show tid ++ " " ++ show t ++ ": " ++ s

internalServerError c err = do
  log $ show err
  -- I wonder why Firefox fails to parse this correctly without a Content-Length header?
  TLS.sendData c "HTTP/1.1 500 Internal Server Error\r\nContent-Length: 0\r\n\r\n"

listen :: PortID -> (Handle -> IO ()) -> IO ()
listen port f = do
  s <- listenOn port
  forever $ do
    (h, _, _) <- accept s
    forkIO $ handle logError (f h)
 where logError :: SomeException -> IO ()
       logError e = log (show e) >> throw e

curl :: Curl.URLString -> [Curl.CurlOption] -> IO (Either String (Curl.CurlResponse_ [(String, String)] String))
curl url options = Curl.withCurlDo $ do
  c <- Curl.initialize
  r <- Curl.do_curl_ c url options
  if Curl.respCurlCode r /= Curl.CurlOK
    then return $ Left $ show (Curl.respCurlCode r) ++ " -- " ++ Curl.respStatusLine r
    else return $ Right r

post url fields = curl url $ Curl.CurlPostFields fields : Curl.method_POST

get url = curl url Curl.method_GET

connect host port = do
  proto <- BSD.getProtocolNumber "tcp"
  bracketOnError
    (Socket.socket Socket.AF_INET Socket.Stream proto)
    (Socket.sClose)
    (\sock -> do
       he <- BSD.getHostByName host
       Socket.connect sock (Socket.SockAddrInet port (BSD.hostAddress he))
       return sock)

-- Lazily read everything from a TLS context. Note that we don't check
-- for EOF and instead let termination throw an exception.
tlsGetContents :: TLS.Context -> IO BL.ByteString
tlsGetContents ctx = fmap BL.fromChunks lazyRead
 where lazyRead = unsafeInterleaveIO loop
       loop = do
         c <- TLS.recvData ctx
         cs <- lazyRead
         return (c:cs)
