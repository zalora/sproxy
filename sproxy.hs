{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Cookies
import HTTP
import Permissions

import Control.Applicative ((<$>), (<*>))
import Control.Concurrent (forkIO, newEmptyMVar, putMVar, takeMVar, myThreadId)
import Control.Exception (finally, bracketOnError, handle, throw, SomeException)
import Control.Monad (forever, mzero, join, liftM, when)
import Control.Monad.Error (runErrorT)
import Control.Monad.Trans (liftIO)
import Crypto.Random (createEntropyPool, CPRG(..), SystemRNG)
import qualified Data.Aeson as Aeson
import Data.Aeson (parseJSON)
import qualified Data.ByteString.UTF8 as BU
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.UTF8 as BLU
import Data.Certificate.X509 (X509)
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
import Options.Applicative
import System.IO (Handle, stdin, hClose, openFile, IOMode (ReadMode))
import System.IO.Unsafe (unsafeInterleaveIO)
import qualified System.Log.Logger as Log
import Data.Yaml
import Data.String.Conversions

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
                     , cfSslKey :: FilePath
                     , cfSslCert :: FilePath
                     , cfSslExtraCerts :: [FilePath]
                     , cfPermissions :: Permissions
                     }
  deriving Show

instance FromJSON Config where
    parseJSON o@(Object m) = Config <$>
        m .: "domain" <*>
        m .: "contact" <*>
        m .: "url" <*>
        m .: "client_id" <*>
        m .: "client_secret" <*>
        m .: "auth_token_key" <*>
        m .: "ssl_key" <*>
        m .: "ssl_cert" <*>
        m .: "ssl_extra_certs" <*>
        parseJSON o
    parseJSON _ = mzero


type Certificates = [(X509, Maybe TLS.PrivateKey)]

loadSSLCertificates :: Config -> IO Certificates
loadSSLCertificates config = do
    sslKey <- TLS.fileReadPrivateKey (cfSslKey config)
    sslCert <- TLS.fileReadCertificate (cfSslCert config)
    sslExtraCerts <- mapM TLS.fileReadCertificate (cfSslExtraCerts config)
    return $ (sslCert, Just sslKey) : (map (\x -> (x, Nothing)) sslExtraCerts)



data SProxyApp = SProxyApp {
  appConfigFile :: FilePath
}

main :: IO ()
main = execParser opts >>= runWithOptions
  where
    parser = SProxyApp <$> strOption (long "config" <>
                                      noArgError ShowHelpText <>
                                      metavar "CONFIG" <>
                                      help "config file path")
    opts = info parser (fullDesc <> progDesc "sproxy: proxy for single sign-on")


runWithOptions :: SProxyApp -> IO ()
runWithOptions opts = do
  -- Make sure we have all necessary config options. Read them from the given
  -- config file.

  Log.updateGlobalLogger "sproxy" (Log.setLevel Log.DEBUG)
  config' :: Either ParseException Config <- decodeFileEither (appConfigFile opts)
  case config' of
    Left err -> log $ ("error parsing configuration file " ++
        appConfigFile opts ++ ": " ++ show err)
    Right config -> do
      certificates <- loadSSLCertificates config
      -- Immediately fork a new thread for accepting connections since
      -- the main thread is special and expensive to communicate with.
      wait <- newEmptyMVar
      forkIO (handle handleError (listen (PortNumber 443) (serve config certificates)) `finally` (putMVar wait ()))
      -- Listen on port 80 just to redirect everything to HTTPS.
      forkIO (handle handleError (listen (PortNumber 80) (redirectToHttps config)))
      takeMVar wait
 where handleError :: SomeException -> IO ()
       handleError e = log $ show e

redirectToHttps :: Config -> Handle -> IO ()
redirectToHttps cf h = do
  input <- BL.hGetContents h
  case oneRequest input of
    (Nothing, _) -> return ()
    (Just _, _) -> do
      -- For now, don't even try to intelligently redirect the request,
      -- just send them to the URL from the config. Maybe later we can do
      -- something fancier.
      BL.hPutStr h $ rawResponse $ response 302 "Found" [("Location", BU.fromString $ cfURL cf)] ""

serve :: Config -> Certificates -> Handle -> IO ()
serve cf certificates h = do
  rng <- cprgCreate `liftM` createEntropyPool :: IO SystemRNG
  let params = TLS.defaultParamsServer { TLS.pCertificates = certificates
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
           (Just request@(method, url, headers, _), rest) -> do
             -- TODO: Don't loop for more input on Connection: close header.
             log $ show (method, url, headers)
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
                       Right resp -> do
                         case Aeson.decode $ BLU.fromString $ Curl.respBody resp of
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
                                     let cookie = setCookie (HTTP.MkCookie (cfDomain cf) "gauth" (show clientToken) Nothing Nothing Nothing) authShelfLife
                                         resp' = response 302 "Found" [("Location", BU.fromString $ cfURL cf), ("Set-Cookie", BU.fromString cookie)] ""
                                     TLS.sendData c $ rawResponse resp'
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
                             continue <- requestWithEmail c request (cfPermissions cf) (authEmail token) (cfContact cf)
                             when continue $ serve' c rest
       redirectForAuth c = do
         let authURL = "https://accounts.google.com/o/oauth2/auth?scope=https%3A%2F%2Fwww.googleapis.com%2Fauth%2Fuserinfo.email+https%3A%2F%2Fwww.googleapis.com%2Fauth%2Fuserinfo.email&state=%2Fprofile&redirect_uri=" ++ cfURL cf ++ "&response_type=code&client_id=" ++ cfClientID cf ++ "&approval_prompt=force"
         TLS.sendData c $ rawResponse $ response 302 "Found" [("Location", BU.fromString $ authURL)] ""

-- Check our access control list for this user's request and forward it to the backend if allowed.
requestWithEmail :: TLS.Context -> Request -> Permissions -> String -> String -> IO (Bool)
requestWithEmail c (method, url, headers, body) permissions email _ =
    case isAuthorized permissions email (fmap cs $ lookup "Host" headers) (cs url) of
        Right () -> do
            -- TODO: Make the backend address configurable.
            -- TODO: Reuse connections to the backend server.
            h <- connectTo "127.0.0.1" $ PortNumber 8080
            BL.hPutStr h $ rawRequest (method, url, headers ++ [("From", BU.fromString email)], body)
            input <- BL.hGetContents h
            continue <- case oneResponse input of
                (Nothing, _) -> return False -- no more responses
                (Just resp@(_, headers', _), _) -> do
                    TLS.sendData c $ rawResponse resp
                    return $ lookup "Connection" headers' /= Just "close"
            hClose h
            return continue
        -- TODO: Send out a page that allows the user to request authorization.
        Left permissionError -> do
            log ("authentication failed: " ++ permissionError)
            TLS.sendData c $ rawResponse $ response 403 "Forbidden" [] "Access Denied"
            return True

log s = do
  tid <- myThreadId
  t <- getCurrentTime
  Log.debugM "sproxy" $ show tid ++ " " ++ show t ++ ": " ++ s

internalServerError c err = do
  log $ show err
  -- I wonder why Firefox fails to parse this correctly without a Content-Length header?
  TLS.sendData c $ rawResponse $ response 500 "Internal Server Error" [] "Internal Server Error"

listen :: PortID -> (Handle -> IO ()) -> IO ()
listen port f = do
  s <- listenOn port
  putStrLn ("listening on port " ++ show port)
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
