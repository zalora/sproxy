{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module Proxy (
  run

-- exported to silence warnings
, AccessToken(..)
, ConfigFile(..)

-- exported for testing
, runProxy
) where

import Control.Concurrent (forkIO, myThreadId)
import Control.Exception
import Data.Typeable (typeOf)
import Control.Monad (forever, mzero, liftM, when)
import Crypto.Random (createEntropyPool, CPRG(..), SystemRNG)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.UTF8 as BU
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.UTF8 as BLU
import Data.Default (def)
import Data.List (intercalate)
import Data.Time.Clock (getCurrentTime)
import Data.Maybe
import Data.Map as Map (fromList, toList, insert, delete)
import Data.String.Conversions (cs)
import qualified Data.X509 as X509
import Data.Yaml
import Network (PortID(..), listenOn, sClose, connectTo)
import Network.Socket (SockAddr, PortNumber, accept, socketToHandle)
import qualified Network.Curl as Curl
import qualified Network.HTTP.Cookie as HTTP
import Network.HTTP.Types (hCookie, urlEncode, urlDecode)
import qualified Network.HTTP.Types.URI as Query
import qualified Network.TLS as TLS
import qualified Network.TLS.Extra as TLS
import qualified Network.URI as URI
import Options.Applicative hiding (action)
import System.IO
import System.IO.Unsafe (unsafeInterleaveIO)
import qualified System.Log.Logger as Log

import Prelude hiding (log)

import Util
import ConfigFile
import Cookies
import HTTP
import Authorize

-- * These are JSON responses that come from Google.

data AccessToken = AccessToken {accessToken :: String, expiresIn :: Integer, tokenType :: String}
instance Aeson.FromJSON AccessToken where
  parseJSON (Aeson.Object v) = AccessToken <$>
                               v Aeson..: "access_token" <*>
                               v Aeson..: "expires_in" <*>
                               v Aeson..: "token_type"
  parseJSON _ = mzero

data UserInfo = UserInfo {userEmail :: String, userGivenName :: String, userFamilyName :: String} deriving (Show)
instance Aeson.FromJSON UserInfo where
  parseJSON (Aeson.Object v) = UserInfo <$>
                               v Aeson..: "email" <*>
                               v Aeson..: "given_name" <*>
                               v Aeson..: "family_name"
  parseJSON _ = mzero

-- https://wiki.zalora.com/Main_Page -> https://wiki.zalora.com/
-- Note that this always uses https:
rootURI :: Request -> URI.URI
rootURI (Request _ _ headers _) =
  let host = cs $ fromMaybe (error "Host header not found") $ lookup "Host" headers
  in URI.URI "https:" (Just $ URI.URIAuth "" host "") "/" "" ""

-- Note that this is always used for redirects and hence always uses https:
requestURI :: Request -> URI.URI
requestURI (Request _ path headers _) =
  let host = fromMaybe (error "Host header not found") $ lookup "Host" headers
  in fromJust $ URI.parseURI $ "https://" ++ cs host ++ cs path

-- * command line options

data SProxyApp = SProxyApp {
  appConfigFile :: FilePath
}

run :: IO ()
run = execParser opts >>= runWithOptions
  where
    parser = SProxyApp <$> strOption (long "config" <>
                                      noArgError ShowHelpText <>
                                      metavar "CONFIG" <>
                                      value "config/sproxy.yml" <>
                                      help "config file path")
    opts = info parser (fullDesc <> progDesc "sproxy: proxy for single sign-on")


-- * main functionality


-- | Reads the configuration file and the ssl certificate files and starts
-- the server
runWithOptions :: SProxyApp -> IO ()
runWithOptions opts = do
  -- Make sure we have all necessary config options. Read them from the given
  -- config file.

  Log.updateGlobalLogger "sproxy" (Log.setLevel Log.DEBUG)
  config' :: Either ParseException ConfigFile <- decodeFileEither (appConfigFile opts)
  case config' of
    Left err -> log $ ("error parsing configuration file " ++
        appConfigFile opts ++ ": " ++ show err)
    Right config -> do
      clientSecret <- strip <$> readFile (cfClientSecretFile config)
      authTokenKey <- readFile (cfAuthTokenKeyFile config)
      credential <- either error reverseCerts `fmap` TLS.credentialLoadX509 (cfSslCerts config) (cfSslKey config)
      -- Immediately fork a new thread for accepting connections since
      -- the main thread is special and expensive to communicate with.

      _ <- forkIO (handle handleError (runProxy 443 config credential clientSecret authTokenKey (withDatabaseAuthorizeAction . cs $ cfDatabase config)))
      -- Listen on port 80 just to redirect everything to HTTPS.
      handle handleError (listen 80 redirectToHttps)
 where handleError :: SomeException -> IO ()
       handleError e = log $ show e
       -- Usually combined certs are in server, intermediate order,
       -- but the tls library expects them in the opposite order.
       reverseCerts (X509.CertificateChain certs, key) = (X509.CertificateChain $ reverse certs, key)

runProxy :: PortNumber -> ConfigFile -> TLS.Credential -> String -> String -> WithAuthorizeAction -> IO ()
runProxy port config credential clientSecret authTokenKey authorize = (listen port (serve config credential clientSecret authTokenKey authorize))

-- | Redirects requests to https.
redirectToHttps :: SockAddr -> Handle -> IO ()
redirectToHttps _ h = do
  input <- BL.hGetContents h
  case oneRequest input of
    (Nothing, _) -> return ()
    (Just request, _) -> BL.hPutStr h $ rawResponse $ response 303 "See Other" [("Location", cs $ show $ requestURI request)] ""

type SendData = BL.ByteString -> IO ()

-- | Actual server:
-- - ssl handshake
-- - google authentication
-- - our authorization
-- - redirecting requests to localhost:8080
serve :: ConfigFile -> TLS.Credential -> String -> String -> WithAuthorizeAction -> SockAddr -> Handle -> IO ()
serve cf credential clientSecret authTokenKey withAuthorizeAction addr h = do
  rng <- cprgCreate `liftM` createEntropyPool :: IO SystemRNG
  -- TODO: Work in the intermediate certificates.
  let params = def { TLS.serverShared = def { TLS.sharedCredentials = TLS.Credentials [credential] }
                   , TLS.serverSupported = def { TLS.supportedVersions = [TLS.SSL3, TLS.TLS10, TLS.TLS11, TLS.TLS12]
                                               , TLS.supportedCiphers = TLS.ciphersuite_all } }
  ctx <- TLS.contextNew h params rng
  TLS.handshake ctx
  input <- tlsGetContents ctx
  withAuthorizeAction $ do
    \authorize -> serve_ (TLS.sendData ctx) authorize input
  TLS.bye ctx
  where
    serve_ :: SendData -> AuthorizeAction -> BL.ByteString -> IO ()
    serve_ send authorize = go
      where
        go :: BL.ByteString -> IO ()
        go input = case oneRequest input of
          (Nothing, _) -> return () -- no more requests
          (Just request@(Request _ url headers _), rest) -> do
            -- TODO: Don't loop for more input on Connection: close header.
            -- Check if this is an authorization response.
            case URI.parseURIReference $ BU.toString url of
              Nothing -> internalServerError send "Failed to parse request URI" >> go rest
              Just uri -> do
                let query = Query.parseQuery $ BU.fromString $ URI.uriQuery uri
                -- This isn't a perfect test, but it's perfect for testing.
                case (lookup "state" query, lookup "code" query) of
                  (Just (Just path), Just (Just code)) -> do
                    authenticate authTokenKey clientSecret cf send request path code >> go rest
                  _ -> do
                    -- Check for an auth cookie.
                    case removeCookie (cfCookieName cf) (parseCookies headers) of
                      Nothing -> redirectForAuth cf request send >> go rest
                      Just (authCookie, cookies) -> do
                        auth <- validAuth authTokenKey authCookie
                        case auth of
                          Nothing -> redirectForAuth cf request send >> go rest
                          Just token -> do
                            continue <- forwardRequest cf send authorize cookies addr request token
                            when continue $ go rest

redirectForAuth :: ConfigFile -> Request -> SendData -> IO ()
redirectForAuth cf request send = do
  let redirectUri = rootURI request
      path = urlEncode True (requestPath request)
      authURL = "https://accounts.google.com/o/oauth2/auth?scope=https%3A%2F%2Fwww.googleapis.com%2Fauth%2Fuserinfo.email+https%3A%2F%2Fwww.googleapis.com%2Fauth%2Fuserinfo.profile&state=" ++ cs path ++ "&redirect_uri=" ++ (cs $ show $ redirectUri) ++ "&response_type=code&client_id=" ++ cfClientID cf ++ "&approval_prompt=force&access_type=offline"
  send . rawResponse $ response 302 "Found" [("Location", BU.fromString $ authURL)] ""

authenticate :: String -> String -> ConfigFile -> SendData -> Request -> BS.ByteString -> BS.ByteString -> IO ()
authenticate authTokenKey clientSecret cf send request path code = do
  tokenRes <- post "https://accounts.google.com/o/oauth2/token" ["code=" ++ BU.toString code, "client_id=" ++ cfClientID cf, "client_secret=" ++ clientSecret, "redirect_uri=" ++ (cs $ show $ rootURI request), "grant_type=authorization_code"]
  case tokenRes of
    Left err -> internalServerError send err
    Right resp -> do
      case Aeson.decode $ BLU.fromString $ Curl.respBody resp of
        Nothing -> do
          internalServerError send "Received an invalid response from Google's authentication server."
        Just token -> do
          infoRes <- get $ "https://www.googleapis.com/oauth2/v1/userinfo?access_token=" ++ accessToken token
          case infoRes of
            Left err -> internalServerError send err
            Right i -> do
              case Aeson.decode $ BLU.fromString $ Curl.respBody i of
                Nothing -> internalServerError send "Received an invalid user info response from Google's authentication server."
                Just userInfo -> do
                  clientToken <- authToken authTokenKey (userEmail userInfo) (userGivenName userInfo, userFamilyName userInfo)
                  let cookie = setCookie (HTTP.MkCookie cookieDomain cookieName (show clientToken) Nothing Nothing Nothing) authShelfLife
                      resp' = response 302 "Found" [("Location", cs $ (show $ (rootURI request) {URI.uriPath = ""}) ++ cs (urlDecode False path)), ("Set-Cookie", BU.fromString cookie)] ""
                  send $ rawResponse resp'
  where
    cookieDomain = cfCookieDomain cf
    cookieName = cfCookieName cf

-- Check our access control list for this user's request and forward it to the backend if allowed.
forwardRequest :: ConfigFile -> SendData -> AuthorizeAction -> [(Name, Cookies.Value)] -> SockAddr -> Request -> AuthToken -> IO Bool
forwardRequest cf send authorize cookies addr (Request method path headers body) token = do
    groups <- authorize (authEmail token) (maybe (error "No Host") cs $ lookup "Host" headers) path method
    ip <- formatSockAddr addr
    case groups of
        [] -> do
            -- TODO: Send back a page that allows the user to request authorization.
            send . rawResponse $ response 403 "Forbidden" [] "Access Denied"
            return True
        _ -> do
            -- TODO: Reuse connections to the backend server.
            h <- connectTo (cfBackendAddress cf) (PortNumber $ fromIntegral $ cfBackendPort cf)
            let downStreamHeaders =
                    toList $
                    insert "From" (cs $ authEmail token) $
                    insert "X-Groups" (cs $ intercalate "," groups) $
                    insert "X-Given-Name" (cs $ fst $ authName token) $
                    insert "X-Family-Name" (cs $ snd $ authName token) $
                    addForwardedForHeader ip $
                    insert "Connection" "close" $
                    setCookies $
                    fromList headers
            BL.hPutStr h $ rawRequest (Request method path downStreamHeaders body)
            input <- BL.hGetContents h
            continue <- case oneResponse input of
                (Nothing, _) -> return False
                (Just (status, headers_, body_), _) -> do
                    send $ rawResponse (status, removeConnectionHeader headers_, body_)
                    return True
            hClose h
            return continue
  where
    setCookies = case cookies of
      [] -> delete hCookie
      _  -> insert hCookie (formatCookies cookies)

log :: String -> IO ()
log s = do
  tid <- myThreadId
  t <- getCurrentTime
  Log.debugM "sproxy" $ show tid ++ " " ++ show t ++ ": " ++ s

internalServerError :: SendData -> String -> IO ()
internalServerError send err = do
  log $ show err
  -- I wonder why Firefox fails to parse this correctly without a Content-Length header?
  send . rawResponse $ response 500 "Internal Server Error" [] "Internal Server Error"

listen :: PortNumber -> (SockAddr -> Handle -> IO ()) -> IO ()
listen port action = bracket (listenOn $ PortNumber port) sClose $ \s -> forever $ do
  (clientSocket, addr) <- accept s
  h <- socketToHandle clientSocket ReadWriteMode
  forkIO $ handle logError (action addr h `finally` hClose h)
 where logError :: SomeException -> IO ()
       logError (SomeException e) = log (show (typeOf e) ++ " (" ++ show e ++ ")")

curl :: Curl.URLString -> [Curl.CurlOption] -> IO (Either String (Curl.CurlResponse_ [(String, String)] String))
curl url options = Curl.withCurlDo $ do
  c <- Curl.initialize
  r <- Curl.do_curl_ c url options
  if Curl.respCurlCode r /= Curl.CurlOK
    then return $ Left $ show (Curl.respCurlCode r) ++ " -- " ++ Curl.respStatusLine r
    else return $ Right r

post :: Curl.URLString -> [String] -> IO (Either String (Curl.CurlResponse_ [(String, String)] String))
post url fields = curl url $ Curl.CurlPostFields fields : Curl.method_POST

get :: Curl.URLString -> IO (Either String (Curl.CurlResponse_ [(String, String)] String))
get url = curl url Curl.method_GET

-- Lazily read everything from a TLS context. Note that we don't check
-- for EOF and instead let termination throw an exception.
tlsGetContents :: TLS.Context -> IO BL.ByteString
tlsGetContents ctx = fmap BL.fromChunks lazyRead
 where lazyRead = unsafeInterleaveIO loop
       loop = do
         x <- TLS.recvData ctx
         xs <- lazyRead
         return (x:xs)
