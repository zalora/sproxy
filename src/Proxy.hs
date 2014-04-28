{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module Proxy (
  run

-- exported for testing
, Config(..)
, runProxy
) where

import Control.Concurrent (forkIO)
import Control.Exception
import Data.Typeable (typeOf)
import Control.Monad (forever, liftM)
import Crypto.Random (createEntropyPool, CPRG(..), SystemRNG)
import qualified Data.ByteString as B
import qualified Data.ByteString.UTF8 as BU
import qualified Data.ByteString.Lazy as BL
import Data.Default (def)
import Data.List (intercalate)
import Data.Maybe
import Data.Map as Map (fromList, toList, insert, delete)
import Data.String.Conversions (cs)
import qualified Data.X509 as X509
import Data.Yaml
import Network (PortID(..), listenOn, sClose, connectTo)
import Network.Socket (Socket, SockAddr, PortNumber, accept, close)
import qualified Network.Socket.ByteString as Socket
import Network.HTTP.Types
import qualified Network.TLS as TLS
import qualified Network.TLS.Extra as TLS
import qualified Network.URI as URI
import Options.Applicative hiding (action)
import System.IO

import Network.HTTP.Toolkit

import Type
import Util
import qualified Log
import Authenticate
import Cookies
import HTTP
import Authorize

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
data ConfigFile = ConfigFile {
  cfCookieDomain :: String
, cfCookieName :: String
, cfClientID :: String
, cfClientSecretFile :: FilePath
, cfAuthTokenKeyFile :: FilePath
, cfSslKey :: FilePath
, cfSslCerts :: FilePath
, cfDatabase :: String
, cfBackendAddress :: String
, cfBackendPort :: Integer
} deriving Show

instance FromJSON ConfigFile where
  parseJSON (Object m) = ConfigFile
    <$> m .: "cookie_domain"
    <*> m .: "cookie_name"
    <*> m .: "client_id"
    <*> m .: "client_secret"
    <*> m .: "auth_token_key"
    <*> m .: "ssl_key"
    <*> m .: "ssl_certs"
    <*> m .: "database"
    <*> m .: "backend_address"
    <*> m .: "backend_port"
  parseJSON _ = empty

data Config = Config {
  configTLSCredential :: TLS.Credential
, configBackendAddress :: String
, configBackendPort :: PortNumber
} deriving (Eq, Show)

-- | Reads the configuration file and the ssl certificate files and starts
-- the server
runWithOptions :: SProxyApp -> IO ()
runWithOptions opts = do
  Log.setup
  config' :: Either ParseException ConfigFile <- decodeFileEither (appConfigFile opts)
  case config' of
    Left err -> Log.debug $ ("error parsing configuration file " ++
        appConfigFile opts ++ ": " ++ show err)
    Right cf -> do
      clientSecret <- strip <$> readFile (cfClientSecretFile cf)
      authTokenKey <- readFile (cfAuthTokenKeyFile cf)
      credential <- either error reverseCerts `fmap` TLS.credentialLoadX509 (cfSslCerts cf) (cfSslKey cf)

      let authConfig = AuthConfig {
              authConfigCookieDomain = cfCookieDomain cf
            , authConfigCookieName = cfCookieName cf
            , authConfigClientID = cfClientID cf
            , authConfigClientSecret = clientSecret
            , authConfigAuthTokenKey = authTokenKey
            }
          config = Config {
              configTLSCredential = credential
            , configBackendAddress = cfBackendAddress cf
            , configBackendPort = fromInteger $ cfBackendPort cf
            }

      -- Immediately fork a new thread for accepting connections since
      -- the main thread is special and expensive to communicate with.

      _ <- forkIO (handle handleError (runProxy 443 config authConfig (withDatabaseAuthorizeAction . cs $ cfDatabase cf)))
      -- Listen on port 80 just to redirect everything to HTTPS.
      handle handleError (listen 80 redirectToHttps)
 where handleError :: SomeException -> IO ()
       handleError e = Log.debug $ show e
       -- Usually combined certs are in server, intermediate order,
       -- but the tls library expects them in the opposite order.
       reverseCerts (X509.CertificateChain certs, key) = (X509.CertificateChain $ reverse certs, key)

runProxy :: PortNumber -> Config -> AuthConfig -> WithAuthorizeAction -> IO ()
runProxy port config authConfig authorize = (listen port (serve config authConfig authorize))

-- | Redirects requests to https.
redirectToHttps :: SockAddr -> Socket -> IO ()
redirectToHttps _ sock = do
  conn <- makeConnection (Socket.recv sock 4096)
  request <- readRequest conn
  simpleResponse (Socket.sendAll sock) seeOther303 [("Location", cs $ show $ requestURI request)] ""
  where
    requestURI (Request _ path headers _) =
      let host = fromMaybe (error "Host header not found") $ lookup "Host" headers
      in fromJust $ URI.parseURI $ "https://" ++ cs host ++ cs path

-- | Actual server:
-- - ssl handshake
-- - google authentication
-- - our authorization
-- - redirecting requests to localhost:8080
serve :: Config -> AuthConfig -> WithAuthorizeAction -> SockAddr -> Socket -> IO ()
serve config authConfig withAuthorizeAction addr sock = do
  rng <- cprgCreate `liftM` createEntropyPool :: IO SystemRNG
  -- TODO: Work in the intermediate certificates.
  let params = def { TLS.serverShared = def { TLS.sharedCredentials = TLS.Credentials [configTLSCredential config] }
                   , TLS.serverSupported = def { TLS.supportedVersions = [TLS.SSL3, TLS.TLS10, TLS.TLS11, TLS.TLS12]
                                               , TLS.supportedCiphers = TLS.ciphersuite_all } }
  ctx <- TLS.contextNew sock params rng
  TLS.handshake ctx
  conn <- makeConnection (TLS.recvData ctx)
  withAuthorizeAction $ serve_ (TLS.sendData ctx . BL.fromStrict) conn
  TLS.bye ctx
  where
    serve_ :: SendData -> Connection -> AuthorizeAction -> IO ()
    serve_ send conn authorize = go
      where
        go :: IO ()
        go = forever $ readRequest conn >>= \request -> case request of
          Request _ url headers _ -> do
            -- TODO: Don't loop for more input on Connection: close header.
            -- Check if this is an authorization response.
            case URI.parseURIReference $ BU.toString url of
              Nothing -> internalServerError send "Failed to parse request URI"
              Just uri -> do
                let query = parseQuery $ BU.fromString $ URI.uriQuery uri
                -- This isn't a perfect test, but it's perfect for testing.
                case (URI.uriPath uri, lookup "state" query, lookup "code" query) of
                  ("/oauth2callback", Just (Just path), Just (Just code)) -> do
                    authenticate authConfig send request path code
                  _ -> do
                    -- Check for an auth cookie.
                    case removeCookie (authConfigCookieName authConfig) (parseCookies headers) of
                      Nothing -> redirectForAuth authConfig request send
                      Just (authCookie, cookies) -> do
                        auth <- validAuth authConfig authCookie
                        case auth of
                          Nothing -> redirectForAuth authConfig request send
                          Just token -> do
                            forwardRequest config send authorize cookies addr request token

-- Check our access control list for this user's request and forward it to the backend if allowed.
forwardRequest :: Config -> SendData -> AuthorizeAction -> [(Name, Cookies.Value)] -> SockAddr -> Request BodyReader -> AuthToken -> IO ()
forwardRequest config send authorize cookies addr request@(Request method path headers _) token = do
    groups <- authorize (authEmail token) (maybe (error "No Host") cs $ lookup "Host" headers) path method
    ip <- formatSockAddr addr
    case groups of
        [] -> do
            -- TODO: Send back a page that allows the user to request authorization.
            simpleResponse send forbidden403 [] "Access Denied"
        _ -> do
            -- TODO: Reuse connections to the backend server.
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
            bracket (connectTo host port) hClose $ \h -> do
              sendRequest (B.hPutStr h) request{requestHeaders = downStreamHeaders}
              conn <- connectionFromHandle h
              response <- readResponse method conn
              sendResponse send response{responseHeaders = removeConnectionHeader (responseHeaders response)}
  where
    host = configBackendAddress config
    port = PortNumber (configBackendPort config)
    setCookies = case cookies of
      [] -> delete hCookie
      _  -> insert hCookie (formatCookies cookies)

listen :: PortNumber -> (SockAddr -> Socket -> IO ()) -> IO ()
listen port action = bracket (listenOn $ PortNumber port) sClose $ \serverSock -> forever $ do
  (sock, addr) <- accept serverSock
  forkIO $ handle logError (action addr sock `finally` close sock)
 where logError :: SomeException -> IO ()
       logError (SomeException e) = Log.debug (show (typeOf e) ++ " (" ++ show e ++ ")")
