{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module Proxy (
  run

-- exported for testing
, Config(..)
, runProxy
) where

import Control.Applicative
import Control.Monad
import Control.Concurrent
import Control.Exception
import System.IO.Error
import GHC.IO.Exception
import Data.Monoid
import Data.Typeable (typeOf)
import Crypto.Random (createEntropyPool, CPRG(..), SystemRNG)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Data.Default (def)
import Data.List (intercalate)
import Data.Maybe
import Data.Map as Map (fromList, toList, insert, delete)
import Data.String.Conversions (cs)
import qualified Data.X509 as X509
import Network (PortID(..), listenOn, sClose, connectTo)
import Network.Socket (Socket, SockAddr, PortNumber, accept, close)
import qualified Network.Socket.ByteString as Socket
import Network.HTTP.Types
import qualified Network.TLS as TLS
import qualified Network.TLS.Extra as TLS
import System.IO
import Network.HTTP.Toolkit

import Type
import Util
import qualified Log
import Authenticate
import Cookies
import Authorize
import ConfigFile
import HTTP

data Config = Config {
  configTLSCredential :: TLS.Credential
, configBackendAddress :: String
, configBackendPort :: PortNumber
} deriving (Eq, Show)

-- | Reads the configuration file and the ssl certificate files and starts
-- the server
run :: ConfigFile -> AuthorizeAction -> IO ()
run cf authorize = do
  Log.setup (cfLogLevel cf)
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
        , configBackendPort = cfBackendPort cf
        }

  mvar <- newEmptyMVar

  -- Immediately fork a new thread for accepting connections since
  -- the main thread is special and expensive to communicate with.
  void . forkIO $ (runProxy (cfListen cf) config authConfig authorize `catch` logException) `finally` putMVar mvar ()

  -- Listen on port 80 just to redirect everything to HTTPS.
  when (cfRedirectHttpToHttps cf) $ do
    void . forkIO $ listen 80 redirectToHttps `catch` logException

  takeMVar mvar
  where
    -- Usually combined certs are in server, intermediate order,
    -- but the tls library expects them in the opposite order.
    reverseCerts (X509.CertificateChain certs, key) = (X509.CertificateChain $ reverse certs, key)

runProxy :: PortNumber -> Config -> AuthConfig -> AuthorizeAction -> IO ()
runProxy port config authConfig authorize = (listen port (serve config authConfig authorize))

-- | Redirects requests to https.
redirectToHttps :: SockAddr -> Socket -> IO ()
redirectToHttps _ sock = do
  conn <- makeInputStream (Socket.recv sock 4096)
  request <- readRequest True conn

  case baseUri (requestHeaders request) of
    Just uri -> do
      let location = uri <> requestPath request
      Log.debug ("Redirecting HTTP request to " ++ show location)
      simpleResponse send seeOther303 [("Location", location)] ""
    Nothing -> do
      hostHeaderMissing send request
  where
    send = Socket.sendAll sock

-- | Actual server:
-- - ssl handshake
-- - google authentication
-- - our authorization
-- - redirecting requests to localhost:8080
serve :: Config -> AuthConfig -> AuthorizeAction -> SockAddr -> Socket -> IO ()
serve config authConfig authorize addr sock = do
  rng <- cprgCreate <$> createEntropyPool :: IO SystemRNG
  -- TODO: Work in the intermediate certificates.
  let params = def { TLS.serverShared = def { TLS.sharedCredentials = TLS.Credentials [configTLSCredential config] }
                   , TLS.serverSupported = def { TLS.supportedVersions = [TLS.SSL3, TLS.TLS10, TLS.TLS11, TLS.TLS12]
                                               , TLS.supportedCiphers = TLS.ciphersuite_all } }
  ctx <- TLS.contextNew sock params rng
  TLS.handshake ctx
  conn <- makeInputStream (TLS.recvData ctx)
  serve_ (TLS.sendData ctx . BL.fromStrict) conn
  TLS.bye ctx
  where
    serve_ :: SendData -> InputStream -> IO ()
    serve_ send conn = go
      where
        -- TODO: Don't loop for more input on Connection: close header.
        -- Check if this is an authorization response.
        go :: IO ()
        go = forever $ do
          request@(Request _ path headers _) <- readRequest True conn
          case baseUri (requestHeaders request) of
            Nothing -> do
              hostHeaderMissing send request
            Just uri -> do
              let (segments, query) = (decodePath . extractPath) path
              let redirectPath = fromMaybe "/" $ join $ lookup "state" query
              case segments of
                ["sproxy", "oauth2callback"] -> do
                  case join $ lookup "code" query of
                    Nothing -> simpleResponse send badRequest400 [] "400 Bad Request"
                    Just code -> authenticate authConfig send uri redirectPath code
                ["sproxy", "logout"] -> do
                  logout authConfig send (uri <> redirectPath)
                _ -> do
                  -- Check for an auth cookie.
                  case removeCookie (authConfigCookieName authConfig) (parseCookies headers) of
                    Nothing -> redirectForAuth authConfig path uri send
                    Just (authCookie, cookies) -> do
                      auth <- validAuth authConfig authCookie
                      case auth of
                        Nothing -> redirectForAuth authConfig path uri send
                        Just token -> do
                          forwardRequest config send authorize cookies addr request token

-- Check our access control list for this user's request and forward it to the backend if allowed.
forwardRequest :: Config -> SendData -> AuthorizeAction -> [(Name, Cookies.Value)] -> SockAddr -> Request BodyReader -> AuthToken -> IO ()
forwardRequest config send authorize cookies addr request@(Request method path headers _) token = do
    groups <- authorize (authEmail token) (maybe (error "No Host") cs $ lookup "Host" headers) path method
    ip <- formatSockAddr addr
    case groups of
        [] -> accessDenied send (authEmail token)
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
              conn <- inputStreamFromHandle h
              response <- readResponse True method conn
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
  forkIO $ (action addr sock `finally` close sock) `catch` exceptionHandler
  where
    exceptionHandler e
      | e `isException` UnexpectedEndOfInput || e `isException` TLS.HandshakeFailed TLS.Error_EOF || isResourceVanished e =
          Log.debug ("client closed connection (" ++ show e ++ ")")
      | otherwise = logException e

logException :: SomeException -> IO ()
logException (SomeException e) = Log.error (show (typeOf e) ++ " (" ++ show e ++ ")")

isException :: (Eq a, Exception a) => SomeException -> a -> Bool
isException e v = fromMaybe False $ (== v) <$> fromException e

isResourceVanished :: SomeException -> Bool
isResourceVanished = fromMaybe False . fmap ((== ResourceVanished) . ioeGetErrorType) . fromException
