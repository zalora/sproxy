{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module Proxy (
  run
) where

import Control.Monad hiding (forM_)
import Data.Foldable (forM_)
import Control.Concurrent
import Control.Exception
import System.IO.Error
import GHC.IO.Exception
import Data.Monoid
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy as BL
import Data.Default (def)
import Data.List (intercalate)
import Data.Maybe
import Data.Map as Map (fromListWith, toList, insert, delete)
import Data.String.Conversions (cs)
import qualified Data.X509 as X509
import Network (PortID(..), connectTo)
import Network.Socket (accept, close, socket, listen, bind, maxListenQueue,
                       getSocketName, Socket, SockAddr(SockAddrInet),
                       Family(AF_INET), SocketType(Stream),
                       setSocketOption, SocketOption(ReuseAddr))
import qualified Network.Socket.ByteString as Socket
import Network.HTTP.Types
import qualified Network.TLS as TLS
import qualified Network.TLS.Extra as TLS
import System.IO
import System.Entropy (getEntropy)
import qualified Data.ByteString.Base64 as Base64
import Network.HTTP.Toolkit

import qualified System.Logging.Facade as Log

import Type
import Util
import Logging
import Authenticate
import Cookies
import Authorize
import ConfigFile
import HTTP

data Config = Config {
  configTLSCredential :: TLS.Credential
, configBackendAddress :: String
, configBackendPortID :: PortID
} deriving (Eq, Show)

-- | Reads the configuration file and the ssl certificate files and starts
-- the server
run :: ConfigFile -> AuthorizeAction -> IO ()
run cf authorize = do
  Logging.setup (cfLogLevel cf) (cfLogTarget cf)
  clientSecret <- strip <$> readFile (cfClientSecretFile cf)
  authTokenKey <- B8.unpack . Base64.encode <$> getEntropy 32
  credential <- either error reverseCerts <$> TLS.credentialLoadX509 (cfSslCerts cf) (cfSslKey cf)

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
        , configBackendPortID =
            case cfBackendSocket cf of
              Just path -> UnixSocket path
              _ -> PortNumber . fromIntegral $ cfBackendPort cf
        }

  case configBackendPortID config of
    UnixSocket path -> Log.info $ "Backend on UNIX socket " ++ path
    PortNumber p -> Log.info $ "Backend on "
                       ++ configBackendAddress config ++ ":"
                       ++ show p
    _ -> return () -- XXX can't happen

  mvar <- newEmptyMVar

  sock <- socket AF_INET Stream 0
  setSocketOption sock ReuseAddr 1
  bind sock $ SockAddrInet (fromIntegral $ cfListen cf) 0
  void . forkIO $ (runProxy sock config authConfig authorize `catch` logException)
                 `finally` (close sock >> putMVar mvar ())

  let listen80 = fromMaybe (443 == cfListen cf) (cfRedirectHttpToHttps cf)
  when listen80 $ do
    sock80 <- socket AF_INET Stream 0
    setSocketOption sock80 ReuseAddr 1
    bind sock80 $ SockAddrInet 80 0
    void . forkIO $ (runOnSocket sock80 redirectToHttps `catch` logException)
                    `finally` close sock80

  takeMVar mvar
  where
    -- Usually combined certs are in server, intermediate order,
    -- but the tls library expects them in the opposite order.
    reverseCerts (X509.CertificateChain certs, key) = (X509.CertificateChain $ reverse certs, key)

runProxy :: Socket -> Config -> AuthConfig -> AuthorizeAction -> IO ()
runProxy serverSock config authConfig authorize =
  runOnSocket serverSock (serve config authConfig authorize)

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
    Nothing -> hostHeaderMissing request >>= sendResponse send
  where
    send = Socket.sendAll sock

-- | Actual server:
-- - ssl handshake
-- - google authentication
-- - our authorization
-- - redirecting requests to localhost:8080
serve :: Config -> AuthConfig -> AuthorizeAction -> SockAddr -> Socket -> IO ()
serve config authConfig authorize addr sock = do
  -- TODO: Work in the intermediate certificates.
  let params = def { TLS.serverShared = def { TLS.sharedCredentials = TLS.Credentials [configTLSCredential config] }
                   , TLS.serverSupported = def { TLS.supportedVersions = [TLS.TLS10, TLS.TLS11, TLS.TLS12]
                                               , TLS.supportedCiphers = TLS.ciphersuite_all } }
  ctx <- TLS.contextNew sock params
  TLS.handshake ctx
  conn <- makeInputStream (TLS.recvData ctx)
  serve_ (TLS.sendData ctx . BL.fromStrict) conn
  TLS.bye ctx
  where
    serve_ :: SendData -> InputStream -> IO ()
    serve_ send conn = go
      where
        go :: IO ()
        go = do
          continue <- handleOne
          when continue go

        handleOne :: IO Bool
        handleOne = do
          request@(Request _ path headers _) <- readRequest True conn
          mResponse <- case baseUri (requestHeaders request) of
            Nothing -> Just <$> hostHeaderMissing request
            Just uri -> do
              let (segments, query) = (decodePath . extractPath) path
              let redirectPath = fromMaybe "/" $ join $ lookup "state" query
              case segments of
                ["sproxy", "oauth2callback"] ->
                  case join $ lookup "code" query of
                    Nothing -> Just <$> badRequest
                    Just code -> Just <$> authenticate authConfig uri redirectPath code
                ["sproxy", "logout"] ->
                  Just <$> logout authConfig (uri <> redirectPath)
                -- sproxy sites are private by design. It doesn't make sense to index the authentication page.
                ["robots.txt"] -> Just <$> mkTextResponse ok200 "User-agent: *\nDisallow: /"
                _ -> -- Check for an auth cookie.
                  case removeCookie (authConfigCookieName authConfig) (parseCookies headers) of
                    Nothing -> Just <$> redirectForAuth authConfig path uri
                    Just (authCookie, cookies) -> do
                      auth <- validAuth authConfig authCookie
                      case auth of
                        Nothing -> Just <$> redirectForAuth authConfig path uri
                        Just token ->
                          forwardRequest config send authorize cookies addr request token
          forM_ mResponse (sendResponse send)
          return ((not . isConnectionClose) headers)

-- Check our access control list for this user's request and forward it to the backend if allowed.
forwardRequest :: Config -> SendData -> AuthorizeAction -> [(Name, Cookies.Value)] -> SockAddr -> Request BodyReader -> AuthToken -> IO (Maybe (Response BodyReader))
forwardRequest config send authorize cookies addr request@(Request method path headers _) token = do
    groups <- authorize (authEmail token) (maybe (error "No Host") cs $ lookup "Host" headers) path method
    ip <- formatSockAddr addr
    case groups of
        [] -> Just <$> accessDenied (authEmail token)
        _ -> do
            -- TODO: Reuse connections to the backend server.
            let downStreamHeaders =
                    toList $
                    insert "From" (cs $ authEmail token) $
                    insert "X-Groups" (cs $ intercalate "," groups) $
                    insert "X-Given-Name" (cs $ fst $ authName token) $
                    insert "X-Family-Name" (cs $ snd $ authName token) $
                    insert "X-Forwarded-Proto" "https" $
                    addForwardedForHeader ip $
                    insert "Connection" "close" $
                    setCookies $ fromListWith (\a b -> B8.concat [a, ",", b]) headers
            Log.debug $ show downStreamHeaders
            bracket (connectTo host portID) hClose $ \h -> do
              sendRequest (B8.hPutStr h) request{requestHeaders = downStreamHeaders}
              conn <- inputStreamFromHandle h
              response <- readResponse True method conn
              sendResponse send response{responseHeaders = removeConnectionHeader (responseHeaders response)}
              return Nothing
  where
    host = configBackendAddress config
    portID = configBackendPortID config
    setCookies = case cookies of
      [] -> delete hCookie
      _  -> insert hCookie (formatCookies cookies)

runOnSocket :: Socket -> (SockAddr -> Socket -> IO ()) -> IO ()
runOnSocket serverSock action = do
  listen serverSock maxListenQueue
  name <- getSocketName serverSock
  Log.info $ "Listening on " ++ show name
  forever $ do
    (sock, addr) <- accept serverSock
    forkIO $ (action addr sock `finally` close sock) `catches` handlers
    where
      handlers = [ Handler ioH
                 , Handler toolkitH
                 , Handler tlsH
                 , Handler logException ]

      ioH :: IOException -> IO ()
      ioH e
        | ResourceVanished == ioeGetErrorType e = clientClosedConection e
        | otherwise = logException' e

      toolkitH :: ToolkitError -> IO ()
      toolkitH e@UnexpectedEndOfInput = clientClosedConection e
      toolkitH e = logException' e

      tlsH :: TLS.TLSException -> IO ()
      tlsH e@(TLS.HandshakeFailed TLS.Error_EOF) = clientClosedConection e
      tlsH e@(TLS.HandshakeFailed (TLS.Error_Protocol (_, _, _))) = clientError e
      tlsH e@(TLS.HandshakeFailed (TLS.Error_Packet_Parsing _)) = clientError e
      tlsH e = logException' e

      logException' :: Exception e => e -> IO ()
      logException' = logException . toException

      clientClosedConection :: Exception e => e -> IO ()
      clientClosedConection e = Log.debug ("client closed connection (" ++ displayException e ++ ")")

      clientError :: Exception e => e -> IO ()
      clientError e = Log.debug ("client error (" ++ displayException e ++ ")")

logException :: SomeException -> IO ()
logException = Log.error . displayException

