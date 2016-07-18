{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Proxy (
  run
) where

import Control.Concurrent
import Control.Exception
import Control.Monad hiding (forM_)
import Data.Default (def)
import Data.Foldable (forM_)
import Data.List (intercalate)
import Data.Map as Map (fromListWith, toList, insert, delete)
import Data.Maybe
import Data.Monoid
import GHC.IO.Exception
import Network (PortID(..), connectTo)
import Network.HTTP.Toolkit
import Network.HTTP.Types
import Network.Socket (accept, close, socket, listen, bind,
  maxListenQueue, getSocketName, Socket, SockAddr(SockAddrInet),
  Family(AF_INET), SocketType(Stream), setSocketOption,
  SocketOption(ReuseAddr))
import System.Entropy (getEntropy)
import System.IO
import System.IO.Error
import System.Posix.Directory (changeWorkingDirectory)
import System.Posix.User (getRealUserID, setGroupID, setUserID,
  getUserEntryForName, UserEntry(..), GroupEntry(..), setGroups,
  getAllGroupEntries)
import Foreign.C.Types (CTime(..))
import qualified Data.ByteString.Base64 as Base64
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy as BL
import qualified Data.X509 as X509
import qualified Network.Socket.ByteString as Socket
import qualified Network.TLS as TLS
import qualified Network.TLS.Extra as TLS

import Authenticate (logout, authenticationRequired, validAuth)
import Authenticate.Token (AuthToken(..), AuthUser(..))
import Authenticate.Types (AuthConfig(..), OAuthClient(..))
import Authorize
import ConfigFile
import Cookies
import HTTP
import Type
import Util
import qualified Authenticate.Google as Google
import qualified Authenticate.LinkedIn as LinkedIn
import qualified Logging as Log

data Config = Config {
  configTLSCredential :: TLS.Credential
, configBackendAddress :: String
, configBackendPortID :: PortID
} deriving (Eq, Show)

-- | Reads the configuration file and the ssl certificate files and starts
-- the server
run :: ConfigFile -> AuthorizeAction -> IO ()
run cf authorize = do
  Log.start (cfLogLevel cf)

  sock <- socket AF_INET Stream 0
  setSocketOption sock ReuseAddr 1
  bind sock $ SockAddrInet (fromIntegral $ cfListen cf) 0

  sock80 <- if fromMaybe (443 == cfListen cf) (cfRedirectHttpToHttps cf)
    then do
      s <- socket AF_INET Stream 0
      setSocketOption s ReuseAddr 1
      bind s $ SockAddrInet 80 0
      return (Just s)
    else
      return Nothing

  uid <- getRealUserID
  when (0 == uid) $ do
    let user = cfUser cf
    u <- getUserEntryForName user
    groupIDs <- map groupID . filter (\g -> user `elem` groupMembers g)
             <$> getAllGroupEntries
    Log.info $ "Switching to user " ++ show user
    setGroups groupIDs
    setGroupID $ userGroupID u
    setUserID $ userID u
    changeWorkingDirectory (homeDirectory u)
      `catch` (\e -> logException e >> changeWorkingDirectory "/")

  authTokenKey <- B8.unpack . Base64.encode <$> getEntropy 32
  credential <- either error reverseCerts <$> TLS.credentialLoadX509 (cfSslCerts cf) (cfSslKey cf)

  googleCLient <- mkOAuthClient (cfGoogleClientID cf) (cfGoogleClientSecretFile cf)
  linkedInCLient <- mkOAuthClient (cfLinkedInClientID cf) (cfLinkedInClientSecretFile cf)

  let authConfig = AuthConfig {
          authConfigCookieDomain = cfCookieDomain cf
        , authConfigCookieName = cfCookieName cf
        , authConfigAuthTokenKey = authTokenKey
        , authConfigShelfLife = CTime . fromIntegral $ cfSessionShelfLife cf
        , authConfigGoogleClient = googleCLient
        , authConfigLinkedInClient = linkedInCLient
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

  void . forkIO $ (runOnSocket sock (serve config authConfig authorize) `catch` logException)
                 `finally` (close sock >> putMVar mvar ())

  case sock80 of
    Nothing -> return ()
    Just s -> void . forkIO $ (runOnSocket s redirectToHttps `catch` logException)
                             `finally` close s

  takeMVar mvar
  where
    -- Usually combined certs are in server, intermediate order,
    -- but the tls library expects them in the opposite order.
    reverseCerts (X509.CertificateChain certs, key) = (X509.CertificateChain $ reverse certs, key)

    mkOAuthClient :: Maybe String -> Maybe FilePath -> IO (Maybe OAuthClient)
    mkOAuthClient _ Nothing = return Nothing
    mkOAuthClient Nothing _ = return Nothing
    mkOAuthClient (Just cid) (Just f) = do
      sec <- strip <$> readFile f
      return . Just $ OAuthClient { oauthClientId = B8.pack cid
                                  , oauthClientSecret = B8.pack sec}


-- | Redirects requests to https.
redirectToHttps :: SockAddr -> Socket -> IO ()
redirectToHttps _ sock = do
  conn <- makeInputStream (Socket.recv sock 4096)
  request <- readRequest True conn

  case baseUri (requestHeaders request) of
    Just uri -> do
      let location = uri <> requestPath request
      Log.debug ("Redirecting HTTP request to " ++ show location)
      simpleResponse send movedPermanently301 [("Location", location)] ""
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
            Just base -> do
              let (segments, query) = (decodePath . extractPath) path
              let redirectPath = fromMaybe "/" $ join $ lookup "state" query
              case segments of
                ["sproxy", "oauth2callback"] ->
                  case join $ lookup "code" query of
                    Nothing -> Just <$> badRequest
                    Just code -> Just <$> Google.authenticate authConfig base redirectPath code
                ["sproxy", "oauth2callback", "linkedin"] ->
                  case join $ lookup "code" query of
                    Nothing -> Just <$> badRequest
                    Just code -> Just <$> LinkedIn.authenticate authConfig base redirectPath code
                ["sproxy", "logout"] -> Just <$> logout authConfig (base <> redirectPath)
                ["robots.txt"] -> Just <$> mkTextResponse ok200 "User-agent: *\nDisallow: /"
                _ -> -- Check for an auth cookie.
                  case removeCookie (authConfigCookieName authConfig) (parseCookies headers) of
                    Nothing -> Just <$> authenticationRequired authConfig base path
                    Just (authCookie, cookies) -> do
                      auth <- validAuth authConfig authCookie
                      case auth of
                        Nothing -> Just <$> authenticationRequired authConfig base path
                        Just token ->
                          forwardRequest config send authorize cookies addr request token
          forM_ mResponse (sendResponse send)
          return ((not . isConnectionClose) headers)

-- Check our access control list for this user's request and forward it to the backend if allowed.
forwardRequest :: Config
               -> SendData
               -> AuthorizeAction
               -> [(Name, Cookies.Value)]
               -> SockAddr
               -> Request BodyReader
               -> AuthToken
               -> IO (Maybe (Response BodyReader))
forwardRequest config send authorize cookies addr request@(Request method path headers _) token = do
    groups <- authorize (authUserEmail . authUser $ token)
                        (fromMaybe (error "No Host") $ lookup "Host" headers)
                        path method
    ip <- formatSockAddr addr
    case groups of
        [] -> Just <$> accessDenied (authUserEmail . authUser $ token)
        _ -> do
            -- TODO: Reuse connections to the backend server.
            let downStreamHeaders =
                    toList $
                    insert "From" (B8.pack . authUserEmail . authUser $ token) $
                    insert "X-Groups" (B8.pack $ intercalate "," groups) $
                    insert "X-Given-Name" (B8.pack . authUserGivenName . authUser $ token) $
                    insert "X-Family-Name" (B8.pack . authUserFamilyName . authUser $ token) $
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
      tlsH e@(TLS.HandshakeFailed (TLS.Error_Misc _)) = clientError e
      tlsH e = logException' e

      logException' :: Exception e => e -> IO ()
      logException' = logException . toException

      clientClosedConection :: Exception e => e -> IO ()
      clientClosedConection e = Log.debug ("client closed connection (" ++ displayException e ++ ")")

      clientError :: Exception e => e -> IO ()
      clientError e = Log.debug ("client error (" ++ displayException e ++ ")")

logException :: SomeException -> IO ()
logException = Log.error . displayException

