module Sproxy.Server (
  server
) where

import Control.Concurrent (forkIO)
import Control.Exception (bracketOnError)
import Control.Monad (void, when)
import Data.ByteString.Char8 (pack)
import Data.HashMap.Strict as HM (fromList, lookup, toList)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Word (Word16)
import Data.Yaml.Include (decodeFileEither)
import Network.HTTP.Client (Manager, ManagerSettings(..), defaultManagerSettings, newManager, socketConnection)
import Network.HTTP.Client.Internal (Connection)
import Network.Socket ( Socket, Family(AF_INET, AF_UNIX), SockAddr(SockAddrInet, SockAddrUnix),
  SocketOption(ReuseAddr), SocketType(Stream), bind, close, connect, inet_addr,
  listen, maxListenQueue, setSocketOption, socket )
import Network.Wai (Application)
import Network.Wai.Handler.WarpTLS (tlsSettingsChain, runTLSSocket)
import Network.Wai.Handler.Warp ( Settings, defaultSettings, runSettingsSocket,
  setHTTP2Disabled, setOnException )
import System.Entropy (getEntropy)
import System.Environment (setEnv)
import System.Exit (exitFailure)
import System.FilePath.Glob (compile)
import System.IO (hPutStrLn, stderr)
import System.Posix.User ( GroupEntry(..), UserEntry(..),
  getAllGroupEntries, getRealUserID,
  getUserEntryForName, setGroupID, setGroups, setUserID )

import Sproxy.Application (sproxy, redirect)
import Sproxy.Application.OAuth2.Common (OAuth2Client)
import Sproxy.Config (BackendConf(..), ConfigFile(..), OAuth2Conf(..))
import qualified Sproxy.Application.OAuth2 as OAuth2
import qualified Sproxy.Logging as Log
import qualified Sproxy.Server.DB as DB


{- TODO:
 - Log.error && exitFailure should be replaced
 - by Log.fatal && wait for logger thread to print && exitFailure
-}

server :: FilePath -> IO ()
server configFile = do
  cf <- readConfigFile configFile
  Log.start $ cfLogLevel cf

  sock <- socket AF_INET Stream 0
  setSocketOption sock ReuseAddr 1
  bind sock $ SockAddrInet (fromIntegral $ cfListen cf) 0

  maybe80 <- if fromMaybe (443 == cfListen cf) (cfListen80 cf)
    then do
      sock80 <- socket AF_INET Stream 0
      setSocketOption sock80 ReuseAddr 1
      bind sock80 $ SockAddrInet 80 0
      return (Just sock80)
    else
      return Nothing

  uid <- getRealUserID
  when (0 == uid) $ do
    let user = cfUser cf
    Log.info $ "switching to user " ++ show user
    u <- getUserEntryForName user
    groupIDs <- map groupID . filter (elem user . groupMembers)
             <$> getAllGroupEntries
    setGroups groupIDs
    setGroupID $ userGroupID u
    setUserID $ userID u

  ds <- newDataSource cf
  db <- DB.start (cfHome cf) ds

  key <- maybe
           (Log.info "using new random key" >> getEntropy 32)
           (return . pack)
           (cfKey cf)

  let
    settings =
      (if cfHTTP2 cf then id else setHTTP2Disabled) $
      setOnException (\_ _ -> return ())
      defaultSettings

  oauth2clients <- HM.fromList <$> mapM newOAuth2Client (HM.toList (cfOAuth2 cf))

  backends <-
    mapM (\be -> do
      m <- newBackendManager be
      return (compile $ beName be, be, m)
    ) $ cfBackends cf


  warpServer <- newServer cf

  case maybe80 of
    Nothing     -> return ()
    Just sock80 -> do
      let httpsPort = fromMaybe (cfListen cf) (cfHttpsPort cf)
      Log.info "listening on port 80 (HTTP redirect)"
      listen sock80 maxListenQueue
      void . forkIO $ runSettingsSocket settings sock80 (redirect httpsPort)

  -- XXX 2048 is from bindPortTCP from streaming-commons used internally by runTLS.
  -- XXX Since we don't call runTLS, we listen socket here with the same options.
  Log.info $ "proxy listening on port " ++ show (cfListen cf)
  listen sock (max 2048 maxListenQueue)
  warpServer settings sock (sproxy key db oauth2clients backends)


newDataSource :: ConfigFile -> IO (Maybe DB.DataSource)
newDataSource cf =
  case (cfDataFile cf, cfDatabase cf) of
    (Nothing, Just str) -> do
      case cfPgPassFile cf of
        Nothing -> return ()
        Just f  -> do
          Log.info $ "pgpassfile: " ++ show f
          setEnv "PGPASSFILE" f
      return . Just $ DB.PostgreSQL str

    (Just f, Nothing)   -> return . Just $ DB.File f

    (Nothing, Nothing)  -> return Nothing
    _ -> do
      Log.error "only one data source can be used"
      exitFailure


newOAuth2Client :: (Text, OAuth2Conf) -> IO (Text, OAuth2Client)
newOAuth2Client (name, cfg) =
  case HM.lookup name OAuth2.providers of
    Nothing -> do Log.error $ "OAuth2 provider " ++ show name ++ " is not supported"
                  exitFailure
    Just provider -> do
      Log.info $ "oauth2: adding " ++ show name
      return (name, provider (client_id, client_secret))
  where client_id = pack $ oa2ClientId cfg
        client_secret = pack $ oa2ClientSecret cfg


newBackendManager :: BackendConf -> IO Manager
newBackendManager be = do
  openConn <-
    case (beSocket be, bePort be) of
      (Just f, Nothing) -> do
        Log.info $ "backend `" ++ beName be ++ "' on UNIX socket " ++ f
        return $ openUnixSocketConnection f

      (Nothing, Just n) -> do
        Log.info $ "backend `" ++ beName be ++ "' on " ++ beAddress be ++ ":" ++ show n
        return $ openTCPConnection (beAddress be) n

      _ -> do
            Log.error "either backend port number or UNIX socket path is required."
            exitFailure

  newManager defaultManagerSettings {
    managerRawConnection = return $ \_ _ _ -> openConn
  , managerConnCount = beConnCount be
  }


newServer :: ConfigFile -> IO (Settings -> Socket -> Application -> IO ())
newServer cf
  | cfSsl cf =
    case (cfSslKey cf, cfSslCert cf) of
      (Just k, Just c) ->
        return $ runTLSSocket (tlsSettingsChain c (cfSslCertChain cf) k)
      _ -> do Log.error "missings SSL certificate"
              exitFailure
  | otherwise = do
      Log.warn "not using SSL!"
      return runSettingsSocket


openUnixSocketConnection :: FilePath -> IO Connection
openUnixSocketConnection f =
  bracketOnError
   (socket AF_UNIX Stream 0)
   close
   (\s -> do
      connect s (SockAddrUnix f)
      socketConnection s 8192)


openTCPConnection :: String -> Word16 -> IO Connection
openTCPConnection addr port =
  bracketOnError
   (socket AF_INET Stream 0)
   close
   (\s -> do
      a <- inet_addr addr
      connect s (SockAddrInet (fromIntegral port) a)
      socketConnection s 8192)


readConfigFile :: FilePath -> IO ConfigFile
readConfigFile f = do
  r <- decodeFileEither f
  case r of
    Left e -> do
      hPutStrLn stderr $ "FATAL: " ++ f ++ ": " ++ show e
      exitFailure
    Right cf -> return cf

