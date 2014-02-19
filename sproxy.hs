{-# LANGUAGE ExtendedDefaultRules, QuasiQuotes, ScopedTypeVariables #-}

module Main where

import Cookies
import HTTP

import Control.Concurrent (forkIO, newEmptyMVar, putMVar, takeMVar, myThreadId)
import Control.Exception (finally, bracket, bracketOnError, handle, throw, SomeException)
import Control.Monad (forever, mzero, liftM, when)
import Crypto.Random (createEntropyPool, CPRG(..), SystemRNG)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.UTF8 as BU
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.UTF8 as BLU
import Data.Default (def)
import Data.List (find, intercalate)
import Data.Time.Clock (getCurrentTime)
import Data.Maybe
import Data.Map as Map (fromList, toList, insert)
import Data.String.Conversions (cs)
import qualified Data.X509 as X509
import Data.Yaml
import qualified Database.PostgreSQL.Simple as PSQL
import Network (PortID(..), listenOn, connectTo, accept)
import qualified Network.Curl as Curl
import qualified Network.HTTP.Cookie as HTTP
import Network.HTTP.Types.Method (Method)
import qualified Network.HTTP.Types.URI as Query
import qualified Network.BSD as BSD
import qualified Network.Socket as Socket
import qualified Network.TLS as TLS
import qualified Network.TLS.Extra as TLS
import qualified Network.URI as URI
import Options.Applicative
import System.IO (Handle, hClose)
import System.IO.Unsafe (unsafeInterleaveIO)
import qualified System.Log.Logger as Log
import Text.InterpolatedString.Perl6 (q)

import Prelude hiding (log)

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


-- * configuration

data Config = Config { cfCookieDomain :: String
                     , cfCookieName :: String
                     , cfContact :: String
                     , cfClientID :: String
                     , cfClientSecretFile :: FilePath
                     , cfAuthTokenKeyFile :: FilePath
                     , cfSslKey :: FilePath
                     , cfSslCerts :: FilePath
                     , cfDatabase :: String
                     }
  deriving Show

-- The configuration file is YAML, but the YAML library uses JSON instances.
instance FromJSON Config where
    parseJSON (Object m) = Config <$>
        m .: "cookie_domain" <*>
        m .: "cookie_name" <*>
        m .: "contact" <*>
        m .: "client_id" <*>
        m .: "client_secret" <*>
        m .: "auth_token_key" <*>
        m .: "ssl_key" <*>
        m .: "ssl_certs" <*>
        m .: "database"
    parseJSON _ = mzero

-- | Returns redirectUri for a given request.
mkRedirectURI :: Request -> Maybe String
mkRedirectURI (method, path, headers, body) =
    let host = fromMaybe (error "Host header not found") $ lookup "Host" headers
        redirectUri = URI.URI "https:" (Just (URI.URIAuth "" (cs host) ""))
            -- For now, we strip the query from the path.
            -- Google also does not allow to redirect to subpaths when they 
            -- are not explicitly configured so we
            -- could/should probably disregard the whole path. (?)
            (takeWhile (/= '?') $ cs path)
            "" ""
    in Just $ show redirectUri

-- * command line options

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


-- * main functionality


-- | Reads the configuration file and the ssl certificate files and starts
-- the server
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
      clientSecret <- readFile (cfClientSecretFile config)
      authTokenKey <- readFile (cfAuthTokenKeyFile config)
      credential <- either error reverseCerts `fmap` TLS.credentialLoadX509 (cfSslCerts config) (cfSslKey config)
      -- Immediately fork a new thread for accepting connections since
      -- the main thread is special and expensive to communicate with.
      wait <- newEmptyMVar
      forkIO (handle handleError (listen (PortNumber 443) (serve config credential clientSecret authTokenKey))
                                  `finally` (putMVar wait ()))
      -- Listen on port 80 just to redirect everything to HTTPS.
      forkIO (handle handleError (listen (PortNumber 80) (redirectToHttps config)))
      takeMVar wait
 where handleError :: SomeException -> IO ()
       handleError e = log $ show e
       -- Usually combined certs are in server, intermediate order,
       -- but the tls library expects them in the opposite order.
       reverseCerts (X509.CertificateChain certs, key) = (X509.CertificateChain $ reverse certs, key)

-- | Redirects requests to https.
redirectToHttps :: Config -> Handle -> IO ()
redirectToHttps cf h = do
  input <- BL.hGetContents h
  case oneRequest input of
    (Nothing, _) -> return ()
    (Just request, _) -> do
      BL.hPutStr h $ rawResponse $ case mkRedirectURI request of
        Nothing ->
            response 404 "Not Found" [] "404 - Not Found"
        Just redirectUri ->
            response 303 "See Other" [("Location", cs redirectUri)] ""

-- | Actual server:
-- - ssl handshake
-- - google authentication
-- - our authorization
-- - redirecting requests to localhost:8080
serve :: Config -> TLS.Credential -> String -> String -> Handle -> IO ()
serve cf credential clientSecret authTokenKey h = do
  rng <- cprgCreate `liftM` createEntropyPool :: IO SystemRNG
  -- TODO: Work in the intermediate certificates.
  let params = def { TLS.serverShared = def { TLS.sharedCredentials = TLS.Credentials [credential] }
                   , TLS.serverSupported = def { TLS.supportedVersions = [TLS.SSL3, TLS.TLS10, TLS.TLS11, TLS.TLS12]
                                               , TLS.supportedCiphers = TLS.ciphersuite_all } }
  ctx <- TLS.contextNew h params rng
  TLS.handshake ctx
  input <- tlsGetContents ctx
  -- Establish a DB connection (used for authorization checks).
  bracket
    (PSQL.connectPostgreSQL (cs $ cfDatabase cf))
    (PSQL.close)
    (\db -> serve' ctx db input)
  TLS.bye ctx
 where serve' c db input =
         -- TODO: Clean this up. The logic is really messy.
         case oneRequest input of
           (Nothing, _) -> return () -- no more requests
           (Just request@(method, url, headers, _), rest) -> do
             -- TODO: Don't loop for more input on Connection: close header.
             case mkRedirectURI request of
               Nothing -> do
                 -- uri does not exist -> 404
                 TLS.sendData c $ rawResponse $ response 404 "Not Found" [] "404 - Not Found"
                 serve' c db rest
               Just redirectUri ->
                 -- Check if this is an authorization response.
                 case URI.parseURIReference $ BU.toString url of
                   Nothing -> internalServerError c "Failed to parse request URI" >> serve' c db rest
                   Just uri -> do
                     let query = Query.parseQuery $ BU.fromString $ URI.uriQuery uri
                     -- This isn't a perfect test, but it's perfect for testing.
                     case (lookup "state" query, lookup "code" query) of
                       (Just (Just _), Just (Just code)) -> do
                         tokenRes <- post "https://accounts.google.com/o/oauth2/token" ["code=" ++ BU.toString code, "client_id=" ++ cfClientID cf, "client_secret=" ++ clientSecret, "redirect_uri=" ++ redirectUri, "grant_type=authorization_code"]
                         case tokenRes of
                           Left err -> internalServerError c err >> serve' c db rest
                           Right resp -> do
                             case Aeson.decode $ BLU.fromString $ Curl.respBody resp of
                               Nothing -> do
                                  internalServerError c "Received an invalid response from Google's authentication server." >> serve' c db rest
                               Just token -> do
                                 infoRes <- get $ "https://www.googleapis.com/oauth2/v1/userinfo?access_token=" ++ accessToken token
                                 case infoRes of
                                   Left err -> internalServerError c err >> serve' c db rest
                                   Right info -> do
                                     case Aeson.decode $ BLU.fromString $ Curl.respBody info of
                                       Nothing -> internalServerError c "Received an invalid user info response from Google's authentication server." >> serve' c db rest
                                       Just userInfo -> do
                                         clientToken <- authToken authTokenKey (userEmail userInfo) (userGivenName userInfo, userFamilyName userInfo)
                                         let cookie = setCookie (HTTP.MkCookie (cfCookieDomain cf) (cfCookieName cf) (show clientToken) Nothing Nothing Nothing) authShelfLife
                                             resp' = response 302 "Found" [("Location", cs redirectUri), ("Set-Cookie", BU.fromString cookie)] ""
                                         TLS.sendData c $ rawResponse resp'
                                         serve' c db rest
                       _ -> do
                         -- Check for an auth cookie.
                         let (_, cookies) = processCookieHeaders (cfCookieDomain cf) headers
                         case find (\x -> HTTP.ckName x == (cfCookieName cf)) cookies of
                           Nothing -> redirectForAuth c redirectUri >> serve' c db rest
                           Just authCookie -> do
                             auth <- validAuth authTokenKey (HTTP.ckValue authCookie)
                             case auth of
                               Nothing -> redirectForAuth c redirectUri >> serve' c db rest
                               Just token -> do
                                 continue <- forwardRequest c db request token
                                 when continue $ serve' c db rest
       redirectForAuth c redirectUri = do
         let authURL = "https://accounts.google.com/o/oauth2/auth?scope=https%3A%2F%2Fwww.googleapis.com%2Fauth%2Fuserinfo.email+https%3A%2F%2Fwww.googleapis.com%2Fauth%2Fuserinfo.profile&state=%2Fprofile&redirect_uri=" ++ redirectUri ++ "&response_type=code&client_id=" ++ cfClientID cf ++ "&approval_prompt=force&access_type=offline"
         TLS.sendData c $ rawResponse $ response 302 "Found" [("Location", BU.fromString $ authURL)] ""

-- Check our access control list for this user's request and forward it to the backend if allowed.
forwardRequest :: TLS.Context -> PSQL.Connection -> Request -> AuthToken -> IO (Bool)
forwardRequest c db (method, path, headers, body) token = do
    groups <- authorizedGroups db (authEmail token) (maybe (error "No Host") cs $ lookup "Host" headers) path method
    case groups of
        [] -> do
            -- TODO: Send back a page that allows the user to request authorization.
            TLS.sendData c $ rawResponse $ response 403 "Forbidden" [] "Access Denied"
            return True
        _ -> do
            -- TODO: Make the backend address configurable.
            -- TODO: Reuse connections to the backend server.
            h <- connectTo "127.0.0.1" $ PortNumber 8080
            let downStreamHeaders =
                    toList $
                    insert "From" (cs $ authEmail token) $
                    insert "X-Groups" (cs $ intercalate "," groups) $
                    insert "X-Given-Name" (cs $ fst $ authName token) $
                    insert "X-Family-Name" (cs $ snd $ authName token) $
                    fromList headers
            BL.hPutStr h $ rawRequest (method, path, downStreamHeaders, body)
            input <- BL.hGetContents h
            continue <- case oneResponse input of
                (Nothing, _) -> return False -- no more responses
                (Just resp@(_, headers', _), _) -> do
                    TLS.sendData c $ rawResponse resp
                    return $ lookup "Connection" headers' /= Just "close"
            hClose h
            return continue

authorizedGroups :: PSQL.Connection -> String -> BS.ByteString -> BS.ByteString -> Method -> IO [String]
authorizedGroups db email domain path method =
  (fmap PSQL.fromOnly) `fmap` PSQL.query db [q|
SELECT gp."group" FROM group_privilege gp
INNER JOIN group_member gm ON gm."group" = gp."group"
INNER JOIN "group" g ON gp."group" = g."group"
WHERE ? LIKE email
AND ? LIKE "domain"
AND privilege IN (
  SELECT p.privilege FROM privilege p
  INNER JOIN privilege_rule pr ON pr."domain" = p."domain" AND pr.privilege = p.privilege
  WHERE ? LIKE pr."domain" AND ? LIKE "path" AND ? ILIKE "method"
  ORDER by array_length(regexp_split_to_array("path", '/'), 1) DESC LIMIT 1
)
|] (email, domain, domain, path, method)

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
