{-# LANGUAGE ExtendedDefaultRules, QuasiQuotes, ScopedTypeVariables #-}
module Proxy (
  run

-- exported to silence warnings
, AccessToken(..)
, Config(..)
) where

import Cookies
import HTTP

import Control.Concurrent (forkIO, newEmptyMVar, putMVar, takeMVar, myThreadId)
import Control.Exception (finally, bracket, handle, throw, SomeException)
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
import qualified Database.PostgreSQL.Simple as PSQL
import Network (PortID(..), listenOn, connectTo, accept)
import qualified Network.Curl as Curl
import qualified Network.HTTP.Cookie as HTTP
import Network.HTTP.Types (Method, hCookie)
import qualified Network.HTTP.Types.URI as Query
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
                     , cfBackendAddress :: String
                     , cfBackendPort :: Integer
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
        m .: "database" <*>
        m .: "backend_address" <*>
        m .: "backend_port"
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
      _ <- forkIO (handle handleError (listen (PortNumber 443) (serve config credential clientSecret authTokenKey))
                                  `finally` (putMVar wait ()))
      -- Listen on port 80 just to redirect everything to HTTPS.
      _ <- forkIO (handle handleError (listen (PortNumber 80) redirectToHttps))
      takeMVar wait
 where handleError :: SomeException -> IO ()
       handleError e = log $ show e
       -- Usually combined certs are in server, intermediate order,
       -- but the tls library expects them in the opposite order.
       reverseCerts (X509.CertificateChain certs, key) = (X509.CertificateChain $ reverse certs, key)

-- | Redirects requests to https.
redirectToHttps :: Handle -> IO ()
redirectToHttps h = do
  input <- BL.hGetContents h
  case oneRequest input of
    (Nothing, _) -> return ()
    (Just request, _) -> BL.hPutStr h $ rawResponse $ response 303 "See Other" [("Location", cs $ show $ requestURI request)] ""

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
           (Just request@(Request _ url headers _), rest) -> do
             -- TODO: Don't loop for more input on Connection: close header.
             -- Check if this is an authorization response.
             case URI.parseURIReference $ BU.toString url of
               Nothing -> internalServerError c "Failed to parse request URI" >> serve' c db rest
               Just uri -> do
                 let query = Query.parseQuery $ BU.fromString $ URI.uriQuery uri
                 -- This isn't a perfect test, but it's perfect for testing.
                 case (lookup "state" query, lookup "code" query) of
                   (Just (Just _), Just (Just code)) -> do
                     tokenRes <- post "https://accounts.google.com/o/oauth2/token" ["code=" ++ BU.toString code, "client_id=" ++ cfClientID cf, "client_secret=" ++ clientSecret, "redirect_uri=" ++ (cs $ show $ rootURI request), "grant_type=authorization_code"]
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
                               Right i -> do
                                 case Aeson.decode $ BLU.fromString $ Curl.respBody i of
                                   Nothing -> internalServerError c "Received an invalid user info response from Google's authentication server." >> serve' c db rest
                                   Just userInfo -> do
                                     clientToken <- authToken authTokenKey (userEmail userInfo) (userGivenName userInfo, userFamilyName userInfo)
                                     let cookie = setCookie (HTTP.MkCookie cookieDomain cookieName (show clientToken) Nothing Nothing Nothing) authShelfLife
                                         resp' = response 302 "Found" [("Location", cs $ show $ rootURI request), ("Set-Cookie", BU.fromString cookie)] ""
                                     TLS.sendData c $ rawResponse resp'
                                     serve' c db rest
                   _ -> do
                     -- Check for an auth cookie.
                     case removeCookie cookieName (parseCookies headers) of
                       Nothing -> redirectForAuth c (rootURI request) >> serve' c db rest
                       Just (authCookie, cookies) -> do
                         auth <- validAuth authTokenKey authCookie
                         case auth of
                           Nothing -> redirectForAuth c (rootURI request) >> serve' c db rest
                           Just token -> do
                             continue <- forwardRequest cf c db cookies request token
                             when continue $ serve' c db rest

       cookieDomain = cfCookieDomain cf
       cookieName = cfCookieName cf

       redirectForAuth c redirectUri = do
         let authURL = "https://accounts.google.com/o/oauth2/auth?scope=https%3A%2F%2Fwww.googleapis.com%2Fauth%2Fuserinfo.email+https%3A%2F%2Fwww.googleapis.com%2Fauth%2Fuserinfo.profile&state=%2Fprofile&redirect_uri=" ++ (cs $ show $ redirectUri) ++ "&response_type=code&client_id=" ++ cfClientID cf ++ "&approval_prompt=force&access_type=offline"
         TLS.sendData c $ rawResponse $ response 302 "Found" [("Location", BU.fromString $ authURL)] ""

-- Check our access control list for this user's request and forward it to the backend if allowed.
forwardRequest :: Config -> TLS.Context -> PSQL.Connection -> [(Name, Cookies.Value)] -> Request -> AuthToken -> IO Bool
forwardRequest cf c db cookies (Request method path headers body) token = do
    groups <- authorizedGroups db (authEmail token) (maybe (error "No Host") cs $ lookup "Host" headers) path method
    case groups of
        [] -> do
            -- TODO: Send back a page that allows the user to request authorization.
            TLS.sendData c $ rawResponse $ response 403 "Forbidden" [] "Access Denied"
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
                    setCookies $
                    delete hCookie $
                    fromList headers
            BL.hPutStr h $ rawRequest (Request method path downStreamHeaders body)
            input <- BL.hGetContents h
            continue <- case oneResponse input of
                (Nothing, _) -> return False -- no more responses
                (Just resp@(_, headers', _), _) -> do
                    TLS.sendData c $ rawResponse resp
                    return $ lookup "Connection" headers' /= Just "close"
            hClose h
            return continue
  where
    setCookies = case cookies of
      [] -> id
      _  -> insert hCookie (formatCookies cookies)

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

log :: String -> IO ()
log s = do
  tid <- myThreadId
  t <- getCurrentTime
  Log.debugM "sproxy" $ show tid ++ " " ++ show t ++ ": " ++ s

internalServerError :: TLS.Context -> String -> IO ()
internalServerError c err = do
  log $ show err
  -- I wonder why Firefox fails to parse this correctly without a Content-Length header?
  TLS.sendData c $ rawResponse $ response 500 "Internal Server Error" [] "Internal Server Error"

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
