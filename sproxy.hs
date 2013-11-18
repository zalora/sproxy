{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Cookies

import Control.Applicative ((<$>), (<*>))
import Control.Concurrent (forkIO, newEmptyMVar, putMVar, takeMVar)
import Control.Exception (finally, bracketOnError)
import Control.Monad (forever, mzero, join)
import Control.Monad.Error (runErrorT)
import Control.Monad.Trans (liftIO)
import qualified Data.Aeson as Aeson
import Data.Aeson (parseJSON)
import qualified Data.ByteString.UTF8 as BU
import qualified Data.ByteString.Lazy.UTF8 as BLU
import qualified Data.ConfigFile as CF
import Data.List (find)
import Network (PortID(..), listenOn)
import qualified Network.Curl as Curl
import qualified Network.HTTP as HTTP
import qualified Network.HTTP.Cookie as HTTP
import qualified Network.HTTP.Types.URI as Query
import qualified Network.BSD as BSD
import qualified Network.Socket as Socket
import Network.TCP (socketConnection)
import qualified Network.URI as URI
import System.IO (hPutStrLn, stderr, stdin)

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

main = do
  -- Make sure we have all necessary config options. Read them from stdin.
  config' <- runErrorT $ do
    cf <- join $ liftIO $ CF.readhandle CF.emptyCP stdin
    domain <- CF.get cf "DEFAULT" "domain"
    contact <- CF.get cf "DEFAULT" "contact"
    url <- CF.get cf "DEFAULT" "url"
    -- The client ID and client secret come from Google's "Cloud Console".
    clientID <- CF.get cf "DEFAULT" "client_id"
    clientSecret <- CF.get cf "DEFAULT" "client_secret"
    authTokenKey <- CF.get cf "DEFAULT" "auth_token_key"
    -- We'll need more complex rules for access control, but for now let's just use a list.
    authorizedEmails <- words `fmap` CF.get cf "DEFAULT" "authorized_emails"
    return (domain, contact, url, clientID, clientSecret, authTokenKey, authorizedEmails)
  case config' of
    Left err -> hPutStrLn stderr $ show err
    Right config -> do
      -- Immediately fork a new thread for accepting connections since
      -- the main thread is special and expensive to communicate with.
      wait <- newEmptyMVar
      forkIO (listen (PortNumber 80) (flip serve config) `finally` putMVar wait ())
      takeMVar wait

listen :: PortID -> (Socket.Socket -> IO ()) -> IO ()
listen port f = do
  s <- listenOn port
  forever $ do
    (s', _) <- Socket.accept s
    forkIO $ f s'

internalServerError c err = do
  hPutStrLn stderr $ show err
  HTTP.respondHTTP c $ HTTP.Response (5,0,0) "Internal Server Error" [] "\n"

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

serve :: Socket.Socket -> (String, String, String, String, String, String, [String]) -> IO ()
serve s (domain, contact, url, clientID, clientSecret, authTokenKey, authorizedEmails) = do
  c <- socketConnection "" 0 s
  res <- HTTP.receiveHTTP c
  case res of
    Left err -> internalServerError c err
    Right req -> do
      print req
      -- Check if this is an authorization response.
      let uri = HTTP.rqURI req
          query = Query.parseQuery $ BU.fromString $ URI.uriQuery uri
      -- This isn't a perfect test, but it's perfect for testing.
      case (lookup "state" query, lookup "code" query) of
        (Just (Just _), Just (Just code)) -> do
          tokenRes <- post "https://accounts.google.com/o/oauth2/token" ["code=" ++ BU.toString code, "client_id=" ++ clientID, "client_secret=" ++ clientSecret, "redirect_uri=" ++ url, "grant_type=authorization_code"]
          case tokenRes of
            Left err -> internalServerError c err
            Right response -> do
              case Aeson.decode $ BLU.fromString $ Curl.respBody response of
                Nothing -> internalServerError c "Received an invalid response from Google's authentication server."
                Just token -> do
                  infoRes <- get $ "https://www.googleapis.com/oauth2/v1/userinfo?access_token=" ++ accessToken token
                  case infoRes of
                    Left err -> internalServerError c err
                    Right info -> do
                      case Aeson.decode $ BLU.fromString $ Curl.respBody info of
                        Nothing -> internalServerError c "Received an invalid user info response from Google's authentication server."
                        Just userInfo -> do
                          clientToken <- authToken authTokenKey $ userEmail userInfo
                          HTTP.respondHTTP c $ HTTP.Response (3,0,2) "Found" [HTTP.mkHeader HTTP.HdrSetCookie $ setCookie (HTTP.MkCookie domain "gauth" (show clientToken) Nothing Nothing Nothing) authShelfLife, HTTP.mkHeader HTTP.HdrLocation url] "\n"
        _ -> do
          -- Check for an auth cookie.
          let (_, cookies) = processCookieHeaders domain $ HTTP.rqHeaders req
          case find (\x -> HTTP.ckName x == "gauth") cookies of
            Nothing -> redirectForAuth c
            Just authCookie -> do
              auth <- validAuth authTokenKey $ HTTP.ckValue authCookie
              case auth of
                Nothing -> redirectForAuth c
                Just token -> requestWithEmail c req authorizedEmails (authEmail token) contact
 where redirectForAuth c = do
         let authURL = "https://accounts.google.com/o/oauth2/auth?scope=https%3A%2F%2Fwww.googleapis.com%2Fauth%2Fuserinfo.email+https%3A%2F%2Fwww.googleapis.com%2Fauth%2Fuserinfo.email&state=%2Fprofile&redirect_uri=" ++ url ++ "&response_type=code&client_id=" ++ clientID ++ "&approval_prompt=force"
         HTTP.respondHTTP c $ HTTP.Response (3,0,2) "Found" [HTTP.mkHeader HTTP.HdrLocation authURL] "\n"

-- Check our access control list for this user's request and forward it to the backend if allowed.
requestWithEmail :: HTTP.HandleStream String -> HTTP.Request String -> [String] -> String -> String -> IO ()
requestWithEmail client req authorizedEmails email _ | email `elem` authorizedEmails = do
  s <- connect "127.0.0.1" 8080
  c <- socketConnection "" 0 s
  res <- HTTP.sendHTTP c req {HTTP.rqHeaders = HTTP.rqHeaders req ++ [HTTP.mkHeader HTTP.HdrFrom email]}
  case res of
    Left err -> internalServerError client err
    Right response -> do
      print response
      HTTP.respondHTTP client response
-- TODO: For some reason the following response triggers a problem in
-- stunnel. My other HTTP responses (302) were having this problem
-- until I added a newline to the body. That doesn't seem to help for
-- this one. For now, I set the TIMEOUTclose to 3 seconds in stunnel
-- to work around the problem.
requestWithEmail client _ _ _ contact = HTTP.respondHTTP client $ HTTP.Response (4,0,3) "Forbidden" [] $ "You aren't authorized to access this page. To gain access, contact " ++ contact ++ "\n"
