{-# LANGUAGE OverloadedStrings #-}
module Authenticate (
  AuthConfig(..)
, AccessToken(..)
, redirectForAuth
, authenticate
) where

import           Control.Applicative
import           Data.Maybe
import           Data.ByteString (ByteString)
import qualified Data.ByteString.UTF8 as UTF8
import qualified Data.ByteString.Lazy.UTF8 as LazyUTF8
import           Data.String.Conversions (cs)
import           Data.Aeson
import           Network.HTTP.Cookie (Cookie(MkCookie))
import           Network.HTTP.Types (urlEncode, urlDecode)
import qualified Network.Curl as Curl
import qualified Network.URI as URI

import           Type
import           Cookies
import           HTTP

data AuthConfig = AuthConfig {
  authConfigCookieDomain :: String
, authConfigCookieName :: String
, authConfigClientID :: String
, authConfigClientSecret :: String
, authConfigAuthTokenKey :: String
} deriving (Eq, Show)

data AccessToken = AccessToken {
  accessToken :: String
, expiresIn :: Integer
, tokenType :: String
} deriving (Eq, Show)

instance FromJSON AccessToken where
  parseJSON (Object v) = AccessToken
    <$> v .: "access_token"
    <*> v .: "expires_in"
    <*> v .: "token_type"
  parseJSON _ = empty

data UserInfo = UserInfo {
  userEmail :: String
, userGivenName :: String
, userFamilyName :: String
} deriving (Eq, Show)

instance FromJSON UserInfo where
  parseJSON (Object v) = UserInfo
    <$> v .: "email"
    <*> v .: "given_name"
    <*> v .: "family_name"
  parseJSON _ = empty

-- https://wiki.zalora.com/Main_Page -> https://wiki.zalora.com/
-- Note that this always uses https:
rootURI :: Request -> URI.URI
rootURI (Request _ _ headers _) =
  let host = cs $ fromMaybe (error "Host header not found") $ lookup "Host" headers
  in URI.URI "https:" (Just $ URI.URIAuth "" host "") "/" "" ""

redirectForAuth :: AuthConfig -> Request -> SendData -> IO ()
redirectForAuth c request send = do
  let redirectUri = rootURI request
      path = urlEncode True (requestPath request)
      authURL = "https://accounts.google.com/o/oauth2/auth?scope=https%3A%2F%2Fwww.googleapis.com%2Fauth%2Fuserinfo.email+https%3A%2F%2Fwww.googleapis.com%2Fauth%2Fuserinfo.profile&state=" ++ cs path ++ "&redirect_uri=" ++ (cs $ show $ redirectUri) ++ "&response_type=code&client_id=" ++ authConfigClientID c ++ "&approval_prompt=force&access_type=offline"
  send . rawResponse $ response 302 "Found" [("Location", UTF8.fromString $ authURL)] ""

authenticate :: AuthConfig -> SendData -> Request -> ByteString -> ByteString -> IO ()
authenticate config send request path code = do
  tokenRes <- post "https://accounts.google.com/o/oauth2/token" ["code=" ++ UTF8.toString code, "client_id=" ++ clientID, "client_secret=" ++ clientSecret, "redirect_uri=" ++ (cs $ show $ rootURI request), "grant_type=authorization_code"]
  case tokenRes of
    Left err -> internalServerError send err
    Right resp -> do
      case decode $ LazyUTF8.fromString $ Curl.respBody resp of
        Nothing -> do
          internalServerError send "Received an invalid response from Google's authentication server."
        Just token -> do
          infoRes <- get $ "https://www.googleapis.com/oauth2/v1/userinfo?access_token=" ++ accessToken token
          case infoRes of
            Left err -> internalServerError send err
            Right i -> do
              case decode $ LazyUTF8.fromString $ Curl.respBody i of
                Nothing -> internalServerError send "Received an invalid user info response from Google's authentication server."
                Just userInfo -> do
                  clientToken <- authToken authTokenKey (userEmail userInfo) (userGivenName userInfo, userFamilyName userInfo)
                  let cookie = setCookie (MkCookie cookieDomain cookieName (show clientToken) Nothing Nothing Nothing) authShelfLife
                      resp' = response 302 "Found" [("Location", cs $ (show $ (rootURI request) {URI.uriPath = ""}) ++ cs (urlDecode False path)), ("Set-Cookie", UTF8.fromString cookie)] ""
                  send $ rawResponse resp'
  where
    cookieDomain = authConfigCookieDomain config
    cookieName = authConfigCookieName config
    clientID = authConfigClientID config
    clientSecret = authConfigClientSecret config
    authTokenKey = authConfigAuthTokenKey config

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
