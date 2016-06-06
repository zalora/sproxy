{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Authenticate (
  AuthConfig(..)
, AuthToken(..)
, validAuth
, redirectForAuth
, authenticate
, logout

-- exported to silence warnings
, AccessToken(..)
) where

import           Control.Applicative
import           Control.Exception
import           Text.Read (readMaybe)
import           Data.Monoid
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.UTF8 as UTF8
import qualified Data.ByteString.Lazy.Char8 as BL8
import           Data.String.Conversions (cs)
import           Data.List.Split (splitOn)
import           Data.Aeson
import           Network.HTTP.Types
import           System.Posix.Types (EpochTime)
import           System.Posix.Time (epochTime)
import           Data.Digest.Pure.SHA (hmacSha1, showDigest)
import           Network.HTTP.Conduit (simpleHttp, parseUrl, httpLbs, RequestBody(..))
import qualified Network.HTTP.Conduit as HTTP
import           Network.HTTP.Toolkit


import           Cookies
import           HTTP
import qualified Logging as Log

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

data AuthToken = AuthToken {
  authEmail  :: String
, authName   :: (String, String)
, authExpiry :: EpochTime
, authDigest :: String -- HMAC hash
}

-- Here is the format of the actual cookie we send to the client.
instance Show AuthToken where
  show a = authEmail a ++ ":" ++ authNameString (authName a) ++ ":" ++ show (authExpiry a) ++ ":" ++ authDigest a
    where authNameString (given, family) = given ++ ":" ++ family

instance Read AuthToken where
  readsPrec _ s = case splitOn ":" s of
    [email, given, family, expire, digest] -> [(AuthToken email (given, family) (read expire) digest, "")]
    _ -> []

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

redirectUri :: ByteString -> ByteString
redirectUri baseUri = baseUri <> "/sproxy/oauth2callback"

authUrl :: ByteString -> ByteString -> AuthConfig -> ByteString
authUrl path baseUri c = mconcat [
    "https://accounts.google.com/o/oauth2/auth?scope=https%3A%2F%2Fwww.googleapis.com%2Fauth%2Fuserinfo.email+https%3A%2F%2Fwww.googleapis.com%2Fauth%2Fuserinfo.profile&"
  , "state=", urlEncode True path
  , "&redirect_uri=", redirectUri baseUri
  , "&response_type=code&client_id=", B8.pack (authConfigClientID c)
  , "&approval_prompt=force&access_type=offline"
  ]

redirectForAuth :: AuthConfig -> ByteString -> ByteString -> IO (Response BodyReader)
redirectForAuth c path baseUri = mkResponse found302 [("Location", authUrl path baseUri c)] ""

authenticate :: AuthConfig -> ByteString -> ByteString -> ByteString -> IO (Response BodyReader)
authenticate config baseUri path code = do
  Log.info ("authentication request with code " ++ show code)
  tokenRes <- try $ post "https://accounts.google.com/o/oauth2/token"
              (cs $ "code=" ++ cs code ++ "&client_id=" ++ clientID
                ++ "&client_secret=" ++ clientSecret ++ "&redirect_uri="
                ++ cs (redirectUri baseUri) ++ "&grant_type=authorization_code")
  case tokenRes of
    Left err -> authenticationFailed ("error while authenticating: " ++ show (err :: HTTP.HttpException))
    Right resp ->
      case decode (HTTP.responseBody resp) of
        Nothing ->
          authenticationFailed "Received an invalid response from Google's authentication server."
        Just token -> do
          infoRes <- try $ simpleHttp ("https://www.googleapis.com/oauth2/v1/userinfo?access_token=" ++ accessToken token)
          case infoRes of
            Left err -> authenticationFailed ("error while retrieving user info: " ++ show (err :: HTTP.HttpException))
            Right body ->
              case decode body of
                Nothing -> authenticationFailed "Received an invalid user info response from Google's authentication server."
                Just userInfo -> do
                  clientToken <- authToken authTokenKey (userEmail userInfo) (userGivenName userInfo, userFamilyName userInfo)
                  let cookie = setCookie cookieDomain cookieName (show clientToken) authShelfLife
                  mkResponse found302 [("Location", baseUri <> urlDecode False path), ("Set-Cookie", UTF8.fromString cookie)] ""
  where
    cookieDomain = authConfigCookieDomain config
    cookieName = authConfigCookieName config
    clientID = authConfigClientID config
    clientSecret = authConfigClientSecret config
    authTokenKey = authConfigAuthTokenKey config

logout :: AuthConfig -> ByteString -> IO (Response BodyReader)
logout config url = do
  let cookie = invalidateCookie cookieDomain cookieName
  mkResponse found302 [("Location", url), ("Set-Cookie", UTF8.fromString cookie)] ""
  where
    cookieDomain = authConfigCookieDomain config
    cookieName = authConfigCookieName config

post :: String -> ByteString -> IO (HTTP.Response BL8.ByteString)
post url body = do
  r' <- parseUrl url
  let r = r' { HTTP.method = "POST"
             , HTTP.requestBody = RequestBodyBS body
             , HTTP.requestHeaders
               = [ ("Content-Type" , "application/x-www-form-urlencoded")
                 ]
             }
  manager <- HTTP.newManager HTTP.tlsManagerSettings
  httpLbs r manager

validAuth :: AuthConfig -> String -> IO (Maybe AuthToken)
validAuth config token =
  case readMaybe token of
    Nothing -> return Nothing
    Just t -> do
      now <- epochTime
      if tokenDigest key t == authDigest t && authExpiry t > now
        then return $ Just t
        else return Nothing
  where
    key = authConfigAuthTokenKey config

-- | Create an AuthToken with the default expiration time, automatically
-- calculating the digest.
authToken :: String -> String -> (String, String) -> IO AuthToken
authToken key email name = do
  now <- epochTime
  let expires = now + authShelfLife
      digest = tokenDigest key AuthToken {
          authEmail = email
        , authName = name
        , authExpiry = expires
        , authDigest = ""
        }
      token = AuthToken {
          authEmail  = email
        , authName = name
        , authExpiry = expires
        , authDigest = digest
        }
  return token

-- | This generates the HMAC digest of the auth token using SHA1.
-- Eventually, we need to rotate the key used to generate the HMAC, while still
-- storing old keys long enough to use them for any valid login session. Without
-- this, authentication is less secure.
tokenDigest :: String -> AuthToken -> String
tokenDigest key a = showDigest $ hmacSha1 (BL8.pack key) (BL8.pack token)
  where token = show (authEmail a) ++ show (authExpiry a)

authShelfLife :: EpochTime
authShelfLife = 30 * 24 * 60 * 60 -- 30 days
