{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Authenticate.LinkedIn (
  authUrl
, authenticate
) where

import Control.Applicative (empty)
import Control.Exception (try)
import Data.Aeson (FromJSON, decode, parseJSON, Value(Object), (.:))
import Data.ByteString (ByteString)
import Data.Char (toLower)
import Data.Maybe (fromJust)
import Data.Monoid ((<>))
import Network.HTTP.Toolkit (Response, BodyReader)
import Network.HTTP.Types (found302, urlEncode, urlDecode)
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.UTF8 as UTF8
import qualified Network.HTTP.Conduit as HTTP

import Authenticate.Types (AccessToken(..), AuthConfig(..), OAuthClient(..))
import Authenticate.Token (AuthUser(..), mkAuthToken)
import Cookies (setCookie)
import HTTP (authenticationFailed, mkResponse, post, get)
import qualified Logging as Log

authUrl :: AuthConfig -> ByteString -> ByteString -> Maybe ByteString
authUrl c base path = url . oauthClientId <$> authConfigLinkedInClient c
  where
    url cid = mconcat [
        "https://www.linkedin.com/oauth/v2/authorization"
      , "?scope=r_basicprofile%20r_emailaddress"
      , "&state=", urlEncode True path
      , "&redirect_uri=", redirectUri base
      , "&response_type=code&client_id=", cid
      ]

authenticate :: AuthConfig -> ByteString -> ByteString -> ByteString -> IO (Response BodyReader)
authenticate acfg base path code = do
  Log.debug ("LinkedIn authentication request with code " ++ show code)
  tokenRes <- try $ post "https://www.linkedin.com/oauth/v2/accessToken"
              ("code=" <> code <> "&client_id=" <> clientId
                <> "&client_secret=" <> clientSecret <> "&redirect_uri="
                <> (redirectUri base) <> "&grant_type=authorization_code")
  case tokenRes of
    Left err -> authenticationFailed ("error while authenticating: " ++ show (err :: HTTP.HttpException))
    Right tresp -> do
      Log.debug (show tresp)
      case decode (HTTP.responseBody tresp) of
        Nothing ->
          authenticationFailed "Received an invalid response from LinkedIn authentication server."
        Just oat -> do
          infoRes <- try $ get ("https://api.linkedin.com/v1/people/~:(email-address,first-name,last-name)?format=json")
                               [ ("Authorization", "Bearer " <> B8.pack (accessToken oat)) ]
          case infoRes of
            Left err -> authenticationFailed ("error while retrieving user info: " ++ show (err :: HTTP.HttpException))
            Right uresp -> do
              Log.debug (show uresp)
              case decode (HTTP.responseBody uresp) of
                Nothing -> authenticationFailed "Received an invalid user info response from LinkedIn authentication server."
                Just (LinkedInUserInfo{emailAddress, firstName, lastName}) -> do
                  token <- mkAuthToken acfg AuthUser{ authUserEmail = map toLower emailAddress
                                                    , authUserGivenName = firstName
                                                    , authUserFamilyName = lastName }
                  let cookie = setCookie cookieDomain cookieName (show token) (authConfigShelfLife acfg)
                  mkResponse found302 [("Location", base <> urlDecode False path), ("Set-Cookie", UTF8.fromString cookie)] ""
  where
    cookieDomain = authConfigCookieDomain acfg
    cookieName = authConfigCookieName acfg
    clientId = oauthClientId . fromJust $ authConfigLinkedInClient acfg
    clientSecret = oauthClientSecret . fromJust $ authConfigLinkedInClient acfg


redirectUri :: ByteString -> ByteString
redirectUri base = base <> "/sproxy/oauth2callback/linkedin"

data LinkedInUserInfo = LinkedInUserInfo {
  emailAddress :: String
, firstName :: String
, lastName :: String
} deriving (Eq, Show)

instance FromJSON LinkedInUserInfo where
  parseJSON (Object v) = LinkedInUserInfo
    <$> v .: "emailAddress"
    <*> v .: "firstName"
    <*> v .: "lastName"
  parseJSON _ = empty

