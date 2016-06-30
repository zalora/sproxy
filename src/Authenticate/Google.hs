{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Authenticate.Google (
  authUrl
, authenticate
) where

import Control.Applicative (empty)
import Control.Exception (try)
import Data.Aeson (FromJSON, decode, parseJSON, Value(Object), (.:))
import Data.ByteString (ByteString)
import Data.Maybe (fromJust)
import Data.Monoid ((<>))
import Network.HTTP.Toolkit (Response, BodyReader)
import Network.HTTP.Types (found302, urlEncode, urlDecode)
import qualified Data.ByteString.UTF8 as UTF8
import qualified Network.HTTP.Conduit as HTTP

import Authenticate.Types (AccessToken(..), AuthConfig(..), OAuthClient(..))
import Authenticate.Token (AuthUser(..), mkAuthToken)
import Cookies (setCookie)
import HTTP (authenticationFailed, mkResponse, post, get)
import qualified Logging as Log

authUrl :: AuthConfig -> ByteString -> ByteString -> Maybe ByteString
authUrl c base path = url . oauthClientId <$> authConfigGoogleClient c
  where
    url cid = mconcat [
        "https://accounts.google.com/o/oauth2/auth"
      , "?scope=https%3A%2F%2Fwww.googleapis.com%2Fauth%2Fuserinfo.email+https%3A%2F%2Fwww.googleapis.com%2Fauth%2Fuserinfo.profile"
      , "&state=", urlEncode True path
      , "&redirect_uri=", redirectUri base
      , "&response_type=code&client_id=", cid
      , "&approval_prompt=force&access_type=offline"
      ]

authenticate :: AuthConfig -> ByteString -> ByteString -> ByteString -> IO (Response BodyReader)
authenticate acfg base path code = do
  Log.info ("Google authentication request with code " ++ show code)
  tokenRes <- try $ post "https://accounts.google.com/o/oauth2/token"
              ("code=" <> code <> "&client_id=" <> clientId
                <> "&client_secret=" <> clientSecret <> "&redirect_uri="
                <> (redirectUri base) <> "&grant_type=authorization_code")
  case tokenRes of
    Left err -> authenticationFailed ("error while authenticating: " ++ show (err :: HTTP.HttpException))
    Right tresp -> do
      Log.debug (show tresp)
      case decode (HTTP.responseBody tresp) of
        Nothing ->
          authenticationFailed "Received an invalid response from Google's authentication server."
        Just gtoken -> do
          infoRes <- try $ get ("https://www.googleapis.com/oauth2/v1/userinfo?access_token=" ++ accessToken gtoken) []
          case infoRes of
            Left err -> authenticationFailed ("error while retrieving user info: " ++ show (err :: HTTP.HttpException))
            Right uresp ->
              case decode (HTTP.responseBody uresp) of
                Nothing -> authenticationFailed "Received an invalid user info response from Google's authentication server."
                Just (GoogleUserInfo{email, givenName, familyName}) -> do
                  token <- mkAuthToken acfg AuthUser{ authUserEmail = email
                                                    , authUserGivenName = givenName
                                                    , authUserFamilyName = familyName }
                  let cookie = setCookie cookieDomain cookieName (show token) (authConfigShelfLife acfg)
                  mkResponse found302 [("Location", base <> urlDecode False path), ("Set-Cookie", UTF8.fromString cookie)] ""
  where
    cookieDomain = authConfigCookieDomain acfg
    cookieName = authConfigCookieName acfg
    clientId = oauthClientId . fromJust $ authConfigGoogleClient acfg
    clientSecret = oauthClientSecret . fromJust $ authConfigGoogleClient acfg


redirectUri :: ByteString -> ByteString
redirectUri base = base <> "/sproxy/oauth2callback"

data GoogleUserInfo = GoogleUserInfo {
  email :: String
, givenName :: String
, familyName :: String
} deriving (Eq, Show)

instance FromJSON GoogleUserInfo where
  parseJSON (Object v) = GoogleUserInfo
    <$> v .: "email"
    <*> v .: "given_name"
    <*> v .: "family_name"
  parseJSON _ = empty


