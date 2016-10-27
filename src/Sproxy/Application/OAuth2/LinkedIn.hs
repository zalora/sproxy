{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
module Sproxy.Application.OAuth2.LinkedIn (
  provider
) where

import Control.Applicative (empty)
import Control.Exception (Exception, throwIO)
import Data.Aeson (FromJSON, decode, parseJSON, Value(Object), (.:))
import Data.ByteString.Char8 (pack)
import Data.ByteString.Lazy (ByteString)
import Data.Monoid ((<>))
import Data.Typeable (Typeable)
import Network.HTTP.Types (hContentType)
import Network.HTTP.Types.URI (urlEncode)
import qualified Network.HTTP.Conduit as H

import Sproxy.Application.Cookie (AuthUser(..))
import Sproxy.Application.OAuth2.Common (AccessTokenBody(accessToken), OAuth2Client(..), OAuth2Provider)


provider :: OAuth2Provider
provider (client_id, client_secret) =
  OAuth2Client {
    oauth2Description = "LinkedIn"
  , oauth2AuthorizeURL = \state redirect_uri ->
         "https://www.linkedin.com/oauth/v2/authorization"
      <> "?scope=r_basicprofile%20r_emailaddress"
      <> "&client_id=" <> urlEncode True client_id
      <> "&redirect_uri=" <> urlEncode True redirect_uri
      <> "&response_type=code"
      <> "&state=" <> urlEncode True state

  , oauth2Authenticate = \code redirect_uri -> do
      let treq = H.setQueryString [
                ("client_id"     , Just client_id)
              , ("client_secret" , Just client_secret)
              , ("code"          , Just code)
              , ("grant_type"    , Just "authorization_code")
              , ("redirect_uri"  , Just redirect_uri)
              ] $ (H.parseRequest_ "POST https://www.linkedin.com/oauth/v2/accessToken") {
                H.requestHeaders = [
                  (hContentType, "application/x-www-form-urlencoded")
                ]
              }
      mgr   <- H.newManager H.tlsManagerSettings
      tresp <- H.httpLbs treq mgr
      case decode $ H.responseBody tresp of
        Nothing -> throwIO $ LinkedInException tresp
        Just atResp -> do
          let ureq = (H.parseRequest_ "https://api.linkedin.com/v1/people/\
                \~:(email-address,first-name,last-name)?format=json") {
                  H.requestHeaders = [ ("Authorization", "Bearer " <> pack (accessToken atResp)) ]
                }
          uresp <- H.httpLbs ureq mgr
          case decode $ H.responseBody uresp of
            Nothing -> throwIO $ LinkedInException uresp
            Just u -> return AuthUser { auEmail = emailAddress u
                                      , auGivenName = firstName u
                                      , auFamilyName = lastName u }
  }


data LinkedInException = LinkedInException (H.Response ByteString)
  deriving (Show, Typeable)


instance Exception LinkedInException


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

