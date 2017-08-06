{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}

module Sproxy.Application.OAuth2.LinkedIn
  ( provider
  ) where

import Control.Applicative (empty)
import Control.Exception (Exception, throwIO)
import Data.Aeson
       (FromJSON, Value(Object), (.:), decode, parseJSON)
import Data.ByteString.Lazy (ByteString)
import Data.Monoid ((<>))
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import Data.Typeable (Typeable)
import qualified Network.HTTP.Conduit as H
import Network.HTTP.Types.URI (urlEncode)

import Sproxy.Application.Cookie
       (newUser, setFamilyName, setGivenName)
import Sproxy.Application.OAuth2.Common
       (AccessTokenBody(accessToken), OAuth2Client(..), OAuth2Provider)

provider :: OAuth2Provider
provider (client_id, client_secret) =
  OAuth2Client
  { oauth2Description = "LinkedIn"
  , oauth2AuthorizeURL =
      \state redirect_uri ->
        "https://www.linkedin.com/oauth/v2/authorization" <>
        "?scope=r_basicprofile%20r_emailaddress" <>
        "&client_id=" <>
        urlEncode True client_id <>
        "&redirect_uri=" <>
        urlEncode True redirect_uri <>
        "&response_type=code" <>
        "&state=" <>
        urlEncode True state
  , oauth2Authenticate =
      \code redirect_uri -> do
        let treq =
              H.urlEncodedBody
                [ ("client_id", client_id)
                , ("client_secret", client_secret)
                , ("code", code)
                , ("grant_type", "authorization_code")
                , ("redirect_uri", redirect_uri)
                ] $
              H.parseRequest_
                "POST https://www.linkedin.com/oauth/v2/accessToken"
        mgr <- H.newManager H.tlsManagerSettings
        tresp <- H.httpLbs treq mgr
        case decode $ H.responseBody tresp of
          Nothing -> throwIO $ LinkedInException tresp
          Just atResp -> do
            let ureq =
                  (H.parseRequest_
                     "https://api.linkedin.com/v1/people/\
                \~:(email-address,first-name,last-name)?format=json")
                  { H.requestHeaders =
                      [ ( "Authorization"
                        , "Bearer " <> encodeUtf8 (accessToken atResp))
                      ]
                  }
            uresp <- H.httpLbs ureq mgr
            case decode $ H.responseBody uresp of
              Nothing -> throwIO $ LinkedInException uresp
              Just u ->
                return $
                setFamilyName (lastName u) $
                setGivenName (firstName u) $ newUser (emailAddress u)
  }

data LinkedInException =
  LinkedInException (H.Response ByteString)
  deriving (Show, Typeable)

instance Exception LinkedInException

data LinkedInUserInfo = LinkedInUserInfo
  { emailAddress :: Text
  , firstName :: Text
  , lastName :: Text
  } deriving (Eq, Show)

instance FromJSON LinkedInUserInfo where
  parseJSON (Object v) =
    LinkedInUserInfo <$> v .: "emailAddress" <*> v .: "firstName" <*>
    v .: "lastName"
  parseJSON _ = empty
