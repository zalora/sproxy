{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}

module Sproxy.Application.OAuth2.Google
  ( provider
  ) where

import Control.Applicative (empty)
import Control.Exception (Exception, throwIO)
import Data.Aeson
       (FromJSON, Value(Object), (.:), decode, parseJSON)
import Data.ByteString.Lazy (ByteString)
import Data.Monoid ((<>))
import Data.Text (Text, unpack)
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
  { oauth2Description = "Google"
  , oauth2AuthorizeURL =
      \state redirect_uri ->
        "https://accounts.google.com/o/oauth2/v2/auth" <> "?scope=" <>
        urlEncode
          True
          "https://www.googleapis.com/auth/userinfo.email https://www.googleapis.com/auth/userinfo.profile" <>
        "&client_id=" <>
        urlEncode True client_id <>
        "&prompt=select_account" <>
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
              H.parseRequest_ "POST https://www.googleapis.com/oauth2/v4/token"
        mgr <- H.newManager H.tlsManagerSettings
        tresp <- H.httpLbs treq mgr
        case decode $ H.responseBody tresp of
          Nothing -> throwIO $ GoogleException tresp
          Just atResp -> do
            ureq <-
              H.parseRequest $
              unpack
                ("https://www.googleapis.com/oauth2/v1/userinfo?access_token=" <>
                 accessToken atResp)
            uresp <- H.httpLbs ureq mgr
            case decode $ H.responseBody uresp of
              Nothing -> throwIO $ GoogleException uresp
              Just u ->
                return $
                setFamilyName (familyName u) $
                setGivenName (givenName u) $ newUser (email u)
  }

data GoogleException =
  GoogleException (H.Response ByteString)
  deriving (Show, Typeable)

instance Exception GoogleException

data GoogleUserInfo = GoogleUserInfo
  { email :: Text
  , givenName :: Text
  , familyName :: Text
  } deriving (Eq, Show)

instance FromJSON GoogleUserInfo where
  parseJSON (Object v) =
    GoogleUserInfo <$> v .: "email" <*> v .: "given_name" <*> v .: "family_name"
  parseJSON _ = empty
