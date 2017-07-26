{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}

module Sproxy.Application.OAuth2.Yandex
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
  { oauth2Description = "Yandex"
  , oauth2AuthorizeURL =
      \state _redirect_uri ->
        "https://oauth.yandex.ru/authorize" <> "?state=" <> urlEncode True state <>
        "&client_id=" <>
        urlEncode True client_id <>
        "&response_type=code" <>
        "&force_confirm=yes"
  , oauth2Authenticate =
      \code _redirect_uri -> do
        let treq =
              H.urlEncodedBody
                [ ("grant_type", "authorization_code")
                , ("client_id", client_id)
                , ("client_secret", client_secret)
                , ("code", code)
                ] $
              H.parseRequest_ "POST https://oauth.yandex.ru/token"
        mgr <- H.newManager H.tlsManagerSettings
        tresp <- H.httpLbs treq mgr
        case decode $ H.responseBody tresp of
          Nothing -> throwIO $ YandexException tresp
          Just atResp -> do
            let ureq =
                  (H.parseRequest_ "https://login.yandex.ru/info?format=json")
                  { H.requestHeaders =
                      [ ( "Authorization"
                        , "OAuth " <> encodeUtf8 (accessToken atResp))
                      ]
                  }
            uresp <- H.httpLbs ureq mgr
            case decode $ H.responseBody uresp of
              Nothing -> throwIO $ YandexException uresp
              Just u ->
                return $
                setFamilyName (lastName u) $
                setGivenName (firstName u) $ newUser (defaultEmail u)
  }

data YandexException =
  YandexException (H.Response ByteString)
  deriving (Show, Typeable)

instance Exception YandexException

data YandexUserInfo = YandexUserInfo
  { defaultEmail :: Text
  , firstName :: Text
  , lastName :: Text
  } deriving (Eq, Show)

instance FromJSON YandexUserInfo where
  parseJSON (Object v) =
    YandexUserInfo <$> v .: "default_email" <*> v .: "first_name" <*>
    v .: "last_name"
  parseJSON _ = empty
