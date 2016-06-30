{-# LANGUAGE OverloadedStrings #-}

module Authenticate.Types (
  AccessToken(..)
, AuthConfig(..)
, OAuthClient(..)
) where

import Control.Applicative (empty)
import Data.Aeson (FromJSON, parseJSON, Value(Object), (.:), (.:?))
import Data.ByteString (ByteString)
import System.Posix.Types (EpochTime)

data OAuthClient = OAuthClient {
  oauthClientId :: ByteString
, oauthClientSecret :: ByteString
} deriving (Eq, Show)

data AuthConfig = AuthConfig {
  authConfigCookieDomain   :: String
, authConfigCookieName     :: String
, authConfigGoogleClient   :: Maybe OAuthClient
, authConfigLinkedInClient :: Maybe OAuthClient
, authConfigAuthTokenKey   :: String
, authConfigShelfLife      :: EpochTime
} deriving (Eq, Show)

-- | RFC6749. We ignore optional token_type ("Bearer" from Google, omitted by LinkedIn)
--   and expires_in because we don't use them, *and* expires_in creates troubles:
--   it's an integer from Google and string from LinkedIn (sic!)
data AccessToken = AccessToken {
  accessToken :: String
} deriving (Eq, Show)

instance FromJSON AccessToken where
  parseJSON (Object v) = AccessToken
    <$> v .:  "access_token"
  parseJSON _ = empty

