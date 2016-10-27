{-# LANGUAGE OverloadedStrings #-}
module Sproxy.Application.OAuth2.Common (
  AccessTokenBody(..)
, OAuth2Client(..)
, OAuth2Provider
) where

import Control.Applicative (empty)
import Data.Aeson (FromJSON, parseJSON, Value(Object), (.:))
import Data.ByteString(ByteString)

import Sproxy.Application.Cookie (AuthUser)

data OAuth2Client = OAuth2Client {
  oauth2Description :: String
, oauth2AuthorizeURL
    :: ByteString -- state
    -> ByteString -- redirect url
    -> ByteString
, oauth2Authenticate
    :: ByteString -- code
    -> ByteString -- redirect url
    -> IO AuthUser
}

type OAuth2Provider = (ByteString, ByteString) -> OAuth2Client

-- | RFC6749. We ignore optional token_type ("Bearer" from Google, omitted by LinkedIn)
--   and expires_in because we don't use them, *and* expires_in creates troubles:
--   it's an integer from Google and string from LinkedIn (sic!)
data AccessTokenBody = AccessTokenBody {
  accessToken :: String
} deriving (Eq, Show)

instance FromJSON AccessTokenBody where
  parseJSON (Object v) = AccessTokenBody
    <$> v .:  "access_token"
  parseJSON _ = empty

