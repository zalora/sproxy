{-# LANGUAGE OverloadedStrings #-}
module Sproxy.Application.OAuth2 (
  providers
) where

import Data.HashMap.Strict (HashMap, fromList)
import Data.Text (Text)

import Sproxy.Application.OAuth2.Common (OAuth2Provider)
import qualified Sproxy.Application.OAuth2.Google as Google
import qualified Sproxy.Application.OAuth2.LinkedIn as LinkedIn

providers :: HashMap Text OAuth2Provider
providers = fromList [
    ("google"   , Google.provider)
  , ("linkedin" , LinkedIn.provider)
  ]

