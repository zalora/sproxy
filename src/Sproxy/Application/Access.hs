{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Sproxy.Application.Access
  ( Inquiry
  , Question(..)
  ) where

import Data.Aeson (FromJSON)
import Data.HashMap.Strict (HashMap)
import Data.Text (Text)
import GHC.Generics (Generic)

data Question = Question
  { path :: Text
  , method :: Text
  } deriving (Generic, Show)

instance FromJSON Question

type Inquiry = HashMap Text Question
