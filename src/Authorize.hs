{-# LANGUAGE QuasiQuotes #-}
module Authorize (
  AuthorizeAction
, Email
, Domain
, RequestPath
, Group
) where

import           Data.ByteString (ByteString)
import           Network.HTTP.Types

type AuthorizeAction = Email -> Domain -> RequestPath -> Method -> IO [Group]

type Email = String
type Domain = ByteString
type RequestPath = ByteString
type Group = String
