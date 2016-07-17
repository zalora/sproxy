{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Authenticate (
  logout
, authenticationRequired
, validAuth
) where

import Data.ByteString (ByteString)
import Data.Monoid ((<>))
import Network.HTTP.Toolkit (Response, BodyReader)
import Network.HTTP.Types (found302, Status(..), urlEncode)
import System.Posix.Time (epochTime)
import Text.InterpolatedString.Perl6 (qc)
import Text.Read (readMaybe)
import qualified Data.ByteString.UTF8 as UTF8

import Authenticate.Token (AuthToken(..), tokenDigest)
import Authenticate.Types (AuthConfig(..))
import Cookies (invalidateCookie)
import HTTP (mkResponse, mkHtmlResponse)
import qualified Authenticate.Google as Google
import qualified Authenticate.LinkedIn as LinkedIn

authenticationRequired :: AuthConfig -> ByteString -> ByteString -> IO (Response BodyReader)
authenticationRequired c base path =
  mkHtmlResponse (Status 511 "Authentication Required") body
  where
    google :: ByteString
    google = maybe "" (\u -> [qc|<p><a href="{u}">Authenticate with Google</a></p>|]) (Google.authUrl c base path)
    linkedin :: ByteString
    linkedin = maybe "" (\u -> [qc|<p><a href="{u}">Authenticate with LinkedIn</a></p>|]) (LinkedIn.authUrl c base path)
    body = [qc|
      <!DOCTYPE html>
      <html lang="en">
        <head>
          <meta charset="utf-8">
          <title>Authentication required</title>
        </head>
        <body style="text-align:center;">
        <h1>Authentication required</h1>
          {google}
          {linkedin}
        </body>
      </html>
      |]

-- ("Location", Google.authUrl path baseUri clientId)

logout :: AuthConfig -> ByteString -> IO (Response BodyReader)
logout config url = do
  let cookie = invalidateCookie cookieDomain cookieName
  mkResponse found302 [("Location", url), ("Set-Cookie", UTF8.fromString cookie)] ""
  where
    cookieDomain = authConfigCookieDomain config
    cookieName = authConfigCookieName config

validAuth :: AuthConfig -> String -> IO (Maybe AuthToken)
validAuth config token =
  case readMaybe token of
    Nothing -> return Nothing
    Just t -> do
      now <- epochTime
      if tokenDigest key t == authDigest t && authExpiry t > now
        then return $ Just t
        else return Nothing
  where
    key = authConfigAuthTokenKey config

