{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}
module HTTP (
  authenticationFailed
, accessDenied
) where

import           Network.HTTP.Types
import           Network.HTTP.Toolkit
import           Text.InterpolatedString.Perl6 (qc)

import           Type
import qualified Log

authenticationFailed :: SendData -> String -> IO ()
authenticationFailed send err = do
  Log.error err
  simpleResponse send internalServerError500 [("Content-Type", "text/html")] [qc|
<!DOCTYPE html>
<html lang="en">
  <head>
    <meta charset="utf-8">
    <title>Authentication Failed</title>
  </head>
  <body>
  <h1>Authentication Failed</h1>
    <p>Authentication Failed for an unknown reason.</p>
    <p><a href="/">Try again!</a></p>
  </body>
</html>
|]

accessDenied :: SendData -> String -> IO ()
accessDenied send email = simpleResponse send forbidden403 [("Content-Type", "text/html")] [qc|
<!DOCTYPE html>
<html lang="en">
  <head>
    <meta charset="utf-8">
    <title>Access Denied</title>
  </head>
  <body>
  <h1>Access Denied</h1>
    <p>You are currently logged in as <strong>{email}</strong>.</p>
    <p><a href="/sproxy/logout">logout</a></p>
  </body>
</html>
|]
