{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}
module HTTP (
  internalServerError
, accessDenied
) where

import           Network.HTTP.Types
import           Network.HTTP.Toolkit
import           Text.InterpolatedString.Perl6 (qc)

import           Type
import qualified Log

internalServerError :: SendData -> String -> IO ()
internalServerError send err = do
  Log.debug $ show err
  simpleResponse send internalServerError500 [] "Internal Server Error"

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
