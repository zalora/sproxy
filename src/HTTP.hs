{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}
module HTTP (
  hostHeaderMissing
, authenticationFailed
, accessDenied
, badRequest
, mkResponse
) where

import           Control.Applicative
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B
import           Network.HTTP.Types
import           Network.HTTP.Toolkit
import           Network.HTTP.Toolkit.Body
import           Text.InterpolatedString.Perl6 (qc)

import qualified Log

hostHeaderMissing :: Request a -> IO (Response BodyReader)
hostHeaderMissing r = do
  Log.warning $ "Host header missing for request: " ++ show (requestMethod r, requestPath r, requestHeaders r)
  mkTextResponse badRequest400 "400 Bad Request"

authenticationFailed :: String -> IO (Response BodyReader)
authenticationFailed err = do
  Log.error err
  mkHtmlResponse internalServerError500 [qc|
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

accessDenied :: String -> IO (Response BodyReader)
accessDenied email = mkHtmlResponse forbidden403 [qc|
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

badRequest :: IO (Response BodyReader)
badRequest = mkTextResponse badRequest400 "400 Bad Request"

mkResponse :: Status -> [Header] -> ByteString -> IO (Response BodyReader)
mkResponse status headers_ body = Response http11 status headers <$> fromByteString body
  where
    headers = ("Content-Length", B.pack . show . B.length $ body) : headers_

mkTextResponse :: Status -> ByteString -> IO (Response BodyReader)
mkTextResponse status = mkResponse status [("Content-Type", "text/plain")]

mkHtmlResponse :: Status -> ByteString -> IO (Response BodyReader)
mkHtmlResponse status = mkResponse status [("Content-Type", "text/html")]
