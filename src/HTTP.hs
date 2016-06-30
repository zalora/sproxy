{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module HTTP (
  hostHeaderMissing
, accessDenied
, authenticationFailed
, badRequest
, get
, mkHtmlResponse
, mkResponse
, mkTextResponse
, post
) where

import Network.HTTP.Toolkit
import Network.HTTP.Toolkit.Body
import Network.HTTP.Types
import Text.InterpolatedString.Perl6 (qc)
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy.Char8 as BL8
import qualified Network.HTTP.Conduit as HTTP

import qualified Logging as Log

hostHeaderMissing :: Request a -> IO (Response BodyReader)
hostHeaderMissing r = do
  Log.warn $ "Host header missing for request: " ++ show (requestMethod r, requestPath r, requestHeaders r)
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

mkResponse :: Status -> [Header] -> B8.ByteString -> IO (Response BodyReader)
mkResponse status headers_ body = Response status headers <$> fromByteString body
  where
    headers = ("Content-Length", B8.pack . show . B8.length $ body) : headers_

mkTextResponse :: Status -> B8.ByteString -> IO (Response BodyReader)
mkTextResponse status = mkResponse status [("Content-Type", "text/plain")]

mkHtmlResponse :: Status -> B8.ByteString -> IO (Response BodyReader)
mkHtmlResponse status = mkResponse status [("Content-Type", "text/html")]

post :: String -> B8.ByteString -> IO (HTTP.Response BL8.ByteString)
post url body = do
  r' <- HTTP.parseUrl url
  let r = r' { HTTP.method = "POST"
             , HTTP.requestBody = HTTP.RequestBodyBS body
             , HTTP.requestHeaders
               = [ ("Content-Type" , "application/x-www-form-urlencoded")
                 ]
             }
  manager <- HTTP.newManager HTTP.tlsManagerSettings
  HTTP.httpLbs r manager

get :: String -> [Header] -> IO (HTTP.Response BL8.ByteString)
get url hdrs = do
  r' <- HTTP.parseUrl url
  let r = r' { HTTP.method = "GET"
             , HTTP.requestHeaders = hdrs
             }
  manager <- HTTP.newManager HTTP.tlsManagerSettings
  HTTP.httpLbs r manager

