{-# LANGUAGE OverloadedStrings #-}
module HTTP (
  sendRequest
, sendResponse
, sendResponse_
, internalServerError
) where

import           Data.Foldable (forM_)
import           Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.UTF8 as UTF8
import qualified Data.CaseInsensitive as CI
import           Network.HTTP.Types
import           Network.HTTP.Toolkit.Body

import           Type
import qualified Log

internalServerError :: SendData -> String -> IO ()
internalServerError send err = do
  Log.debug $ show err
  sendResponse send internalServerError500 [] "Internal Server Error"

sendRequest :: SendData -> Method -> ByteString -> [Header] -> BodyReader -> IO ()
sendRequest send method path headers body = do
  sendHeader send startLine headers
  sendBody send body
  where
    startLine = B8.unwords [method, path, "HTTP/1.1"]

sendResponse :: SendData -> Status -> [Header] -> ByteString -> IO ()
sendResponse send status headers_ body = do
  sendHeader send (statusLine status) headers
  send body
  where
    headers = ("Content-Length", UTF8.fromString $ show $ B.length body) : headers_

sendResponse_ :: SendData -> Status -> [Header] -> BodyReader -> IO ()
sendResponse_ send status headers body = do
  sendHeader send (statusLine status) headers
  sendBody send body

statusLine :: Status -> ByteString
statusLine status = B.concat ["HTTP/1.1 ", UTF8.fromString $ show (statusCode status), " ", statusMessage status]

sendHeader :: SendData -> ByteString -> [Header] -> IO ()
sendHeader send startLine headers = do
  send startLine
  send "\r\n"
  forM_ headers $ \(k, v) -> do
    send $ B.concat [CI.original k, ": ", v, "\r\n"]
  send "\r\n"
