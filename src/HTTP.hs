{-# LANGUAGE OverloadedStrings #-}
module HTTP (
  sendResponse_
, internalServerError
) where

import           Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B
import           Network.HTTP.Types
import           Network.HTTP.Toolkit
import qualified Network.HTTP.Toolkit.Body as Body

import           Type
import qualified Log

internalServerError :: SendData -> String -> IO ()
internalServerError send err = do
  Log.debug $ show err
  sendResponse_ send internalServerError500 [] "Internal Server Error"

sendResponse_ :: SendData -> Status -> [Header] -> ByteString -> IO ()
sendResponse_ send status headers_ body = do
  Body.fromByteString body >>= sendResponse send . Response status headers
  where
    headers = ("Content-Length", B.pack . show . B.length $ body) : headers_
