{-# LANGUAGE OverloadedStrings #-}
module HTTP (
  internalServerError
) where

import           Network.HTTP.Types
import           Network.HTTP.Toolkit

import           Type
import qualified Log

internalServerError :: SendData -> String -> IO ()
internalServerError send err = do
  Log.debug $ show err
  simpleResponse send internalServerError500 [] "Internal Server Error"
