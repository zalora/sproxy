{-# LANGUAGE OverloadedStrings #-}
module Util where

import           Control.Applicative
import           Data.Char
import           Data.String
import           Data.Monoid
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.ByteString (ByteString)
import           Network.HTTP.Types
import           Network.Socket

formatSockAddr :: SockAddr -> IO HostName
formatSockAddr addr = (fst <$> getNameInfo [NI_NUMERICHOST] True False addr) >>= maybe err return
  where
    err = error "Util.formatSockAddr: impossible internal error"

addForwardedForHeader :: HostName -> Map HeaderName ByteString -> Map HeaderName ByteString
addForwardedForHeader ip = Map.insertWith combine "X-Forwarded-For" (fromString ip)
  where
    combine new old = mconcat [old, ", ", new]

removeConnectionHeader :: [Header] -> [Header]
removeConnectionHeader = filter ((/= hConnection) . fst)

strip :: String -> String
strip = reverse . dropWhile isSpace . reverse . dropWhile isSpace

baseUri :: [Header] -> Maybe ByteString
baseUri headers = ("https://" <>) <$> lookup "Host" headers
