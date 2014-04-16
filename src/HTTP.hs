{-# LANGUAGE OverloadedStrings #-}
module HTTP (Request(..), Response, oneRequest, oneResponse, rawRequest, rawResponse, response) where

import Control.Applicative ((<$>), (<*>), (<*), (*>))
import Data.Attoparsec.ByteString.Char8 (Parser, char, skipSpace, isSpace, endOfLine, takeTill, take, decimal, hexadecimal)
import Data.Attoparsec.ByteString.Lazy (parse, Result(..))
import Data.Attoparsec.Combinator (manyTill)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.UTF8 as BU
import qualified Data.CaseInsensitive as CI
import Data.String.Conversions (cs)
import Network.HTTP.Types.Header (Header)
import Network.HTTP.Types.Method (Method)
import Network.HTTP.Types.Status (Status(..), mkStatus)
import Numeric (showHex)
import Text.Read (readMaybe)

import Prelude hiding (length, take)

type Body = BL.ByteString
type Response = (Status, [Header], Body)

data Request = Request {
  requestMethod :: Method
, requestPath :: BS.ByteString
, requestHeaders :: [Header]
, requestBody :: Body
} deriving (Eq, Show)

-- These parsers sacrifice correctness for simplicity/speed.

isEndOfLine :: Char -> Bool
isEndOfLine '\r' = True
isEndOfLine '\n' = True
isEndOfLine _    = False

-- TODO: Support multi-line headers.
headerP :: Parser Header
headerP = (\h v -> (CI.mk h, v)) <$> (takeTill (== ':') <* char ':' <* skipSpace) <*> (takeTill isEndOfLine <* endOfLine)

requestLineP :: Parser (Method, BS.ByteString)
requestLineP = (,) <$> (takeTill isSpace <* skipSpace) <*> (takeTill isSpace <* takeTill isEndOfLine <* endOfLine)

responseLineP :: Parser Status
responseLineP = mkStatus <$> ((takeTill isSpace <* skipSpace) *> (decimal <* skipSpace)) <*> (takeTill isEndOfLine <* endOfLine)

chunkP :: Parser (Int, BS.ByteString)
chunkP = do
  length <- hexadecimal
  extensions <- takeTill isEndOfLine -- Ignore chunked extensions.
  chunk <- take (length + 4) -- + 4 is to catch the preceeding and trailing \r\n
  return $ (length, cs (showHex length "") `BS.append` extensions `BS.append` chunk)

oneRequest :: BL.ByteString -> (Maybe Request, BL.ByteString)
oneRequest s = case parse ((,) <$> requestLineP <*> manyTill headerP endOfLine) s of
                 Fail "" _ _ -> (Nothing, "")
                 Fail _ _ err -> error err
                 Done rest ((method, url), headers) -> let (body, rest') = mkBody headers rest in
                                                         (Just (Request method url headers body), rest')

oneResponse :: BL.ByteString -> (Maybe Response, BL.ByteString)
oneResponse s = case parse ((,) <$> responseLineP <*> manyTill headerP endOfLine) s of
                  Fail "" _ _ -> (Nothing, "")
                  Fail _ _ err -> error err
                  Done rest (status, headers) -> let (body, rest') = responseBody headers rest in
                                                   (Just (status, headers, body), rest')

mkBody :: [Header] -> BL.ByteString -> (Body, BL.ByteString)
mkBody hs s =
  case lookup "Content-Length" hs of
    Nothing -> case lookup "Transfer-Encoding" hs of
                 Just "chunked" -> chunkedBody s
                 _ -> ("", s)
    Just cl -> case readMaybe $ B8.unpack cl of
                 Nothing -> error "Malformed Content-Length header."
                 Just i -> BL.splitAt i s
 where chunkedBody :: BL.ByteString -> (Body, BL.ByteString)
       chunkedBody bs =
         case parse chunkP bs of
           Fail _ _ _ -> error "Failed reading chunked transfer encoding."
           Done rest (0, body) -> (BL.fromStrict body, rest)
           -- TODO: Support trailing headers.
           Done rest (_, body) -> let (body', rest') = chunkedBody rest
                                  in (BL.fromStrict body `BL.append` body', rest')

responseBody :: [Header] -> BL.ByteString -> (Body, BL.ByteString)
responseBody hs s =
  case lookup "Connection" hs of
    Just "close" -> (s, "") -- Read up until the end of the string.
    _ -> mkBody hs s

-- Note that this may not exactly match the original request.
rawRequest :: Request -> BL.ByteString
rawRequest (Request method url headers body) =
  BL.fromChunks ([method, " ", url, " HTTP/1.1\r\n"] ++ map headerBS headers ++ ["\r\n"]) `BL.append` body

rawResponse :: Response -> BL.ByteString
rawResponse (status, headers, body) =
  BL.fromChunks (["HTTP/1.1 ", BU.fromString $ show (statusCode status), " ", statusMessage status, "\r\n"] ++ map headerBS headers ++ ["\r\n"]) `BL.append` body

headerBS :: Header -> BS.ByteString
headerBS (k, v) = CI.original k `BS.append` ": " `BS.append` v `BS.append` "\r\n"

-- convenience responses
response :: Int -> BS.ByteString -> [Header] -> BL.ByteString -> Response
response code message headers body = (mkStatus code message, headers ++ [("Content-Length", BU.fromString $ show $ BL.length body)], body)
