module HTTP (Request, Response, oneRequest, oneResponse, rawRequest, rawResponse) where

import Control.Applicative ((<$>), (<*>), (<*), (*>))
import Data.Attoparsec.ByteString.Char8 (Parser, char, skipSpace, isSpace, endOfLine, takeTill, decimal)
import Data.Attoparsec.ByteString.Lazy (parse, Result(..))
import Data.Attoparsec.Combinator (manyTill)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.UTF8 as BU
import qualified Data.CaseInsensitive as CI
import Network.HTTP.Types.Header (Header)
import Network.HTTP.Types.Method (Method)
import Network.HTTP.Types.Status (Status(..), mkStatus)
import Text.Read (readMaybe)

type URL = BS.ByteString
type Body = BL.ByteString
type Request = (Method, URL, [Header], Body)
type Response = (Status, [Header], Body)

-- These parsers sacrifice correctness for simplicity/speed.

isEndOfLine :: Char -> Bool
isEndOfLine '\r' = True
isEndOfLine '\n' = True
isEndOfLine _    = False

-- TODO: Support multi-line headers.
headerP :: Parser Header
headerP = (\h v -> (CI.mk h, v)) <$> (takeTill (== ':') <* char ':' <* skipSpace) <*> (takeTill isEndOfLine <* endOfLine)

requestLineP :: Parser (Method, URL)
requestLineP = (,) <$> (takeTill isSpace <* skipSpace) <*> (takeTill isSpace <* takeTill isEndOfLine <* endOfLine)

responseLineP :: Parser Status
responseLineP = mkStatus <$> ((takeTill isSpace <* skipSpace) *> (decimal <* skipSpace)) <*> (takeTill isEndOfLine <* endOfLine)

oneRequest :: BL.ByteString -> (Maybe Request, BL.ByteString)
oneRequest s = case parse ((,) <$> requestLineP <*> manyTill headerP endOfLine) s of
                 Fail "" _ _ -> (Nothing, "")
                 Fail _ _ err -> error err
                 Done rest ((method, url), headers) -> let (body, rest') = requestBody headers rest in
                                                         (Just (method, url, headers, body), rest')

oneResponse :: BL.ByteString -> (Maybe Response, BL.ByteString)
oneResponse s = case parse ((,) <$> responseLineP <*> manyTill headerP endOfLine) s of
                  Fail "" _ _ -> (Nothing, "")
                  Fail _ _ err -> error err
                  Done rest (status, headers) -> let (body, rest') = responseBody headers rest in
                                                   (Just (status, headers, body), rest')

requestBody :: [Header] -> BL.ByteString -> (Body, BL.ByteString)
requestBody hs s =
  case lookup "Content-Length" hs of
    -- TODO: Support Transfer-Encoding (e.g. chunked).
    Nothing -> ("", s)
    Just cl -> case readMaybe $ B8.unpack cl of
                 Nothing -> error "Malformed Content-Length header."
                 Just i -> BL.splitAt i s

responseBody :: [Header] -> BL.ByteString -> (Body, BL.ByteString)
responseBody hs s =
  case lookup "Connection" hs of
    Just "close" -> (s, "") -- Read up until the end of the string.
    _ -> case lookup "Content-Length" hs of
           Nothing -> ("", s)
           Just cl -> case readMaybe $ B8.unpack cl of
                        Nothing -> error "Malformed Content-Length header."
                        Just i -> BL.splitAt i s

-- Note that this may not exactly match the original request.
rawRequest :: Request -> BL.ByteString
rawRequest (method, url, headers, body) =
  -- TODO: Switch to HTTP/1.1 once we support re-using the client connection.
  BL.fromChunks ([method, " ", url, " HTTP/1.0\r\n"] ++ map headerBS headers ++ ["\r\n"]) `BL.append` body

rawResponse :: Response -> BL.ByteString
rawResponse (status, headers, body) =
  BL.fromChunks (["HTTP/1.1 ", BU.fromString $ show (statusCode status), " ", statusMessage status] ++ map headerBS headers ++ ["\r\n"]) `BL.append` body

headerBS :: Header -> BS.ByteString
headerBS (k, v) = CI.original k `BS.append` ": " `BS.append` v `BS.append` "\r\n"
