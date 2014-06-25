{-# LANGUAGE OverloadedStrings #-}
module Cookies (
  Name
, Value
, parseCookies
, removeCookie
, formatCookies
, setCookie
, invalidateCookie
) where

import           Data.Monoid
import           Data.String
import           Control.Applicative
import qualified Data.ByteString as BS
import Data.Char hiding (isSpace)
import Data.List (partition, intercalate)
import Network.HTTP.Types.Header (Header, hCookie)
import System.Posix.Types (EpochTime)
import Data.Attoparsec.ByteString.Char8

type Name = String
type Value = String

removeCookie :: String -> [(Name, Value)] -> Maybe (Value, [(Name, Value)])
removeCookie name cookies = case partition ((== name) . fst) cookies of
  ((_, x):_, xs) -> Just (x, xs)
  _ -> Nothing

setCookie :: String -> String -> String -> EpochTime -> String
setCookie domain name value maxAge = mkCookie domain [
    (name, value)
  , ("Max-Age", show maxAge)
  ]

invalidateCookie :: String -> String -> String
invalidateCookie domain name = mkCookie domain [
    (name, "deleted")
  , ("expires", "Thu, 01 Jan 1970 00:00:00 GMT")
  ]

mkCookie :: String -> [(String, String)] -> String
mkCookie domain = intercalate "; " . map format . (++ defaults) . map (fmap Just)
  where
    defaults = [
        ("Domain", Just domain)
      , ("path", Just "/")
      , ("HttpOnly", Nothing)
      , ("Secure", Nothing)
      ]
    format (name, mValue) = maybe name (\value -> name ++ "=" ++ value) mValue


formatCookies :: [(Name, Value)] -> BS.ByteString
formatCookies = mconcat . intercalate ["; "] . map formatCookie
  where
    formatCookie (name, value) = [fromString name, "=", fromString value]

parseCookies :: [Header] -> [(Name, Value)]
parseCookies = foldr headerToCookies []

headerToCookies :: Header -> [(Name, Value)] -> [(Name, Value)]
headerToCookies (name, val) acc
  | name == hCookie = case parseOnly cookies val of
      Left{}  -> acc
      Right x -> x ++ acc
  | otherwise = acc
  where
   cookies :: Parser [(Name, Value)]
   cookies = sepBy1 cookie (";" *> skipSpace)

   cookie :: Parser (Name, Value)
   cookie = (,) <$> word <*> (skipSpace *> "=" *> skipSpace *> value)

   value :: Parser String
   value = quotedstring <|> many1 (satisfy $ not . (==';')) <|> return ""

quotedstring :: Parser String
quotedstring = char '"' *> many (satisfy $ not . (=='"')) <* char '"'

word :: Parser String
word = many1 (satisfy (\x -> isAlphaNum x || x=='_' || x=='.' || x=='-' || x==':'))
