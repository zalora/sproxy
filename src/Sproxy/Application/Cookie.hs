{-# LANGUAGE OverloadedStrings #-}
module Sproxy.Application.Cookie (
  AuthCookie(..)
, AuthUser
, cookieDecode
, cookieEncode
, getEmail
, getEmailUtf8
, getFamilyNameUtf8
, getGivenNameUtf8
, newUser
, setFamilyName
, setGivenName
) where

import Data.ByteString (ByteString)
import Data.Text (Text, toLower, strip)
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Foreign.C.Types (CTime(..))
import qualified Data.Serialize as DS

import qualified Sproxy.Application.State as State

data AuthUser = AuthUser {
  auEmail      :: ByteString
, auGivenName  :: ByteString
, auFamilyName :: ByteString
}

data AuthCookie = AuthCookie {
  acUser   :: AuthUser
, acExpiry :: CTime
}

instance DS.Serialize AuthCookie where
  put c = DS.put (auEmail u, auGivenName u, auFamilyName u, x)
          where u = acUser c
                x = (\(CTime i) -> i) $ acExpiry c
  get = do
    (e, n, f, x) <- DS.get
    return AuthCookie {
        acUser = AuthUser { auEmail = e, auGivenName = n, auFamilyName = f }
      , acExpiry = CTime x
      }


cookieDecode :: ByteString -> ByteString -> Either String AuthCookie
cookieDecode key d = State.decode key d >>= DS.decode

cookieEncode :: ByteString -> AuthCookie -> ByteString
cookieEncode key = State.encode key . DS.encode


getEmail :: AuthUser -> Text
getEmail = decodeUtf8 . auEmail

getEmailUtf8 :: AuthUser -> ByteString
getEmailUtf8 = auEmail

getGivenNameUtf8 :: AuthUser -> ByteString
getGivenNameUtf8 = auGivenName

getFamilyNameUtf8 :: AuthUser -> ByteString
getFamilyNameUtf8 = auFamilyName


newUser :: Text -> AuthUser
newUser email = AuthUser {
    auEmail = encodeUtf8 . toLower . strip $ email
  , auGivenName = ""
  , auFamilyName = ""
  }

setGivenName :: Text -> AuthUser -> AuthUser
setGivenName given au = au{ auGivenName = encodeUtf8 . strip $ given }

setFamilyName :: Text -> AuthUser -> AuthUser
setFamilyName family au = au{ auFamilyName = encodeUtf8 . strip $ family }

