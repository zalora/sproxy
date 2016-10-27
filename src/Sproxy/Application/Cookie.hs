module Sproxy.Application.Cookie (
  AuthCookie(..)
, AuthUser(..)
, cookieDecode
, cookieEncode
) where

import Data.ByteString (ByteString)
import Foreign.C.Types (CTime(..))
import qualified Data.Serialize as DS

import qualified Sproxy.Application.State as State

data AuthUser = AuthUser {
  auEmail      :: String
, auGivenName  :: String
, auFamilyName :: String
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


