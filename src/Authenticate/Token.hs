{-# LANGUAGE OverloadedStrings #-}

module Authenticate.Token (
  AuthToken(..)
, AuthUser(..)
, mkAuthToken
, tokenDigest
) where

import Data.Digest.Pure.SHA (hmacSha1, showDigest)
import Data.List.Split (splitOn)
import System.Posix.Time (epochTime)
import System.Posix.Types (EpochTime)
import qualified Data.ByteString.Lazy.Char8 as BL8

import Authenticate.Types (AuthConfig(authConfigAuthTokenKey, authConfigShelfLife))

data AuthUser = AuthUser {
  authUserEmail :: String
, authUserGivenName :: String
, authUserFamilyName :: String
} deriving (Eq)

data AuthToken = AuthToken {
  authDigest :: String
, authExpiry :: EpochTime
, authUser   :: AuthUser
}

-- Here is the format of the actual cookie we send to the client.
instance Show AuthToken where
  show a = user ++ ":" ++ show (authExpiry a) ++ ":" ++ authDigest a
    where
      u = authUser a
      user = authUserEmail u ++
          ":" ++ authUserGivenName u ++
          ":" ++ authUserFamilyName u

instance Read AuthToken where
  readsPrec _ s = case splitOn ":" s of
    [email, fn, ln, xpr, dgst]
      -> [(AuthToken
          { authUser = AuthUser email fn ln
          , authExpiry = read xpr
          , authDigest = dgst
          }, "")]
    _ -> []


-- | This generates the HMAC digest of the auth token using SHA1.
tokenDigest :: String -> AuthToken -> String
tokenDigest key a = showDigest $ hmacSha1 (BL8.pack key) (BL8.pack token)
  where token = show (authUserEmail . authUser $ a) ++ show (authExpiry a)

-- | Create an AuthToken with the default expiration time, automatically
-- calculating the digest.
mkAuthToken :: AuthConfig -> AuthUser -> IO AuthToken
mkAuthToken acfg auser = do
  now <- epochTime
  let
      expires = now + authConfigShelfLife acfg
      token = AuthToken {
          authUser = auser
        , authExpiry = expires
        , authDigest = ""
        }
      digest = tokenDigest (authConfigAuthTokenKey acfg) token
  return $ token {authDigest = digest}

