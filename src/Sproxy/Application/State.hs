module Sproxy.Application.State (
  decode
, encode
) where

import Data.ByteString (ByteString)
import Data.ByteString.Lazy (fromStrict, toStrict)
import Data.Digest.Pure.SHA (hmacSha1, bytestringDigest)
import qualified Data.ByteString.Base64 as Base64
import qualified Data.Serialize as DS


-- FIXME: Compress / decompress ?


encode :: ByteString -> ByteString -> ByteString
encode key payload = Base64.encode . DS.encode $ (payload, digest key payload)


decode :: ByteString -> ByteString -> Either String ByteString
decode key d = do
  (payload, dgst) <- DS.decode =<< Base64.decode d
  if dgst /= digest key payload
  then Left "junk"
  else Right payload
  

digest :: ByteString -> ByteString -> ByteString
digest key payload = toStrict . bytestringDigest $ hmacSha1 (fromStrict key) (fromStrict payload)

