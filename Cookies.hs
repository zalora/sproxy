module Cookies (processCookieHeaders, setCookie, AuthToken(..), authToken, validAuth, authShelfLife) where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy.Char8 as BL8
import Data.Char
import Data.Digest.Pure.SHA (hmacSha1, showDigest)
import Data.List.Split (splitOn)
import Network.HTTP.Cookie hiding (processCookieHeaders)
import Network.HTTP.Types.Header (Header)
import System.Posix.Time (epochTime)
import System.Posix.Types (EpochTime)
import Text.Parsec (char, many, many1, satisfy, parse, (<|>), sepBy1)
import Text.Parsec.ByteString (Parser)
import Text.Read (readMaybe)

data AuthToken = AuthToken { authEmail  :: String
                           , authExpiry :: EpochTime
                           , authDigest :: String -- HMAC hash
                           }

-- Here is the format of the actual cookie we send to the client.

instance Show AuthToken where
  show a = authEmail a ++ ":" ++ show (authExpiry a) ++ ":" ++ (authDigest a)

instance Read AuthToken where
  readsPrec _ =
    \s -> case splitOn ":" s of
            [email, expire, digest] -> [(AuthToken email (read expire) digest, "")]
            _ -> []

authShelfLife :: EpochTime
authShelfLife = 30 * 24 * 60 * 60 -- 30 days

-- | Create an AuthToken with the default expiration time, automatically
-- calculating the digest.
authToken :: String -> String -> IO AuthToken
authToken key email = do
  now <- epochTime
  let expires = now + authShelfLife
      digest = tokenDigest key AuthToken { authEmail = email
                                         , authExpiry = expires
                                         , authDigest = ""
                                         }
  return AuthToken { authEmail  = email
                   , authExpiry = expires
                   , authDigest = digest
                   }

-- | This generates the HMAC digest of the auth token using SHA1.
-- Eventually, we need to rotate the key used to generate the HMAC, while still
-- storing old keys long enough to use them for any valid login session. Without
-- this, authentication is less secure.
tokenDigest :: String -> AuthToken -> String
tokenDigest key a = showDigest $ hmacSha1 (BL8.pack key) (BL8.pack token)
  where token = show (authEmail a) ++ show (authExpiry a)

validAuth :: String -> String -> IO (Maybe AuthToken)
validAuth key token = do
  case readMaybe token of
    Nothing -> return Nothing
    Just t -> do
      now <- epochTime
      if tokenDigest key t == authDigest t && authExpiry t > now
        then return $ Just t
        else return Nothing

setCookie cookie maxAge =
  ckName cookie ++ "=" ++ ckValue cookie ++
  "; Max-Age=" ++ show maxAge ++ "; Domain=" ++ ckDomain cookie ++ "; HttpOnly; Secure"

-- headerToCookies in Network.HTTP.Cookie is designed to work with the
-- Set-Cookie header. This is a variant for the Cookie header
-- (HdrSetCookie -> HdrCookie).
processCookieHeaders :: String -> [Header] -> ([BS.ByteString], [Cookie])
processCookieHeaders dom hdrs = foldr (headerToCookies dom) ([],[]) hdrs

-- | @headerToCookies dom hdr acc@ 
headerToCookies :: String -> Header -> ([BS.ByteString], [Cookie]) -> ([BS.ByteString], [Cookie])
headerToCookies dom ("Cookie", val) (accErr, accCookie) = 
    case parse cookies "" val of
        Left{}  -> (val:accErr, accCookie)
        Right x -> (accErr, x ++ accCookie)
  where
   cookies :: Parser [Cookie]
   cookies = sepBy1 cookie (char ';' >> spaces_l)

   cookie :: Parser Cookie
   cookie =
       do { name <- word
          ; spaces_l
          ; char '='
          ; spaces_l
          ; val1 <- cvalue
          ; return $ mkCookie name val1
          }

   cvalue :: Parser String
   
   spaces_l = many (satisfy isSpace)

   cvalue = quotedstring <|> many1 (satisfy $ not . (==';')) <|> return ""
   
   mkCookie :: String -> String -> Cookie
   mkCookie nm cval = 
	  MkCookie { ckName    = nm
                   , ckValue   = cval
                   , ckDomain  = map toLower dom
                   , ckPath    = Nothing
                   , ckVersion = Nothing
                   , ckComment = Nothing
                   }
headerToCookies _ _ acc = acc

word, quotedstring :: Parser String
quotedstring =
    do { char '"'  -- "
       ; str <- many (satisfy $ not . (=='"'))
       ; char '"'
       ; return str
       }

word = many1 (satisfy (\x -> isAlphaNum x || x=='_' || x=='.' || x=='-' || x==':'))
