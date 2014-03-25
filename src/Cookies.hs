module Cookies (parseCookies, setCookie, AuthToken(..), authToken, validAuth, authShelfLife) where

import           Control.Applicative
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy.Char8 as BL8
import Data.Char hiding (isSpace)
import Data.Digest.Pure.SHA (hmacSha1, showDigest)
import Data.List.Split (splitOn)
import Network.HTTP.Cookie
import Network.HTTP.Types.Header (Header)
import System.Posix.Time (epochTime)
import System.Posix.Types (EpochTime)
import Data.Attoparsec.ByteString.Char8
import Text.Read (readMaybe)

data AuthToken = AuthToken { authEmail  :: String
                           , authName   :: (String, String)
                           , authExpiry :: EpochTime
                           , authDigest :: String -- HMAC hash
                           }

-- Here is the format of the actual cookie we send to the client.

instance Show AuthToken where
  show a = authEmail a ++ ":" ++ authNameString (authName a) ++ ":" ++
             show (authExpiry a) ++ ":" ++ (authDigest a)
    where authNameString (given, family) = given ++ ":" ++ family

instance Read AuthToken where
  readsPrec _ =
    \s -> case splitOn ":" s of
            [email, given, family, expire, digest] -> [(AuthToken email (given, family) (read expire) digest, "")]
            _ -> []

authShelfLife :: EpochTime
authShelfLife = 30 * 24 * 60 * 60 -- 30 days

-- | Create an AuthToken with the default expiration time, automatically
-- calculating the digest.
authToken :: String -> String -> (String, String) -> IO AuthToken
authToken key email name = do
  now <- epochTime
  let expires = now + authShelfLife
      digest = tokenDigest key AuthToken { authEmail = email
                                         , authName = name
                                         , authExpiry = expires
                                         , authDigest = ""
                                         }
  return AuthToken { authEmail  = email
                   , authName = name
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

setCookie :: Cookie -> EpochTime -> String
setCookie cookie maxAge =
  ckName cookie ++ "=" ++ ckValue cookie ++
  "; Max-Age=" ++ show maxAge ++ "; Domain=" ++ ckDomain cookie ++ "; HttpOnly; Secure"

-- headerToCookies in Network.HTTP.Cookie is designed to work with the
-- Set-Cookie header. This is a variant for the Cookie header
-- (HdrSetCookie -> HdrCookie).
parseCookies :: String -> [Header] -> [Cookie]
parseCookies dom hdrs = snd $ foldr (headerToCookies dom) ([],[]) hdrs

-- | @headerToCookies dom hdr acc@
headerToCookies :: String -> Header -> ([BS.ByteString], [Cookie]) -> ([BS.ByteString], [Cookie])
headerToCookies dom ("Cookie", val) (accErr, accCookie) =
    case parseOnly cookies val of
        Left{}  -> (val:accErr, accCookie)
        Right x -> (accErr, x ++ accCookie)
  where
   cookies :: Parser [Cookie]
   cookies = sepBy1 cookie (";" *> spaces_l)

   cookie :: Parser Cookie
   cookie = mkCookie <$> word <*> (spaces_l *> "=" *> spaces_l *> cvalue)

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
quotedstring = char '"' *> many (satisfy $ not . (=='"')) <* char '"'

word = many1 (satisfy (\x -> isAlphaNum x || x=='_' || x=='.' || x=='-' || x==':'))
