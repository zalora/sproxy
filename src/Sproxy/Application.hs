{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module Sproxy.Application (
  sproxy
, redirect
) where

import Blaze.ByteString.Builder (toByteString)
import Blaze.ByteString.Builder.ByteString (fromByteString)
import Control.Exception (SomeException, catch)
import Data.ByteString (ByteString)
import Data.ByteString as BS (break, intercalate)
import Data.Char (toLower)
import Data.ByteString.Char8 (pack, unpack)
import Data.ByteString.Lazy (fromStrict)
import Data.Conduit (Flush(Chunk), mapOutput)
import Data.HashMap.Strict as HM (HashMap, foldrWithKey, lookup)
import Data.List (find, partition)
import Data.Map as Map (delete, fromListWith, insert, insertWith, toList)
import Data.Maybe (fromJust, fromMaybe)
import Data.Monoid ((<>))
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Data.Word (Word16)
import Data.Word8 (_colon)
import Foreign.C.Types (CTime(..))
import Network.HTTP.Client.Conduit (bodyReaderSource)
import Network.HTTP.Conduit (requestBodySourceChunkedIO, requestBodySourceIO)
import Network.HTTP.Types (RequestHeaders, ResponseHeaders, hConnection,
  hContentLength, hContentType, hCookie, hLocation, methodGet)
import Network.HTTP.Types.Status ( Status(..), badRequest400, forbidden403, found302,
  internalServerError500, methodNotAllowed405, movedPermanently301,
  networkAuthenticationRequired511, notFound404, ok200, seeOther303, temporaryRedirect307 )
import Network.Socket (NameInfoFlag(NI_NUMERICHOST), getNameInfo)
import Network.Wai.Conduit (sourceRequestBody, responseSource)
import System.FilePath.Glob (Pattern, match)
import System.Posix.Time (epochTime)
import Text.InterpolatedString.Perl6 (qc)
import Web.Cookie (Cookies, parseCookies, renderCookies)
import qualified Network.HTTP.Client as BE
import qualified Network.Wai as W
import qualified Web.Cookie as WC

import Sproxy.Application.Cookie (AuthCookie(..), AuthUser(..), cookieDecode, cookieEncode)
import Sproxy.Application.OAuth2.Common (OAuth2Client(..))
import Sproxy.Config(BackendConf(..))
import Sproxy.Server.DB (Database, userExists, userGroups)
import qualified Sproxy.Application.State as State
import qualified Sproxy.Logging as Log


redirect :: Word16 -> W.Application
redirect p req resp =
  case W.requestHeaderHost req of
    Nothing -> badRequest "missing host" req resp
    Just host -> do
      Log.info $ "redirecting to " ++ show location ++ ": " ++ showReq req
      resp $ W.responseBuilder status [(hLocation, location)] mempty
      where
        status = if W.requestMethod req == methodGet then movedPermanently301 else temporaryRedirect307
        (domain, _) = BS.break (== _colon) host
        newhost = if p == 443 then domain else domain <> ":" <> pack (show p)
        location = "https://" <> newhost <> W.rawPathInfo req <> W.rawQueryString req


sproxy :: ByteString -> Database -> HashMap Text OAuth2Client -> [(Pattern, BackendConf, BE.Manager)] -> W.Application
sproxy key db oa2 backends = logException $ \req resp -> do
  Log.debug $ "sproxy <<< " ++ showReq req
  case W.requestHeaderHost req of
    Nothing -> badRequest "missing host" req resp
    Just host ->
      case find (\(p, _, _) -> match p (unpack host)) backends of
        Nothing -> notFound "backend" req resp
        Just (_, be, mgr) -> do
          let cookieName = pack $ beCookieName be
              cookieDomain = pack <$> beCookieDomain be
          case W.pathInfo req of
            ["robots.txt"] -> get robots req resp
            (".sproxy":proxy) ->
              case proxy of
                ["logout"] ->
                  case extractCookie key Nothing cookieName req of
                    Nothing -> notFound "logout without the cookie" req resp
                    Just _  -> get (logout cookieName cookieDomain) req resp
                ["oauth2", provider] ->
                    case HM.lookup provider oa2 of
                      Nothing -> notFound "OAuth2 provider" req resp
                      Just oa2c -> get (oauth2callback key db (provider, oa2c) be) req resp
                _ -> notFound "proxy" req resp
            _ -> do
              now <- Just <$> epochTime
              case extractCookie key now cookieName req of
                Nothing -> authenticationRequired key oa2 req resp
                Just cs@(authCookie, _) ->
                  authorize db cs req >>= \case
                    Nothing   -> forbidden authCookie req resp
                    Just req' -> forward mgr req' resp


robots :: W.Application
robots _ resp = resp $
  W.responseLBS ok200 [(hContentType, "text/plain; charset=utf-8")]
  "User-agent: *\nDisallow: /"


oauth2callback :: ByteString -> Database -> (Text, OAuth2Client) -> BackendConf -> W.Application
oauth2callback key db (provider, oa2c) be req resp =
  case param "code" of
    Nothing   -> badRequest "missing auth code" req resp
    Just code -> 
      case param "state" of
        Nothing    -> badRequest "missing auth state" req resp
        Just state ->
          case State.decode key state of
            Left msg   -> badRequest ("invalid state: " ++ msg) req resp
            Right path -> do
              au <- oauth2Authenticate oa2c code (redirectURL req provider)
              let email = map toLower $ auEmail au
              Log.info $ "login `" ++ email ++ "' by " ++ show provider
              exists <- userExists db email
              if exists then authenticate key be au{auEmail = email} path req resp
                        else userNotFound email req resp
  where
    param p = do
      (_, v) <- find ((==) p . fst) $ W.queryString req
      v


-- XXX: RFC6265: the user agent MUST NOT attach more than one Cookie header field
extractCookie :: ByteString -> Maybe CTime -> ByteString -> W.Request -> Maybe (AuthCookie, Cookies)
extractCookie key now name req = do
  (_, cookies)   <- find ((==) hCookie . fst) $ W.requestHeaders req
  (auth, others) <- discriminate cookies
  case cookieDecode key auth of
    Left _ -> Nothing
    Right cookie -> if maybe True (acExpiry cookie >) now
      then Just (cookie, others) else Nothing
  where discriminate cs =
          case partition ((==) name . fst) $ parseCookies cs of
            ((_, x):_, xs) -> Just (x, xs)
            _              -> Nothing


authenticate :: ByteString -> BackendConf -> AuthUser -> ByteString -> W.Application
authenticate key be user path req resp = do
  now <- epochTime
  let host = fromJust $ W.requestHeaderHost req
      domain = pack <$> beCookieDomain be
      expiry = now + CTime (beCookieMaxAge be)
      authCookie = AuthCookie { acUser = user, acExpiry = expiry }
      cookie = WC.def {
        WC.setCookieName = pack $ beCookieName be
      , WC.setCookieHttpOnly = True
      , WC.setCookiePath = Just "/"
      , WC.setCookieSameSite = Nothing
      , WC.setCookieSecure = True
      , WC.setCookieValue = cookieEncode key authCookie
      , WC.setCookieDomain = domain
      , WC.setCookieExpires = Just . posixSecondsToUTCTime . realToFrac $ expiry
      }
  resp $ W.responseLBS seeOther303 [
           (hLocation, "https://" <> host <> path)
         , ("Set-Cookie", toByteString $ WC.renderSetCookie cookie)
         ] ""


authorize :: Database -> (AuthCookie, Cookies) -> W.Request -> IO (Maybe W.Request)
authorize db (authCookie, otherCookies) req = do
  grps <- userGroups db email domain path method
  if null grps then return Nothing
  else do
    ip <- pack . fromJust . fst <$> getNameInfo [NI_NUMERICHOST] True False (W.remoteHost req)
    return . Just $ req {
    W.requestHeaders = toList $
      insert "From" (pack email) $
      insert "X-Groups" (BS.intercalate "," grps) $
      insert "X-Given-Name" given $
      insert "X-Family-Name" family $
      insert "X-Forwarded-Proto" "https" $
      insertWith (flip combine) "X-Forwarded-For" ip $
      setCookies otherCookies $
      fromListWith combine $ W.requestHeaders req
  }
  where
    user = acUser authCookie
    email = auEmail user
    given = pack $ auGivenName user
    family = pack $ auFamilyName user
    domain = decodeUtf8 . fromJust $ W.requestHeaderHost req
    path = decodeUtf8 $ W.rawPathInfo req
    method = decodeUtf8 $ W.requestMethod req
    combine a b = a <> "," <> b
    setCookies [] = delete hCookie
    setCookies cs = insert hCookie (toByteString . renderCookies $ cs)


forward :: BE.Manager -> W.Application
forward mgr req resp = do
  let beReq = BE.defaultRequest
        { BE.method = W.requestMethod req
        , BE.path = W.rawPathInfo req
        , BE.queryString = W.rawQueryString req
        , BE.requestHeaders = modifyRequestHeaders $ W.requestHeaders req
        , BE.redirectCount = 0
        , BE.decompress = const False
        , BE.requestBody = case W.requestBodyLength req of
            W.ChunkedBody   -> requestBodySourceChunkedIO (sourceRequestBody req)
            W.KnownLength l -> requestBodySourceIO (fromIntegral l) (sourceRequestBody req)
        }
      msg = unpack (BE.method beReq <> " " <> BE.path beReq <> BE.queryString beReq)
  Log.debug $ "BACKEND <<< " ++ msg ++ " " ++ show (BE.requestHeaders beReq)
  BE.withResponse beReq mgr $ \res -> do
        let status = BE.responseStatus res
            headers = modifyResponseHeaders $ BE.responseHeaders res
            body = mapOutput (Chunk . fromByteString) . bodyReaderSource $ BE.responseBody res
            logging = if statusCode status `elem` [ 400, 500 ] then
                      Log.warn else Log.debug
        logging $ "BACKEND >>> " ++ show (statusCode status) ++ " on " ++ msg ++ "\n"
        resp $ responseSource status headers body


modifyRequestHeaders :: RequestHeaders -> RequestHeaders
modifyRequestHeaders = filter (\(n, _) -> n `notElem` ban)
  where
    ban =
      [
        hConnection
      , hContentLength -- XXX to avoid duplicate header
      ]

modifyResponseHeaders :: ResponseHeaders -> ResponseHeaders
modifyResponseHeaders = filter (\(n, _) -> n `notElem` ban)
  where
    ban =
      [
        hConnection
      ]

authenticationRequired :: ByteString -> HashMap Text OAuth2Client -> W.Application
authenticationRequired key oa2 req resp = do
  Log.info $ "511 Unauthenticated: " ++ showReq req
  resp $ W.responseLBS networkAuthenticationRequired511 [(hContentType, "text/html; charset=utf-8")] page
  where
    path = if W.requestMethod req == methodGet
           then W.rawPathInfo req <> W.rawQueryString req
           else "/"
    state = State.encode key path
    authLink :: Text -> OAuth2Client -> ByteString -> ByteString
    authLink provider oa2c html = 
      let u = oauth2AuthorizeURL oa2c state (redirectURL req provider)
          d = pack $ oauth2Description oa2c
      in [qc|{html}<p><a href="{u}">Authenticate with {d}</a></p>|]
    authHtml = foldrWithKey authLink "" oa2
    page = fromStrict [qc|
<!DOCTYPE html>
<html lang="en">
  <head>
    <meta charset="utf-8">
    <title>Authentication required</title>
  </head>
  <body style="text-align:center;">
  <h1>Authentication required</h1>
  {authHtml}
  </body>
</html>
|]


forbidden :: AuthCookie -> W.Application
forbidden ac req resp = do
  Log.info $ "403 Forbidden (" ++ email ++ "): " ++ showReq req
  resp $ W.responseLBS forbidden403 [(hContentType, "text/html; charset=utf-8")] page
  where
    email = auEmail . acUser $ ac
    page = fromStrict [qc|
<!DOCTYPE html>
<html lang="en">
  <head>
    <meta charset="utf-8">
    <title>Access Denied</title>
  </head>
  <body>
  <h1>Access Denied</h1>
    <p>You are currently logged in as <strong>{email}</strong></p>
    <p><a href="/.sproxy/logout">Logout</a></p>
  </body>
</html>
|]


userNotFound :: String -> W.Application
userNotFound email _ resp = do
  Log.info $ "404 User not found (" ++ email ++ ")"
  resp $ W.responseLBS notFound404 [(hContentType, "text/html; charset=utf-8")] page
  where
    page = fromStrict [qc|
<!DOCTYPE html>
<html lang="en">
  <head>
    <meta charset="utf-8">
    <title>Access Denied</title>
  </head>
  <body>
  <h1>Access Denied</h1>
    <p>You are not allowed to login as <strong>{email}</strong></p>
    <p><a href="/">Main page</a></p>
  </body>
</html>
|]


logout :: ByteString -> Maybe ByteString -> W.Application
logout name domain req resp = do
  let host = fromJust $ W.requestHeaderHost req
      cookie = WC.def {
        WC.setCookieName = name
      , WC.setCookieHttpOnly = True
      , WC.setCookiePath = Just "/"
      , WC.setCookieSameSite = Just WC.sameSiteStrict
      , WC.setCookieSecure = True
      , WC.setCookieValue = "goodbye"
      , WC.setCookieDomain = domain
      , WC.setCookieExpires = Just . posixSecondsToUTCTime . realToFrac $ CTime 0
      }
  resp $ W.responseLBS found302 [
           (hLocation, "https://" <> host)
         , ("Set-Cookie", toByteString $ WC.renderSetCookie cookie)
         ] ""


badRequest ::String -> W.Application
badRequest msg req resp = do
  Log.warn $ "400 Bad Request (" ++ msg ++ "): " ++ showReq req
  resp $ W.responseLBS badRequest400 [] "Bad Request"


notFound ::String -> W.Application
notFound msg req resp = do
  Log.warn $ "404 Not Found (" ++ msg ++ "): " ++ showReq req
  resp $ W.responseLBS notFound404 [] "Not Found"


logException :: W.Middleware
logException app req resp = catch (app req resp) $ \e -> do
  Log.error $ "500 Internal Error: " ++ show (e :: SomeException) ++ " on " ++ showReq req
  resp $ W.responseLBS internalServerError500 [] "Internal Error"


get :: W.Middleware
get app req resp
  | W.requestMethod req == methodGet = app req resp
  | otherwise = do
    Log.warn $ "405 Method Not Allowed: " ++ showReq req
    resp $ W.responseLBS methodNotAllowed405 [("Allow", "GET")] "Method Not Allowed"


redirectURL :: W.Request -> Text -> ByteString
redirectURL req provider =
  "https://" <> fromJust (W.requestHeaderHost req)
             <> "/.sproxy/oauth2/" <> encodeUtf8 provider


-- XXX: make sure not to reveal the cookie, which can be valid (!)
showReq :: W.Request -> String
showReq req = 
    unpack ( W.requestMethod req <> " "
           <> fromMaybe "<no host>" (W.requestHeaderHost req)
           <> W.rawPathInfo req <> W.rawQueryString req <> " " )
    ++ show (fromMaybe "-" $ W.requestHeaderReferer req) ++ " "
    ++ show (fromMaybe "-" $ W.requestHeaderUserAgent req)
    ++ " from " ++ show (W.remoteHost req)

