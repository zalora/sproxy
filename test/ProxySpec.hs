{-# LANGUAGE OverloadedStrings #-}
module ProxySpec (main, spec) where

import           Test.Hspec

import           Control.Applicative
import           Control.Exception
import           Control.Concurrent
import           Data.IORef
import           Data.String
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy as L
import           Network.HTTP.Types
import           Network.Wai (Application)
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp
import           Network.HTTP.Conduit
import qualified Network.TLS as TLS
import           Network.Connection

import           Proxy
import           Authenticate

main :: IO ()
main = hspec spec

chunkedRequestBody :: [ByteString] -> RequestBody
chunkedRequestBody chunks = RequestBodyStreamChunked $ \action -> do
  ref <- newIORef chunks
  action $ atomicModifyIORef ref $ \xs -> case xs of
    y : ys -> (ys, y)
    _ -> (xs, "")

backendMock :: [Header] -> L.ByteString -> MVar (RequestHeaders, [ByteString]) -> Application
backendMock headers response mvar request respond = do
  body <- consumeBody (Wai.requestBody request)
  putMVar mvar (Wai.requestHeaders request, body)
  respond $ Wai.responseLBS status200 (("Content-Type", "text/plain") : headers) response

consumeBody :: IO ByteString -> IO [ByteString]
consumeBody bodyReader = go
  where
    go :: IO [ByteString]
    go = do
      bs <- bodyReader
      case bs of
        "" -> return []
        _ -> (bs:) <$> go

authTokenKey :: String
authTokenKey = "some-secret"

performRequest :: (Request -> Request) -> IO (Response L.ByteString)
performRequest f = do
  manager <- newManager (mkManagerSettings tlsSettings Nothing)
  request <- f <$> parseUrl "https://localhost:4060"
  httpLbs request{redirectCount = 0, checkStatus = \_ _ _ -> Nothing} manager
  where
    tlsSettings = TLSSettingsSimple {settingDisableCertificateValidation = True, settingDisableSession = False, settingUseServerName = True}

performRequestWithCookie :: (Request -> Request) -> IO (Response L.ByteString)
performRequestWithCookie f = do
  cookie <- show <$> authToken authTokenKey "me@example.com" ("John", "Doe")
  performRequest (setSproxyCookie cookie . f)
  where
    setSproxyCookie cookie r = r {requestHeaders = ("Cookie", fromString $ "sproxy=" ++ cookie) : requestHeaders r}

get :: IO L.ByteString
get = responseBody <$> performRequestWithCookie id

post :: RequestBody -> IO L.ByteString
post body = responseBody <$> performRequestWithCookie (\r -> r {method = "POST", requestBody = body})

spec :: Spec
spec = around withProxy $ do
  describe "runProxy" $ do
    it "forwards requests to backend app" $ do
      withBackendMock [] "hello" $ \_ -> do
        get `shouldReturn` "hello"

    it "handles chunked response bodies" $ do
      let response = L.fromChunks (replicate 2300 "hello")
      withBackendMock [] response $ \_ -> do
        get `shouldReturn` response

    it "handles response bodies with Content-Length" $ do
      withBackendMock [("Content-Length", "5")] "hello"$ \_ -> do
        get `shouldReturn` "hello"

    it "handles chunked request bodies" $ do
      withBackendMock [] "" $ \mvar -> do
        let body = ["foo", "bar", "baz"]
        _ <- post (chunkedRequestBody body)
        request <- readMVar mvar
        (lookup "Transfer-Encoding" $ fst request) `shouldBe` Just "chunked"
        snd request `shouldBe` body

    it "handles request bodies with Content-Length" $ do
      withBackendMock [] "" $ \mvar -> do
        _ <- post (RequestBodyBS "foo")
        request <- readMVar mvar
        (lookup "Content-Length" $ fst request) `shouldBe` Just "3"
        snd request `shouldBe` ["foo"]

    context "when user does not send cookie" $ do
      it "redirects user to Google OAuth page" $ do
        withBackendMock [] "hello" $ \_ -> do
          r <- performRequest id
          responseStatus r `shouldBe` found302
          lookup "Location" (responseHeaders r) `shouldBe` Just "https://accounts.google.com/o/oauth2/auth?scope=https%3A%2F%2Fwww.googleapis.com%2Fauth%2Fuserinfo.email+https%3A%2F%2Fwww.googleapis.com%2Fauth%2Fuserinfo.profile&state=%2F&redirect_uri=https://localhost:4060/sproxy/oauth2callback&response_type=code&client_id=some-client-id&approval_prompt=force&access_type=offline"

    describe "/sproxy/logout" $ do
      it "invalidates session cookie" $ do
        withBackendMock [] "hello" $ \_ -> do
          r <- performRequestWithCookie (\r -> r{path = "/sproxy/logout"})
          lookup "Set-Cookie" (responseHeaders r) `shouldBe` (Just . B.unwords) [
              "sproxy=deleted;"
            , "expires=Thu, 01 Jan 1970 00:00:00 GMT;"
            , "Domain=example.com;"
            , "path=/;"
            , "HttpOnly;"
            , "Secure"
            ]

      it "redirects user to /" $ do
        withBackendMock [] "hello" $ \_ -> do
          r <- performRequestWithCookie (\r -> r{path = "/sproxy/logout"})
          responseStatus r `shouldBe` found302
          lookup "Location" (responseHeaders r) `shouldBe` Just "https://localhost:4060/"

      context "when alternate redirect path is specified" $ do
        it "redirects user to specified path" $ do
          withBackendMock [] "hello" $ \_ -> do
            r <- performRequestWithCookie (\r -> r{path = "/sproxy/logout?state=%2Ffoo%2Fbar%3Ftest%3D23"})
            responseStatus r `shouldBe` found302
            lookup "Location" (responseHeaders r) `shouldBe` Just "https://localhost:4060/foo/bar?test=23"
  where
    withProxy :: IO a -> IO a
    withProxy action = do
      Right credential <- TLS.credentialLoadX509 "config/server.crt.example" "config/server.key.example"
      let config = Config {
              configTLSCredential = credential
            , configBackendAddress = "127.0.0.1"
            , configBackendPort = 4061
            }
      withService (startProxy config) action

    -- Start a service and run an action while the service is running;
    -- terminate service after action has completed.
    withService :: IO () -> IO a -> IO a
    withService service action = bracket (forkIO service) killThread (\_ -> action)

    withBackendMock :: [Header] -> L.ByteString -> (MVar (RequestHeaders, [ByteString]) -> IO ()) -> IO ()
    withBackendMock mockedHeaders mockedBody clientAction = do
      mvar <- newEmptyMVar
      withService (Warp.run 4061 $ backendMock mockedHeaders mockedBody mvar) (clientAction mvar)

    startProxy config = runProxy 4060 config authConfig (\_ _ _ _ -> return ["admin"])

    authConfig = AuthConfig {
        authConfigCookieDomain = "example.com"
      , authConfigCookieName = "sproxy"
      , authConfigClientID = "some-client-id"
      , authConfigClientSecret = error "authConfigClientSecret"
      , authConfigAuthTokenKey = authTokenKey
      }
