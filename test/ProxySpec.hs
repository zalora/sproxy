{-# LANGUAGE OverloadedStrings #-}
module ProxySpec (main, spec) where

import           Test.Hspec
import           Network.HTTP.Conduit
import           Network.Wai (Application, responseLBS)
import           Network.HTTP.Types (status200)
import           Network.Wai.Handler.Warp (run)
import           Control.Applicative
import           Control.Exception
import           Control.Concurrent
import qualified Network.TLS as TLS
import           Network.Connection
import           Data.ByteString.Lazy (ByteString)

import           Proxy hiding (run)
import           Authenticate

main :: IO ()
main = hspec spec

app :: Application
app _ = return $ responseLBS status200 [("Content-Type", "text/plain")] "hello"

get :: String -> IO ByteString
get url = do
  manager <- newManager (mkManagerSettings tlsSettings Nothing)
  request <- parseUrl url
  responseBody <$> httpLbs request {requestHeaders = [("Cookie", "sproxy=me@example.com:John:Doe:1400250481:a9e80b4adad0203a2202155d1f3e0cbfcb427d20")]} manager
  where
    tlsSettings = TLSSettingsSimple {settingDisableCertificateValidation = True, settingDisableSession = False, settingUseServerName = True}

spec :: Spec
spec = do
  describe "runProxy" $ do
    it "forwards requests to backend app" $ do
      Right credential <- TLS.credentialLoadX509 "config/server.crt.example" "config/server.key.example"
      let config = Config {
              configTLSCredential = credential
            , configBackendAddress = "127.0.0.1"
            , configBackendPort = 4061
            }
      with (run 4061 app) $ do
        with (startProxy config) $ do
          get "https://localhost:4060" `shouldReturn` "hello"
  where
    with action = bracket (forkIO action) killThread . const

    startProxy config = runProxy 4060 config authConfig (\action -> action (\_ _ _ _ -> return ["admin"]))

    clientSecret = "5i8VKL5PBzDgr5rGm-cg0LuN"
    authTokenKey = "k+ua8EuGyb0rrBV1237I2+ioqtxUOJQ3x3l6JUz/Qu7JaYfA5z0ihnwWLnq8bK+v\nsHuCMgq6R7/f9QuKkpz47DuKdcGNxhFRiL7Ee75gvDzHNTm/IsD43hlJ8ggEaPXW\n7NQFRK19v9H8jyndKxJUWvyPVSwQGIv0JOSS2mXz2/dn8sq1TlVPsIX/VS00VCFm\nya0A7oOJ/cfBz/z0MXvSzg==\n"

    authConfig = AuthConfig {
        authConfigCookieDomain = error "authConfigCookieName"
      , authConfigCookieName = "sproxy"
      , authConfigClientID = error "authConfigClientID"
      , authConfigClientSecret = clientSecret
      , authConfigAuthTokenKey = authTokenKey
      }
