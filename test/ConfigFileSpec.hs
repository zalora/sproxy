module ConfigFileSpec (main, spec) where

import           Test.Hspec
import           System.Logging.LogSink.Config

import           ConfigFile

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "withConfigFile" $ do
    let conf = ConfigFile {
            cfLogLevel = DEBUG
          , cfLogTarget = StdErr
          , cfListen = 443
          , cfRedirectHttpToHttps = True
          , cfCookieDomain = "dev.zalora.com"
          , cfCookieName = "sproxy-dev"
          , cfClientID = "a611zak494jxdgdn6ltlkn547rme91ig.apps.googleusercontent.com"
          , cfClientSecretFile = "config/client_secret"
          , cfSslKey = "config/server.key.example"
          , cfSslCerts = "config/server.crt.example"
          , cfDatabase = "user=you dbname=sproxy"
          , cfBackendAddress = "127.0.0.1"
          , cfBackendPort = 8080
          }
    it "parses config file" $ do
      withConfigFile "config/sproxy.yml.example" (`shouldBe` conf)
