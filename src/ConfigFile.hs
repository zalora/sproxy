{-# LANGUAGE OverloadedStrings #-}

module ConfigFile where

import           Control.Applicative
import           Data.Word
import           System.IO
import           System.Exit
import           Data.Aeson
import           Data.Yaml

import Logging (LogLevel(Debug))

withConfigFile :: FilePath -> (ConfigFile -> IO a) -> IO a
withConfigFile configFile action = do
  c <- decodeFileEither configFile
  case c of
    Left err -> do
      hPutStrLn stderr ("error parsing configuration file " ++ configFile ++ ": " ++ show err)
      exitFailure
    Right config -> action config

data ConfigFile = ConfigFile {
  cfListen :: Word16
, cfLogLevel :: LogLevel
, cfRedirectHttpToHttps :: Maybe Bool
, cfCookieDomain :: String
, cfCookieName :: String
, cfClientID :: String
, cfClientSecretFile :: FilePath
, cfSslKey :: FilePath
, cfSslCerts :: FilePath
, cfDatabase :: String
, cfBackendAddress :: String
, cfBackendPort :: Word16
, cfBackendSocket :: Maybe String
, cfUser :: String
} deriving (Eq, Show)

instance FromJSON ConfigFile where
  parseJSON (Object m) = ConfigFile <$>
        m .:? "listen" .!= 443
    <*> m .:? "log_level" .!= Debug
    <*> m .:? "redirect_http_to_https"
    <*> m .: "cookie_domain"
    <*> m .: "cookie_name"
    <*> m .: "client_id"
    <*> m .: "client_secret"
    <*> m .: "ssl_key"
    <*> m .: "ssl_certs"
    <*> m .: "database"
    <*> m .:? "backend_address" .!= "127.0.0.1"
    <*> m .:? "backend_port" .!= 8080
    <*> m .:? "backend_socket"
    <*> m .:? "user" .!= "sproxy"
  parseJSON _ = empty

