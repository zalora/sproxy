{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module ConfigFile where

import           Control.Applicative
import           Data.Char
import           Data.Word
import           Text.Read
import           System.IO
import           System.Exit
import           Data.Aeson
import           Data.Yaml
import           System.Logging.LogSink.Config

withConfigFile :: FilePath -> (ConfigFile -> IO a) -> IO a
withConfigFile configFile action = do
  c <- decodeFileEither configFile
  case c of
    Left err -> do
      hPutStrLn stderr ("error parsing configuration file " ++ configFile ++ ": " ++ show err)
      exitFailure
    Right config -> action config

data ConfigFile = ConfigFile {
  cfLogLevel :: LogLevel
, cfLogTarget :: LogTarget
, cfListen :: Word16
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
        (m .:? "log_level" .!= "debug" >>= parseLogLevel)
    <*> (m .:? "log_target" .!= "stderr" >>= parseLogTarget)
    <*> m .:? "listen" .!= 443
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

deriving instance Read LogLevel

parseLogLevel :: String -> Parser LogLevel
parseLogLevel s = (maybe err return . readMaybe . map toUpper) s
  where
    err = fail ("invalid log_level " ++ show s)

parseLogTarget :: String -> Parser LogTarget
parseLogTarget s = case s of
  "stderr" -> return StdErr
  "syslog" -> return SysLog
  _ -> fail ("invalid log_target " ++ show s)

