{-# LANGUAGE OverloadedStrings #-}
module ConfigFile where

import           Control.Applicative
import           Data.Char
import           Text.Read
import           System.IO
import           System.Exit
import           Data.Aeson
import           Data.Yaml
import           System.Log.Logger
import           Network (PortNumber)

withConfigFile :: FilePath -> (ConfigFile -> IO a) -> IO a
withConfigFile configFile action = do
  c <- decodeFileEither configFile
  case c of
    Left err -> do
      hPutStrLn stderr ("error parsing configuration file " ++ configFile ++ ": " ++ show err)
      exitFailure
    Right config -> action config

data ConfigFile = ConfigFile {
  cfLogLevel :: Priority
, cfListen :: PortNumber
, cfRedirectHttpToHttps :: Bool
, cfCookieDomain :: String
, cfCookieName :: String
, cfClientID :: String
, cfClientSecretFile :: FilePath
, cfAuthTokenKeyFile :: FilePath
, cfSslKey :: FilePath
, cfSslCerts :: FilePath
, cfDatabase :: String
, cfBackendAddress :: String
, cfBackendPort :: PortNumber
} deriving (Eq, Show)

instance FromJSON ConfigFile where
  parseJSON (Object m) = ConfigFile <$>
        (m .: "log_level" >>= parseLogLevel)
    <*> (fromInteger <$> m .: "listen")
    <*> (m .: "redirect_http_to_https")
    <*> m .: "cookie_domain"
    <*> m .: "cookie_name"
    <*> m .: "client_id"
    <*> m .: "client_secret"
    <*> m .: "auth_token_key"
    <*> m .: "ssl_key"
    <*> m .: "ssl_certs"
    <*> m .: "database"
    <*> m .: "backend_address"
    <*> (fromInteger <$> m .: "backend_port")
  parseJSON _ = empty

parseLogLevel :: String -> Parser Priority
parseLogLevel s = (maybe err return . readMaybe . map toUpper) s
  where
    err = fail ("invalid log_level " ++ show s)
