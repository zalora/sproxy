{-# LANGUAGE OverloadedStrings #-}
module ConfigFile where

import           Control.Applicative
import           Data.Yaml

data ConfigFile = ConfigFile {
  cfCookieDomain :: String
, cfCookieName :: String
, cfClientID :: String
, cfClientSecretFile :: FilePath
, cfAuthTokenKeyFile :: FilePath
, cfSslKey :: FilePath
, cfSslCerts :: FilePath
, cfDatabase :: String
, cfBackendAddress :: String
, cfBackendPort :: Integer
} deriving Show

instance FromJSON ConfigFile where
  parseJSON (Object m) = ConfigFile
    <$> m .: "cookie_domain"
    <*> m .: "cookie_name"
    <*> m .: "client_id"
    <*> m .: "client_secret"
    <*> m .: "auth_token_key"
    <*> m .: "ssl_key"
    <*> m .: "ssl_certs"
    <*> m .: "database"
    <*> m .: "backend_address"
    <*> m .: "backend_port"
  parseJSON _ = empty
