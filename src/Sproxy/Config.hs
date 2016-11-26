{-# LANGUAGE OverloadedStrings #-}
module Sproxy.Config (
  BackendConf(..)
, ConfigFile(..)
, OAuth2Conf(..)
) where

import Control.Applicative (empty)
import Data.Aeson (FromJSON, parseJSON)
import Data.HashMap.Strict (HashMap)
import Data.Int (Int64)
import Data.Text (Text)
import Data.Word (Word16)
import Data.Yaml (Value(Object), (.:), (.:?), (.!=))

import Sproxy.Logging (LogLevel(Debug))

data ConfigFile = ConfigFile {
  cfListen       :: Word16
, cfUser         :: String
, cfHome         :: FilePath
, cfLogLevel     :: LogLevel
, cfSslCert      :: FilePath
, cfSslKey       :: FilePath
, cfSslCertChain :: [FilePath]
, cfKey          :: Maybe FilePath
, cfListen80     :: Maybe Bool
, cfBackends     :: [BackendConf]
, cfOAuth2       :: HashMap Text OAuth2Conf
, cfDataFile     :: Maybe FilePath
, cfDatabase     :: Maybe String
, cfPgPassFile   :: Maybe FilePath
, cfHTTP2        :: Bool
} deriving (Show)

instance FromJSON ConfigFile where
  parseJSON (Object m) = ConfigFile <$>
        m .:? "listen"         .!= 443
    <*> m .:? "user"           .!= "sproxy"
    <*> m .:? "home"           .!= "."
    <*> m .:? "log_level"      .!= Debug
    <*> m .:  "ssl_cert"
    <*> m .:  "ssl_key"
    <*> m .:? "ssl_cert_chain" .!= []
    <*> m .:? "key"
    <*> m .:? "listen80"
    <*> m .:  "backends"
    <*> m .:  "oauth2"
    <*> m .:? "datafile"
    <*> m .:? "database"
    <*> m .:? "pgpassfile"
    <*> m .:? "http2"          .!= True
  parseJSON _ = empty


data BackendConf = BackendConf {
  beName         :: String
, beAddress      :: String
, bePort         :: Maybe Word16
, beSocket       :: Maybe FilePath
, beCookieName   :: String
, beCookieDomain :: Maybe String
, beCookieMaxAge :: Int64
, beConnCount    :: Int
} deriving (Show)

instance FromJSON BackendConf where
  parseJSON (Object m) = BackendConf <$>
        m .:? "name"           .!= "*"
    <*> m .:? "address"        .!= "127.0.0.1"
    <*> m .:? "port"
    <*> m .:? "socket"
    <*> m .:? "cookie_name"    .!= "sproxy"
    <*> m .:? "cookie_domain"
    <*> m .:? "cookie_max_age" .!= (7 * 24 * 60 * 60)
    <*> m .:? "conn_count"     .!= 32
  parseJSON _ = empty


data OAuth2Conf = OAuth2Conf {
  oa2ClientId     :: String
, oa2ClientSecret :: FilePath
} deriving (Show)

instance FromJSON OAuth2Conf where
  parseJSON (Object m) = OAuth2Conf <$>
        m .: "client_id"
    <*> m .: "client_secret"
  parseJSON _ = empty

