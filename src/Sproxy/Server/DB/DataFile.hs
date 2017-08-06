{-# LANGUAGE OverloadedStrings #-}

module Sproxy.Server.DB.DataFile
  ( DataFile(..)
  , GroupMember(..)
  , GroupPrivilege(..)
  , PrivilegeRule(..)
  ) where

import Control.Applicative (empty)
import Data.Aeson (FromJSON, parseJSON)
import Data.Text (Text)
import Data.Yaml (Value(Object), (.:))

data DataFile = DataFile
  { groupMember :: [GroupMember]
  , groupPrivilege :: [GroupPrivilege]
  , privilegeRule :: [PrivilegeRule]
  } deriving (Show)

instance FromJSON DataFile where
  parseJSON (Object m) =
    DataFile <$> m .: "group_member" <*> m .: "group_privilege" <*>
    m .: "privilege_rule"
  parseJSON _ = empty

data GroupMember = GroupMember
  { gmGroup :: Text
  , gmEmail :: Text
  } deriving (Show)

instance FromJSON GroupMember where
  parseJSON (Object m) = GroupMember <$> m .: "group" <*> m .: "email"
  parseJSON _ = empty

data GroupPrivilege = GroupPrivilege
  { gpGroup :: Text
  , gpDomain :: Text
  , gpPrivilege :: Text
  } deriving (Show)

instance FromJSON GroupPrivilege where
  parseJSON (Object m) =
    GroupPrivilege <$> m .: "group" <*> m .: "domain" <*> m .: "privilege"
  parseJSON _ = empty

data PrivilegeRule = PrivilegeRule
  { prDomain :: Text
  , prPrivilege :: Text
  , prPath :: Text
  , prMethod :: Text
  } deriving (Show)

instance FromJSON PrivilegeRule where
  parseJSON (Object m) =
    PrivilegeRule <$> m .: "domain" <*> m .: "privilege" <*> m .: "path" <*>
    m .: "method"
  parseJSON _ = empty
