{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module Sproxy.Server.DB (
  Database
, DataSource(..)
, userExists
, userGroups
, start
) where

import Control.Concurrent (forkIO, threadDelay)
import Control.Exception (SomeException, bracket, catch, finally)
import Control.Monad (forever, void)
import Data.ByteString.Char8 (pack)
import Data.Pool (Pool, createPool, withResource)
import Data.Text (Text, toLower, unpack)
import Database.SQLite.Simple (NamedParam((:=)))
import Text.InterpolatedString.Perl6 (q, qc)
import qualified Database.PostgreSQL.Simple as PG
import qualified Database.SQLite.Simple as SQLite

import qualified Sproxy.Logging as Log


type Database = Pool SQLite.Connection

data DataSource = PostgreSQL String -- | File FilePath

{- TODO:
 - Hash remote tables and update the local only when the remote change
 - Switch to REGEX
 - Generalize sync procedures for different tables
 -}

start :: FilePath -> Maybe DataSource -> IO Database
start home ds = do
  Log.info $ "home directory: " ++ show home
  db <- createPool    
    (do c   <- SQLite.open $ home ++ "/sproxy.sqlite3"
        lvl <- Log.level
        SQLite.setTrace c (if lvl == Log.Debug then Just $ Log.debug . unpack else Nothing)
        return c)
    SQLite.close
    1    -- stripes
    3600 -- keep alive (seconds). FIXME: no much sense as it's a local file
    128  -- max connections. FIXME: make configurable?

  withResource db $ \c -> SQLite.execute_ c "PRAGMA journal_mode=WAL"
  populate db ds
  return db


userExists :: Database -> Text -> IO Bool
userExists db email = do
  r <- withResource db $ \c -> fmap SQLite.fromOnly <$> SQLite.queryNamed c
    "SELECT EXISTS (SELECT 1 FROM group_member WHERE :email LIKE email LIMIT 1)"
    [ ":email"  := email ]
  return $ head r


userGroups :: Database -> Text -> Text -> Text -> Text -> IO [Text]
userGroups db email domain path method =
  withResource db $ \c -> fmap SQLite.fromOnly <$> SQLite.queryNamed c [q|
      SELECT gm."group" FROM group_privilege gp JOIN group_member gm ON gm."group"  = gp."group"
      WHERE :email LIKE gm.email
      AND :domain LIKE gp.domain
      AND gp.privilege IN (
        SELECT privilege FROM privilege_rule
        WHERE :domain LIKE domain
        AND :path LIKE path
        AND method = :method
        ORDER BY length(path) - length(replace(path, '/', '')) DESC LIMIT 1
      )
  |] [ ":email"  := email -- XXX always in lower case
     , ":domain" := toLower domain
     , ":path"   := path
     , ":method" := method -- XXX case-sensitive by RFC2616
     ]


populate :: Database -> Maybe DataSource -> IO ()

populate db Nothing = do
  Log.warn "db: no data source defined"
  withResource db $ \c -> SQLite.withTransaction c $ do
    createGroupMember c
    createGroupPrivilege c
    createPrivilegeRule c

-- XXX We keep only required minimum of the data, without any integrity check.
-- XXX Integrity check should be done somewhere else, e. g. in the master PostgreSQL database,
-- XXX or during importing the config file.
populate db (Just (PostgreSQL connstr)) =
  void . forkIO . forever . flip finally (7 `minutes` threadDelay)
  . logException $ do
    Log.info $ "db: synchronizing with " ++ show connstr
    withResource db $ \c -> SQLite.withTransaction c $
      bracket (PG.connectPostgreSQL $ pack connstr) PG.close $
        \pg -> PG.withTransaction pg $ do

          Log.info "db: syncing group_member"
          dropGroupMember c
          createGroupMember c
          PG.forEach_ pg
            [q|SELECT "group", lower(email) FROM group_member|] $ \r ->
              SQLite.execute c
                [q|INSERT INTO group_member("group", email) VALUES (?, ?)|]
                  (r :: (Text, Text))
          count c "group_member"

          Log.info "db: syncing group_privilege"
          dropGroupPrivilege c
          createGroupPrivilege c
          PG.forEach_ pg
            [q|SELECT "group", lower(domain), privilege FROM group_privilege|] $ \r ->
              SQLite.execute c
                [q|INSERT INTO group_privilege("group", domain, privilege) VALUES (?, ?, ?)|]
                  (r :: (Text, Text, Text))
          count c "group_privilege"

          Log.info "db: syncing privilege_rule"
          dropPrivilegeRule c
          createPrivilegeRule c
          PG.forEach_ pg
            [q|SELECT lower(domain), privilege, path, method FROM privilege_rule|] $ \r ->
              SQLite.execute c
                [q|INSERT INTO privilege_rule(domain, privilege, path, method) VALUES (?, ?, ?, ?)|]
                  (r :: (Text, Text, Text, Text))
          count c "privilege_rule"


dropGroupMember :: SQLite.Connection -> IO ()
dropGroupMember c = SQLite.execute_ c "DROP TABLE IF EXISTS group_member"

createGroupMember :: SQLite.Connection -> IO ()
createGroupMember c = SQLite.execute_ c [q|
  CREATE TABLE IF NOT EXISTS group_member (
    "group" TEXT,
    email TEXT,
    PRIMARY KEY ("group", email)
  )
|]


dropGroupPrivilege :: SQLite.Connection -> IO ()
dropGroupPrivilege c = SQLite.execute_ c "DROP TABLE IF EXISTS group_privilege"

createGroupPrivilege :: SQLite.Connection -> IO ()
createGroupPrivilege c = SQLite.execute_ c [q|
  CREATE TABLE IF NOT EXISTS group_privilege (
    "group" TEXT,
    domain TEXT,
    privilege TEXT,
    PRIMARY KEY ("group", domain, privilege)
  )
|]


dropPrivilegeRule :: SQLite.Connection -> IO ()
dropPrivilegeRule c = SQLite.execute_ c "DROP TABLE IF EXISTS privilege_rule"

createPrivilegeRule :: SQLite.Connection -> IO ()
createPrivilegeRule c = SQLite.execute_ c [q|
  CREATE TABLE IF NOT EXISTS privilege_rule (
    domain TEXT,
    privilege TEXT,
    path TEXT,
    method TEXT,
    PRIMARY KEY (domain, path, method)
  )
|]


count :: SQLite.Connection -> String -> IO ()
count c table = do
  r <- fmap SQLite.fromOnly <$> SQLite.query_ c [qc|SELECT COUNT(*) FROM {table}|]
  Log.info $ "db: " ++ table ++ " rows: " ++ show (head r :: Integer)


logException :: IO () -> IO ()
logException a = catch a $ \e ->
  Log.error $ "db: " ++ show (e :: SomeException)


minutes :: Int -> (Int -> IO ()) -> IO ()
minutes us f = f $ us * 60 * 1000000

