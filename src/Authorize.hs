{-# LANGUAGE QuasiQuotes #-}
module Authorize (
  AuthorizeAction
, Email
, Domain
, RequestPath
, Group

, withDatabaseAuthorizeAction
) where

import Control.Exception
import Data.ByteString (ByteString)
import Data.Pool
import Data.Time.Clock
import Database.PostgreSQL.Simple
import Network.HTTP.Types
import Text.InterpolatedString.Perl6 (q)
import qualified Data.ByteString.Char8 as B8

type AuthorizeAction = Email -> Domain -> RequestPath -> Method -> IO [Group]

type Email = String
type Domain = ByteString
type RequestPath = ByteString
type Group = String

type ConnectionPool = Pool Connection

createConnectionPool :: String -> IO ConnectionPool
createConnectionPool database = createPool open close 1 connectionIdleTime connectionPoolSize
  where
    open :: IO Connection
    open = connectPostgreSQL (B8.pack database)

    connectionPoolSize :: Int
    connectionPoolSize = 5

    connectionIdleTime :: NominalDiffTime
    connectionIdleTime = 5

destroyConnectionPool :: ConnectionPool -> IO ()
destroyConnectionPool = destroyAllResources

withConnectionPool :: String -> (ConnectionPool -> IO a) -> IO a
withConnectionPool database = bracket (createConnectionPool database) destroyConnectionPool

withDatabaseAuthorizeAction :: String -> (AuthorizeAction -> IO a) -> IO a
withDatabaseAuthorizeAction database action = withConnectionPool database $ \pool -> do
  let authorizeAction :: AuthorizeAction
      authorizeAction domain email path method = withResource pool $ \conn -> authorizedGroups conn domain email path method
  action authorizeAction

authorizedGroups :: Connection -> AuthorizeAction
authorizedGroups db email domain path method =
  fmap fromOnly `fmap` query db [q|
SELECT gp."group" FROM group_privilege gp
INNER JOIN group_member gm ON gm."group" = gp."group"
WHERE ? LIKE email
AND ? LIKE "domain"
AND privilege IN (
  SELECT privilege FROM privilege_rule
  WHERE ? LIKE "domain" AND ? LIKE "path" AND ? ILIKE "method"
  ORDER by array_length(regexp_split_to_array("path", '/'), 1) DESC LIMIT 1
)
|] (email, domain, domain, path, method)
