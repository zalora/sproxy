{-# LANGUAGE QuasiQuotes #-}
module Authorize (
  AuthorizeAction
, Email
, Domain
, RequestPath
, Group

, withDatabaseAuthorizeAction
) where

import           Control.Exception
import           Data.ByteString (ByteString)
import           Network.HTTP.Types
import           Text.InterpolatedString.Perl6 (q)
import qualified Database.PostgreSQL.Simple as PSQL
import           Data.Pool
import           Data.Time.Clock

type AuthorizeAction = Email -> Domain -> RequestPath -> Method -> IO [Group]

type Email = String
type Domain = ByteString
type RequestPath = ByteString
type Group = String

withConnectionPool :: ByteString -> (Pool PSQL.Connection -> IO a) -> IO a
withConnectionPool database = bracket (createPool (PSQL.connectPostgreSQL database) PSQL.close 1 connectionIdleTime connectionPoolSize) destroyAllResources
  where
    connectionPoolSize :: Int
    connectionPoolSize = 5

    connectionIdleTime :: NominalDiffTime
    connectionIdleTime = 5

withDatabaseAuthorizeAction :: ByteString -> (AuthorizeAction -> IO a) -> IO a
withDatabaseAuthorizeAction database action = withConnectionPool database $ \pool -> do
  let authorizeAction :: AuthorizeAction
      authorizeAction domain email path method = withResource pool $ \conn -> authorizedGroups conn domain email path method
  action authorizeAction

authorizedGroups :: PSQL.Connection -> AuthorizeAction
authorizedGroups db email domain path method =
  (fmap PSQL.fromOnly) `fmap` PSQL.query db [q|
SELECT gp."group" FROM group_privilege gp
INNER JOIN group_member gm ON gm."group" = gp."group"
INNER JOIN "group" g ON gp."group" = g."group"
WHERE ? LIKE email
AND ? LIKE "domain"
AND privilege IN (
  SELECT p.privilege FROM privilege p
  INNER JOIN privilege_rule pr ON pr."domain" = p."domain" AND pr.privilege = p.privilege
  WHERE ? LIKE pr."domain" AND ? LIKE "path" AND ? ILIKE "method"
  ORDER by array_length(regexp_split_to_array("path", '/'), 1) DESC LIMIT 1
)
|] (email, domain, domain, path, method)
