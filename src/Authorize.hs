{-# LANGUAGE QuasiQuotes #-}
module Authorize (
  WithAuthorizeAction
, AuthorizeAction
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

type WithAuthorizeAction = (AuthorizeAction -> IO ()) -> IO ()
type AuthorizeAction = Email -> Domain -> RequestPath -> Method -> IO [Group]

type Email = String
type Domain = ByteString
type RequestPath = ByteString
type Group = String

withDatabaseAuthorizeAction :: ByteString -> (AuthorizeAction -> IO a) -> IO a
withDatabaseAuthorizeAction database action = bracket (PSQL.connectPostgreSQL database) PSQL.close (\db -> action (authorizedGroups db))

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
