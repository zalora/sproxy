/*

-- as super user:

-- NOT idempotent
CREATE DATABASE sproxy;
CREATE ROLE sproxy;            -- this is for management tools like sproxy-web
CREATE ROLE "sproxy-readonly"; -- this is for sproxy itself (sic!)

-- idempotent from here on:
ALTER DATABASE sproxy OWNER TO sproxy;
ALTER ROLE "sproxy-readonly" LOGIN;
ALTER ROLE sproxy LOGIN;

\c sproxy;

SET ROLE sproxy;
-- as database owner (sproxy) from here on:

GRANT SELECT ON ALL TABLES IN SCHEMA public TO "sproxy-readonly";
ALTER DEFAULT PRIVILEGES IN SCHEMA public GRANT SELECT ON TABLES TO "sproxy-readonly";

*/


BEGIN;

CREATE TABLE IF NOT EXISTS "group" (
  "group" TEXT NOT NULL PRIMARY KEY
);

-- | group        |
-- |--------------|
-- | data science |
-- | devops       |
-- | all          |
-- | regional     |


CREATE TABLE IF NOT EXISTS group_member (
  "group" TEXT REFERENCES "group" ("group") ON UPDATE CASCADE ON DELETE CASCADE NOT NULL,
  email TEXT NOT NULL,
  PRIMARY KEY ("group", email)
);

-- | group        | email                  |
-- |--------------+------------------------|
-- | data science | blah@example.com        |
-- | data science | foo@example.com         |
-- | devops       | devops1@example.com     |
-- | devops       | devops2@example.com     |
-- | all          | %@example.com           |

-- Find out which groups a user (email address) belongs to:
-- SELECT "group" FROM group_member WHERE 'email.address' LIKE email

CREATE TABLE IF NOT EXISTS domain (
  domain TEXT NOT NULL PRIMARY KEY
);

-- | domain                |
-- |-----------------------|
-- | app1.example.com      |
-- | app2.example.com      |
-- | app3.example.com      |

CREATE TABLE IF NOT EXISTS privilege (
  "domain" TEXT REFERENCES domain (domain) ON UPDATE CASCADE ON DELETE CASCADE NOT NULL,
  privilege TEXT NOT NULL,
  PRIMARY KEY ("domain", privilege)
);

-- | domain                | privilege  |
-- |-----------------------+------------|
-- | app3.example.com      | view       |
-- | app3.example.com      | export     |
-- | app1.example.com      | list users |
-- | app1.example.com      | add users  |

CREATE TABLE IF NOT EXISTS privilege_rule (
  "domain" TEXT NOT NULL,
  privilege TEXT NOT NULL,
  "path" TEXT NOT NULL,
  "method" TEXT NOT NULL,
  FOREIGN KEY ("domain", privilege) REFERENCES privilege ("domain", privilege) ON UPDATE CASCADE ON DELETE CASCADE,
  PRIMARY KEY ("domain", "path", "method")
);

-- | domain                | privilege  | path      | method |
-- |-----------------------+------------+-----------+--------|
-- | app3.example.com      | view       | /%        | %      |
-- | app3.example.com      | export     | /export/% | %      |
-- | app1.example.com      | list users | /users    | GET    |
-- | app1.example.com      | list users | /user/%   | GET    |
-- | app1.example.com      | add users  | /users    | POST   |

CREATE TABLE IF NOT EXISTS group_privilege (
  "group" TEXT REFERENCES "group" ("group") ON UPDATE CASCADE ON DELETE CASCADE NOT NULL,
  "domain" TEXT NOT NULL,
  privilege TEXT NOT NULL,
  FOREIGN KEY ("domain", privilege) REFERENCES privilege ("domain", privilege) ON UPDATE CASCADE ON DELETE CASCADE,
  PRIMARY KEY ("group", "domain", privilege)
);

-- | group        | domain                | privilege  |
-- |--------------+-----------------------+------------|
-- | data science | app3.example.com      | view       |
-- | data science | app3.example.com      | export     |
-- | all          | app1.example.com      | list users |
-- | devops       | app1.example.com      | add users  |

-- Check if the user is authorized for the request. Let's break it
-- down for understanding:

-- The privilege required to access a URL is the most specific
-- (longest) match. To determine length, we look at the number of
-- slashes in the URL pattern (number of path components).
--
-- SELECT p.privilege FROM privilege p
-- INNER JOIN privilege_rule pr ON pr."domain" = p."domain" AND pr.privilege = p.privilege
-- WHERE 'app3.example.com' LIKE pr."domain" AND '/export/test' LIKE "path" AND 'GET' ILIKE "method"
-- ORDER by array_length(regexp_split_to_array("path", '/'), 1) DESC LIMIT 1
--
-- To get the groups that grant the user access, put that in a subquery:
--
-- SELECT gp."group" FROM group_privilege gp
-- INNER JOIN group_member gm ON gm."group" = gp."group"
-- WHERE 'blah@example.com' LIKE email
-- AND 'app3.example.com' LIKE "domain"
-- AND privilege IN (
--   SELECT p.privilege FROM privilege p
--   INNER JOIN privilege_rule pr ON pr."domain" = p."domain" AND pr.privilege = p.privilege
--   WHERE 'app3.example.com' LIKE pr."domain" AND '/export/test' LIKE "path" AND 'GET' ILIKE "method"
--   ORDER by array_length(regexp_split_to_array("path", '/'), 1) DESC LIMIT 1
-- )
--
-- If you just want to know if a user has access or not, you can
-- change the first line to:
--
-- SELECT COUNT(*) > 0 FROM group_privilege gp
--
-- Note for the future: If you want to support wildcards that match
-- only a single path component (e.g. app1.example.com/user/:/email),
-- you could try something like:
--
-- WHERE 'url' ~ regexp_replace(url, ':', '[^/]+')
--
-- But you'd also have to escape any regexp special characters in the
-- url as well (i.e. dots).

-- Example data for development:
/*
  INSERT INTO domain (domain) VALUES ('example.com');
  INSERT INTO "group" ("group") VALUES ('dev');
  INSERT INTO group_member ("group", email) VALUES ('dev', '%');
  INSERT INTO privilege (domain, privilege) VALUES ('example.com', 'full');
  INSERT INTO group_privilege ("group", domain, privilege) VALUES ('dev', 'example.com', 'full');
  INSERT INTO privilege_rule (domain, privilege, path, method) VALUES ('example.com', 'full', '%', '%');
*/

END;

