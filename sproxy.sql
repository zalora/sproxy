-- CREATE DATABASE sproxy;
-- CREATE ROLE sproxy WITH LOGIN;
-- GRANT SELECT ON ALL TABLES IN SCHEMA public TO sproxy;

CREATE TABLE "group" (
  "group" TEXT NOT NULL PRIMARY KEY
);

-- | group        |
-- |--------------|
-- | data science |
-- | devops       |
-- | all          |
-- | regional     |
-- | SG HQ        |

CREATE TABLE group_member (
  "group" TEXT REFERENCES "group" ("group") ON UPDATE CASCADE ON DELETE CASCADE NOT NULL,
  email TEXT NOT NULL,
  PRIMARY KEY ("group", email)
);

-- | group        | email                  |
-- |--------------+------------------------|
-- | data science | dat.le@zalora.com      |
-- | data science | soenke.hahn@zalora.com |
-- | devops       | chris.forno@zalora.com |
-- | devops       | soenke.hahn@zalora.com |
-- | all          | %@zalora.com           |
-- | all          | %@zalora.sg            |
-- | all          | %@zalora.vn            |
-- | all          | %@zalora.com.hk        |
-- | all          | %@zalora.com.my        |
-- | all          | %@zalora.com.ph        |
-- | all          | %@zalora.co.id         |
-- | all          | %@zalora.co.th         |
-- | regional     | %@zalora.com           |
-- | SG           | %@zalora.sg            |

-- Find out which groups a user (email address) belongs to:
-- SELECT "group" FROM group_member WHERE 'email.address' LIKE email

CREATE TABLE privilege (
  privilege TEXT PRIMARY KEY NOT NULL,
  url TEXT NOT NULL,
  "method" TEXT NOT NULL
);

-- | privilege      | url                            | method |
-- |----------------+--------------------------------+--------|
-- | redsift        | redsift.ds.zalora.com/%        | %      |
-- | redsift export | redsift.ds.zalora.com/export/% | %      |
-- | list users     | admin.zalora.com/users         | GET    |
-- | add users      | admin.zalora.com/users         | POST   |

CREATE TABLE group_privilege (
  "group" TEXT REFERENCES "group" ("group") ON UPDATE CASCADE ON DELETE CASCADE NOT NULL,
  privilege TEXT REFERENCES privilege (privilege) ON UPDATE CASCADE ON DELETE CASCADE NOT NULL,
  PRIMARY KEY ("group", privilege)
);

-- | group        | privilege      |
-- |--------------+----------------|
-- | data science | redsift        |
-- | data science | redsift export |
-- | all          | list users     |
-- | devops       | add users      |

-- Check if the user is authorized for the request. Let's break it
-- down for understanding:

-- The privilege required to access a URL is the most specific
-- (longest) match. To determine length, we look at the number of
-- slashes in the URL pattern (number of path components).
--
-- SELECT privilege FROM privilege
-- WHERE 'redsift.zalora.com/export/test' LIKE url AND 'GET' ILIKE "method"
-- ORDER by array_length(regexp_split_to_array(url, '/'), 1) DESC LIMIT 1

-- Checking that the user has this privilege can be done with a subselect:
--
-- SELECT COUNT(*) > 0 FROM group_privilege gp
-- INNER JOIN group_member gm ON gm."group" = gp."group"
-- INNER JOIN "group" g ON gp."group" = g."group"
-- WHERE 'chris.forno@zalora.com' LIKE email
-- AND privilege IN (
--   SELECT privilege FROM privilege
--   WHERE 'redsift.zalora.com/' LIKE url AND 'GET' ILIKE "method"
--   ORDER by array_length(regexp_split_to_array(url, '/'), 1) DESC LIMIT 1
-- )
--
-- Or, to get the groups that grant the user access (by changing just
-- the first line):
--
-- SELECT gp."group" FROM group_privilege gp
-- INNER JOIN group_member gm ON gm."group" = gp."group"
-- INNER JOIN "group" g ON gp."group" = g."group"
-- WHERE 'chris.forno@zalora.com' LIKE email
-- AND privilege IN (
--   SELECT privilege FROM privilege
--   WHERE 'redsift.zalora.com/' LIKE url AND 'GET' ILIKE "method"
--   ORDER by array_length(regexp_split_to_array(url, '/'), 1) DESC LIMIT 1
-- )
--
-- Note for the future: If you want to support wildcards that match
-- only a single path component (e.g. admin.zalora.com/user/*/email),
-- you could try something like:
--
-- WHERE 'url' ~ regexp_replace(url, '\\*', '[^/]+')
--
-- But you'd also have to escape any regexp special characters in the
-- url as well (i.e. dots).
