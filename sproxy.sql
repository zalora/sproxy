-- CREATE DATABASE sproxy;
-- CREATE ROLE sproxy WITH LOGIN;
-- GRANT SELECT ON ALL TABLES IN SCHEMA public TO sproxy;

CREATE TABLE IF NOT EXISTS "group" (
  "group" TEXT NOT NULL PRIMARY KEY
);

-- | group        |
-- |--------------|
-- | data science |
-- | devops       |
-- | all          |
-- | regional     |
-- | SG HQ        |

CREATE TABLE IF NOT EXISTS group_member (
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

CREATE TABLE IF NOT EXISTS privilege (
  "domain" TEXT NOT NULL,
  privilege TEXT NOT NULL,
  PRIMARY KEY ("domain", privilege)
);

-- | domain                | privilege  |
-- |-----------------------+------------|
-- | redsift.ds.zalora.com | view       |
-- | redsift.ds.zalora.com | export     |
-- | admin.zalora.com      | list users |
-- | admin.zalora.com      | add users  |

CREATE TABLE IF NOT EXISTS privilege_rule (
  "domain" TEXT NOT NULL,
  privilege TEXT NOT NULL,
  "path" TEXT NOT NULL,
  "method" TEXT NOT NULL,
  FOREIGN KEY ("domain", privilege) REFERENCES privilege ("domain", privilege) ON UPDATE CASCADE ON DELETE CASCADE,
  PRIMARY KEY ("domain", privilege, "path", "method")
);

-- | domain                | privilege  | path      | method |
-- |-----------------------+------------+-----------+--------|
-- | redsift.ds.zalora.com | view       | /%        | %      |
-- | redsift.ds.zalora.com | export     | /export/% | %      |
-- | admin.zalora.com      | list users | /users    | GET    |
-- | admin.zalora.com      | list users | /user/%   | GET    |
-- | admin.zalora.com      | add users  | /users    | POST   |

CREATE TABLE IF NOT EXISTS group_privilege (
  "group" TEXT REFERENCES "group" ("group") ON UPDATE CASCADE ON DELETE CASCADE NOT NULL,
  "domain" TEXT NOT NULL,
  privilege TEXT NOT NULL,
  FOREIGN KEY ("domain", privilege) REFERENCES privilege ("domain", privilege) ON UPDATE CASCADE ON DELETE CASCADE,
  PRIMARY KEY ("group", "domain", privilege)
);

-- | group        | domain                | privilege  |
-- |--------------+-----------------------+------------|
-- | data science | redsift.ds.zalora.com | view       |
-- | data science | redsift.ds.zalora.com | export     |
-- | all          | admin.zalora.com      | list users |
-- | devops       | admin.zalora.com      | add users  |

-- Check if the user is authorized for the request. Let's break it
-- down for understanding:

-- The privilege required to access a URL is the most specific
-- (longest) match. To determine length, we look at the number of
-- slashes in the URL pattern (number of path components).
--
-- SELECT p.privilege FROM privilege p
-- INNER JOIN privilege_rule pr ON pr."domain" = p."domain" AND pr.privilege = p.privilege
-- WHERE 'redsift.ds.zalora.com' LIKE pr."domain" AND '/export/test' LIKE "path" AND 'GET' ILIKE "method"
-- ORDER by array_length(regexp_split_to_array("path", '/'), 1) DESC LIMIT 1
--
-- To get the groups that grant the user access, put that in a subquery:
--
-- SELECT gp."group" FROM group_privilege gp
-- INNER JOIN group_member gm ON gm."group" = gp."group"
-- INNER JOIN "group" g ON gp."group" = g."group"
-- WHERE 'chris.forno@zalora.com' LIKE email
-- AND 'redsift.ds.zalora.com' LIKE "domain"
-- AND privilege IN (
--   SELECT p.privilege FROM privilege p
--   INNER JOIN privilege_rule pr ON pr."domain" = p."domain" AND pr.privilege = p.privilege
--   WHERE 'redsift.ds.zalora.com' LIKE pr."domain" AND '/export/test' LIKE "path" AND 'GET' ILIKE "method"
--   ORDER by array_length(regexp_split_to_array("path", '/'), 1) DESC LIMIT 1
-- )
--
-- If you just want to know if a user has access or not, you can
-- change the first line to:
--
-- SELECT COUNT(*) > ) FROM group_privilege gp
--
-- Note for the future: If you want to support wildcards that match
-- only a single path component (e.g. admin.zalora.com/user/:/email),
-- you could try something like:
--
-- WHERE 'url' ~ regexp_replace(url, ':', '[^/]+')
--
-- But you'd also have to escape any regexp special characters in the
-- url as well (i.e. dots).
