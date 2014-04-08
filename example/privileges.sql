INSERT INTO domain (domain) VALUES ('dev.zalora.com');
INSERT INTO "group" ("group") VALUES ('dev');
INSERT INTO group_member ("group", email) VALUES ('dev', 'me@zalora.com');
INSERT INTO privilege (domain, privilege) VALUES ('dev.zalora.com', 'full');
INSERT INTO group_privilege ("group", domain, privilege) VALUES ('dev', 'dev.zalora.com', 'full');
INSERT INTO privilege_rule (domain, privilege, path, method) VALUES ('dev.zalora.com', 'full', '%', '%');
