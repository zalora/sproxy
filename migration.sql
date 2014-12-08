\set ON_ERROR_STOP true

/*
Update the primary key for the privilege_rule table if necessary

Original columns: domain, privilege, path, method
Adjusted columns: domain, path, method
*/
DO LANGUAGE plpgsql $$
BEGIN
	PERFORM
		1
	FROM pg_index, pg_class, pg_attribute
	WHERE
		pg_class.oid = 'privilege_rule'::regclass
		AND indrelid = pg_class.oid
		AND pg_attribute.attrelid = pg_class.oid
		AND pg_attribute.attnum = any(pg_index.indkey)
		AND indisprimary
	HAVING
		array_agg(pg_attribute.attname) :: TEXT[] = ARRAY['domain', 'path', 'method'] :: TEXT[];

	IF FOUND THEN
		RAISE NOTICE 'The primary key for privilege_rule is already up to date.';
	ELSE
		ALTER TABLE privilege_rule DROP CONSTRAINT "privilege_rule_pkey";
		ALTER TABLE privilege_rule ADD PRIMARY KEY ("domain", "path", "method");
		RAISE NOTICE 'The primary key for privilege_rule has been updated.';
	END IF;

EXCEPTION WHEN unique_violation THEN
	RAISE INFO 'Duplicate privileges exist in privilege_rule.  The following query will reveal all duplicates:

	SELECT * FROM privilege_rule AS p1 WHERE exists (
		SELECT 1 FROM privilege_rule AS p2
		WHERE
			p1.domain = p2.domain
			AND p1.path = p2.path
			AND p1.method = p2.method
		GROUP BY domain, path, method
		HAVING count(*) > 1);';
	RAISE 'The primary key for privilege_rule could not be adjusted.';
END;
$$;
