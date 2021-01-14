/*MIGRATION_DESCRIPTION
--CREATE: $name$-Example
New object Example will be created in schema $name$
--CREATE: $name$-Example-ID
New property ID will be created for Example in $name$
MIGRATION_DESCRIPTION*/

DO \$\$ BEGIN
	IF EXISTS(SELECT * FROM pg_class c JOIN pg_namespace n ON n.oid = c.relnamespace WHERE n.nspname = '-DSL-' AND c.relname = 'database_setting') THEN	
		IF EXISTS(SELECT * FROM "-DSL-".Database_Setting WHERE Key ILIKE 'mode' AND NOT Value ILIKE 'unsafe') THEN
			RAISE EXCEPTION 'Database upgrade is forbidden. Change database mode to allow upgrade';
		END IF;
	END IF;
END \$\$ LANGUAGE plpgsql;

DO \$X\$ BEGIN
	IF EXISTS(SELECT * FROM pg_namespace n JOIN pg_description d ON d.objoid = n.oid WHERE n.nspname = '-NGS-' AND d.description LIKE 'NGS generated%') THEN
		ALTER SCHEMA "-NGS-" RENAME TO "-DSL-";

		CREATE OR REPLACE FUNCTION "-DSL-".Create_Type_Cast(function VARCHAR, schema VARCHAR, from_name VARCHAR, to_name VARCHAR)
		RETURNS void AS
		\$MIG\$
		DECLARE header VARCHAR;
		DECLARE source VARCHAR;
		DECLARE footer VARCHAR;
		DECLARE col_name VARCHAR;
		DECLARE type VARCHAR = '"' || schema || '"."' || to_name || '"';
		BEGIN
			header = 'CREATE OR REPLACE FUNCTION ' || function || '
		RETURNS ' || type || '
		AS
		\$BODY\$
		SELECT ROW(';
			footer = ')::' || type || '
		\$BODY\$ IMMUTABLE LANGUAGE sql;';
			source = '';
			FOR col_name IN 
				SELECT 
					CASE WHEN 
						EXISTS (SELECT * FROM "-DSL-".Load_Type_Info() f 
							WHERE f.type_schema = schema AND f.type_name = from_name AND f.column_name = t.column_name)
						OR EXISTS(SELECT * FROM pg_proc p JOIN pg_type t_in ON p.proargtypes[0] = t_in.oid 
							JOIN pg_namespace n_in ON t_in.typnamespace = n_in.oid JOIN pg_namespace n ON p.pronamespace = n.oid
							WHERE array_upper(p.proargtypes, 1) = 0 AND n.nspname = 'public' AND t_in.typname = from_name AND p.proname = t.column_name) THEN t.column_name
						ELSE null
					END
				FROM "-DSL-".Load_Type_Info() t
				WHERE 
					t.type_schema = schema 
					AND t.type_name = to_name
				ORDER BY t.column_index 
			LOOP
				IF col_name IS NULL THEN
					source = source || 'null, ';
				ELSE
					source = source || '\$1."' || col_name || '", ';
				END IF;
			END LOOP;
			IF (LENGTH(source) > 0) THEN 
				source = SUBSTRING(source, 1, LENGTH(source) - 2);
			END IF;
			EXECUTE (header || source || footer);
		END
		\$MIG\$ LANGUAGE plpgsql;

		CREATE OR REPLACE FUNCTION "-DSL-".Load_Last_Migration()
		RETURNS "-DSL-".Database_Migration AS
		\$\$
		SELECT m FROM "-DSL-".Database_Migration m
		ORDER BY Ordinal DESC 
		LIMIT 1
		\$\$ LANGUAGE sql SECURITY DEFINER STABLE;

		CREATE OR REPLACE FUNCTION "-DSL-".Persist_Concepts(dsls TEXT, implementations BYTEA, version VARCHAR)
		  RETURNS void AS
		\$\$
		BEGIN
			INSERT INTO "-DSL-".Database_Migration(Dsls, Implementations, Version) VALUES(dsls, implementations, version);
		END;
		\$\$ LANGUAGE plpgsql SECURITY DEFINER;

		CREATE OR REPLACE FUNCTION "-DSL-".Safe_Notify(target varchar, name varchar, operation varchar, uris varchar[]) RETURNS VOID AS
		\$\$
		DECLARE message VARCHAR;
		DECLARE array_size INT;
		BEGIN
			array_size = array_upper(uris, 1);
			message = name || ':' || operation || ':' || uris::TEXT;
			IF (array_size > 0 and length(message) < 8000) THEN 
				PERFORM pg_notify(target, message);
			ELSEIF (array_size > 1) THEN
				PERFORM "-DSL-".Safe_Notify(target, name, operation, (SELECT array_agg(u) FROM (SELECT unnest(uris) u LIMIT (array_size+1)/2) u));
				PERFORM "-DSL-".Safe_Notify(target, name, operation, (SELECT array_agg(u) FROM (SELECT unnest(uris) u OFFSET (array_size+1)/2) u));
			ELSEIF (array_size = 1) THEN
				RAISE EXCEPTION 'uri can''t be longer than 8000 characters';
			END IF;	
		END
		\$\$ LANGUAGE PLPGSQL SECURITY DEFINER;
	END IF;
END \$X\$ LANGUAGE plpgsql;

DO \$X\$ BEGIN
	IF EXISTS(SELECT * FROM pg_namespace n JOIN pg_description d ON d.objoid = n.oid WHERE n.nspname = '-DSL-')
		AND NOT EXISTS(select * from pg_proc p join pg_namespace n on p.pronamespace = n.oid where n.nspname = '-DSL-' AND p.proname = 'add_enum_label') THEN
		CREATE OR REPLACE FUNCTION "-DSL-".Add_Enum_Label(ns varchar, tn varchar, lbl varchar) RETURNS VOID AS
		\$\$ 
		BEGIN
			IF NOT EXISTS(SELECT * FROM pg_enum e JOIN pg_type t ON e.enumtypid = t.oid JOIN pg_namespace n ON n.oid = t.typnamespace WHERE n.nspname = ns AND t.typname = tn AND e.enumlabel = lbl) THEN
				IF NOT EXISTS(SELECT * FROM pg_catalog.pg_user WHERE usename = CURRENT_USER AND usesuper) THEN
					RAISE EXCEPTION 'Unable to change enum label. Function -DSL-.add_enum_label is not created with superuser powers.';
				END IF;
				--TODO: detect OID wraparounds and throw an exception in that case
				INSERT INTO pg_enum(enumtypid, enumlabel, enumsortorder)
				SELECT t.oid, lbl, (SELECT MAX(enumsortorder) + 1 FROM pg_enum e WHERE e.enumtypid = t.oid)
				FROM pg_type t 
				INNER JOIN pg_namespace n ON n.oid = t.typnamespace 
				WHERE n.nspname = ns AND t.typname = tn;
			END IF;
		END
		\$\$ SECURITY DEFINER LANGUAGE PLPGSQL;
	END IF;
	IF EXISTS(SELECT * FROM pg_namespace n JOIN pg_description d ON d.objoid = n.oid WHERE n.nspname = '-DSL-')
		AND NOT EXISTS(select * from pg_proc p join pg_namespace n on p.pronamespace = n.oid where n.nspname = '-DSL-' AND p.proname = 'rename_enum_label') THEN
		CREATE OR REPLACE FUNCTION "-DSL-".Rename_Enum_Label(ns varchar, tn varchar, oldLbl varchar, newLbl varchar, ticks bigint) RETURNS VOID AS
		\$\$ 
		BEGIN
			IF NOT EXISTS(SELECT * FROM pg_catalog.pg_user WHERE usename = CURRENT_USER AND usesuper) THEN
				RAISE EXCEPTION 'Unable to rename enum label. Function -DSL-.rename_enum_label is not created with superuser powers.';
			END IF;
			IF (EXISTS(SELECT * FROM pg_enum e JOIN pg_type t ON e.enumtypid = t.oid JOIN pg_namespace n ON n.oid = t.typnamespace WHERE n.nspname = ns AND t.typname = tn AND e.enumlabel = newLbl)
				AND EXISTS(SELECT * FROM pg_enum e JOIN pg_type t ON e.enumtypid = t.oid JOIN pg_namespace n ON n.oid = t.typnamespace WHERE n.nspname = ns AND t.typname = tn AND e.enumlabel = oldLbl)) THEN
				--TODO: guard against old enums
				UPDATE pg_enum SET enumlabel = '-old-' || newLbl || '-' || ticks || '-'
				WHERE enumlabel = newLbl AND enumtypid IN (
					SELECT t.oid
					FROM pg_type t 
					INNER JOIN pg_namespace n ON n.oid = t.typnamespace 
					WHERE n.nspname = ns AND t.typname = tn
				);
			END IF;
			UPDATE pg_enum SET enumlabel = newLbl
			WHERE enumlabel = oldLbl AND enumtypid IN (
				SELECT t.oid
				FROM pg_type t 
				INNER JOIN pg_namespace n ON n.oid = t.typnamespace 
				WHERE n.nspname = ns AND t.typname = tn
			);
		END
		\$\$ SECURITY DEFINER LANGUAGE PLPGSQL;
	END IF;
END \$X\$ LANGUAGE plpgsql;

DO \$\$ BEGIN
	IF NOT EXISTS(SELECT * FROM pg_namespace WHERE nspname = '$name$') THEN
		CREATE SCHEMA "$name$";
		COMMENT ON SCHEMA "$name$" IS 'NGS generated';
	END IF;
END \$\$ LANGUAGE plpgsql;

DO \$x\$ BEGIN
	IF EXISTS(SELECT * FROM pg_namespace n JOIN pg_description d ON d.objoid = n.oid WHERE n.nspname = '-DSL-') THEN
		CREATE OR REPLACE FUNCTION "-DSL-".Load_Type_Info(
			OUT type_schema character varying, 
			OUT type_name character varying, 
			OUT column_name character varying, 
			OUT column_schema character varying,
			OUT column_type character varying, 
			OUT column_index smallint, 
			OUT is_not_null boolean,
			OUT is_ngs_generated boolean)
		  RETURNS SETOF record AS
		\$BODY\$
		SELECT 
			ns.nspname::varchar, 
			cl.relname::varchar, 
			atr.attname::varchar, 
			ns_ref.nspname::varchar,
			typ.typname::varchar, 
			(SELECT COUNT(*) + 1
			FROM pg_attribute atr_ord
			WHERE 
				atr.attrelid = atr_ord.attrelid
				AND atr_ord.attisdropped = false
				AND atr_ord.attnum > 0
				AND atr_ord.attnum < atr.attnum)::smallint, 
			atr.attnotnull,
			coalesce(d.description LIKE 'NGS generated%', false)
		FROM 
			pg_attribute atr
			INNER JOIN pg_class cl ON atr.attrelid = cl.oid
			INNER JOIN pg_namespace ns ON cl.relnamespace = ns.oid
			INNER JOIN pg_type typ ON atr.atttypid = typ.oid
			INNER JOIN pg_namespace ns_ref ON typ.typnamespace = ns_ref.oid
			LEFT JOIN pg_description d ON d.objoid = cl.oid
										AND d.objsubid = atr.attnum
		WHERE
			cl.relkind IN ('r', 'p', 'v', 'c')
			AND ns.nspname NOT LIKE 'pg_%'
			AND ns.nspname != 'information_schema'
			AND atr.attnum > 0
			AND atr.attisdropped = FALSE
		ORDER BY 1, 2, 6
		\$BODY\$
		  LANGUAGE SQL STABLE;
	END IF;
END \$x\$ LANGUAGE plpgsql;

DO \$x\$ BEGIN
	IF EXISTS(SELECT * FROM pg_namespace n JOIN pg_description d ON d.objoid = n.oid WHERE n.nspname = '-DSL-') THEN
		CREATE OR REPLACE FUNCTION "-DSL-".Add_List_Partitions(_schema TEXT, _table TEXT, _values TEXT[]) RETURNS VOID AS
		\$body\$
		DECLARE n TEXT;
		BEGIN
					FOR n IN SELECT 'CREATE TABLE IF NOT EXISTS ' || quote_ident(_schema) || '.' || quote_ident(_table || ':' || v) 
							|| ' PARTITION OF ' || quote_ident(_schema) || '.' || quote_ident(_table) || ' FOR VALUES IN (' || quote_literal(v) || ')'
						FROM UNNEST(_values) v 
						WHERE NOT EXISTS(SELECT * 
								FROM pg_type t 
								INNER JOIN pg_namespace n on t.typnamespace = n.oid 
								WHERE n.nspname = _schema 
								AND t.typname = _table || ':' || v)
					LOOP
						EXECUTE n;
					END LOOP;
		END;
		\$body\$ LANGUAGE plpgsql;
	END IF;
END \$x\$ LANGUAGE plpgsql;

DO \$\$
DECLARE script VARCHAR;
BEGIN
	IF NOT EXISTS(SELECT * FROM pg_namespace WHERE nspname = '-DSL-') THEN
		CREATE SCHEMA "-DSL-";
		COMMENT ON SCHEMA "-DSL-" IS 'NGS generated';
	END IF;
	IF NOT EXISTS(SELECT * FROM pg_namespace WHERE nspname = 'public') THEN
		CREATE SCHEMA public;
		COMMENT ON SCHEMA public IS 'NGS generated';
	END IF;
	SELECT array_to_string(array_agg('DROP VIEW IF EXISTS ' || quote_ident(n.nspname) || '.' || quote_ident(cl.relname) || ' CASCADE;'), '')
	INTO script
	FROM pg_class cl
	INNER JOIN pg_namespace n ON cl.relnamespace = n.oid
	INNER JOIN pg_description d ON d.objoid = cl.oid
	WHERE cl.relkind = 'v' AND d.description LIKE 'NGS volatile%';
	IF length(script) > 0 THEN
		EXECUTE script;
	END IF;
END \$\$ LANGUAGE plpgsql;

CREATE TABLE IF NOT EXISTS "-DSL-".Database_Migration
(
	Ordinal SERIAL PRIMARY KEY,
	Dsls TEXT,
	Implementations BYTEA,
	Version VARCHAR,
	Applied_At TIMESTAMPTZ DEFAULT (CURRENT_TIMESTAMP)
);

CREATE OR REPLACE FUNCTION "-DSL-".Load_Last_Migration()
RETURNS "-DSL-".Database_Migration AS
\$\$
SELECT m FROM "-DSL-".Database_Migration m
ORDER BY Ordinal DESC 
LIMIT 1
\$\$ LANGUAGE sql SECURITY DEFINER STABLE;

CREATE OR REPLACE FUNCTION "-DSL-".Persist_Concepts(dsls TEXT, implementations BYTEA, version VARCHAR)
  RETURNS void AS
\$\$
BEGIN
	INSERT INTO "-DSL-".Database_Migration(Dsls, Implementations, Version) VALUES(dsls, implementations, version);
END;
\$\$ LANGUAGE plpgsql SECURITY DEFINER;

CREATE OR REPLACE FUNCTION "-DSL-".Split_Uri(s text) RETURNS TEXT[] AS
\$\$
DECLARE i int;
DECLARE pos int;
DECLARE len int;
DECLARE res TEXT[];
DECLARE cur TEXT;
DECLARE c CHAR(1);
BEGIN
	pos = 0;
	i = 1;
	cur = '';
	len = length(s);
	LOOP
		pos = pos + 1;
		EXIT WHEN pos > len;
		c = substr(s, pos, 1);
		IF c = '/' THEN
			res[i] = cur;
			i = i + 1;
			cur = '';
		ELSE
			IF c = '\' THEN
				pos = pos + 1;
				c = substr(s, pos, 1);
			END IF;		
			cur = cur || c;
		END IF;
	END LOOP;
	res[i] = cur;
	return res;
END
\$\$ LANGUAGE plpgsql SECURITY DEFINER IMMUTABLE;

DO \$X\$ BEGIN
	IF NOT EXISTS(select * from pg_aggregate where aggfnoid::TEXT = '"-DSL-".array_combine') THEN
		CREATE AGGREGATE "-DSL-".array_combine(anyarray)(SFUNC = array_cat, STYPE = anyarray, INITCOND = '{}');
	END IF;
END \$X\$ LANGUAGE plpgsql;

CREATE OR REPLACE FUNCTION "-DSL-".Load_Type_Info(
	OUT type_schema character varying, 
	OUT type_name character varying, 
	OUT column_name character varying, 
	OUT column_schema character varying,
	OUT column_type character varying, 
	OUT column_index smallint, 
	OUT is_not_null boolean,
	OUT is_ngs_generated boolean)
  RETURNS SETOF record AS
\$BODY\$
SELECT 
	ns.nspname::varchar, 
	cl.relname::varchar, 
	atr.attname::varchar, 
	ns_ref.nspname::varchar,
	typ.typname::varchar, 
	(SELECT COUNT(*) + 1
	FROM pg_attribute atr_ord
	WHERE 
		atr.attrelid = atr_ord.attrelid
		AND atr_ord.attisdropped = false
		AND atr_ord.attnum > 0
		AND atr_ord.attnum < atr.attnum)::smallint, 
	atr.attnotnull,
	coalesce(d.description LIKE 'NGS generated%', false)
FROM 
	pg_attribute atr
	INNER JOIN pg_class cl ON atr.attrelid = cl.oid
	INNER JOIN pg_namespace ns ON cl.relnamespace = ns.oid
	INNER JOIN pg_type typ ON atr.atttypid = typ.oid
	INNER JOIN pg_namespace ns_ref ON typ.typnamespace = ns_ref.oid
	LEFT JOIN pg_description d ON d.objoid = cl.oid
								AND d.objsubid = atr.attnum
WHERE
	cl.relkind IN ('r', 'p', 'v', 'c')
	AND ns.nspname NOT LIKE 'pg_%'
	AND ns.nspname != 'information_schema'
	AND atr.attnum > 0
	AND atr.attisdropped = FALSE
ORDER BY 1, 2, 6
\$BODY\$
  LANGUAGE SQL STABLE;

CREATE OR REPLACE FUNCTION "-DSL-".Add_List_Partitions(_schema TEXT, _table TEXT, _values TEXT[]) RETURNS VOID AS
\$BODY\$
DECLARE n TEXT;
BEGIN
			FOR n IN SELECT 'CREATE TABLE IF NOT EXISTS ' || quote_ident(_schema) || '.' || quote_ident(_table || ':' || v) 
					|| ' PARTITION OF ' || quote_ident(_schema) || '.' || quote_ident(_table) || ' FOR VALUES IN (' || quote_literal(v) || ')'
				FROM UNNEST(_values) v 
				WHERE NOT EXISTS(SELECT * 
						FROM pg_type t 
						INNER JOIN pg_namespace n on t.typnamespace = n.oid 
						WHERE n.nspname = _schema 
						AND t.typname = _table || ':' || v)
			LOOP
				EXECUTE n;
			END LOOP;
END;
\$BODY\$ LANGUAGE plpgsql;

DO \$X\$ BEGIN
	IF EXISTS(SELECT * FROM pg_namespace n JOIN pg_description d ON d.objoid = n.oid WHERE n.nspname = '-DSL-')
		AND NOT EXISTS(select * from pg_proc p join pg_namespace n on p.pronamespace = n.oid where n.nspname = '-DSL-' AND p.proname = 'add_enum_label') THEN
		CREATE OR REPLACE FUNCTION "-DSL-".Add_Enum_Label(ns varchar, tn varchar, lbl varchar) RETURNS VOID AS
		\$\$ 
		BEGIN
			IF NOT EXISTS(SELECT * FROM pg_enum e JOIN pg_type t ON e.enumtypid = t.oid JOIN pg_namespace n ON n.oid = t.typnamespace WHERE n.nspname = ns AND t.typname = tn AND e.enumlabel = lbl) THEN
				IF NOT EXISTS(SELECT * FROM pg_catalog.pg_user WHERE usename = CURRENT_USER AND usesuper) THEN
					RAISE EXCEPTION 'Unable to change enum label. Function -DSL-.add_enum_label is not created with superuser powers.';
				END IF;
				--TODO: detect OID wraparounds and throw an exception in that case
				INSERT INTO pg_enum(enumtypid, enumlabel, enumsortorder)
				SELECT t.oid, lbl, (SELECT MAX(enumsortorder) + 1 FROM pg_enum e WHERE e.enumtypid = t.oid)
				FROM pg_type t 
				INNER JOIN pg_namespace n ON n.oid = t.typnamespace 
				WHERE n.nspname = ns AND t.typname = tn;
			END IF;
		END
		\$\$ SECURITY DEFINER LANGUAGE PLPGSQL;
	END IF;
END \$X\$ LANGUAGE plpgsql;

DO \$X\$ BEGIN
	IF EXISTS(SELECT * FROM pg_namespace n JOIN pg_description d ON d.objoid = n.oid WHERE n.nspname = '-DSL-')
		AND NOT EXISTS(select * from pg_proc p join pg_namespace n on p.pronamespace = n.oid where n.nspname = '-DSL-' AND p.proname = 'rename_enum_Label') THEN
		CREATE OR REPLACE FUNCTION "-DSL-".Rename_Enum_Label(ns varchar, tn varchar, oldLbl varchar, newLbl varchar, ticks bigint) RETURNS VOID AS
		\$\$ 
		BEGIN
			IF NOT EXISTS(SELECT * FROM pg_catalog.pg_user WHERE usename = CURRENT_USER AND usesuper) THEN
				RAISE EXCEPTION 'Unable to rename enum label. Function -DSL-.rename_enum_label is not created with superuser powers.';
			END IF;
			IF (EXISTS(SELECT * FROM pg_enum e JOIN pg_type t ON e.enumtypid = t.oid JOIN pg_namespace n ON n.oid = t.typnamespace WHERE n.nspname = ns AND t.typname = tn AND e.enumlabel = newLbl)
				AND EXISTS(SELECT * FROM pg_enum e JOIN pg_type t ON e.enumtypid = t.oid JOIN pg_namespace n ON n.oid = t.typnamespace WHERE n.nspname = ns AND t.typname = tn AND e.enumlabel = oldLbl)) THEN
				--TODO: guard against old enums
				UPDATE pg_enum SET enumlabel = '-old-' || newLbl || '-' || ticks || '-'
				WHERE enumlabel = newLbl AND enumtypid IN (
					SELECT t.oid
					FROM pg_type t 
					INNER JOIN pg_namespace n ON n.oid = t.typnamespace 
					WHERE n.nspname = ns AND t.typname = tn
				);
			END IF;
			UPDATE pg_enum SET enumlabel = newLbl
			WHERE enumlabel = oldLbl AND enumtypid IN (
				SELECT t.oid
				FROM pg_type t 
				INNER JOIN pg_namespace n ON n.oid = t.typnamespace 
				WHERE n.nspname = ns AND t.typname = tn
			);
		END
		\$\$ SECURITY DEFINER LANGUAGE PLPGSQL;
	END IF;
END \$X\$ LANGUAGE plpgsql;

CREATE OR REPLACE FUNCTION "-DSL-".Safe_Notify(target varchar, name varchar, operation varchar, uris varchar[]) RETURNS VOID AS
\$\$
DECLARE message VARCHAR;
DECLARE array_size INT;
BEGIN
	array_size = array_upper(uris, 1);
	message = name || ':' || operation || ':' || uris::TEXT;
	IF (array_size > 0 and length(message) < 8000) THEN 
		PERFORM pg_notify(target, message);
	ELSEIF (array_size > 1) THEN
		PERFORM "-DSL-".Safe_Notify(target, name, operation, (SELECT array_agg(u) FROM (SELECT unnest(uris) u LIMIT (array_size+1)/2) u));
		PERFORM "-DSL-".Safe_Notify(target, name, operation, (SELECT array_agg(u) FROM (SELECT unnest(uris) u OFFSET (array_size+1)/2) u));
	ELSEIF (array_size = 1) THEN
		RAISE EXCEPTION 'uri can''t be longer than 8000 characters';
	END IF;	
END
\$\$ LANGUAGE PLPGSQL SECURITY DEFINER;

CREATE OR REPLACE FUNCTION "-DSL-".cast_int(int[]) RETURNS TEXT AS
\$\$ SELECT \$1::TEXT[]::TEXT \$\$ LANGUAGE SQL IMMUTABLE COST 1;
CREATE OR REPLACE FUNCTION "-DSL-".cast_bigint(bigint[]) RETURNS TEXT AS
\$\$ SELECT \$1::TEXT[]::TEXT \$\$ LANGUAGE SQL IMMUTABLE COST 1;

DO \$\$ BEGIN
	-- unfortunately only superuser can create such casts
	IF EXISTS(SELECT * FROM pg_catalog.pg_user WHERE usename = CURRENT_USER AND usesuper) THEN
		IF NOT EXISTS (SELECT * FROM pg_catalog.pg_cast c JOIN pg_type s ON c.castsource = s.oid JOIN pg_type t ON c.casttarget = t.oid WHERE s.typname = '_int4' AND t.typname = 'text') THEN
			CREATE CAST (int[] AS text) WITH FUNCTION "-DSL-".cast_int(int[]) AS ASSIGNMENT;
		END IF;
		IF NOT EXISTS (SELECT * FROM pg_cast c JOIN pg_type s ON c.castsource = s.oid JOIN pg_type t ON c.casttarget = t.oid WHERE s.typname = '_int8' AND t.typname = 'text') THEN
			CREATE CAST (bigint[] AS text) WITH FUNCTION "-DSL-".cast_bigint(bigint[]) AS ASSIGNMENT;
		END IF;
	END IF;
END \$\$ LANGUAGE plpgsql;

CREATE OR REPLACE FUNCTION "-DSL-".Generate_Uri2(text, text) RETURNS text AS 
\$\$
BEGIN
	RETURN replace(replace(\$1, '\','\\'), '/', '\/')||'/'||replace(replace(\$2, '\','\\'), '/', '\/');
END;
\$\$ LANGUAGE PLPGSQL IMMUTABLE;

CREATE OR REPLACE FUNCTION "-DSL-".Generate_Uri3(text, text, text) RETURNS text AS 
\$\$
BEGIN
	RETURN replace(replace(\$1, '\','\\'), '/', '\/')||'/'||replace(replace(\$2, '\','\\'), '/', '\/')||'/'||replace(replace(\$3, '\','\\'), '/', '\/');
END;
\$\$ LANGUAGE PLPGSQL IMMUTABLE;

CREATE OR REPLACE FUNCTION "-DSL-".Generate_Uri4(text, text, text, text) RETURNS text AS 
\$\$
BEGIN
	RETURN replace(replace(\$1, '\','\\'), '/', '\/')||'/'||replace(replace(\$2, '\','\\'), '/', '\/')||'/'||replace(replace(\$3, '\','\\'), '/', '\/')||'/'||replace(replace(\$4, '\','\\'), '/', '\/');
END;
\$\$ LANGUAGE PLPGSQL IMMUTABLE;

CREATE OR REPLACE FUNCTION "-DSL-".Generate_Uri5(text, text, text, text, text) RETURNS text AS 
\$\$
BEGIN
	RETURN replace(replace(\$1, '\','\\'), '/', '\/')||'/'||replace(replace(\$2, '\','\\'), '/', '\/')||'/'||replace(replace(\$3, '\','\\'), '/', '\/')||'/'||replace(replace(\$4, '\','\\'), '/', '\/')||'/'||replace(replace(\$5, '\','\\'), '/', '\/');
END;
\$\$ LANGUAGE PLPGSQL IMMUTABLE;

CREATE OR REPLACE FUNCTION "-DSL-".Generate_Uri(text[]) RETURNS text AS 
\$\$
BEGIN
	RETURN (SELECT array_to_string(array_agg(replace(replace(u, '\','\\'), '/', '\/')), '/') FROM unnest(\$1) u);
END;
\$\$ LANGUAGE PLPGSQL IMMUTABLE;

CREATE TABLE IF NOT EXISTS "-DSL-".Database_Setting
(
	Key VARCHAR PRIMARY KEY,
	Value TEXT NOT NULL
);

CREATE OR REPLACE FUNCTION "-DSL-".Create_Type_Cast(function VARCHAR, schema VARCHAR, from_name VARCHAR, to_name VARCHAR)
RETURNS void AS
\$\$
DECLARE header VARCHAR;
DECLARE source VARCHAR;
DECLARE footer VARCHAR;
DECLARE col_name VARCHAR;
DECLARE type VARCHAR = '"' || schema || '"."' || to_name || '"';
BEGIN
	header = 'CREATE OR REPLACE FUNCTION ' || function || '
RETURNS ' || type || '
AS
\$BODY\$
SELECT ROW(';
	footer = ')::' || type || '
\$BODY\$ IMMUTABLE LANGUAGE sql;';
	source = '';
	FOR col_name IN 
		SELECT 
			CASE WHEN 
				EXISTS (SELECT * FROM "-DSL-".Load_Type_Info() f 
					WHERE f.type_schema = schema AND f.type_name = from_name AND f.column_name = t.column_name)
				OR EXISTS(SELECT * FROM pg_proc p JOIN pg_type t_in ON p.proargtypes[0] = t_in.oid 
					JOIN pg_namespace n_in ON t_in.typnamespace = n_in.oid JOIN pg_namespace n ON p.pronamespace = n.oid
					WHERE array_upper(p.proargtypes, 1) = 0 AND n.nspname = 'public' AND t_in.typname = from_name AND p.proname = t.column_name) THEN t.column_name
				ELSE null
			END
		FROM "-DSL-".Load_Type_Info() t
		WHERE 
			t.type_schema = schema 
			AND t.type_name = to_name
		ORDER BY t.column_index 
	LOOP
		IF col_name IS NULL THEN
			source = source || 'null, ';
		ELSE
			source = source || '\$1."' || col_name || '", ';
		END IF;
	END LOOP;
	IF (LENGTH(source) > 0) THEN 
		source = SUBSTRING(source, 1, LENGTH(source) - 2);
	END IF;
	EXECUTE (header || source || footer);
END
\$\$ LANGUAGE plpgsql;

DO \$\$ BEGIN
	IF NOT EXISTS(SELECT * FROM pg_class c JOIN pg_namespace n ON n.oid = c.relnamespace WHERE n.nspname = '-DSL-' AND c.relname = 'command_log') THEN	
		CREATE TABLE "-DSL-".command_log (id SERIAL PRIMARY KEY, name VARCHAR NOT NULL, at TIMESTAMPTZ NOT NULL DEFAULT NOW(), command JSONB NOT NULL);
	END IF;
END \$\$ LANGUAGE plpgsql;

DO \$\$ BEGIN
	IF NOT EXISTS(SELECT * FROM pg_type t JOIN pg_namespace n ON n.oid = t.typnamespace WHERE n.nspname = '$name$' AND t.typname = '-ngs_Example_type-') THEN	
		CREATE TYPE "$name$"."-ngs_Example_type-" AS ();
		COMMENT ON TYPE "$name$"."-ngs_Example_type-" IS 'NGS generated';
	END IF;
END \$\$ LANGUAGE plpgsql;

DO \$\$ BEGIN
	IF NOT EXISTS(SELECT * FROM pg_class c JOIN pg_namespace n ON n.oid = c.relnamespace WHERE n.nspname = '$name$' AND c.relname = 'Example') THEN	
		CREATE TABLE "$name$"."Example" ();
		COMMENT ON TABLE "$name$"."Example" IS 'NGS generated';
	END IF;
END \$\$ LANGUAGE plpgsql;

DO \$\$ BEGIN
	IF NOT EXISTS(SELECT * FROM pg_class c JOIN pg_namespace n ON n.oid = c.relnamespace WHERE n.nspname = '$name$' AND c.relname = 'Example_sequence') THEN
		CREATE SEQUENCE "$name$"."Example_sequence";
		COMMENT ON SEQUENCE "$name$"."Example_sequence" IS 'NGS generated';
	END IF;
END \$\$ LANGUAGE plpgsql;

DO \$\$ BEGIN
	IF NOT EXISTS(SELECT * FROM "-DSL-".Load_Type_Info() WHERE type_schema = '$name$' AND type_name = 'Example' AND column_name = 'ID') THEN
		ALTER TABLE "$name$"."Example" ADD COLUMN "ID" INT NOT NULL DEFAULT 0;
		COMMENT ON COLUMN "$name$"."Example"."ID" IS 'NGS generated';
	END IF;
END \$\$ LANGUAGE plpgsql;

DO \$\$ BEGIN
	IF NOT EXISTS(SELECT * FROM "-DSL-".Load_Type_Info() WHERE type_schema = '$name$' AND type_name = '-ngs_Example_type-' AND column_name = 'ID') THEN
		ALTER TYPE "$name$"."-ngs_Example_type-" ADD ATTRIBUTE "ID" INT;
		COMMENT ON COLUMN "$name$"."-ngs_Example_type-"."ID" IS 'NGS generated';
	END IF;
END \$\$ LANGUAGE plpgsql;

CREATE OR REPLACE VIEW "$name$"."Example_entity" AS
SELECT _entity."ID" AS "ID"
FROM
	"$name$"."Example" _entity
	;
COMMENT ON VIEW "$name$"."Example_entity" IS 'NGS volatile';

CREATE OR REPLACE FUNCTION "URI"("$name$"."Example_entity") RETURNS TEXT AS \$\$
SELECT CAST(\$1."ID" as TEXT)
\$\$ LANGUAGE SQL IMMUTABLE;

CREATE OR REPLACE FUNCTION "$name$"."cast_Example_to_type"("$name$"."-ngs_Example_type-") RETURNS "$name$"."Example_entity" AS \$\$ SELECT \$1::text::"$name$"."Example_entity" \$\$ IMMUTABLE LANGUAGE sql;
CREATE OR REPLACE FUNCTION "$name$"."cast_Example_to_type"("$name$"."Example_entity") RETURNS "$name$"."-ngs_Example_type-" AS \$\$ SELECT \$1::text::"$name$"."-ngs_Example_type-" \$\$ IMMUTABLE LANGUAGE sql;

DO \$\$ BEGIN
	IF NOT EXISTS(SELECT * FROM pg_cast c JOIN pg_type s ON c.castsource = s.oid JOIN pg_type t ON c.casttarget = t.oid JOIN pg_namespace n ON n.oid = s.typnamespace AND n.oid = t.typnamespace
					WHERE n.nspname = '$name$' AND s.typname = 'Example_entity' AND t.typname = '-ngs_Example_type-') THEN
		CREATE CAST ("$name$"."-ngs_Example_type-" AS "$name$"."Example_entity") WITH FUNCTION "$name$"."cast_Example_to_type"("$name$"."-ngs_Example_type-") AS IMPLICIT;
		CREATE CAST ("$name$"."Example_entity" AS "$name$"."-ngs_Example_type-") WITH FUNCTION "$name$"."cast_Example_to_type"("$name$"."Example_entity") AS IMPLICIT;
	END IF;
END \$\$ LANGUAGE plpgsql;

CREATE OR REPLACE VIEW "$name$"."Example_unprocessed_events" AS
SELECT _aggregate."ID"
FROM
	"$name$"."Example_entity" _aggregate
;
COMMENT ON VIEW "$name$"."Example_unprocessed_events" IS 'NGS volatile';

CREATE OR REPLACE FUNCTION "$name$"."insert_Example"(IN _inserted "$name$"."Example_entity"[]) RETURNS VOID AS
\$\$
BEGIN
	
	INSERT INTO "$name$"."Example" ("ID") VALUES(_inserted[1]."ID");
	
	PERFORM pg_notify('aggregate_roots', '$name$.Example:Insert:' || array["URI"(_inserted[1])]::TEXT);
END
\$\$
LANGUAGE plpgsql SECURITY DEFINER;;

CREATE OR REPLACE FUNCTION "$name$"."persist_Example"(
IN _inserted "$name$"."Example_entity"[], IN _updated_original "$name$"."Example_entity"[], IN _updated_new "$name$"."Example_entity"[], IN _deleted "$name$"."Example_entity"[]) 
	RETURNS VARCHAR AS
\$\$
DECLARE cnt int;
DECLARE uri VARCHAR;
DECLARE tmp record;
DECLARE msg TEXT;
DECLARE _update_count int = array_upper(_updated_original, 1);
DECLARE _delete_count int = array_upper(_deleted, 1);

BEGIN

	

	SET CONSTRAINTS ALL DEFERRED;

	

	INSERT INTO "$name$"."Example" ("ID")
	SELECT _i."ID" 
	FROM unnest(_inserted) _i;

	

	IF _update_count > 0 THEN
		UPDATE "$name$"."Example" AS _tbl SET "ID" = (_u.changed)."ID"
		FROM (SELECT unnest(_updated_original) as original, unnest(_updated_new) as changed) _u
		WHERE _tbl."ID" = (_u.original)."ID";

		GET DIAGNOSTICS cnt = ROW_COUNT;
		IF cnt != _update_count THEN 
			RETURN 'Updated ' || cnt || ' row(s). Expected to update ' || _update_count || ' row(s).';
		END IF;
	END IF;

	

	IF _delete_count > 0 THEN
		DELETE FROM "$name$"."Example" AS _tbl
		WHERE (_tbl."ID") IN (SELECT _d."ID" FROM unnest(_deleted) _d);

		GET DIAGNOSTICS cnt = ROW_COUNT;
		IF cnt != _delete_count THEN 
			RETURN 'Deleted ' || cnt || ' row(s). Expected to delete ' || _delete_count || ' row(s).';
		END IF;
	END IF;

	
	PERFORM "-DSL-".Safe_Notify('aggregate_roots', '$name$.Example', 'Insert', (SELECT array_agg(_i."URI") FROM unnest(_inserted) _i));
	PERFORM "-DSL-".Safe_Notify('aggregate_roots', '$name$.Example', 'Update', (SELECT array_agg(_u."URI") FROM unnest(_updated_original) _u));
	PERFORM "-DSL-".Safe_Notify('aggregate_roots', '$name$.Example', 'Change', (SELECT array_agg((_u.changed)."URI") FROM (SELECT unnest(_updated_original) as original, unnest(_updated_new) as changed) _u WHERE (_u.changed)."ID" != (_u.original)."ID"));
	PERFORM "-DSL-".Safe_Notify('aggregate_roots', '$name$.Example', 'Delete', (SELECT array_agg(_d."URI") FROM unnest(_deleted) _d));

	SET CONSTRAINTS ALL IMMEDIATE;

	RETURN NULL;
END
\$\$
LANGUAGE plpgsql SECURITY DEFINER;

CREATE OR REPLACE FUNCTION "$name$"."update_Example"(IN _original "$name$"."Example_entity"[], IN _updated "$name$"."Example_entity"[]) RETURNS VARCHAR AS
\$\$
DECLARE cnt int;
BEGIN
	
	UPDATE "$name$"."Example" AS _tab SET "ID" = _updated[1]."ID" WHERE _tab."ID" = _original[1]."ID";
	GET DIAGNOSTICS cnt = ROW_COUNT;
	
	PERFORM pg_notify('aggregate_roots', '$name$.Example:Update:' || array["URI"(_original[1])]::TEXT);
	IF (_original[1]."ID" != _updated[1]."ID") THEN
		PERFORM pg_notify('aggregate_roots', '$name$.Example:Change:' || array["URI"(_updated[1])]::TEXT);
	END IF;
	RETURN CASE WHEN cnt = 0 THEN 'No rows updated' ELSE NULL END;
END
\$\$
LANGUAGE plpgsql SECURITY DEFINER;;

SELECT "-DSL-".Create_Type_Cast('"$name$"."cast_Example_to_type"("$name$"."-ngs_Example_type-")', '$name$', '-ngs_Example_type-', 'Example_entity');
SELECT "-DSL-".Create_Type_Cast('"$name$"."cast_Example_to_type"("$name$"."Example_entity")', '$name$', 'Example_entity', '-ngs_Example_type-');

CREATE OR REPLACE FUNCTION "-DSL-".submit_commands(IN _commands JSONB[], IN _name VARCHAR, IN _at TIMESTAMPTZ = CURRENT_TIMESTAMP, OUT id VARCHAR) 
	RETURNS SETOF VARCHAR AS
\$\$
		INSERT INTO "-DSL-".command_log (name, at, command)
		SELECT _name, _at, c
		FROM unnest(_commands) c
		RETURNING id::text
\$\$
LANGUAGE SQL SECURITY DEFINER;

DO \$\$ 
DECLARE _pk VARCHAR;
BEGIN
	IF EXISTS(SELECT * FROM pg_index i JOIN pg_class c ON i.indrelid = c.oid JOIN pg_namespace n ON c.relnamespace = n.oid WHERE i.indisprimary AND n.nspname = '$name$' AND c.relname = 'Example') THEN
		SELECT array_to_string(array_agg(sq.attname), ', ') INTO _pk
		FROM
		(
			SELECT atr.attname
			FROM pg_index i
			JOIN pg_class c ON i.indrelid = c.oid 
			JOIN pg_attribute atr ON atr.attrelid = c.oid 
			WHERE 
				c.oid = '"$name$"."Example"'::regclass
				AND atr.attnum = any(i.indkey)
				AND indisprimary
			ORDER BY (SELECT i FROM generate_subscripts(i.indkey,1) g(i) WHERE i.indkey[i] = atr.attnum LIMIT 1)
		) sq;
		IF ('ID' != _pk) THEN
			RAISE EXCEPTION 'Different primary key defined for table $name$.Example. Expected primary key: ID. Found: %', _pk;
		END IF;
	ELSE
		ALTER TABLE "$name$"."Example" ADD CONSTRAINT "pk_Example" PRIMARY KEY("ID");
		COMMENT ON CONSTRAINT "pk_Example" ON "$name$"."Example" IS 'NGS generated';
	END IF;
END \$\$ LANGUAGE plpgsql;

DO \$\$ 
BEGIN
	IF NOT EXISTS(SELECT * FROM pg_class c JOIN pg_namespace n ON c.relnamespace = n.oid WHERE n.nspname = '$name$' AND c.relname = 'Example_ID_seq' AND c.relkind = 'S') THEN
		CREATE SEQUENCE "$name$"."Example_ID_seq";
		ALTER TABLE "$name$"."Example"	ALTER COLUMN "ID" SET DEFAULT NEXTVAL('"$name$"."Example_ID_seq"');
		PERFORM SETVAL('"$name$"."Example_ID_seq"', COALESCE(MAX("ID"), 0) + 1000) FROM "$name$"."Example";
	END IF;
END \$\$ LANGUAGE plpgsql;

SELECT "-DSL-".Persist_Concepts('"dsl/$name$.dsl"=>"module $name$ {

  aggregate Example;
  
}"', '\x','2.6.7577.26048');
SELECT pg_notify('migration', 'new');