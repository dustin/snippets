-- Add PL/pgsql language to a database

CREATE FUNCTION plpgsql_call_handler () RETURNS OPAQUE AS
	'/usr/local/pgsql/lib/plpgsql.so' LANGUAGE 'C';

CREATE TRUSTED PROCEDURAL LANGUAGE 'plpgsql'
	HANDLER plpgsql_call_handler
	LANCOMPILER 'PL/pgSQL';
