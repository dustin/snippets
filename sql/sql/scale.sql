-- SQL stuff for scaling across multiple database servers...

create sequence db_host_seq;

create table db_host(
	host_key integer default nextval('db_host_seq'),
	host_name text,
	isme bool
);

create function hostname() returns text as
	'select host_name from db_host where isme=\'t\''
	language 'sql';

create function unique_id() returns text as
	'select ((hostname()) || (''.''::text))
	          || (nextval(''unique_id'')::text)'
	language 'sql';
