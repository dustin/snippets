-- Copyright (c) 1998  Dustin Sallings
--
-- $Id: photo.sql,v 1.13 1998/11/08 01:25:37 dustin Exp $
--
-- Use this to bootstrap your SQL database to do cool shite with the
-- photo album.

-- add support for PL/pgsql

CREATE FUNCTION plpgsql_call_handler () RETURNS OPAQUE AS
        '/usr/local/pgsql/lib/plpgsql.so' LANGUAGE 'C';

CREATE TRUSTED PROCEDURAL LANGUAGE 'plpgsql'
        HANDLER plpgsql_call_handler
        LANCOMPILER 'PL/pgSQL';

-- Where the picture info is stored.

create table album(
	keywords varchar(50),
	descr    text,
	cat      integer,
	taken    date,
	size     integer,
	addedby  varchar(16),
	ts       datetime default datetime(now()),
	id       serial
);

create index album_bycat on album(cat);
grant all on album to nobody;
-- implicit sequence
grant all on album_id_seq to nobody;

-- Get rid of all image_store data when an album entry is deleted:

create function tg_album_delete() returns opaque as
	'begin
		delete from image_store where id = OLD.id;
		return OLD;
	 end;'
	language 'plpgsql';

create trigger album_cleanup_tg before delete on album
	for each row execute procedure tg_album_delete();

-- The categories

create table cat(
	id   serial,
	name text
);

grant all on cat to nobody;
-- implicit sequence
grant all on cat_id_seq to nobody;

-- The ACLs for the categories

create table wwwacl(
	username varchar(16),
	cat      integer
);

create index acl_byname on wwwacl(username);
grant all on wwwacl to nobody;

-- The group file for the Web server's ACL crap.

create table wwwgroup(
	username  varchar(16),
	groupname varchar(16)
);

grant all on wwwgroup to nobody;

-- The passwd file for the Web server's ACL crap.

create table wwwusers(
	username varchar(16),
	password char(13),
	email    text,
	realname text,
	canadd   bool
);

create unique index user_byname on wwwusers(username);
grant all on wwwusers to nobody;

-- Search saves

create table searches (
	id      serial,
	name    text,
	addedby text,
	search  text,
	ts      datetime default('now')
);

grant all on searches to nobody;
-- implicit seqeunce
grant all on searches_id_seq to nobody;

-- Hmm...  Store images in text?  OK, sure...
-- This is keyed of the id in the album table

create table image_store (
	id   integer,
	line integer,
	data varchar(76)
);

grant all on image_store to nobody;
create index images_id on image_store(id);

-- A SQL function to return the count of elements in a category.

create function catsum (integer)
	returns integer AS
	'select count(*) from album where cat = $1'
	language 'SQL';

-- Quick lookup function for moving from a seperate map to a single image
-- source.  This will go away as soon as that map does.

-- create function getimageid(varchar) returns integer as
--	 'select id from image_map where name = $1'
--	 language 'sql';

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

-- MISC QUERIES

-- Garbage collector, unfortunately, this will not work in a view.

-- select distinct id from image_store where id not in
--	(select id from image_map);
