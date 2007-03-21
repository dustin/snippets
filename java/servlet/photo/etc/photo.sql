-- Copyright (c) 1998  Dustin Sallings
--
-- $Id: photo.sql,v 1.25 2000/01/01 03:35:45 dustin Exp $
--
-- Use this to bootstrap your SQL database to do cool shite with the
-- photo album.

begin transaction;

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
	addedby  integer,
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

-- The passwd file for the Web server's ACL crap.

create table wwwusers(
	id       serial,
	username varchar(16),
	password text,
	email    text,
	realname text,
	canadd   bool
);

create unique index user_byname on wwwusers(username);
grant all on wwwusers to nobody;
grant all on wwwusers_id_seq to nobody;

-- get a user ID from a username

create function getwwwuser(text) returns integer as
	'select id from wwwusers where username = $1'
	language 'sql';

-- The ACLs for the categories

create table wwwacl(
	userid   integer,
	cat      integer
);

create index acl_byid on wwwacl(userid);
grant all on wwwacl to nobody;

-- view for showing acls by name

create view show_acl as
	select wwwusers.username, wwwacl.cat, cat.name
	from wwwusers, wwwacl, cat
	where wwwusers.id=wwwacl.userid
	and wwwacl.cat=cat.id
;

grant all on show_acl to nobody;

-- The group file for the Web server's ACL crap.

create table wwwgroup(
	userid    integer,
	groupname varchar(16)
);

grant all on wwwgroup to nobody;

create view show_group as
	select wwwusers.username, wwwgroup.groupname
	from wwwusers, wwwgroup
	where wwwusers.id=wwwgroup.userid
;

grant all on show_group to nobody;

-- Search saves

create table searches (
	searches_id	serial,
	name		text,
	addedby		integer,
	search		text,
	ts			datetime default('now')
);

grant all on searches to nobody;
-- implicit seqeunce
grant all on searches_searches_id_seq to nobody;

-- Hmm...  Store images in text?  OK, sure...
-- This is keyed of the id in the album table

create table image_store (
	id   integer,
	line integer,
	data text
);

grant all on image_store to nobody;
create index images_id on image_store(id);

-- A SQL function to return the count of elements in a category.

create function catsum (integer)
	returns integer AS
	'select count(*) from album where cat = $1'
	language 'SQL';

-- User Agent table, for recording user-agents in logs.

create table user_agent (
	user_agent_id serial,
	user_agent text
);

grant all on user_agent to nobody;

create unique index user_agent_text on user_agent(user_agent);

create function get_agent(text) returns integer as
'
declare
	id integer;
begin
	select user_agent_id into id from user_agent where user_agent = $1;
	if not found then
		insert into user_agent(user_agent) values($1);
		select user_agent_id into id from user_agent where user_agent = $1;
	end if;
	return(id);
end;
' language 'plpgsql';

-- Log image retrievals.

create table photo_log (
	photo_id integer not null,
	wwwuser_id integer not null,
	remote_addr inet not null,
	server_host text not null,
	user_agent integer not null,
	cached boolean not null,
	ts datetime default (datetime(now()))
);

grant all on photo_log to nobody;

create index photo_log_photo_id on photo_log(photo_id);
create index photo_log_wwwuser_id on photo_log(wwwuser_id);
create index photo_log_remote_addr on photo_log(remote_addr);

-- Log of uploaded images.

create table upload_log (
	photo_id integer not null,
	wwwuser_id integer not null,
	stored datetime,
	ts datetime default(datetime(now()))
);

grant all on upload_log to nobody;
create unique index upload_log_photo on upload_log(photo_id);

create view log_user_ip_agent as
	select wwwusers.username, photo_log.remote_addr, user_agent.user_agent
		from wwwusers, photo_log, user_agent
	  where wwwusers.id = photo_log.wwwuser_id and
	  	user_agent.user_agent_id = photo_log.user_agent
;

grant all on log_user_ip_agent to nobody;

create view log_user_ip_keywords as
	select wwwusers.username, photo_log.remote_addr, album.keywords
		from wwwusers, photo_log, album
	  where wwwusers.id = photo_log.wwwuser_id and
	    album.id = photo_log.photo_id
;

grant all on log_user_ip_keywords to nobody;

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

-- MISC QUERIES

-- Garbage collector, unfortunately, this will not work in a view.

-- select distinct id from image_store where id not in
--	(select id from album);

commit;
