-- Copyright (c) 1998  Dustin Sallings
--
-- $Id: photo.sql,v 1.5 1998/04/30 07:08:23 dustin Exp $
--
-- Use this to bootstrap your SQL database to do cool shite with the
-- photo album.

-- Where the picture info is stored.

create table album(
    fn       varchar(32),
    keywords varchar(50),
    descr    text,
    ts       timestamp default now(),
    cat      int,
    taken    date,
    size     int
);

create unique index album_byoid on album(oid);
create index album_bycat on album(cat);
grant all on album to nobody;

-- A sequence for generating categories

create sequence cat_seq;
grant all on cat_seq to nobody;

-- The categories

create table cat(
    id       int default nextval('cat_seq'),
    name     varchar
);

grant all on cat to nobody;

-- The ACLs for the categories

create table wwwacl(
    username varchar(16),
    cat      int
);

create index acl_byname on wwwacl(username);
grant all on wwwacl to nobody;

-- The group file for the Web server's ACL crap.

create table wwwgroup(
    username varchar(16),
    groupname varchar(16)
);

grant all on wwwgroup to nobody;

-- The passwd file for the Web server's ACL crap.

create table wwwusers(
    username varchar(16),
    password char(13),
    email    varchar,
    realname varchar,
    canadd   bool
);

create index user_byname on wwwusers(username);
grant all on wwwusers to nobody;

-- Blobs for images

create sequence img_seq;
grant all on img_seq to nobody;

create table images (
    id       int default nextval('img_seq'),
    loid     oid
);

create unique index img_byid on images(id);
grant all on images to nobody;

-- A SQL function to return the count of elements in a category.

create function catsum (int4 )
    returns int4 AS
	'select count(*) from album where cat = $1'
    language 'SQL';
