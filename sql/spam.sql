-- SPAM Database stuff
-- $Id: spam.sql,v 1.3 1997/10/12 09:38:25 dustin Exp $

create sequence sid_seq increment 1;

create table bill_info (
    spam_id int default nextval('sid_seq'),
    name varchar,
    addr1 varchar,
    addr2 varchar,
    city varchar,
    state char2,
    zip char16,
    country char2,
    phone varchar,
    fax varchar,
    email varchar
);

create unique index bill_id on bill_info (spam_id);

create table trans (
    spam_id int,
    amount money,
    descr text,
    ts timestamp default now()
);

create index trans_byspam on trans (spam_id);

create table messages (
    spam_id int,
    fn      varchar(32),
    subject text,
    md5sum  char(32),
    size    int,
    thedate datetime,
    ts timestamp default now()
);

create index mess_byspam on messages (spam_id);
create index mess_bymd5 on messages (md5sum);
