-- Copyright (c) 1997  Dustin Sallings
--
-- $Id: doit.sql,v 1.3 1999/09/27 21:14:40 dustin Exp $
--
-- SQL routines for radius log management.
--

--
-- Create the table we're going to load the radius data into.
--

-- begin transaction;

create table radius (
          stop datetime not null,
          username char(16),
          sessid char(16),
          sesstime int,
          inoct int,
          outoct int,
		  status_type char(8),
          addr char(16),
		  called_station char(16),
		  calling_station char(16),
		  authenticator char(16),
		  terminate_cause char(16)
      );


--
-- Load up the tab delimited radius file from my parser.
--

copy radius from '/tmp/input';

--
-- Clean up
--

delete from radius where username='';
delete from radius where sesstime=0;

-- make sure all of the usernames are the same case
update radius set username = lower(username);
-- delete from radius where username ~~ '[%]'::text;

--
-- I got this function from the keen list.  :)  It makes a conversion
-- from int4 to timespan.
--

-- create function reltime_timespan(int4) returns timespan as '-'
--     language 'internal';

--
-- Make room for the start times.
--

alter table radius add column start datetime;

--
-- Set all of the start times.
--

-- update radius set start=(stop-reltime_timespan(sesstime));
update radius set start=(stop-timespan(sesstime));

--
-- Create an index on the username so we won't have to scan the *whole*
-- thing every time we want a little data.
--

create index rad_user on radius (username);
create index rad_start on radius using btree (start);
create index rad_stop on radius using btree (stop);

--
-- Go ahead and vacuum, could probably use some optimizations.
-- (this will take a while if there's a lot of data)
--

vacuum verbose analyze;

--
-- Create a table for general statistics on users.
--

select distinct username into table tmp1 from radius;

--
-- Add the columns to hold the statistics.
--

alter table tmp1 add column times int;
alter table tmp1 add column totaltime timespan;
alter table tmp1 add column avgtime timespan;
alter table tmp1 add column avgbpsin int;
alter table tmp1 add column avgbpsout int;

-- find out how many times they dialed up

create function numtimes(text) returns int4 as
	'select count(*) from radius where username= $1'
	language 'sql';

--
-- This functions returns the total time the user was online for all of
-- the data in the radius log.  It's not useful in the real world without
-- taking a date range, but it was fun to play with.  :)
--

create function ttime(text) returns timespan as
       'select timespan(sum(sesstime)) from radius where username= $1'
       language 'sql';

--
-- This is their average session time.
--

create function atime(text) returns timespan as
       'select timespan(avg(sesstime)) from radius where username= $1'
       language 'sql';

--
-- This is the average bits in per second.
--

create function avgoctin(text) returns int4 as
    'select sum(inoct)/sum(sesstime) from radius where username= $1'
    language 'sql';

--
-- This is the average bits out per second.
--

create function avgoctout(text) returns int4 as
    'select sum(outoct)/sum(sesstime) from radius where username= $1'
    language 'sql';

--
-- This sets the totals and averages for each user in the tmp1 table.
--

update tmp1 set times=numtimes(username), totaltime=ttime(username),
				avgtime=atime(username), avgbpsin=avgoctin(username),
				avgbpsout=avgoctout(username);

--
-- Go ahead and slap an index on it for safe keeping.
--

create unique index tmp1_un on tmp1 (username);

--
-- Show The username, how long (i.e. 1 hour 43 mins 41 secs),
-- the user was on, and the start and stop times.
--

-- select username, reltime_timespan(sesstime) as sesstime, start,
--     stop from radius order by start;

--
-- The snapshotter.  :)
--

create function snapshot(datetime) returns setof radius as
    'select * from radius where start <= $1 and stop >= $1'
    language 'sql';

--
-- This shows you everyone who was logged in at *exactly* 13:51:47 on
-- 3-26-1996.
--

-- select username(snapshot('3-26-1996 13:51:47')) as username,
--     start(snapshot('3-26-1996 13:51:47')) as start,
--     stop(snapshot('3-26-1996 13:51:47')) as stop
--     order by username;

--
-- Find users with multiple sessions.
--

create view duplicates as
     select a.username as username,
     a.start as astart,
     a.stop as astop,
     b.start as bstart,
     b.stop as bstop
     from radius a, radius b
     where a.username=b.username and
     a.start>b.start and a.start<=b.stop;

-- commit;
