-- Copyright (c) 2000  Dustin Sallings <dustin@spy.net>
--
-- $Id: ds.sql,v 1.3 2001/10/17 21:48:51 dustin Exp $

-- This is one big fat table where we post movies and have users pick them
-- up
create table show_distribution (
	show_id varchar(32) not null,
	submitted date default now(),
	submitted_to varchar(32) not null,
	length integer not null,
	completed datetime null
);

create index show_distbyuser on show_distribution(submitted_to);

grant all on show_distribution to nobody;

-- Distribution time report
create view show_dist_times as
	select show_id, submitted, submitted_to, completed,
		completed-submitted as time_to_delivery,
		now()-submitted as submitted_to_now
	from show_distribution;
