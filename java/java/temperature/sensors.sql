-- Copyright (c) 1999  Dustin Sallings <dustin@spy.net>
--
-- $Id: sensors.sql,v 1.1 2000/01/26 20:17:28 dustin Exp $

-- Different types of sensors
create table sensor_type (
	sensor_type_id serial,
	sensor_type text not null,
	units varchar(10)
);

create table sensor (
	sensor_id serial,
	serial char(16) not null,
	name text not null
);
create unique index sensor_byserial on sensor(serial);

create table samples (
	ts datetime,
	sensor_id integer not null,
	sample float not null
);
create index samples_bytime on samples(ts);
create index samples_byid on samples(sensor_id);
