-- Copyright (c) 1999  Dustin Sallings <dustin@spy.net>
--
-- $Id: sensors.sql,v 1.3 2001/06/01 08:51:59 dustin Exp $

-- Different types of sensors
create table sensor_types (
	sensor_type_id serial,
	sensor_type text not null,
	units varchar(10) not null,
	primary key(sensor_type_id)
);

-- Here's a description of the sensors.
create table sensors (
	sensor_id serial,
	sensor_type_id integer not null,
	serial char(16) not null,
	name text not null,
	low smallint not null,
	high smallint not null,
	active boolean default true,
	primary key(sensor_id),
	foreign key(sensor_type_id) references sensor_types(sensor_type_id)
);
create unique index sensors_byserial on sensors(serial);
grant select on sensors to tempload;
grant select on sensors to nobody;

-- The actual samples go here.
create table samples (
	ts datetime not null,
	sensor_id integer not null,
	sample float not null,
	foreign key(sensor_id) references sensors(sensor_id)
);
create index samples_bytime on samples(ts);
create index samples_byid on samples(sensor_id);
create unique index samples_bytimeid on samples(ts, sensor_id);
grant insert on samples to tempload;
grant select on samples to nobody;
