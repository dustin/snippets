-- Copyright (c) 2002  Dustin Sallings <dustin@spy.net>
--
-- $Id: zip.sql,v 1.1 2002/12/01 08:52:19 dustin Exp $

-- My zipcode stuff

create table geo.states (
	state_id serial,
	code char(2) not null,
	name varchar(32) not null,
	primary key(state_id)
);
create unique index states_bycode on geo.states(code);
create unique index states_byname on geo.states(name);

create table geo.counties (
	county_id serial,
	state_id integer not null,
	name varchar(40) not null,
	primary key(county_id),
	foreign key(state_id) references geo.states(state_id)
);
create index counties_bystate on geo.counties(state_id);
create index counties_byname on geo.counties(name);

create table geo.cities (
	city_id serial,
	county_id integer not null,
	name varchar(40) not null,
	primary key(city_id),
	foreign key(county_id) references geo.counties(county_id)
);
create index cities_bycounty on geo.cities(county_id);
create index cities_byname on geo.cities(name);

create table geo.zipcodes (
	zipcode integer not null,
	city_id integer not null,
	longitude float not null,
	latitude float not null,
	primary key(zipcode),
	foreign key(city_id) references geo.cities(city_id)
);
create index zipcodes_byzip on geo.zipcodes(zipcode);
create index zipcodes_bycity on geo.zipcodes(city_id);

-- Here's a view for the way things used to look.
create view geo.zipcode_view as
	select
		z.zipcode,
		s.code as state_code,
		s.name as state,
		ci.name as city,
		co.name as county,
		z.longitude,
		z.latitude
	from geo.zipcodes z join geo.cities ci using (city_id)
		join geo.counties co using (county_id)
		join geo.states s using (state_id)
	order by z.zipcode
;
