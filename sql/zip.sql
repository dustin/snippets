-- Copyright (c) 2002  Dustin Sallings <dustin@spy.net>
--
-- $Id: zip.sql,v 1.1 2002/12/01 08:52:19 dustin Exp $

-- My zipcode stuff

create table zips.states (
	state_id serial,
	code char(2) not null,
	name varchar(32) not null,
	primary key(state_id)
);
create unique index states_bycode on zips.states(code);
create unique index states_byname on zips.states(name);

create table zips.counties (
	county_id serial,
	state_id integer not null,
	name varchar(40) not null,
	primary key(county_id),
	foreign key(state_id) references zips.states(state_id)
);
create index counties_bystate on zips.counties(state_id);
create index counties_byname on zips.counties(name);

create table zips.cities (
	city_id serial,
	county_id integer not null,
	name varchar(40) not null,
	primary key(city_id),
	foreign key(county_id) references zips.counties(county_id)
);
create index cities_bycounty on zips.cities(county_id);
create index cities_byname on zips.cities(name);

create table zips.zipcodes (
	zipcode integer not null,
	city_id integer not null,
	longitude float not null,
	latitude float not null,
	primary key(zipcode),
	foreign key(city_id) references zips.cities(city_id)
);
create index zipcodes_byzip on zips.zipcodes(zipcode);
create index zipcodes_bycity on zips.zipcodes(city_id);

-- Here's a view for the way things used to look.
create view zips.zipcode_view as
	select
		z.zipcode,
		s.code as state_code,
		s.name as state,
		ci.name as city,
		co.name as county,
		z.longitude,
		z.latitude
	from zips.zipcodes z join zips.cities ci using (city_id)
		join zips.counties co using (county_id)
		join zips.states s using (state_id)
	order by z.zipcode
;
