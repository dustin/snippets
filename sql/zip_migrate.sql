insert into counties (state_id, name)
select
	distinct
		states.state_id,
		zips.county
from
	states, zips
where
	states.code=zips.state
order by
	state_id, county
;

insert into cities (county_id, name)
select
	distinct
		counties.county_id,
		zips.city
from
	counties, states, zips
where
	states.code=zips.state
	and counties.name=zips.county
	and counties.state_id=states.state_id
order by
	counties.county_id, zips.city
;

insert into zipcodes (zipcode, city_id, longitude, latitude)
select
	zips.zipcode,
	cities.city_id,
	zips.longitude,
	zips.latitude
from
	cities, counties, states, zips
where
	states.code=zips.state
	and counties.name=zips.county
	and cities.name=zips.city
	and counties.state_id=states.state_id
	and cities.county_id=counties.county_id
order by
	zips.zipcode, cities.city_id
;
