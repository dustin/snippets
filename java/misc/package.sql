-- Copyright (c) 2000  Dustin Sallings <dustin@spy.net>
-- $Id: package.sql,v 1.2 2001/11/06 01:38:43 dustin Exp $

create table package_carrier (
	carrier_id serial primary key,
	name varchar(40)
);

create table packages (
	tracking_number varchar(40),
	carrier_id integer,
	pager_id varchar(16),
	descr text,
	foreign key (carrier_id) references package_carrier(carrier_id)
);
create index packages_byid on packages(tracking_number, carrier_id);
create unique index packages_byidpager
	on packages(tracking_number, carrier_id, pager_id);

create table package_status (
	tracking_number varchar(40),
	carrier_id integer,
	status varchar(40),
	status_str text
);
create unique index package_status_byid
	on package_status(tracking_number, carrier_id);

-- Add some data
insert into package_carrier(name) values('UPS');
insert into package_carrier(name) values('FedEx');
