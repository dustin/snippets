-- Copyright (c) 2000  SPY Internetworking
--
-- $Id: ups.sql,v 1.2 2000/10/10 09:02:46 dustin Exp $

-- Packages to watch, and whom to report to
create table ups_packages (
	tracking_number varchar(40),
	-- Who cares?
	pager_id varchar(16),
	-- Description of the package.
	descr text
);

grant all on ups_packages to nobody;
create index ups_packages_bytracking on ups_packages(tracking_number);
create unique index ups_packages_bytwo on
	ups_packages(tracking_number, pager_id);

-- Current status
create table ups_status (
	tracking_number varchar(40),
	-- Package status
	status varchar(40),
	-- Full description
	status_str text
);

grant all on ups_status to nobody;
create unique index ups_status_bytracking on ups_status(tracking_number);
