-- Report stuff
--
-- $Id: report.sql,v 1.1 2003/04/29 07:19:30 dustin Exp $

create view temps_per_month as
	select
			date(date_trunc('month', ts)) as month,
			avg(sample) as temp,
			samples.sensor_id, name
		from
			samples join sensors using (sensor_id)
		group by
			month, samples.sensor_id, name
		order by
			month
;
