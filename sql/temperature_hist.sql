-- Histograms for the temperature stuff

-- Breakdown by hour for the last three months (avg temperature)
select date_part('hour', ts) as hour, avg(sample) as temp
	from samples
	where ts between now()-'3 months'::timespan and now()
	and sensor_id = 5 -- back yard
	group by hour
	order by hour


-- Histogram of temperatures in the last three months, rounded to nearest ten
select round(sample, -1) as sample_10, count(*) as count
	from samples
	where ts between now()-'3 months'::timespan and now()
	and sensor_id = 5 -- Back yard
	group by sample_10
	order by count
