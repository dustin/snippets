-- Test schema

create table testtable (
	id serial,
	test_vc varchar(32),
	test_d date,
	test_t time,
	test_ts timestamp,
	test_n numeric(9,2),
	test_i integer,
	test_f float,
	test_b boolean
);

-- Test data
insert into testtable(test_vc, test_d, test_t, test_ts,
	test_n, test_i, test_f, test_b)
	values('full1', date(now()), time(now()), now(), '1234567.09', 1,
	1.1, true);

insert into testtable(test_vc, test_d, test_t, test_ts,
	test_n, test_i, test_f, test_b)
	values('full2', date(now()), time(now()), now(), '1234567.09', 1,
	1.1, true);

insert into testtable(test_vc, test_t, test_ts,
	test_n, test_i, test_f, test_b)
	values('nulldate', time(now()), now(), '1234567.09', 1,
	1.1, true);

insert into testtable(test_vc, test_d, test_t, test_ts,
	test_n, test_i, test_f)
	values('nullbool', date(now()), time(now()), now(), '1234567.09', 1,
	1.1);

insert into testtable(test_vc, test_d, test_t, test_ts,
	test_n, test_b)
	values('nullnums', date(now()), time(now()), now(), '1234567.09', true);

