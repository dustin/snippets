-- Test schema

drop table testtable;
drop sequence testtable_id_seq;

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
	values('full1', '7/29/1998', '23:02', now(), '123456.09', 1,
	1.1, true);

insert into testtable(test_vc, test_d, test_t, test_ts,
	test_n, test_i, test_f, test_b)
	values('full2', '7/29/1998', '23:02', now(), '123456.09', 1,
	1.1, true);

insert into testtable(test_vc, test_t, test_ts,
	test_n, test_i, test_f, test_b)
	values('nulldate', '23:02', now(), '123456.09', 1,
	1.1, true);

insert into testtable(test_vc, test_d, test_t, test_ts,
	test_n, test_i, test_f)
	values('nullbool', '7/29/1998', '23:02', now(), '1234567.09', 1,
	1.1);

insert into testtable(test_vc, test_d, test_t, test_ts,
	test_n, test_b)
	values('nullnums', '7/29/1998', '23:02', now(), '1234567.09', true);

drop table primary_keys;

create table primary_keys(
	table_name varchar(128) not null,
	primary_key numeric(16) not null default 1,
	incr integer not null default 10,
	primary key(table_name)
);

insert into primary_keys(table_name) values('test_table');
insert into primary_keys(table_name, incr) values('test_table2', 1000);
