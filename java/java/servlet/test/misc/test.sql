-- Copyright (c) 1999  Dustin Sallings <dustin@spy.net>
--
-- $Id: test.sql,v 1.1 2000/01/10 18:33:29 dustin Exp $

create table test (
	test_id serial,
	test_name text
);

grant all on test to nobody;

create table question (
	question_id serial,
	test_id integer,
	question text,
	answer_id integer
);

grant all on question to nobody;
create index question_bytest on question(test_id);

create table answer (
	answer_id serial,
	question_id integer,
	answer text
);

grant all on answer to nobody;
create index answer_byquestion on answer(question_id);
