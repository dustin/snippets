-- Copyright (c) 1999  Dustin Sallings <dustin@spy.net>
--
-- $Id: test.sql,v 1.2 2001/01/27 09:14:02 dustin Exp $

-- drops go here

drop table test_tests;
drop sequence test_tests_test_id_seq;
drop table test_questions;
drop sequence test_questions_question_id_seq;
drop table test_answers;
drop sequence test_answers_answer_id_seq;

-- creates go here

create table test_tests (
	test_id serial,
	test_name text not null,
	ts timestamp default now()
);

grant all on test_tests to nobody;
grant all on test_tests_test_id_seq to nobody;

create table test_questions (
	question_id serial,
	test_id integer not null,
	question text not null,
	shuffle_answers boolean default true,
	ts timestamp default now()
);

grant all on test_questions to nobody;
grant all on test_questions_question_id_seq to nobody;
create index test_questions_bytest on test_questions(test_id);

create table test_answers (
	answer_id serial,
	question_id integer not null,
	answer text not null,
	correct boolean not null,
	ts timestamp default now()
);

grant all on test_answers to nobody;
grant all on test_answers_answer_id_seq to nobody;
create index test_answers_byquestion on test_answers(question_id);
