// Copyright (c) 1999  Dustin Sallings <dustin@spy.net>
//
// $Id: TestTest.java,v 1.1 1999/11/19 07:29:23 dustin Exp $

package net.spy.test;

import java.sql.*;
import java.util.*;
import net.spy.*;

// This class implements the actual test that will be taken by people
public class TestTest {
	protected TestQuestion questions[];
	protected int test_id = -1;
	protected int num_questions = -1;
	protected int current_question_i=0;
	protected TestQuestion current_question=null;

	public TestTest(int test_id, int num_questions) throws Exception {
		this.test_id=test_id;
		this.num_questions=num_questions;

		try {
			SpyDB db = new SpyDB(new TestConfig(), true);
			Connection tdb = db.getConn();
			Statement st = tdb.createStatement();
			int i;

			Vector v = new Vector();

			String query = "select question_id from question where test_id = "
				+ test_id;
			ResultSet rs = st.executeQuery(query);
			while(rs.next()) {
				v.addElement( new Integer(rs.getInt("question_id")) );
			}

			Integer q_list[] = new Integer[v.size()];

			for(i=0; i<v.size(); i++) {
				q_list[i]=(Integer)v.elementAt(i);
			}

			q_list = (Integer [])SpyUtil.shuffle(q_list);

			// Different behavior depending on whether we have enough
			// questions.
			if(q_list.length > num_questions) {
				questions = new TestQuestion[num_questions];
			} else {
				questions = new TestQuestion[q_list.length];
			}

			for(i=0; i<questions.length; i++) {
				questions[i]=new TestQuestion( q_list[i].intValue() );
			}
		} catch(Exception e) {
			throw new Exception("Error generating test:  " + e);
		}
	}

	// Set the current question
	public void setCurrent(int c) {
		current_question_i=c;
	}

	public boolean nextQuestion() {
		boolean ret=false;
		try {
			current_question=questions[current_question_i];
			ret=true;
		} catch(Exception e) {
			// Don't care, return false
		}
		current_question_i++;
		return(ret);
	}

	public TestQuestion getQuestion() {
		return(current_question);
	}

	public TestQuestion getQuestion(int i) {
		return(questions[i]);
	}

	public void dump() {
		int i;
		for(i=0; i<questions.length; i++) {
			questions[i].dump();
		}
	}
}
