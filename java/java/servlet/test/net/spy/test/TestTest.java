// Copyright (c) 1999  Dustin Sallings <dustin@spy.net>
//
// $Id: TestTest.java,v 1.2 2001/01/27 09:14:10 dustin Exp $

package net.spy.test;

import java.sql.*;
import java.util.*;
import net.spy.*;

/**
 * The actual test to be taken.
 */
public class TestTest {

	private TestQuestion questions[];
	private int test_id = -1;
	private int num_questions = -1;
	private int current_question_i=0;
	private TestQuestion current_question=null;

	public TestTest(int test_id, int num_questions) throws Exception {
		this.test_id=test_id;
		this.num_questions=num_questions;

		try {
			SpyDB db = new SpyDB(new TestConfig());

			PreparedStatement pst=db.prepareStatement(
				"select question_id from test_questions where test_id = ?");

			pst.setInt(1, test_id);

			ResultSet rs = pst.executeQuery();

			// Get the questions
			Vector v = new Vector();
			while(rs.next()) {
				v.addElement( new Integer(rs.getInt("question_id")) );
			}
			// We're done with the DB for now.
			rs.close();
			db.close();

			// Prepare to shuffle the questions
			Integer q_list[] = new Integer[v.size()];
			for(int i=0; i<v.size(); i++) {
				q_list[i]=(Integer)v.elementAt(i);
			}

			// Shuffle them
			q_list = (Integer [])SpyUtil.shuffle(q_list);

			// Different behavior depending on whether we have enough
			// questions.
			if(q_list.length > num_questions) {
				questions = new TestQuestion[num_questions];
			} else {
				questions = new TestQuestion[q_list.length];
			}

			// OK, populate our questions.
			for(int i=0; i<questions.length; i++) {
				questions[i]=new TestQuestion( q_list[i].intValue() );
			}

		} catch(Exception e) {
			e.printStackTrace();
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

	public String toString() {
		String ret="";

		for(int i=0; i<questions.length; i++) {
			ret+=questions[i];
		}

		return(ret);
	}
}
