// Copyright (c) 1999  Dustin Sallings <dustin@spy.net>
//
// $Id: TestTest.java,v 1.4 2001/01/28 08:16:06 dustin Exp $

package net.spy.test;

import java.sql.*;
import java.util.*;
import java.io.Serializable;
import net.spy.*;

/**
 * The actual test to be taken.
 */
public class TestTest extends Object implements Serializable{

	private Vector questions=null;
	private int test_id = -1;
	private int num_questions = 20;
	private int current_question_i=0;
	private TestQuestion current_question=null;

	public TestTest() throws Exception {
		super();
	}

	public TestTest(int test_id) throws Exception {
		super();
		this.test_id=test_id;
	}

	public TestTest(int test_id, int num_questions) throws Exception {
		super();
		this.test_id=test_id;
		this.num_questions=num_questions;
	}

	public void setTestid(String test_ids) {
		this.test_id=Integer.parseInt(test_ids);
		reset();
	}

	public void reset() {
		questions=null;
		current_question_i=0;
		current_question=null;
	}

	protected void initTest() throws Exception {

		// If'n we already have questions, don't init again
		if(questions!=null) {
			return;
		}

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

			int nquestions=num_questions;

			// Different behavior depending on whether we have enough
			// questions.
			if(q_list.length < num_questions) {
				nquestions = q_list.length;
			}

			questions=new Vector();
			// OK, populate our questions.
			for(int i=0; i<nquestions; i++) {
				questions.addElement(new TestQuestion( q_list[i].intValue()));
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

	public boolean nextQuestion() throws Exception {
		boolean ret=false;
		initTest();
		try {
			current_question=
				(TestQuestion)questions.elementAt(current_question_i);
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

	public TestQuestion getQuestion(int i) throws Exception {
		initTest();
		return((TestQuestion)questions.elementAt(i));
	}

	public Enumeration getQuestions() throws Exception {
		initTest();
		return(questions.elements());
	}

	public int getNumQuestions() throws Exception {
		initTest();
		return(questions.size());
	}

	public String toString() {
		try {
			initTest();
		} catch(Exception e) {
			e.printStackTrace();
		}
		StringBuffer sb=new StringBuffer();

		for(Enumeration e=questions.elements(); e.hasMoreElements(); ) {
			sb.append(e.nextElement());
			sb.append("\n\n");
		}

		return(sb.toString().trim());
	}
}
