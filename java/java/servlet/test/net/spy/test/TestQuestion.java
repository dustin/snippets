// Copyright (c) 1999  Dustin Sallings <dustin@spy.net>
//
// $Id: TestQuestion.java,v 1.1 1999/11/19 07:29:21 dustin Exp $

package net.spy.test;

import java.sql.*;
import java.util.*;
import net.spy.*;

// This class implements the actual test that will be taken by people
public class TestQuestion {
	protected String question = null;
	protected TestAnswer answers[] = null;

	public TestQuestion(int question_id) throws Exception {
		int correct_answer;
		try {
			SpyDB db = new SpyDB(new TestConfig(), true);
			Connection tdb = db.getConn();
			Statement st = tdb.createStatement();
			int current=0;

			String query="select * from question where question_id = "
				+ question_id;
			ResultSet rs = st.executeQuery(query);
			rs.next();
			question=rs.getString("question");
			correct_answer = rs.getInt("answer_id");

			// We'll be doing four answers for now
			answers = new TestAnswer[4];
			query = "select * from answer where question_id = " +
				question_id;
			rs=st.executeQuery(query);
			while(rs.next()) {
				answers[current++]=new TestAnswer(rs.getString("answer"),
					rs.getInt("answer_id") == correct_answer);
			}
			answers=(TestAnswer [])SpyUtil.shuffle(answers);
		} catch(Exception e) {
			throw new Exception("Error instantiating test:  " + e);
		}
	}

	public String getQuestion() {
		return(question);
	}

	public TestAnswer getAnswer(int which) {
		return(answers[which]);
	}

	public void dump() {
		int i;
		System.out.println(question);
		for(i=0; i<4; i++) {
			System.out.println("\t" + answers[i].isCorrect() + ":  "
				+ answers[i].getAnswer());
		}
	}
}
