// Copyright (c) 1999  Dustin Sallings <dustin@spy.net>
//
// $Id: TestQuestion.java,v 1.3 2001/01/27 09:14:05 dustin Exp $

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
			SpyDB db = new SpyDB(new TestConfig());
			int current=0;

			PreparedStatement pst=db.prepareStatement(
				"select * from test_questions where question_id = ?");

			pst.setInt(1, question_id);

			ResultSet rs = pst.executeQuery();
			rs.next();

			this.question=rs.getString("question");
			boolean shuffle=rs.getBoolean("shuffle_answers");

			// Done with that result set
			rs.close();

			Vector answers_v=new Vector();

			PreparedStatement pst2=db.prepareStatement(
				"select * from test_answers where question_id = ?\n"
					+ " order by answer_id");

			pst2.setInt(1, question_id);

			rs=pst2.executeQuery();

			while(rs.next()) {
				answers_v.addElement(
					new TestAnswer(
						rs.getString("answer"),
						rs.getBoolean("correct")));
			}
			// Done with the DB stuff
			rs.close();
			db.close();

			// Make an array out of it
			answers=new TestAnswer[answers_v.size()];
			answers_v.copyInto(answers);

			// If we're supposed to shuffle this, do so.
			if(shuffle) {
				answers=(TestAnswer [])SpyUtil.shuffle(answers);
			}
		} catch(Exception e) {
			e.printStackTrace();
			throw new Exception("Error instantiating test:  " + e);
		}
	}

	public String getQuestion() {
		return(question);
	}

	public TestAnswer getAnswer(int which) {
		return(answers[which]);
	}

	public TestAnswer getAnswer() {
		TestAnswer ret=null;
		for(int i=0; i<4; i++) {
			if(answers[i].isCorrect()) {
				ret=answers[i];
			}
		}
		return(ret);
	}

	public String toString() {
		String ret=question + "\n";

		for(int i=0; i<answers.length; i++) {
			ret+="\t" + answers[i].isCorrect() + ":  "
				+ answers[i].getAnswer() + "\n";
		}

		return(ret);
	}
}
