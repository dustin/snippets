// Copyright (c) 1999  Dustin Sallings <dustin@spy.net>
//
// $Id: TestQuestion.java,v 1.4 2001/01/27 23:24:56 dustin Exp $

package net.spy.test;

import java.sql.*;
import java.util.*;
import net.spy.*;

// This class implements the actual test that will be taken by people
public class TestQuestion {
	protected String question = null;
	protected Vector answers = null;

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

			answers=new Vector();

			PreparedStatement pst2=db.prepareStatement(
				"select * from test_answers where question_id = ?\n"
					+ " order by answer_id");

			pst2.setInt(1, question_id);

			rs=pst2.executeQuery();

			while(rs.next()) {
				answers.addElement(
					new TestAnswer(
						rs.getString("answer"),
						rs.getBoolean("correct")));
			}
			// Done with the DB stuff
			rs.close();
			db.close();

			// If we're supposed to shuffle this, do so.
			if(shuffle) {
				// Make an array out of it
				TestAnswer answers_a[]=new TestAnswer[answers.size()];
				answers.copyInto(answers_a);
				answers_a=(TestAnswer [])SpyUtil.shuffle(answers_a);
				answers=new Vector();
				for(int i=0; i<answers_a.length; i++) {
					answers.addElement(answers_a[i]);
				}
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
		return((TestAnswer)answers.elementAt(which));
	}

	public TestAnswer getAnswer() {
		TestAnswer ret=null;
		for(Enumeration e=answers.elements();
			ret==null && e.hasMoreElements(); ) {

			TestAnswer ta=(TestAnswer)e.nextElement();

			if(ta.isCorrect()) {
				ret=ta;
			}
		}
		return(ret);
	}

	public String toString() {
		StringBuffer sb=new StringBuffer(question);

		sb.append("\n");

		for(Enumeration e=answers.elements(); e.hasMoreElements(); ) {
			TestAnswer ta=(TestAnswer)e.nextElement();
			sb.append("\t");
			sb.append(ta.isCorrect());
			sb.append(":  ");
			sb.append(ta.getAnswer());
			sb.append("\n");
		}

		return(sb.toString().trim());
	}
}
