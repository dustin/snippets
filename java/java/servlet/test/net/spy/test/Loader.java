// Copyright (c) 2001  Dustin Sallings <dustin@spy.net>
//
//$Id: Loader.java,v 1.1 2001/01/27 09:14:04 dustin Exp $

package net.spy.test;

import java.io.*;
import java.util.*;
import java.sql.*;

import net.spy.*;
import net.spy.test.*;

/**
 * Load questions from stdin.
 *
 * The format of the input stream is as follows:
 *
 * <pre>
 * Question
 * --
 * answer1
 * answer2
 * [...]
 * answer<i>n</i>
 * --
 * Number of the correct answer (one based)
 * -----------
 * </pre>
 */
public class Loader extends Object {

	public static SpyDB db=null;

	public static void store(String question, Vector answers, int correct)
		throws Exception {

		PreparedStatement pst=db.prepareStatement(
			"insert into test_questions(test_id, question) values(?,?)");
		pst.setInt(1, 1);
		pst.setString(2, question);
		System.out.println("Inserting the following:\n" + pst);
		pst.executeUpdate();
		pst.close();
		ResultSet rs=db.executeQuery(
			"select currval('test_questions_question_id_seq')");
		rs.next();
		int id=rs.getInt(1);
		rs.close();
		System.out.println("ID is " + id);

		PreparedStatement pst2=db.prepareStatement(
			"insert into test_answers(question_id, answer, correct)\n"
				+ " values(?, ?, ?)");

		for(int i=0; i<answers.size(); i++) {
			String answer=(String)answers.elementAt(i);
			pst2.setInt(1, id);
			pst2.setString(2, answer);
			pst2.setBoolean(3, ( (i+1) == correct));
			pst2.executeUpdate();
		}
		pst2.close();
	}

	public static void main(String args[]) throws Exception {
		db=new SpyDB(new TestConfig());
		BufferedReader bin =
			new BufferedReader(new InputStreamReader(System.in));
		String in=null;

		String question="";
		Vector answers=new Vector();
		int stage=0;

		while( (in=bin.readLine()) != null) {

			switch(stage) {
				// Getting the question
				case 0:
					if(in.equals("--")) {
						stage=1;
					} else {
						question+=in.trim() + " ";
					}
					break;
				// Getting the answers
				case 1:
					if(in.equals("--")) {
						stage=2;

					} else {
						answers.addElement(in.trim());
					}
					break;
				case 2:
						int correct=Integer.parseInt(in);

						store(question, answers, correct);

						// Reset everything and skip the separator line
						question="";
						answers=new Vector();
						stage=0;
						bin.readLine();
					break;
			}

		} // while

	}

}
