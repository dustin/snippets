/*
 * Copyright (c) 1999 Dustin Sallings
 *
 * $Id: TestSession.java,v 1.5 2001/01/27 09:14:06 dustin Exp $
 */

package net.spy.test;

import java.io.*;
import java.sql.*;
import java.util.*;
import java.net.*;
import sun.misc.*;

import javax.servlet.*;
import javax.servlet.http.*;

import com.oreilly.servlet.*;

import net.spy.*;

// The class
public class TestSession extends Object
{ 
	// This kinda stuff is only persistent for a single connection.
	HttpServletRequest request=null;
	HttpServletResponse response=null;
	HttpSession session=null;
	HttpServlet servlet=null;
	SpyConfig test_config=null;

	// What test someone is taking
	Integer test_no=null;
	TestTest test=null;

	protected void log(String s) {
		servlet.log(s);
	}

	// Constructor.
	public TestSession(HttpServlet s,
		HttpServletRequest request,
		HttpServletResponse response) {

		this.request=request;
		this.response=response;
		servlet=s;
		session=request.getSession(true);
		test_config=new TestConfig();
	}

	// process a request
	public void process() throws ServletException {

		String func;
		if(session.isNew()) {
			log(session.getId() + " is a new session");
		} else {
			log(session.getId() + " is an old session");
		}

		// Get the test number we're operating on.
		test_no = (Integer)session.getValue("test_no");
		test = (TestTest)session.getValue("test");

		func=request.getParameter("func");
		log("func is " + func);
		if(func==null) {
			getTest();
		} else if(func.equalsIgnoreCase("settest")) {
			setTest();
		} else if(func.equalsIgnoreCase("question_add_form")) {
			questionAddForm();
		} else if(func.equalsIgnoreCase("add_question")) {
			addQuestion();
		} else if(func.equalsIgnoreCase("grade")) {
			gradeTest();
		} else {
			throw new ServletException("No known function.");
		}
	}

	protected void gradeTest() throws ServletException {
		int i, total=0, correct=0;
		String out="";
		String failures="";

		for(i=0; ; i++) {
			String s = request.getParameter("question_" + i);
			if(s==null) {
				break;
			}
			int which=Integer.valueOf(s).intValue();
			if(test.getQuestion(i).getAnswer(which).isCorrect()) {
				correct++;
			} else {
				failures+="<li>" + test.getQuestion(i).getQuestion()
					+ "<br>\nA:  "
					+ test.getQuestion(i).getAnswer().getAnswer()
					+ "</li>\n";
			}
			total++;
		}

		double a=correct;
		double b=total;
		double score=(a/b)*100.00;
		int percent=(int)score;

		session.removeValue("test");

		Hashtable h = new Hashtable();
		h.put("TOTAL", "" + total);
		h.put("CORRECT", "" + correct);
		h.put("PERCENT", "" + percent);
		h.put("FAILURES", failures);
		out=tokenize("grade.inc", h);
		send_response(out);
	}

	// Set and generate a test
	protected void setTest() throws ServletException {
		if(test==null) {
			if(test_no == null) {
				test_no = Integer.valueOf(request.getParameter("test_id"));
			}
			session.putValue("test_no", test_no);
			try {
				test = new TestTest(test_no.intValue(), 20);
			} catch(Exception e) {
				throw new ServletException(e.getMessage());
			}
			session.putValue("test", test);
		}

		displayTest();
	}

	protected void displayTest() throws ServletException {
		if(test==null) {
			throw new ServletException("Test is null, how'd you get here?");
		}
		String out = "";

		out += tokenize("test_top.inc", new Hashtable());

		test.setCurrent(0);
		int i=0;
		TestQuestion q=null;
		while(test.nextQuestion()) {
			Hashtable h = new Hashtable();
			h.put("QUESTION", test.getQuestion().getQuestion());
			h.put("ANSWER1", test.getQuestion().getAnswer(0).getAnswer());
			h.put("ANSWER2", test.getQuestion().getAnswer(1).getAnswer());
			h.put("ANSWER3", test.getQuestion().getAnswer(2).getAnswer());
			h.put("ANSWER4", test.getQuestion().getAnswer(3).getAnswer());
			Integer itmp = new Integer(i);
			h.put("Q_NO", itmp.toString());
			out += tokenize("a_question.inc", h);
			i++;
		}

		out += tokenize("test_bottom.inc", new Hashtable());

		send_response(out);
	}

	protected void addQuestion() throws ServletException {
		String test_id=SpyDB.dbquote_str(request.getParameter("test_id"));
		String question=SpyDB.dbquote_str(request.getParameter("question"));
		String answer1=SpyDB.dbquote_str(request.getParameter("answer1"));
		String answer2=SpyDB.dbquote_str(request.getParameter("answer2"));
		String answer3=SpyDB.dbquote_str(request.getParameter("answer3"));
		String answer4=SpyDB.dbquote_str(request.getParameter("answer4"));
		Hashtable h = new Hashtable();
		h.put("TEST_ID", test_id);
		h.put("QUESTION", question);
		h.put("ANSWER1", answer1);
		h.put("ANSWER2", answer2);
		h.put("ANSWER3", answer3);
		h.put("ANSWER4", answer4);

		int question_id, answer_id;

		// Do bigass insert stuff
		SpyDB db = null;
		Connection dbc=null;

		try {
			db = new SpyDB(test_config, true);
			dbc=db.getConn();

			Statement st = dbc.createStatement();
			dbc.setAutoCommit(false);
			String query="insert into question (test_id, question)\n"
				+ "\tvalues(" + test_id + ", '" + question + "')";
			st.executeUpdate(query);
			query = "select currval('question_question_id_seq')";
			ResultSet rs = st.executeQuery(query);
			rs.next();
			question_id=rs.getInt(1);

			query="insert into answer (question_id, answer)\n"
				+ "\tvalues(" + question_id + ", '" + answer1 + "')";
			st.executeUpdate(query);
			query = "select currval('answer_answer_id_seq')";
			rs = st.executeQuery(query);
			rs.next();
			answer_id=rs.getInt(1);
			query="update question set answer_id = " + answer_id + "\n"
				+ "\twhere question_id = " + question_id;
			st.executeUpdate(query);

			query="insert into answer (question_id, answer)\n"
				+ "\tvalues(" + question_id + ", '" + answer2 + "')";
			st.executeUpdate(query);
			query="insert into answer (question_id, answer)\n"
				+ "\tvalues(" + question_id + ", '" + answer3 + "')";
			st.executeUpdate(query);
			query="insert into answer (question_id, answer)\n"
				+ "\tvalues(" + question_id + ", '" + answer4 + "')";
			st.executeUpdate(query);
			dbc.commit();
			h.put("STATUS", "Question was succesfully added.");
		} catch(Exception e) {
			try {
				log(e.getMessage());
				dbc.rollback();
				h.put("STATUS", "Error adding question:  " + e.getMessage());
				throw new ServletException("Error adding question");
			} catch(Exception e2) {
				log(e2.getMessage());
			}
		}
		send_response(tokenize("added_question.inc", h));
	}

	protected void questionAddForm() throws ServletException {
		Hashtable h = new Hashtable();
		h.put("TEST_LIST", listTests());
		send_response(tokenize("question_add_form.inc", h));
	}

	protected void getTest() throws ServletException {
		if(test_no == null) {
			Hashtable h = new Hashtable();
			h.put("TEST_LIST", listTests());
			send_response(tokenize("select_test.inc",h));
		} else {
			if(test == null) {
				setTest();
			} else {
				displayTest();
			}
		}
	}

	protected String tokenize(String file, Hashtable vars) {
		SpyToker t=new SpyToker();
		String ret=null, path=null;

		vars.put("SELF_URI", request.getRequestURI());
		path=test_config.get("includes") + file;
		log("Importing " + path);
		ret=t.tokenize(path, vars);
		return(ret);
	}

	protected String listTests() throws ServletException {
		SpyDB db = new SpyDB(test_config, true);
		String ret="";

		try {
			Connection tdb=db.getConn();
			Statement st = tdb.createStatement();
			ResultSet rs=st.executeQuery("select * from test");

			while(rs.next()) {
				String id = rs.getString(1);
				String test = rs.getString(2);
				ret+="<option value=\"" + id + "\">" + test + "\n";
			}
		} catch(Exception e) {
			// A valid, but empty string will be returned upon error.
		}
		return(ret);
	}

	protected void send_response(String text) {
		response.setContentType("text/html");
		try {
			PrintWriter out=response.getWriter();
			out.print(text);
			out.close();
		} catch(Exception e) {
			// Yeah, yeah
		}
	}
}
