// Copyright (c) 1999  Dustin Sallings <dustin@spy.net>
//
// $Id: TestAnswer.java,v 1.1 1999/11/19 07:29:20 dustin Exp $

package net.spy.test;

import net.spy.*;

// This class implements the actual test that will be taken by people
public class TestAnswer {
	protected String answer = null;
	protected boolean correct=false;

	public TestAnswer(String a, boolean c) {
		answer=a;
		correct=c;
	}

	public String getAnswer() {
		return(answer);
	}

	public boolean isCorrect() {
		return(correct);
	}

	public String toString() {
		return(answer);
	}
}
