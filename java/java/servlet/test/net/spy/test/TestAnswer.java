// Copyright (c) 1999  Dustin Sallings <dustin@spy.net>
//
// $Id: TestAnswer.java,v 1.2 2001/01/28 08:16:03 dustin Exp $

package net.spy.test;

import java.io.Serializable;

import net.spy.*;

// This class implements the actual test that will be taken by people
public class TestAnswer extends Object implements Serializable{
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
