// Copyright (c) 2000  Dustin Sallings <dustin@spy.net>
//
// $Id: RawInput.java,v 1.1 2000/10/18 09:53:15 dustin Exp $

package net.spy.jni;

public class RawInput {

	public native void setRawInput();
	public native void unsetRawInput();

	static {
		System.loadLibrary("rawinput");
	}

	public static void main(String args[]) throws Exception {
		RawInput ri=new RawInput();

		ri.setRawInput();

		System.out.print("Hit a button>  ");
		int c=System.in.read();
		System.out.println("Read " + c);

		ri.unsetRawInput();
	}
}
