// Copyright (c) 2000  Dustin Sallings <dustin@spy.net>
//
// $Id: SpyRunner.java,v 1.2 2000/01/23 10:36:31 dustin Exp $

package net.spy;

import java.lang.*;
import java.lang.reflect.*;
import java.util.*;
import java.io.*;

public class SpyRunner extends Thread {
	Class tclass=null;
	String args[]=null;

	static Properties prop=null;

	public SpyRunner(String object, String args[]) {
		try {
			tclass=Class.forName(object);
			this.args=args;
		} catch(Exception e) {
			System.err.println("Error loading class " + object + ":  " + e);
			e.printStackTrace();
		}
	}

	public void run() {
		try {
			// Class list to find the method dynamically
			Class paramtypes[] = new Class[1];
			String tmp[]=new String[0];
			paramtypes[0]=tmp.getClass();
			Method m = tclass.getMethod("main", paramtypes);

			// Object list to invoke the method
			Object params[]=new Object[1];
			params[0]=args;

			// invoke the method
			m.invoke(tclass, params);
		} catch(Exception e) {
			System.err.println("Error invoking method:  " + e);
			e.printStackTrace();
		}
	}

	public static void main(String args[]) throws Exception {
		initProperties(args[0]);

		String a[]=SpyUtil.split(" ", prop.getProperty("apps"));
		for(int i=0; i<a.length; i++) {
			// System.out.println("Got app:  " + a[i]);

			String classname=prop.getProperty(a[i] + ".class");
			String argstring=prop.getProperty(a[i] + ".args");
			String cargs[]=null;
			if(argstring!=null && argstring.length() > 0) {
				// System.out.println("Got an argstring:  " + argstring);
				cargs=SpyUtil.split(" ", argstring);
			} else {
				// System.out.println("No argstring, using an empty one");
				cargs=new String[0];
			}
			Thread t = new SpyRunner(classname, cargs);
			t.run();
		}
	}

	protected static void initProperties(String from) throws Exception {
		prop=new Properties();
		prop.load(new FileInputStream(from));
	}
}
