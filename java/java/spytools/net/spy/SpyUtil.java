// Copyright (c) 1999  Dustin Sallings <dustin@spy.net>
//
// $Id: SpyUtil.java,v 1.2 1999/12/15 03:58:16 dustin Exp $

package net.spy;

import java.util.*;

public class SpyUtil {
	// Shuffle an array.
	public static Object[] shuffle(Object in[]) {
		Object tmp;
		Object ret[] = in;
		Random r = new Random();
		int size, i;

		size=ret.length-1;

		for(i=0; i<size; i++) {
			// Get a random number the size of the length
			int n = r.nextInt();
			if(n<0) {
				n=-n;
			}
			n=n%size;
			// System.out.println("Index is " + n);
			tmp=ret[i];
			ret[i]=ret[n];
			ret[n]=tmp;
		}
		return(ret);
	}

	// Split a string
	public static String[] split(String on, String input) {
		Vector v = new Vector();
		StringTokenizer st = new StringTokenizer(input, on);
		String ret[];
		int i;

		while( st.hasMoreTokens() ) {
			v.addElement(st.nextToken());
		}

		ret=new String[v.size()];

		for(i=0; i<v.size(); i++) {
			ret[i]=(String)v.elementAt(i);
		}
		return(ret);
	}
}
