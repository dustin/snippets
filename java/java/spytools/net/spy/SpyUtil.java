// Copyright (c) 1999  Dustin Sallings <dustin@spy.net>
//
// $Id: SpyUtil.java,v 1.1 1999/11/19 02:11:01 dustin Exp $

package net.spy;

import java.util.*;

public class SpyUtil {
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
}
