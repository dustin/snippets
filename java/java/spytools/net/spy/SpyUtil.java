// Copyright (c) 1999  Dustin Sallings <dustin@spy.net>
//
// $Id: SpyUtil.java,v 1.10 2001/01/28 02:42:09 dustin Exp $

package net.spy;

import java.util.*;
import java.security.*;
import java.io.*;

/**
 * Miscellaneous utilities.
 */

public class SpyUtil {
	/**
	 * Shuffle (unsort) an array.
	 *
	 * @param in The array of objects to shuffle.
	 */
	public static Object[] shuffle(Object in[]) {
		Object tmp;
		Object ret[] = in;
		SecureRandom r = new SecureRandom();
		int size, i;

		for(i=0; i<ret.length; i++) {
			// Get a random number the size of the length
			int n = r.nextInt();
			if(n<0) {
				n=-n;
			}
			n=n%ret.length;
			tmp=ret[i];
			ret[i]=ret[n];
			ret[n]=tmp;
		}
		return(ret);
	}

	/**
	 * Split a string based on a tokenizer.
	 *
	 * @param on the string to split on (from StringTokenizer)
	 *
	 * @param input the string that needs to be split
	 *
	 * @see StringTokenizer
	 */
	public static String[] split(String on, String input) {
		StringTokenizer st = new StringTokenizer(input, on);

		String ret[]=new String[st.countTokens()];

		int i=0;
		while( st.hasMoreTokens() ) {
			ret[i++]=st.nextToken();
		}

		return(ret);
	}

	/**
	 * Return the contents of a file as a string.
	 *
	 * @param file Path to filename.
	 *
	 * @exception IOException Thrown if the file cannot be opened.
	 */
	 public static String getFileData(String file)
		throws IOException {
		byte b[]=new byte[8192];
		FileInputStream f = new FileInputStream(file);
		String input="", tmp;
		int size;

		while( (size=f.read(b)) >=0 ) {
			tmp = new String(b);
			// Substring to get rid of all the damned nulls
			input += tmp.substring(0, size);

		}
		return(input);
	 }

	 /**
	  * Get a stack from an exception.
	  *
	  * @param e Exception from which we'll be extracting the stack.
	  * @param skip Number of stack entries to skip (usually two or three)
	  */
	public static String getStack(Exception e, int skip) {
		String r="";
		ByteArrayOutputStream bytes = new ByteArrayOutputStream();
		PrintWriter writer = new PrintWriter(bytes, true);
		e.printStackTrace(writer);

		StringTokenizer t = new StringTokenizer(bytes.toString(), "\n");
		for(int i=0; i<skip; i++) {
			t.nextToken();
		}

		while(t.hasMoreTokens()) {
			r+=t.nextToken().substring(4) + ",";
		}
		return(r);
	}
}
