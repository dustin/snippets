/*
 * Copyright (c) 1999 Dustin Sallings
 *
 * $Id: SpyToker.java,v 1.2 2000/01/24 06:40:37 dustin Exp $
 */

package net.spy;

import java.util.*;
import java.io.*;

/**
 * A simple token in-plugger.
 */

public class SpyToker extends Object {

	/**
	 * tokenize a file
	 *
	 * @param file file to tokenize
	 * @param p HashTable whose entries will be looked up to fill in the
	 * tokens in the text file.
	 *
	 * @return tokenized data from the file.
	 */
	public String tokenize(String file, Hashtable p) {
		String input, output="";
		int which;

		// Get our mofo data.
		try {
			input=getfiledata(file);
		} catch (IOException e) {
			return(null);
		}

		while( (which=input.indexOf('%')) >= 0) {
			String tmp;
			output += input.substring(0, which);
			input=input.substring(which+1);

			which=input.indexOf('%');
			if(which>=0) {
				String v;
				tmp = input.substring(0, which);
				input = input.substring(which+1);
				if( (v=(String)p.get(tmp)) != null) {
					output += v;
				} else {
					output += "%" + tmp + "%";
				}
			} else {
				output += "%";
			}
		}
		output += input;
		return(output);
	}

	// Get the shite out of the file.
	private String getfiledata(String file)
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

}
