/*
 * Copyright (c) 1999 Dustin Sallings
 *
 * $Id: Toker.java,v 1.3 1999/09/30 17:43:08 dustin Exp $
 */

import java.util.*;
import java.io.*;

public class Toker {

	// Tokenize a file, replacing all of the stuff in the hash.
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
