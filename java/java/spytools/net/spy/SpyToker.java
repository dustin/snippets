/*
 * Copyright (c) 1999 Dustin Sallings
 *
 * $Id: SpyToker.java,v 1.4 2001/05/22 03:57:34 dustin Exp $
 */

package net.spy;

import java.util.*;
import java.io.*;

/**
 * A simple token in-plugger.
 * <p>
 * Input files are in any textual format, with tokens in the format of
 * %TOKEN% where TOKEN is a valid token that can be found in the hash
 * passed in to the tokenizer.
 */

public class SpyToker extends Object {

	/**
	 * @deprecated give it a File instead.
	 */
	public String tokenize(String file, Hashtable p) {
		return(tokenize(new File(file), p));
	}

	/**
	 * tokenize a file
	 *
	 * @param file file to tokenize
	 * @param p HashTable whose entries will be looked up to fill in the
	 * tokens in the text file.
	 *
	 * @return tokenized data from the file.
	 */
	public String tokenize(File file, Hashtable p) {
		String input=null;
		StringBuffer output=new StringBuffer();
		int which;

		// Get our mofo data.
		try {
			input=SpyUtil.getFileData(file);
		} catch (IOException e) {
			return(null);
		}

		while( (which=input.indexOf('%')) >= 0) {
			output.append(input.substring(0, which));
			input=input.substring(which+1);

			which=input.indexOf('%');
			if(which>=0) {
				String v=null;
				String tmp = input.substring(0, which);
				input = input.substring(which+1);
				if( (v=(String)p.get(tmp)) != null) {
					output.append(v);
				} else {
					output.append("%" + tmp + "%");
				}
			} else {
				output.append("%");
			}
		}
		output.append(input);
		return(output.toString());
	}
}
