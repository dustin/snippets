// Copyright (c) 1999  Dustin Sallings <dustin@spy.net>

package net.spy.net;

import java.net.*;
import java.io.*;
import java.util.*;

import net.spy.SpyUtil;

// Regex type stuff.
import com.oroinc.text.regex.*;

/**
 * Oversimplified HTTP document fetcher.
 */

// Fetch the contents of a URL
public class HTTPFetch {
	protected URL url;

	protected String contents=null;
	protected String stripped=null;

	/**
	 * Create a new HTTPFetch object for a given string representation of a
	 * URL.
	 *
	 * @exception MalformedURLException Thrown if the URL cannot be parsed.
	 */
	public HTTPFetch(String u) throws MalformedURLException {
		url=new URL(u);
	}

	/**
	 * Get a vector containing the individual lines of the document
	 * returned from the URL.
	 *
	 * @exception Exception thrown when something fails.
	 */
	public Vector getLines() throws Exception {
		Vector v = new Vector();
		getData();

		String lines[]=SpyUtil.split("\r\n", contents);
		for(int i=0; i<lines.length; i++) {
			v.addElement(lines[i]);
		}

		return(v);
	}

	/**
	 * Return the contents of the URL as a whole string.
	 *
	 * @exception Exception thrown when something fails.
	 */
	public String getData() throws Exception {
		if(contents==null) {
			contents="";
			try {
				BufferedReader br = getReader();
				String line;
				while( (line=br.readLine()) != null) {
					contents+=line + "\n";
				}
			} catch(Exception e) {
				throw new Exception(e.getMessage());
			}
		}
		return(contents);
	}

	/**
	 * Return the contents of the URL with the HTML tags stripped out.
	 *
	 * @exception Exception thrown when something fails.
	 */
	public String getStrippedData() throws Exception {
		getData();
		if(stripped==null) {
			PatternMatcher matcher = new Perl5Matcher();
			PatternCompiler compiler = new Perl5Compiler();

			// Strip off the HTML tags.
			Pattern pattern = compiler.compile("(<(.|[\r\n])*?>)",
				Perl5Compiler.MULTILINE_MASK);
			stripped = Util.substitute(matcher, pattern,
				new StringSubstitution(""), contents, Util.SUBSTITUTE_ALL);
			// Consolidate spaces
			pattern = compiler.compile("((?:&nbsp;|[ \t])+)",
				Perl5Compiler.MULTILINE_MASK);
			stripped = Util.substitute(matcher, pattern,
				new StringSubstitution(" "), stripped, Util.SUBSTITUTE_ALL);
			// Get rid of double newlines
			pattern = compiler.compile("([\r\n]+)",
				Perl5Compiler.MULTILINE_MASK);
			stripped = Util.substitute(matcher, pattern,
				new StringSubstitution("\n"), stripped, Util.SUBSTITUTE_ALL);
		}
		return(stripped);
	}

	// Get a reader for the above routines.
	protected BufferedReader getReader() throws Exception {
		URLConnection uc = url.openConnection();
		InputStream i = uc.getInputStream();
		BufferedReader br =
			new BufferedReader( new InputStreamReader(i));
		return(br);
	}
}
