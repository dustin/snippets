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

	protected Hashtable headers=null;

	/**
	 * Create a new HTTPFetch object for a given string representation of a
	 * URL.
	 *
	 * @param u String representation of the URL we'll be connecting to.
	 *
	 * @exception MalformedURLException Thrown if the URL cannot be parsed.
	 */
	public HTTPFetch(String u) throws MalformedURLException {
		url=new URL(u);
	}

	/**
	 * Create a new HTTPFetch object for a given string representation of a
	 * URL, including a hash describing extra headers to add.
	 *
	 * @param u String representation of the URL we'll be connecting to.
	 * @param head Hashtable containing headers to set.
	 *
	 * @exception MalformedURLException Thrown if the URL cannot be parsed.
	 */
	public HTTPFetch(String u, Hashtable head) throws MalformedURLException {
		super();
		url=new URL(u);
		headers=head;
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
			BufferedReader br = getReader();
			String line;
			while( (line=br.readLine()) != null) {
				contents+=line + "\n";
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

			// empty space at the beginning
			pattern = compiler.compile("^([ \t]+)");
			stripped = Util.substitute(matcher, pattern,
				new StringSubstitution(""), stripped, Util.SUBSTITUTE_ALL);

			// empty space at the end
			pattern = compiler.compile("([ \t]+)$");
			stripped = Util.substitute(matcher, pattern,
				new StringSubstitution(""), stripped, Util.SUBSTITUTE_ALL);

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
		if(headers!=null) {
			for(Enumeration e=headers.keys(); e.hasMoreElements(); ) {
				String key=(String)e.nextElement();
				String value=(String)headers.get(key);

				uc.setRequestProperty(key, value);
			}
		}
		InputStream i = uc.getInputStream();
		BufferedReader br =
			new BufferedReader( new InputStreamReader(i));
		return(br);
	}
}
