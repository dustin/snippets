/*
 * Copyright (c) 1999 Dustin Sallings
 *
 * $Id: PhotoServlet.java,v 1.2 1999/09/15 18:52:06 dustin Exp $
 */

import java.io.*;
import java.sql.*;
import java.text.*;
import java.util.*;
import sun.misc.*;

import javax.servlet.*;
import javax.servlet.http.*;


// The class
public class PhotoServlet extends HttpServlet
{
	static Connection photo;
	static Statement st;
	static Integer remote_uid;
	static String remote_user, self_uri;

	// Do a POST request
	public void doPost (
		HttpServletRequest request, HttpServletResponse response
	) throws ServletException, IOException {
		doGet(request, response);
	}

	// Do a GET request
	public void doGet (
		HttpServletRequest request, HttpServletResponse response
	) throws ServletException, IOException {

		String source, func;

		// Set the self_uri
		self_uri = request.getRequestURI();

		// Let's see what's up before we continue...
		remote_user = request.getRemoteUser();

		if(remote_user == null) {
			// throw new ServletException("Not authenticated...");
			remote_user = "guest";
		}

		System.out.println("Authenticated as " + remote_user);

		// Load a postgres driver.
		try {
			Class.forName("postgresql.Driver");
		} catch(ClassNotFoundException e) {
			throw new ServletException ("Can't load Postgres " +
				"driver, shit...");
		}

		// The path to the database...
		source="jdbc:postgresql://dhcp-104/photo";

		// Get an actual database connection.
		try {
			photo = DriverManager.getConnection(source, "dustin", "");
		} catch(SQLException e) {
			throw new ServletException ("Can't connect to database, shit...");
		}

		// Get the UID for the username now that we have a database
		// connection.
		getUid();

		// Figure out what they want, default to index.
		func=request.getParameter("func");
		if(func == null) {
			doIndex(request, response);
		} else if(func.equalsIgnoreCase("search")) {
			doFind(request, response);
		} else if(func.equalsIgnoreCase("index")) {
			doIndex(request, response);
		} else if(func.equalsIgnoreCase("findform")) {
			doFindForm(request, response);
		} else if(func.equalsIgnoreCase("display")) {
			doDisplay(request, response);
		} else if(func.equalsIgnoreCase("getimage")) {
			showImage(request, response);
		} else {
			throw new ServletException("No known function.");
		}

	}

	private static String showSaved() throws SQLException, IOException {
		Statement st;
		String query, out="";
		BASE64Decoder base64 = new BASE64Decoder();

		st=photo.createStatement();

		query = "select * from searches order by name,addedby\n";
		ResultSet rs = st.executeQuery(query);
		while(rs.next()) {
			byte data[];
			String tmp;
			data=base64.decodeBuffer(rs.getString(4));
			tmp = new String(data);
			out += "    <li><a href=\"" + self_uri + "?"
				+ tmp + "\">" + rs.getString(2) + "</a></li>\n";
		}
		return(out);
	}

	private static String getCatList() throws SQLException, IOException {
		Statement st;
		String query, out="";

		st=photo.createStatement();

		query = "select * from cat where id in\n"
			  + "(select cat from wwwacl where\n"
			  + "    userid=" + remote_uid + ")\n"
			  + "order by name\n";

		ResultSet rs = st.executeQuery(query);
		while(rs.next()) {
			out += "    <option value=\"" + rs.getString(1)
				+ "\">" + rs.getString(2) + "\n";
		}
		return(out);
	}

	private static void doFindForm(
		HttpServletRequest request, HttpServletResponse response)
		throws ServletException {
		String output = new String("");
		Hashtable h = new Hashtable();

		try {
			h.put("CAT_LIST", getCatList());
		} catch(Exception e) {
			h.put("CAT_LIST", "");
		}
		output += tokenize("findform.inc", h);
		output += tokenize("tail.inc", h);
		send_response(response, output);
	}

	private static void doIndex(
		HttpServletRequest request, HttpServletResponse response)
		throws ServletException {
		String output = new String("");;
		Hashtable h = new Hashtable();

		try {
			h.put("SAVED", showSaved());
		} catch(Exception e) {
			h.put("SAVED", "");
		}
		output += tokenize("index.inc", h);
		output += tokenize("tail.inc", h);
		send_response(response, output);
	}

	// Get the UID
	private static void getUid() throws ServletException {
		String query;
		// This is our exception, in case we need it.
		ServletException x;
		x = new ServletException("Unknown user: " + remote_user);

		query = "select getwwwuser('" + remote_user + "')";
		try {
			st = photo.createStatement();
			ResultSet rs = st.executeQuery(query);
			if(rs.next()) {
				remote_uid=Integer.valueOf(rs.getString(1));
			} else {
				throw x;
			}
		} catch(SQLException e) {
			System.out.println(e.getSQLState());
			throw x;
		}
	}

	// Build the bigass complex search query.
	private static String buildQuery(HttpServletRequest request)
		throws ServletException {
		String query, sub, stmp, order, odirection, fieldjoin, join;
		boolean needao;
		String atmp[];

		sub = "";

		query = "select a.keywords,a.descr,b.name,\n"
			+ "   a.size,a.taken,a.ts,a.id,a.cat,a.addedby,b.id\n"
			+ "   from album a, cat b\n   where a.cat=b.id\n"
			+ "       and a.cat in (select cat from wwwacl\n"
			+ "              where userid=" + remote_uid + ")";

		needao=false;

		// Find out what the fieldjoin is real quick...
		stmp=request.getParameter("fieldjoin");
		if(stmp == null) {
			fieldjoin="and";
		} else {
			fieldjoin=dbquote_str(stmp);
		}

		// Start with categories.
		atmp=request.getParameterValues("cat");

		if(atmp != null) {
			stmp="";
			boolean snao=false;
			int i;

			// Do we need and or or?
			if(needao) {
				sub += " and";
			}
			needao=true;

			for(i=0; i<atmp.length; i++) {
				if(snao) {
					stmp += " or";
				} else {
					snao=true;
				}
				stmp += "\n        a.cat=" + Integer.valueOf(atmp[i]);
			}

			if(atmp.length > 1) {
				sub += "\n     (" + stmp + "\n     )";
			} else {
				sub += "\n   " + stmp;
			}
		}

		// OK, lets look for search strings now...
		stmp = request.getParameter("what");
		if(stmp != null && stmp.length() > 0) {
			String a, b, field;
			boolean needjoin=false;
			a=b="";

			// If we need an and or an or, stick it in here.
			if(needao) {
				sub += " " + fieldjoin;
			}
			needao=true;

			atmp = split(' ', stmp);

			join = dbquote_str(request.getParameter("keyjoin"));
			// Default
			if(join == null) {
				join = "or";
			}

			field = dbquote_str(request.getParameter("field"));
			// Default
			if(field == null) {
				throw new ServletException("No field");
			}

			if(atmp.length > 1) {
				int i;
				sub += "\n     (";
				for(i=0; i<atmp.length; i++) {
					if(needjoin) {
						sub += join;
					} else {
						needjoin=true;
					}
					sub += "\n\t" + field + " ~* '" + atmp[i] + "'";
				}
				sub += "\n     )";
			} else {
				sub += "\n    " + field + " ~* '" + stmp + "'";
			}
		}

		// Starts and ends

		stmp=dbquote_str(request.getParameter("tstart"));
		if(stmp != null && stmp.length()>0) {
			if(needao) {
				join=dbquote_str(request.getParameter("tstartjoin"));
				if(join==null) {
					join="and";
				}
				sub += " " + join;
			}
			needao=true;
			sub += "\n    a.taken>='" + stmp + "'";
		}

		stmp=dbquote_str(request.getParameter("tend"));
		if(stmp != null && stmp.length()>0) {
			if(needao) {
				join=dbquote_str(request.getParameter("tendjoin"));
				if(join==null) {
					join="and";
				}
				sub += " " + join;
			}
			needao=true;
			sub += "\n    a.taken<='" + stmp + "'";
		}

		stmp=dbquote_str(request.getParameter("start"));
		if(stmp != null && stmp.length()>0) {
			if(needao) {
				join=dbquote_str(request.getParameter("startjoin"));
				if(join==null) {
					join="and";
				}
				sub += " " + join;
			}
			needao=true;
			sub += "\n    a.ts>='" + stmp + "'";
		}

		stmp=dbquote_str(request.getParameter("end"));
		if(stmp != null && stmp.length()>0) {
			if(needao) {
				join=dbquote_str(request.getParameter("endjoin"));
				if(join==null) {
					join="and";
				}
				sub += " " + join;
			}
			needao=true;
			sub += "\n    a.ts<='" + stmp + "'";
		}

		// Stick the subquery on the bottom.
		if(sub.length() > 0 ) {
			query += " and\n (" + sub + "\n )";
		}

		// Stick the order under the subquery.
		stmp=dbquote_str(request.getParameter("order"));
		if(stmp != null) {
			order = stmp;
		} else {
			order = "a.taken";
		}

		// Figure out the direction...
		stmp=dbquote_str(request.getParameter("sdirection"));
		if(stmp != null) {
			odirection=stmp;
		} else {
			odirection = "";
		}

		query += "\n order by " + order + " " + odirection;

		return(query);
	}


	// Split that shit
	private static String[] split(char on, String input) {
		Vector v = new Vector();
		StringTokenizer st = new StringTokenizer(input);
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

	// Top of the html...
	private static String start_html(String title) {
		String ret = "<html><head><title>" + title + "</title>\n<head>\n" +
		"</head><body bgcolor=\"#fFfFfF\">";

		return(ret);
	}

	// Find and display images.
	private static void doDisplay(
		HttpServletRequest request, HttpServletResponse response)
		throws ServletException {
		String query, output = "";
		int i;
		Integer image_id;
		String stmp;
		Hashtable h = new Hashtable();

		stmp = request.getParameter("id");
		if(stmp == null) {
			throw new ServletException("Not enough information.");
		}
		image_id=Integer.valueOf(stmp);

		query = "select a.id,a.keywords,a.descr,\n"
			+ "   a.size,a.taken,a.ts,b.name,a.cat,a.addedby,b.id\n"
			+ "   from album a, cat b\n"
			+ "   where a.cat=b.id and a.id=" + image_id
			+ "\n and a.cat in (select cat from wwwacl where "
			+ "userid=" + remote_uid + ")\n";

		output += "<!-- Query:\n" + query + "\n-->\n";

		try {
			st = photo.createStatement();
			ResultSet rs = st.executeQuery(query);

			if(rs.next() == false) {
				throw new ServletException("No data found for that id.");
			}

			h.put("IMAGE",     rs.getString(1));
			h.put("KEYWORDS",  rs.getString(2));
			h.put("INFO",      rs.getString(3));
			h.put("SIZE",      rs.getString(4));
			h.put("TAKEN",     rs.getString(5));
			h.put("TIMESTAMP", rs.getString(6));
			h.put("CAT",       rs.getString(7));
			h.put("CATNUM",    rs.getString(8));
			h.put("ADDEDBY",   rs.getString(9));
		} catch(SQLException e) {
			throw new ServletException("Some kinda SQL problem.");
		}
		output += tokenize("display.inc", h);

		output += tokenize("tail.inc", new Hashtable());

		send_response(response, output);
	}

	// Send the response text...
	private static void send_response(HttpServletResponse response, String text)
	{
		// set content type and other response header fields first
		response.setContentType("text/html");
		try {
			PrintWriter out = response.getWriter();
			out.print(text);
			out.close();
		} catch(Exception e) {
		}
	}

	// Find and display images.
	private static void doFind(
		HttpServletRequest request, HttpServletResponse response)
		throws ServletException {
		String query, output = "";
		int i, start=0, max=0;
		Integer itmp;
		String stmp;

		output += start_html("Find Results");

		query=buildQuery(request);

		output += "<!--\n" + query + "\n-->";

		stmp=request.getParameter("qstart");
		if(stmp != null) {
			itmp=Integer.valueOf(stmp);
			start=itmp.intValue();
		}

		stmp=request.getParameter("maxret");
		if(stmp != null) {
			itmp=Integer.valueOf(stmp);
			max=itmp.intValue();
		}

		output += "<table><tr>\n";

		try {
			// Go through the matches.
			st = photo.createStatement();
			ResultSet rs = st.executeQuery(query);
			i = 0;
			while(rs.next()) {
				if (i >= start && ( ( max == 0 ) || ( i < (max+start) ) ) ) {
					Hashtable h = new Hashtable();

					h.put("KEYWORDS", rs.getString(1));
					h.put("DESCR",    rs.getString(2));
					h.put("CAT",      rs.getString(3));
					h.put("SIZE",     rs.getString(4));
					h.put("TAKEN",    rs.getString(5));
					h.put("TS",       rs.getString(6));
					h.put("IMAGE",    rs.getString(7));
					h.put("CATNUM",   rs.getString(8));
					h.put("ADDEDBY",  rs.getString(9));

					if( ((i+1) % 2) == 0) {
						output += "</tr>\n<tr>\n";
					}

					output += "<td>\n";
					output += tokenize("findmatch.inc", h);
					output += "</td>\n";
				}
				i++;
			}

		} catch(SQLException e) {
			throw new ServletException("Database problem: " + e.getMessage() +
				"\n" + query);
		}

		output += "</tr></table>\n";
		// Do we have anymore?
		if(i > max+start && max > 0) {
			output += linktomore(request, start, max);
		}

		output += tokenize("tail.inc", new Hashtable());

		send_response(response, output);
	}

	// Link to more search results
	private static String linktomore(HttpServletRequest request,
		int start, int max) {
		String ret = "";

		ret += "<form method=\"POST\" action=\"" + self_uri + "\">\n";
		ret += "<input type=hidden name=qstart value=" + (start+max) + ">\n";

		for(Enumeration e=request.getParameterNames(); e.hasMoreElements();) {
			String p = (String)e.nextElement();

			// Don't do this for qstart.
			if(! p.equals("qstart")) {
				ret += "<input type=hidden name=\"" + p;
				ret += "\" value=\"" + request.getParameter(p);
				ret += "\">\n";
			}
		}

		ret += "<input type=\"submit\" value=\"Next\">\n";
		ret += "</form>\n";
		return(ret);
	}

	// Show an image
	private static void showImage(HttpServletRequest request,
		HttpServletResponse response) throws ServletException {

		String query;
		ServletOutputStream		out;

		BASE64Decoder base64 = new BASE64Decoder();

		response.setContentType("image/jpeg");
		String s = request.getParameter("photo_id");
		Integer which = Integer.valueOf(s);

		query = "select * from image_store where id = " + which +
			" order by line";

		System.out.print("Doing query:  " + query + "\n");

		try {
			// Need a binary output thingy.
			out = response.getOutputStream();

			st = photo.createStatement();
			ResultSet rs = st.executeQuery(query);
			while(rs.next()) {
				byte data[];
				data=base64.decodeBuffer(rs.getString(3));
				out.write(data);
			}
		} catch(Exception e) {
			throw new ServletException("Problem getting image.");
		}

		// out.close();
	}

	private static String dbquote_str(String thing) {

		// Quick...handle null
		if(thing == null) {
			return(null);
		}

		String scopy = new String(thing);
		if(scopy.indexOf('\'') >= 0) {
			String sout = new String("");
			StringTokenizer st = new StringTokenizer(scopy, "\'");
			while(st.hasMoreTokens()) {
				String part = st.nextToken();

				if(st.hasMoreTokens()) {
					sout += part + "\'\'";
				} else {
					sout += part;
				}
			}
			scopy=sout;
		}
		return(scopy);
	}

	private static String tokenize(String file, Hashtable vars) {
		Toker t=new Toker();
		String ret;

		vars.put("SELF_URI", self_uri);
		vars.put("HTML_URI", "/~dustin/jphoto/");
		vars.put("REMOTE_USER", remote_user);
		vars.put("REMOTE_UID", remote_uid.toString());
		vars.put("STYLESHEET", "<link rel=\"stylesheet\"href=\""
			+ "/perl/dustin/photo/style.cgi\">");

		ret = t.tokenize("/home/dustin/public_html/jphoto/inc/" + file, vars);

		return(ret);
	}
}
