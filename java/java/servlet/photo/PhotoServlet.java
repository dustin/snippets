/*
 * Copyright (c) 1999 Dustin Sallings
 *
 * $Id: PhotoServlet.java,v 1.11 1999/09/24 06:56:31 dustin Exp $
 */

import java.io.*;
import java.sql.*;
import java.text.*;
import java.util.*;
import java.net.*;
import sun.misc.*;

import javax.servlet.*;
import javax.servlet.http.*;

import com.oreilly.servlet.*;
import com.javaexchange.dbConnectionBroker.*;


// The class
public class PhotoServlet extends HttpServlet
{
	Integer remote_uid;
	String remote_user, self_uri;
	DbConnectionBroker dbs;
	RHash rhash;
	MultipartRequest multi;

	// The once only init thingy.
	public void init(ServletConfig config) throws ServletException {
		super.init(config);
		// Load a postgres driver.
		try {
			Class.forName("postgresql.Driver");
			String source="jdbc:postgresql://dhcp-104/photo";
			dbs = new DbConnectionBroker("postgresql.Driver",
				source, "dustin", "", 2, 6, "/tmp/pool.log", 0.01);
		} catch(Exception e) {
			// System.err.println("dbs broke:  " + e.getMessage());
			throw new ServletException ("dbs broke: " + e.getMessage());
		}

		// Get an rhash to cache images and shite.
		try {
			rhash = new RHash("//dhcp-104/RObjectServer");
		} catch(Exception e) {
			rhash = null;
		}
	}

	// Do a GET request (just call doPost)
	public void doGet (
		HttpServletRequest request, HttpServletResponse response
	) throws ServletException, IOException {
		doPost(request, response);
	}

	// Do a POST request
	public void doPost (
		HttpServletRequest request, HttpServletResponse response
	) throws ServletException, IOException {

		String func, type;

		type = request.getContentType();
		if(type != null && type.startsWith("multipart/form-data")) {
			multi = new MultipartRequest(request, "/tmp");
		} else {
			multi = null;
		}

		// Set the self_uri
		self_uri = request.getRequestURI();

		// Let's see what's up before we continue...
		remote_user = request.getRemoteUser();

		if(remote_user == null) {
			// throw new ServletException("Not authenticated...");
			remote_user = "guest";
		}

		// Get the UID for the username now that we have a database
		// connection.
		getUid();

		// Figure out what they want, default to index.
		if(multi==null) {
			func=request.getParameter("func");
		} else {
			func=multi.getParameter("func");
		}
		log("func is " + func);
		if(func == null) {
			doIndex(request, response);
		} else if(func.equalsIgnoreCase("search")) {
			doFind(request, response);
		} else if(func.equalsIgnoreCase("addimage")) {
			doAddPhoto(request, response);
		} else if(func.equalsIgnoreCase("index")) {
			doIndex(request, response);
		} else if(func.equalsIgnoreCase("findform")) {
			doFindForm(request, response);
		} else if(func.equalsIgnoreCase("addform")) {
			doAddForm(request, response);
		} else if(func.equalsIgnoreCase("catview")) {
			doCatView(request, response);
		} else if(func.equalsIgnoreCase("setstyle")) {
			doSetStyle(request, response);
		} else if(func.equalsIgnoreCase("styleform")) {
			doStyleForm(request, response);
		} else if(func.equalsIgnoreCase("getstylesheet")) {
			doGetStylesheet(request, response);
		} else if(func.equalsIgnoreCase("display")) {
			doDisplay(request, response);
		} else if(func.equalsIgnoreCase("getimage")) {
			showImage(request, response);
		} else {
			throw new ServletException("No known function.");
		}

	}

	// Grab a connection from the pool.
	private Connection getDBConn() throws SQLException {
		Connection photo;

		// The path to the database...
		photo = dbs.getConnection();
		return(photo);
	}

	// Gotta free the connection
	private void freeDBConn(Connection conn) {
		dbs.freeConnection(conn);
	}

	// Get the saved searches.
	private String showSaved() throws SQLException, IOException {
		String query, out="";
		BASE64Decoder base64 = new BASE64Decoder();
		Connection photo=null;

		try {
			photo=getDBConn();
			Statement st=photo.createStatement();

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
		} finally {
			if(photo != null) {
				freeDBConn(photo);
			}
		}
		return(out);
	}

	// Find out if the authenticated user can add stuff.
	private boolean canadd() {
		String query;
		boolean r=false;
		Connection photo=null;

		try{
			photo=getDBConn();
			Statement st=photo.createStatement();

			query = "select canadd from wwwusers where username='"
				+ remote_user + "'";

			ResultSet rs = st.executeQuery(query);
			while(rs.next()) {
				String tmp;
				tmp = rs.getString(1);
				if(tmp.startsWith("t")) {
					r=true;
				}
			}
		} catch(Exception e) {
			log(e.getMessage());
		} finally {
			if(photo != null) {
				freeDBConn(photo);
			}
		}

		return(r);
	}

	// Add an image
	private void doAddPhoto(
		HttpServletRequest request, HttpServletResponse response)
		throws ServletException {
		String category="", keywords="", picture="", info="", taken="";
		String query="", out="", stmp, type;
		int id;
		Hashtable h = new Hashtable();
		Connection photo=null;
		Statement st=null;

		// Make sure the user can add.
		if(!canadd()) {
			send_response(response, tokenize("add_denied.inc", h));
			return;
		}

		// Check that it's the right file type.
		type = multi.getContentType("picture");
		log("Type is " + type);
		if(! (type.startsWith("image/jpeg")) ) {
			h.put("FILENAME", multi.getFilesystemName("picture"));
			send_response(response, tokenize("add_badfiletype.inc", h));
			return;
		}

		stmp=multi.getParameter("category");
		if(stmp!=null) {
			category=dbquote_str(stmp);
		}

		stmp=multi.getParameter("keywords");
		if(stmp!=null) {
			keywords=dbquote_str(stmp);
		}

		stmp=multi.getParameter("info");
		if(stmp!=null) {
			info=dbquote_str(stmp);
		}

		stmp=multi.getParameter("taken");
		if(stmp!=null) {
			taken=dbquote_str(stmp);
		}

		try {
			File f;
			FileInputStream in;
			BASE64Encoder base64=new BASE64Encoder();
			byte data[] = new byte[57];

			photo=getDBConn();
			st=photo.createStatement();
			photo.setAutoCommit(false);
			query = "insert into album(keywords,descr,cat,taken,addedby)\n"
				  + "    values('" + keywords + "',\n\t'" + info + "',\n"
				  + "    \t'" + category + "',\n\t'" + taken + "',\n"
				  + "    '" + remote_user + "')\n";
			st.executeUpdate(query);
			query = "select currval('album_id_seq')\n";
			ResultSet rs = st.executeQuery(query);
			rs.next();
			id=rs.getInt(1);

			// Encode the shit;
			int i=0, size=0, length=0;
			f = multi.getFile("picture");
			in = new FileInputStream(f);

			while( (length=in.read(data)) >=0 ) {
				String tmp;

				size+=length;
				if(length == 57) {
					tmp = base64.encodeBuffer(data);
				} else {
					byte tb[] = new byte[length];
					int j;

					for(j=0; j<length; j++) {
						tb[j] = data[j];
					}
					tmp = base64.encodeBuffer(tb);
				}

				// OK, we have a base64 encoding.
				query = "insert into image_store values(" + id + ", " + i
					  + ", '" + tmp + "')\n";
				st.executeUpdate(query);
				i++;
			}
			query ="update album set size=" + size + "where id=" + id;
			st.executeUpdate(query);

			h.put("ID", ""+id);
			st.executeUpdate("commit");
			out += tokenize("add_success.inc", h);
		} catch(Exception e) {
			log(e.getMessage());
			try {
				st.executeUpdate("rollback");
				h.put("QUERY", query);
				h.put("ERRSTR", e.getMessage());
				out += tokenize("add_success.inc", h);
			} catch(Exception e2) {
				log(e2.getMessage());
				// Nothing to see here.
			}
		} finally {
			if(photo != null) {
				try {
					photo.setAutoCommit(true);
					freeDBConn(photo);
				} catch(Exception e) {
					log(e.getMessage());
				}
			}
		}

		send_response(response, out);
	}

	// Get a list of categories for a select list
	private String getCatList() throws SQLException, IOException {
		String query, out="";

		Connection photo=getDBConn();
		Statement st=photo.createStatement();

		query = "select * from cat where id in\n"
			  + "(select cat from wwwacl where\n"
			  + "    userid=" + remote_uid + ")\n"
			  + "order by name\n";

		ResultSet rs = st.executeQuery(query);
		while(rs.next()) {
			out += "    <option value=\"" + rs.getString(1)
				+ "\">" + rs.getString(2) + "\n";
		}
		freeDBConn(photo);
		return(out);
	}

	// Show the style form
	private void doStyleForm (
		HttpServletRequest request, HttpServletResponse response)
		throws ServletException {
		String output;

		output = tokenize("presetstyle.inc", new Hashtable());
		send_response(response, output);
	}

	// Get the stylesheet from the cookie, or the default.
	private void doGetStylesheet (
		HttpServletRequest request, HttpServletResponse response)
		throws ServletException {
		Cookie cookies[];
		String output = null;
		int i;

		cookies = request.getCookies();

		for(i=0; i<cookies.length && output == null; i++) {
			String s = cookies[i].getName();
			if(s.equalsIgnoreCase("photo_style")) {
				output = cookies[i].getValue();
			}
		}

		if(output == null) {
			output = tokenize("style.css", new Hashtable());
		}

		// This is a little different, so we won't use send_response()
		response.setContentType("text/css");
		try {
			PrintWriter out = response.getWriter();
			out.print(output);
			out.close();
		} catch(Exception e) {
		}
	}

	// Set the style cookie from the POST data.
	private void doSetStyle(
		HttpServletRequest request, HttpServletResponse response)
		throws ServletException {
		Cookie c;
		String stmp="", font="", bgcolor="", c_text="";
		Hashtable h = new Hashtable();

		stmp = request.getParameter("font");
		if(stmp != null && stmp.length() > 1) {
			font = stmp;
		}

		stmp = request.getParameter("bgcolor");
		if(stmp != null && stmp.length() > 1) {
			bgcolor = stmp;
		}

		c_text = "body,td {font-family: " + font + ", Arial; "
			   + "background-color: " + bgcolor + ";}\n";

		stmp = request.getParameter("d_transform");
		if(stmp != null && stmp.length() > 1) {
			c_text += "blockquote {text-transform: " + stmp + ";};\n";
		}

		stmp = request.getParameter("d_transform");
		if(stmp != null && stmp.length() > 1) {
			c_text += "h1,h2,h3,h4,h5 {text-transform: " + stmp + ";};\n";
		}

		// Create the cookie
		c = new Cookie("photo_style", http_encode(c_text));
		// 30 days of cookie
		c.setMaxAge( (30 * 86400) );
		// Describe why we're doing this.
		c.setComment("Your style preferences for the photo album.");
		// Where we'll be using it.
		c.setPath(self_uri);

		// Add it to the responses.
		response.addCookie(c);

		// Prepare output.
		stmp = "";
		h.put("STYLE", c_text);

		stmp = tokenize("setstyle.inc", h);
		send_response(response, stmp);
	}

	// Quick wrapper for URLEncoder
	private String http_encode(String what) {
		return(URLEncoder.encode(what));
	}

	// Show the add an image form.
	private void doAddForm(
		HttpServletRequest request, HttpServletResponse response)
		throws ServletException {
		String output = new String("");
		Hashtable h = new Hashtable();

		try {
			h.put("CAT_LIST", getCatList());
		} catch(Exception e) {
			h.put("CAT_LIST", "");
		}
		output += tokenize("addform.inc", h);
		send_response(response, output);
	}

	// Show the search form.
	private void doFindForm(
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
		send_response(response, output);
	}

	// View categories
	private void doCatView(
		HttpServletRequest request, HttpServletResponse response)
		throws ServletException {
		String output = new String("");
		String query, catstuff="";
		Hashtable h = new Hashtable();
		Connection photo;

		try {
			photo = getDBConn();
		} catch(Exception e) {
			throw new ServletException("Can't get database connection:  "
				+ e.getMessage());
		}

		query = "select name,id,catsum(id) as cs from cat\n"
			  + "where id in\n"
			  + "  (select cat from wwwacl where\n"
			  + "   userid=" + remote_uid + ")\n"
			  + " order by cs desc";

		try {
			Statement st = photo.createStatement();
			ResultSet rs = st.executeQuery(query);
			while(rs.next()) {
				String t;
				if(rs.getInt(3)==1) {
					t = " image";
				} else {
					t = " images";
				}

				catstuff += "<li>" + rs.getString(1) + ":  <a href=\""+self_uri
					+ "?func=search&searchtype=advanced&cat="
					+ rs.getString(2) + "&maxret=5\">"
					+ rs.getString(3) + t + "</a></li>\n";
			}
		} catch(Exception e) {
		}
		finally { freeDBConn(photo); }

		h.put("CATSTUFF", catstuff);

		output += tokenize("catview.inc", h);
		send_response(response, output);
	}

	// Display the index page.
	private void doIndex(
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
		send_response(response, output);
	}

	// Get the UID
	private void getUid() throws ServletException {
		String query;
		// This is our exception, in case we need it.
		ServletException x;
		x = new ServletException("Unknown user: " + remote_user);
		Connection photo;

		try {
			photo=getDBConn();
		} catch(Exception e) {
			throw x;
		}

		query = "select getwwwuser('" + remote_user + "')";
		try {
			Statement st = photo.createStatement();
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
		finally { freeDBConn(photo); }
	}

	// Build the bigass complex search query.
	private String buildQuery(HttpServletRequest request)
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
	private String[] split(char on, String input) {
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

	// Find and display images.
	private void doDisplay(
		HttpServletRequest request, HttpServletResponse response)
		throws ServletException {
		String query, output = "";
		int i;
		Integer image_id;
		String stmp;
		Hashtable h = new Hashtable();
		Connection photo;

		try {
			photo=getDBConn();
		} catch(Exception e) {
			throw new ServletException("Can't get database connection:  "
				+ e.getMessage());
		}

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
			Statement st = photo.createStatement();
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
		finally { freeDBConn(photo); }
		output += tokenize("display.inc", h);

		send_response(response, output);
	}

	// Send the response text...
	private void send_response(HttpServletResponse response, String text)
	{
		// set content type and other response header fields first
		response.setContentType("text/html");
		try {
			PrintWriter out = response.getWriter();
			out.print(text);
			out.print(tokenize("tail.inc", new Hashtable()));
			out.close();
		} catch(Exception e) {
			// I really don't care at this point if this doesn't work...
		}
	}

	// Find and display images.
	private void doFind(
		HttpServletRequest request, HttpServletResponse response)
		throws ServletException {
		String query, output = "";
		int i, start=0, max=0;
		Integer itmp;
		String stmp;
		Connection photo;
		Hashtable h = new Hashtable();

		query=buildQuery(request);

		h.put("QUERY", query);

		output += tokenize("find_top.inc", h);

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

		try {
			photo=getDBConn();
		} catch(Exception e) {
			throw new ServletException("Can't get database connection:  "
				+ e.getMessage());
		}

		try {
			// Go through the matches.
			Statement st = photo.createStatement();
			ResultSet rs = st.executeQuery(query);
			i = 0;
			while(rs.next()) {
				if (i >= start && ( ( max == 0 ) || ( i < (max+start) ) ) ) {
					h = new Hashtable();

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
		finally { freeDBConn(photo); }

		// Do we have anymore?
		if(i > max+start && max > 0) {
			h.put("LINKTOMORE", linktomore(request, start, max));
		} else {
			h.put("LINKTOMORE", "");
		}

		output += tokenize("find_bottom.inc", h);

		send_response(response, output);
	}

	// Link to more search results
	private String linktomore(HttpServletRequest request,
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
	private void showImage(HttpServletRequest request,
		HttpServletResponse response) throws ServletException {

		String query, key;
		ServletOutputStream		out;
		Vector v = null;
		BASE64Decoder base64 = new BASE64Decoder();

		response.setContentType("image/jpeg");
		String s = request.getParameter("photo_id");
		Integer which = Integer.valueOf(s);

		try {
			// Need a binary output thingy.
			out = response.getOutputStream();
		} catch(IOException e) {
			throw new ServletException("IOException:  " + e.getMessage());
		}

		key = "photo_" + which.toString();

		if(rhash!=null) {
			v = (Vector)rhash.get(key);
		} else {
			log("No rhash for image cache, must use database directly");
		}

		if(v==null) {

			v = new Vector();
			Connection photo;
			try {
				photo=getDBConn();
			} catch(Exception e) {
				throw new ServletException("Can't get database connection: "
					+ e.getMessage());
			}
			query = "select * from image_store where id = " + which +
				" order by line";

			System.out.print("Doing query:  " + query + "\n");

			try {

				Statement st = photo.createStatement();
				ResultSet rs = st.executeQuery(query);

				log("Getting image " + which + " from database.");

				while(rs.next()) {
					byte data[];
					data=base64.decodeBuffer(rs.getString(3));
					v.addElement(data);
					out.write(data);
				}
				if(rhash != null) {
					log("Storing " + key + " in RHash");
					rhash.put(key, v);
				} else {
					log("No RHash, can't cache data.");
				}
			} catch(Exception e) {
				throw new ServletException("Problem getting image: " +
					e.getMessage());
			}
			finally { freeDBConn(photo); }
		} else {
			int i;

			log("Getting image " + which + " from RHash.");

			try {
				for( i = 0 ; i < v.size(); i++) {
					out.write( (byte[])v.elementAt(i));
				}
			} catch(IOException e) {
				throw new ServletException("IOException:  " + e.getMessage());
			}
		}
	}

	// Make a strings safe for the database.
	private String dbquote_str(String thing) {

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

	// Tokenize a template file and return the tokenized stuff.
	private String tokenize(String file, Hashtable vars) {
		Toker t=new Toker();
		String ret;

		vars.put("SELF_URI", self_uri);
		vars.put("HTML_URI", "/~dustin/jphoto/");
		vars.put("REMOTE_USER", remote_user);
		vars.put("REMOTE_UID", remote_uid.toString());
		vars.put("LAST_MODIFIED", "recently");
		vars.put("STYLESHEET", "<link rel=\"stylesheet\"href=\""
			+ "/servlet/root/PhotoServlet?func=getstylesheet\">");

		ret = t.tokenize("/home/dustin/public_html/jphoto/inc/" + file, vars);

		return(ret);
	}
}
