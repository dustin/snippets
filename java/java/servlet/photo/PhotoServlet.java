/*
 * Copyright (c) 1999 Dustin Sallings
 *
 * $Id: PhotoServlet.java,v 1.26 1999/10/09 21:50:16 dustin Exp $
 */

import java.io.*;
import java.sql.*;
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
	public Integer remote_uid=null;
	public String remote_user=null, self_uri=null;
	DbConnectionBroker dbs=null;
	RHash rhash=null;
	MultipartRequest multi=null;
	PhotoLogger logger=null;

	// Users
	Hashtable userdb=null;

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

		// Populate the userdb hash
		try {
			userdb=new Hashtable();
			init_userdb();
		} catch(Exception e) {
			throw new ServletException("Can't get userdb:  " + e);
		}

		// Get an rhash to cache images and shite.
		try {
			rhash = new RHash("//dhcp-104/RObjectServer");
		} catch(Exception e) {
			rhash = null;
		}

		logger = new PhotoLogger();
	}

	protected void init_userdb() throws Exception {
		Connection photo=null;
		boolean worked=false;
		Exception why=null;

		try {
			photo=getDBConn();
			Statement st = photo.createStatement();
			ResultSet rs=st.executeQuery("select * from wwwusers");

			while(rs.next()) {
				PhotoUser u = new PhotoUser();

				u.id=new Integer(rs.getInt("id"));
				u.username=rs.getString("username");
				u.password=rs.getString("password");
				u.email=rs.getString("email");
				u.realname=rs.getString("realname");
				u.canadd=rs.getBoolean("canadd");

				log("Adding user " + u.username);
				userdb.put(u.username, u);
			}
			worked=true;
		} catch(Exception e) {
			worked=false;
			why=e;
		} finally {
			freeDBConn(photo);
		}

		if(worked==false) {
			throw new ServletException("Couldn't get list:  " + why);
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

		log("Remote user is " + remote_user);

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
		} else if(func.equalsIgnoreCase("logview")) {
			doLogView(request, response);
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

			query = "select * from searches order by name\n";
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
		boolean r=false;

		try{
			PhotoUser p = (PhotoUser)userdb.get(remote_user);
			r=p.canadd;
		} catch(Exception e) {
			log("Error getting canadd permissions:  " + e.getMessage());
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

		File f;
		f = multi.getFile("picture");

		// Check that it's the right file type.
		type = multi.getContentType("picture");
		log("Type is " + type);
		if( type == null || (! (type.startsWith("image/jpeg"))) ) {
			h.put("FILENAME", multi.getFilesystemName("picture"));
			h.put("FILETYPE", type);
			send_response(response, tokenize("add_badfiletype.inc", h));
			try {
				f.delete();
			} catch(Exception e) {
				log(e.getMessage());
			}
			return;
		}

		stmp=multi.getParameter("category");
		if(stmp!=null) {
			category=PhotoUtil.dbquote_str(stmp);
		}

		stmp=multi.getParameter("keywords");
		if(stmp!=null) {
			keywords=PhotoUtil.dbquote_str(stmp);
		}

		stmp=multi.getParameter("info");
		if(stmp!=null) {
			info=PhotoUtil.dbquote_str(stmp);
		}

		stmp=multi.getParameter("taken");
		if(stmp!=null) {
			taken=PhotoUtil.dbquote_str(stmp);
		}

		try {
			FileInputStream in;
			BASE64Encoder base64=new BASE64Encoder();
			byte data[] = new byte[57];

			photo=getDBConn();
			st=photo.createStatement();
			photo.setAutoCommit(false);
			query = "insert into album(keywords,descr,cat,taken,addedby)\n"
				  + "    values('" + keywords + "',\n\t'" + info + "',\n"
				  + "    \t'" + category + "',\n\t'" + taken + "',\n"
				  + "    '" + remote_uid + "')\n";
			st.executeUpdate(query);
			query = "select currval('album_id_seq')\n";
			ResultSet rs = st.executeQuery(query);
			rs.next();
			id=rs.getInt(1);

			// Encode the shit;
			int i=0, size=0, length=0;
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
			try {
				f.delete();
			} catch(Exception e) {
				log(e.getMessage());
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
		java.util.Date d=new java.util.Date();
		long l=d.getTime();
		l+=36000000L;
		response.setDateHeader("Expires", l);
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
		c = new Cookie("photo_style", URLEncoder.encode(c_text));
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
		h.put("TODAY", PhotoUtil.getToday());
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

		try {
			PhotoUser p=(PhotoUser)userdb.get(remote_user);
			remote_uid = p.id;
		} catch(Exception e) {
			throw x;
		}
	}

	// Build the bigass complex search query.
	private String buildQuery(HttpServletRequest request)
		throws ServletException {
		String query, sub, stmp, order, odirection, fieldjoin, join;
		boolean needao;
		String atmp[];

		sub = "";

		query = "select a.keywords,a.descr,b.name,\n"
			+ "   a.size,a.taken,a.ts,a.id,a.cat,c.username,b.id\n"
			+ "   from album a, cat b, wwwusers c\n   where a.cat=b.id\n"
			+ "       and a.addedby=c.id\n"
			+ "       and a.cat in (select cat from wwwacl\n"
			+ "              where userid=" + remote_uid + ")";

		needao=false;

		// Find out what the fieldjoin is real quick...
		stmp=request.getParameter("fieldjoin");
		if(stmp == null) {
			fieldjoin="and";
		} else {
			fieldjoin=PhotoUtil.dbquote_str(stmp);
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

			atmp = PhotoUtil.split(" ", stmp);

			join = PhotoUtil.dbquote_str(request.getParameter("keyjoin"));
			// Default
			if(join == null) {
				join = "or";
			}

			field = PhotoUtil.dbquote_str(request.getParameter("field"));
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
					sub += "\n\t" + field + " ~* '" + atmp[i] + "' ";
				}
				sub += "\n     )";
			} else {
				sub += "\n    " + field + " ~* '" + stmp + "' ";
			}
		}

		// Starts and ends

		stmp=PhotoUtil.dbquote_str(request.getParameter("tstart"));
		if(stmp != null && stmp.length()>0) {
			if(needao) {
				join=PhotoUtil.dbquote_str(request.getParameter("tstartjoin"));
				if(join==null) {
					join="and";
				}
				sub += " " + join;
			}
			needao=true;
			sub += "\n    a.taken>='" + stmp + "'";
		}

		stmp=PhotoUtil.dbquote_str(request.getParameter("tend"));
		if(stmp != null && stmp.length()>0) {
			if(needao) {
				join=PhotoUtil.dbquote_str(request.getParameter("tendjoin"));
				if(join==null) {
					join="and";
				}
				sub += " " + join;
			}
			needao=true;
			sub += "\n    a.taken<='" + stmp + "'";
		}

		stmp=PhotoUtil.dbquote_str(request.getParameter("start"));
		if(stmp != null && stmp.length()>0) {
			if(needao) {
				join=PhotoUtil.dbquote_str(request.getParameter("startjoin"));
				if(join==null) {
					join="and";
				}
				sub += " " + join;
			}
			needao=true;
			sub += "\n    a.ts>='" + stmp + "'";
		}

		stmp=PhotoUtil.dbquote_str(request.getParameter("end"));
		if(stmp != null && stmp.length()>0) {
			if(needao) {
				join=PhotoUtil.dbquote_str(request.getParameter("endjoin"));
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
		stmp=PhotoUtil.dbquote_str(request.getParameter("order"));
		if(stmp != null) {
			order = stmp;
		} else {
			order = "a.taken";
		}

		// Figure out the direction...
		stmp=PhotoUtil.dbquote_str(request.getParameter("sdirection"));
		if(stmp != null) {
			odirection=stmp;
		} else {
			odirection = "";
		}

		query += "\n order by " + order + " " + odirection;

		return(query);
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
			+ "   a.size,a.taken,a.ts,b.name,a.cat,c.username,b.id\n"
			+ "   from album a, cat b, wwwusers c\n"
			+ "   where a.cat=b.id and a.id=" + image_id
			+ "\n   and a.addedby=c.id\n"
			+ "   and a.cat in (select cat from wwwacl where "
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

		Vector v;
		int i, which;
		boolean thumbnail=false;
		ServletOutputStream out;

		response.setContentType("image/jpeg");
		String s = request.getParameter("photo_id");
		which = Integer.valueOf(s).intValue();

		s=request.getParameter("thumbnail");
		if(s!=null) {
			thumbnail=true;
		}

		// The new swank image extraction object.
		PhotoImage p = new PhotoImage(which, dbs, rhash);


		try {
			// Need a binary output thingy.
			out = response.getOutputStream();

			if(thumbnail) {
				log("Requesting thumbnail");
				v=p.getThumbnail();
			} else {
				log("Requesting full image");
				v=p.getImage();
			}
			logger.log(new PhotoLogImageEntry(remote_uid.intValue(),
				which, p.wasCached(), request));
			for(i = 0; i<v.size(); i++) {
				out.write( (byte[])v.elementAt(i));
			}

		} catch(Exception e) {
			throw new ServletException("IOException:  " + e.getMessage());
		}
	}

	private void doLogView(HttpServletRequest request,
		HttpServletResponse response) throws ServletException {
		String view, out="";

		PhotoLogView logview=new PhotoLogView(dbs, this);

		view=request.getParameter("view");
		if(view==null) {
			throw new ServletException("LogView without view");
		}

		if(view.equalsIgnoreCase("viewers")) {
			String which;
			which=request.getParameter("which");
			if(which==null) {
				throw new ServletException("LogView/viewers without which");
			}
			try {
				out=logview.getViewersOf(Integer.valueOf(which));
			} catch(Exception e) {
				throw new ServletException(e.getMessage());
			}
		}
		send_response(response, out);
	}

	// Tokenize a template file and return the tokenized stuff.
	private String tokenize(String file, Hashtable vars) {
		return(PhotoUtil.tokenize(this, file, vars));
	}
}
