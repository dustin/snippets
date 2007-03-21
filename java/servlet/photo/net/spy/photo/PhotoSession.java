/*
 * Copyright (c) 1999 Dustin Sallings
 *
 * $Id: PhotoSession.java,v 1.13 2000/06/24 08:03:16 dustin Exp $
 */

package net.spy.photo;

import java.io.*;
import java.sql.*;
import java.util.*;
import java.net.*;
import sun.misc.*;

import javax.servlet.*;
import javax.servlet.http.*;

import com.oreilly.servlet.*;

import net.spy.*;
import net.spy.util.*;

// The class
public class PhotoSession extends Object
{ 
	// This kinda stuff is only persistent for a single connection.
	protected Integer remote_uid=null;
	protected String remote_user=null, self_uri=null;
	protected RHash rhash=null;
	protected MultipartRequest multi=null;
	protected SpyLog logger=null;
	protected PhotoSecurity security = null;
	protected HttpSession session=null;

	protected PhotoStorerThread storer_thread = null;

	protected PhotoServlet photo_servlet = null;

	HttpServletRequest request=null;
	HttpServletResponse response=null;

	// Users
	protected Hashtable userdb=null;

	public PhotoSession(PhotoServlet p,
		HttpServletRequest request,
		HttpServletResponse response) {

		photo_servlet=p;
		this.request=request;
		this.response=response;
		this.session=request.getSession(false);

		logger=p.logger;

		self_uri = p.self_uri;
		rhash=p.rhash;
		security=p.security;
		userdb=p.userdb;
	}

	protected void log(String whu) {
		photo_servlet.log(whu);
	}

	// process a request
	public void process() throws ServletException, IOException {

		String func, type;

		type = request.getContentType();
		if(type != null && type.startsWith("multipart/form-data")) {
			multi = new MultipartRequest(request, "/tmp");
		} else {
			multi = null;
		}

		// Set the self_uri
		self_uri = request.getRequestURI();

		getCreds();

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
			doFind();
		} else if(func.equalsIgnoreCase("nextresults")) {
			displaySearchResults();
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
			doDisplay();
		} else if(func.equalsIgnoreCase("logview")) {
			doLogView(request, response);
		} else if(func.equalsIgnoreCase("getimage")) {
			showImage(request, response);
		} else if(func.equalsIgnoreCase("credform")) {
			showCredForm(request, response);
		} else if(func.equalsIgnoreCase("savesearch")) {
			saveSearch(request, response);
		} else if(func.equalsIgnoreCase("setcred")) {
			setCreds(request, response);
			doIndex(request, response);
		} else {
			throw new ServletException("No known function.");
		}
	}

	protected void saveSearch(HttpServletRequest request,
		HttpServletResponse response)
		throws ServletException {
		PhotoSearch ps = new PhotoSearch();
		PhotoUser user = (PhotoUser)userdb.get(remote_user);
		String output="";

		try {
			ps.saveSearch(request, user);
			output=tokenize("addsearch_success.inc", new Hashtable());
		} catch(Exception e) {
			Hashtable h = new Hashtable();
			h.put("MESSAGE", e.getMessage());
			output=tokenize("addsearch_fail.inc", h);
		}
		send_response(response, output);
	}

	protected void getCreds() throws ServletException {
		getUid();
		log("Authenticated as " + remote_user);
	}

	// Show the style form
	protected void showCredForm (
		HttpServletRequest request, HttpServletResponse response)
		throws ServletException {
		String output;

		output = tokenize("authform.inc", new Hashtable());
		send_response(response, output);
	}

	public void setCreds (
		HttpServletRequest request, HttpServletResponse response
	) throws ServletException, IOException {
		String username=null, pass=null;

		username=request.getParameter("username");
		pass=request.getParameter("password");

		log("Verifying password for " + username);
		if(security.checkPW(username, pass)) {
			if(session==null) {
				session=request.getSession(true);
			}
			session.putValue("username", username);
			// Make it valid immediately
			remote_user = "dustin";
			getUid();
		}
	}


	// We need to reinitialize if something bad happens and we can tell..
	protected void reInitialize() {
		log("Application would like to reinitialize.");
		try {
			SpyDB db=new SpyDB(new PhotoConfig(), false);
			db.init();
		} catch(Exception e) {
			// Do nothing.
		}
	}

	// Grab a connection from the pool.
	protected Connection getDBConn() throws Exception {
		SpyDB pdb=new SpyDB(new PhotoConfig(), false);
		return(pdb.getConn());
	}

	// Gotta free the connection
	protected void freeDBConn(Connection conn) {
		SpyDB pdb=new SpyDB(new PhotoConfig(), false);
		pdb.freeDBConn(conn);
	}

	// Get the saved searches.
	protected String showSaved() {
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
		} catch(Exception e) {
			// Nothing
		} finally {
			if(photo != null) {
				freeDBConn(photo);
			}
		}
		return(out);
	}

	// Find out if the authenticated user can add stuff.
	protected boolean canadd() {
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
	protected void doAddPhoto(
		HttpServletRequest request, HttpServletResponse response)
		throws ServletException {
		String category="", keywords="", picture="", info="", taken="";
		String query="", out="", stmp, type;
		int id;
		Hashtable h = new Hashtable();
		Connection photo=null;
		Statement st=null;

		// We need a short lifetime for whatever page this produces
        long l=new java.util.Date().getTime();
        l+=10000L;
        response.setDateHeader("Expires", l);


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
			Vector v = new Vector();
			int bufsize=1024;
			byte data[] = new byte[bufsize];

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
			int size=0, length=0;
			in = new FileInputStream(f);

			while( (length=in.read(data)) >=0 ) {
				String tmp;

				size+=length;
				if(length == bufsize) {
					byte tb[] = new byte[length];
					int j;

					for(j=0; j<length; j++) {
						tb[j] = data[j];
					}
					v.addElement(tb);
				} else {
					byte tb[] = new byte[length];
					int j;

					for(j=0; j<length; j++) {
						tb[j] = data[j];
					}
					v.addElement(tb);
				}
			}
			query ="update album set size=" + size + "where id=" + id;
			st.executeUpdate(query);

			PhotoImage photo_image=new PhotoImage(id);
			photo_image.storeImage(new ImageData(v));

			query = "insert into upload_log values(\n"
				  + "\t" + id + ", " + remote_uid + ")";
			st.executeUpdate(query);

			h.put("ID", ""+id);

			photo.commit();
			out += tokenize("add_success.inc", h);
		} catch(Exception e) {
			log(e.getMessage());
			try {
				photo.rollback();
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
	protected String getCatList() {
		String query, out="";
		Connection photo=null;
		try {
			photo=getDBConn();
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
		} catch(Exception e) {
			// Nothing
		} finally {
				if(photo != null) {
					freeDBConn(photo);
				}
		}
		return(out);
	}

	// Show the style form
	protected void doStyleForm (
		HttpServletRequest request, HttpServletResponse response)
		throws ServletException {
		String output;

		output = tokenize("presetstyle.inc", new Hashtable());
		send_response(response, output);
	}

	// Get the stylesheet from the cookie, or the default.
	protected void doGetStylesheet (
		HttpServletRequest request, HttpServletResponse response)
		throws ServletException {
		Cookie cookies[];
		String output = null;
		int i;

		cookies = request.getCookies();

		if(cookies!=null) {
			for(i=0; i<cookies.length && output == null; i++) {
				String s = cookies[i].getName();
				if(s.equalsIgnoreCase("photo_style")) {
					output = cookies[i].getValue();
				}
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
	protected void doSetStyle(
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
	protected void doAddForm(
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
	protected void doFindForm(
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
	protected void doCatView(
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
	protected void doIndex(
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
	protected void getUid() throws ServletException {
		String query;

		try {
			if(session==null) {
				remote_user="guest";
			} else {
				remote_user=(String)session.getValue("username");
				// If we have a session, but no username, add guest.
				if(remote_user==null) {
					remote_user="guest";
					session.putValue("username", "guest");
				}
			}
			PhotoUser p=(PhotoUser)userdb.get(remote_user);
			remote_uid = p.id;
		} catch(Exception e) {
			throw new ServletException("Unknown user: " + remote_user);
		}
	}

	// Display dispatcher
	protected void doDisplay() throws ServletException {
		String id=null;
		String search_id=null;

		id = request.getParameter("id");
		search_id = request.getParameter("search_id");

		if(id!=null) {
			doDisplayByID();
		} else if(search_id!=null) {
			doDisplayBySearchId();
		}
	}

	protected void doDisplayBySearchId() throws ServletException {
		PhotoSearchResults results=null;
		results=(PhotoSearchResults)session.getValue("search_results");
		int which=Integer.parseInt(request.getParameter("search_id"));
		PhotoSearchResult r = results.get(which);
		Hashtable h = new Hashtable();

		h.put("IMAGE",     r.image);
		h.put("KEYWORDS",  r.keywords);
		h.put("INFO",      r.descr);
		h.put("SIZE",      r.size);
		h.put("TAKEN",     r.taken);
		h.put("TIMESTAMP", r.ts);
		h.put("CAT",       r.cat);
		h.put("CATNUM",    r.catnum);
		h.put("ADDEDBY",   r.addedby);

		if(results.nResults() > which+1) {
			h.put("NEXT",
				"<a href=\"" + self_uri + "?func=display&search_id="
				+ (which+1) + "\">&gt;&gt;&gt;</a><br>");
		} else {
			h.put("NEXT", "");
		}

		if(which>0) {
			h.put("PREV",
				"<a href=\"" + self_uri + "?func=display&search_id="
				+ (which-1) + "\">&lt;&lt;&lt;</a><br>");
		} else {
			h.put("PREV", "");
		}

		String output = tokenize("display.inc", h);
		send_response(response, output);
	}

	// Find and display images.
	protected void doDisplayByID() throws ServletException {
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

			// These don't apply here.
			h.put("PREV",      "");
			h.put("NEXT",      "");

		} catch(SQLException e) {
			throw new ServletException("Some kinda SQL problem.");
		}
		finally { freeDBConn(photo); }
		output += tokenize("display.inc", h);

		send_response(response, output);
	}

	// Send the response text...
	protected void send_response(HttpServletResponse response, String text)
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

	// Display search results
	// This whole thing will fail if there's no session.
	protected void displaySearchResults() throws ServletException {
		if(session==null) {
			throw new ServletException("There's no session!");
		}
		PhotoSearchResults results=
			(PhotoSearchResults)session.getValue("search_results");
		if(results==null) {
			throw new ServletException("There are no search results!");
		}

		String middle="";
		Hashtable h=null;
		int i=0;
		// if we have a starting point, let's start there.
		try {
			String startingS=request.getParameter("startfrom");
			if(startingS==null) {
				int starting=Integer.parseInt(startingS);
				results.set(starting);
			}
		} catch(Exception e) {
			// If there's an exception, something went wrong with finding
			// where to start from.  This is OK, we'll just start from
			// where we were when we last displayed a page.
		}

		for(i=0; i<5; i++) {
			PhotoSearchResult r=results.next();
			if(r!=null) {
				h = new Hashtable();
				h.put("KEYWORDS", r.keywords);
				h.put("DESCR",    r.descr);
				h.put("CAT",      r.cat);
				h.put("SIZE",     r.size);
				h.put("TAKEN",    r.taken);
				h.put("TS",       r.ts);
				h.put("IMAGE",    r.image);
				h.put("CATNUM",   r.catnum);
				h.put("ADDEDBY",  r.addedby);
				h.put("ID",       "" + r.id);

				if( ((i) % 2) == 0) {
					middle += "</tr>\n<tr>\n";
				}

				middle += "<td>\n";
				middle += tokenize("findmatch.inc", h);
				middle += "</td>\n";
			}
		}

		h = new Hashtable();
		h.put("TOTAL", "" + results.nResults());
		h.put("SEARCH", (String)session.getValue("encoded_search"));
		String output = tokenize("find_top.inc", h);
		output += middle;
		h.put("LINKTOMORE", linkToMore(results)); 
		output += tokenize("find_bottom.inc", h);
		send_response(response, output);
	}

	// Find images.
	protected void doFind() throws ServletException {
		String output = "", middle = "";
		PhotoSearch ps = new PhotoSearch();
		PhotoUser user = (PhotoUser)userdb.get(remote_user);

		PhotoSearchResults results=null;

		// Make sure there's a real session.
		if(session==null) {
			session=request.getSession(true);
		}

		// Get the results and put them in the mofo session
		results=ps.performSearch(request, user);
		session.putValue("search_results", results);
		session.putValue("encoded_search",
			ps.encodeSearch(request));

		displaySearchResults();
	}

	// Link to more search results
	protected String linkToMore(PhotoSearchResults results) {
		String ret = "";
		int remaining=results.nRemaining();

		if(remaining>0) {
			int nextwhu=5;
			if(remaining<5) {
				nextwhu=remaining;
			}

			ret += "<form method=\"POST\" action=\"" + self_uri + "\">\n";
			ret += "<input type=hidden name=func value=nextresults>\n";
			ret += "<input type=hidden name=startfrom value="
				+ results.current() + ">\n";

			ret += "<input type=\"submit\" value=\"Next " + nextwhu + "\">\n";
			ret += "</form>\n";
			ret += "<br>\n" + remaining + " pictures remaining.<br>\n";
		}
		return(ret);
	}

	// Show an image
	protected void showImage(HttpServletRequest request,
		HttpServletResponse response) throws ServletException {

		Vector v;
		int i, which;
		boolean thumbnail=false;
		ServletOutputStream out;

		response.setContentType("image/jpeg");
		java.util.Date d=new java.util.Date();
		long l=d.getTime();
		// This is thirty days
		l+=25920000000L;
		response.setDateHeader("Expires", l);

		String s = request.getParameter("photo_id");
		which = Integer.valueOf(s).intValue();

		s=request.getParameter("thumbnail");
		if(s!=null) {
			thumbnail=true;
		}

		if(rhash==null || !rhash.connected()) {
			throw new ServletException("Me hath no working rhash");
		}

		try {
			// The new swank image extraction object.
			PhotoImage p = new PhotoImage(which, rhash);

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
				which, true, request));
			for(i = 0; i<v.size(); i++) {
				out.write( (byte[])v.elementAt(i));
			}

		} catch(Exception e) {
			throw new ServletException("IOException:  " + e.getMessage());
		}
	}

	protected void doLogView(HttpServletRequest request,
		HttpServletResponse response) throws ServletException {
		String view, out="";
		PhotoLogView logview=null;

		try {
			logview=new PhotoLogView(this);
		} catch(Exception e) {
			throw new ServletException(e.getMessage());
		}

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
	protected String tokenize(String file, Hashtable vars) {
		return(PhotoUtil.tokenize(this, file, vars));
	}
}
