/*
 * Copyright (c) 1999  Dustin Sallings <dustin@spy.net>
 *
 * $Id: PhotoSearch.java,v 1.1 1999/10/12 22:58:33 dustin Exp $
 */

import java.io.*;
import java.sql.*;
import java.util.*;
import java.net.*;
import sun.misc.*;

import javax.servlet.*;
import javax.servlet.http.*;

public class PhotoSearch extends Object {

	// Encode the search from the form stuff.
	public String encodeSearch(HttpServletRequest request) {
		String out = "";
		for(Enumeration e=request.getParameterNames(); e.hasMoreElements();) {
			String param=(String)e.nextElement();
			String values[] = request.getParameterValues(param);

			for(int i = 0; i < values.length; i++) {
				if(values[i].length()>0) {
					out+=URLEncoder.encode(param)+"="
						+ URLEncoder.encode(values[i]) +"&";
				}
			}
		}
		BASE64Encoder base64=new BASE64Encoder();
		System.out.println("Pre-encoded search:  " + out);
		out=base64.encodeBuffer(out.getBytes());
		return(out);
	}

	public void saveSearch(HttpServletRequest request, PhotoUser user)
		throws Exception {
		if(user==null || request==null) {
			throw new Exception("Weird, invalid stuff.");
		}

		if( ! user.canadd ) {
			throw new Exception("No permission to save searches.");
		}

		PhotoDB pdb=null;

		try {
			pdb = new PhotoDB();
			Connection c = pdb.getConn();
			Statement st = c.createStatement();
			String stmp=null, name=null, search=null;

			stmp=request.getParameter("name");
			if(stmp== null && stmp.length() == 0) {
				throw new Exception("Invalid name parameter");
			}
			// Quote it and get rid of the whitespace on the ends.
			name=PhotoUtil.dbquote_str(stmp.trim());

			stmp=request.getParameter("search");
			if(stmp== null && stmp.length() == 0) {
				throw new Exception("Invalid search parameter");
			}
			search=PhotoUtil.dbquote_str(stmp.trim());

			String query = "insert into searches (name, addedby, search)\n"
				+ "\tvalues(\n\t'" + name + "', " + user.id
				+ ", '" + search + "')";

			st.executeUpdate(query);
		} finally {
			pdb.freeDBConn();
		}
	}

	// Build the bigass complex search query.
	public String buildQuery(HttpServletRequest request, Integer remote_uid)
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
					sub += "\n\t" + field + " ~* '"
						+ PhotoUtil.dbquote_str(atmp[i]) + "' ";
				}
				sub += "\n     )";
			} else {
				sub += "\n    " + field + " ~* '"
					+ PhotoUtil.dbquote_str(stmp) + "' ";
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

}
