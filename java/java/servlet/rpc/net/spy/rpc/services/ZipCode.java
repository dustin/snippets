// Copyright (c) 2001  Dustin Sallings <dustin@spy.net>
//
// $Id: ZipCode.java,v 1.2 2002/03/05 09:00:49 dustin Exp $

package net.spy.rpc.services;

import java.sql.*;
import java.util.*;

import net.spy.*;
import net.spy.db.*;

/**
 * Get zipcode information.
 */
public class ZipCode extends Object {

	/**
	 * Get an instance of ZipCode.
	 */
	public ZipCode() {
		super();
	}

	private static SpyConfig getConf() {
		return(new SpyConfig(
			new java.io.File("/afs/spy.net/misc/web/etc/zip.conf")));
	}

	private Hashtable getZipData(ResultSet rs) throws Exception {
		Hashtable rv=new Hashtable();
		rv.put("zipcode", new Integer(rs.getInt("zipcode")));
		rv.put("state_code", rs.getString("state_code"));
		rv.put("state", rs.getString("state"));
		rv.put("city", rs.getString("city"));
		rv.put("county", rs.getString("county"));
		rv.put("longitude", new Double(rs.getString("longitude")));
		rv.put("latitude", new Double(rs.getString("latitude")));
		return(rv);
	}

	/**
	 * Lookup a zipcode.
	 */
	public Hashtable lookupZip(int zipcode) throws Exception {
		Hashtable h=null;

		SpyCacheDB db=new SpyCacheDB(getConf());
		PreparedStatement pst=db.prepareStatement(
			"select * from zipcode_view where zipcode=?", 900);
		pst.setInt(1, zipcode);
		ResultSet rs=pst.executeQuery();

		try {
			if(!rs.next()) {
				throw new Exception(zipcode + " not found in this database.");
			}
			h=getZipData(rs);
		} finally {
			rs.close();
			pst.close();
			db.close();
		}

		return(h);
	}

	private boolean isValidKey(String key) {
		boolean isvalid=false;

		if(key.equals("zipcode")) {
			isvalid=true;
		} else if(key.equals("state_code")) {
			isvalid=true;
		} else if(key.equals("state")) {
			isvalid=true;
		} else if(key.equals("city")) {
			isvalid=true;
		} else if(key.equals("county")) {
			isvalid=true;
		}

		return(isvalid);
	}

	/**
	 * Lookup zipcodes by a specification.  The specification defines the
	 * limiters of the query.  The following attributes may be set (all
	 * lowercase):
	 *
	 * <ul>
	 *  <li>zipcode - To specify a single zipcode to look up</li>
	 *  <li>state_code - Two character state code (all caps)</li>
	 *  <li>state - Full name of the state</li>
	 *  <li>city - Name of the city</li>
	 *  <li>county - Name of the county</li>
	 * </ul>
	 *
	 * @return a Vector of Hashtables representing the zipcodes.  No more
	 * than 250 records may be returned.
	 */
	public Vector lookupZips(Hashtable spec) throws Exception {
		Vector keys=new Vector();
		for(Enumeration e=spec.keys(); e.hasMoreElements();) {
			keys.addElement(e.nextElement());
		}

		StringBuffer query=new StringBuffer();
		query.append("select * from zipcode_view ");

		if(keys.size() > 0) {
			query.append("where ");
			for(Enumeration e=keys.elements(); e.hasMoreElements(); ) {
				String k=(String)e.nextElement();

				if(!isValidKey(k)) {
					throw new Exception(k + " is an invalid parameter.");
				}

				query.append(k);
				query.append("=?");

				if(e.hasMoreElements()) {
					query.append(" and ");
				}
			}
		}

		query.append(" limit 250");

		SpyDB db=new SpyDB(getConf());
		PreparedStatement pst=db.prepareStatement(query.toString());

		for(int i=0; i<keys.size(); i++) {
			String k=(String)keys.elementAt(i);
			pst.setString(i+1, spec.get(k).toString());
		}

		System.err.println("Query:  " + pst);

		ResultSet rs=pst.executeQuery();
		Vector rv=new Vector();
		while(rs.next()) {
			rv.addElement(getZipData(rs));
		}
		rs.close();
		pst.close();
		db.close();

		return(rv);
	}

	/**
	 * Testing and what not.
	 */
	public static void main(String args[]) throws Exception {
		ZipCode zc=new ZipCode();
		Hashtable h=new Hashtable();
		for(int i=0; i<args.length; i+=2) {
			h.put(args[i], args[i+1]);
		}

		System.out.println(zc.lookupZips(h));
	}

}
