// Copyright (c) 2001  Dustin Sallings <dustin@spy.net>
//
// $Id: ZipCode.java,v 1.1 2002/03/05 05:40:34 dustin Exp $

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

	/**
	 * Lookup a zipcode.
	 */
	public Hashtable lookupZip(int zipcode) throws Exception {
		Hashtable h=new Hashtable();

		SpyCacheDB db=new SpyCacheDB(getConf());
		PreparedStatement pst=db.prepareStatement(
			"select * from zipcode_view where zipcode=?", 900);
		pst.setInt(1, zipcode);
		ResultSet rs=pst.executeQuery();

		if(rs.next()) {
			h.put("zipcode", new Integer(rs.getInt("zipcode")));
			h.put("state_code", rs.getString("state_code"));
			h.put("state", rs.getString("state"));
			h.put("city", rs.getString("city"));
			h.put("county", rs.getString("county"));
			h.put("longitude", new Double(rs.getString("longitude")));
			h.put("latitude", new Double(rs.getString("latitude")));
		} else {
			rs.close();
			pst.close();
			db.close();
			throw new Exception(zipcode + " not found in this database.");
		}
		rs.close();
		pst.close();
		db.close();

		return(h);
	}

	/**
	 * Testing and what not.
	 */
	public static void main(String args[]) throws Exception {
		ZipCode zc=new ZipCode();

		System.out.println(zc.lookupZip(Integer.parseInt(args[0])));
	}

}
