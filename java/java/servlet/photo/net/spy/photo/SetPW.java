/*
 * Copyright (c) 1999 Dustin Sallings <dustin@spy.net>
 *
 * $Id: SetPW.java,v 1.3 2000/05/30 04:53:22 dustin Exp $
 */

package net.spy.photo;

import java.sql.*;
import java.io.*;

import net.spy.*;

public class SetPW {
	public static void main(String args[]) {
		try {
			BufferedReader rd=
				new BufferedReader(new InputStreamReader(System.in));
			PhotoConfig config = new PhotoConfig();
			PhotoSecurity security=new PhotoSecurity();

			String user = null;
			if(args.length<1) {
				System.out.print("Enter username:  ");
				user=rd.readLine();
			} else {
				user = args[0];
			}

			String newpw=null;
			if(args.length<2) {
				System.out.print("Enter password:  ");
				newpw=rd.readLine();
				newpw=security.getDigest(newpw);
			} else {
				newpw = security.getDigest(args[1]);
			}

			System.out.println("New password is " + newpw);

			Class.forName(config.get("dbDriverName"));
			Connection db =
			DriverManager.getConnection(config.get("dbSource"),
				config.get("dbUser"), config.get("dbPass"));
			String query="update wwwusers set password='" + newpw + "'\n"
				+ "\twhere username='" + user + "'";
			Statement st = db.createStatement();
			st.executeUpdate(query);
		} catch(Exception e) {
			System.out.println(e);
			e.printStackTrace();
		}
	}
}
