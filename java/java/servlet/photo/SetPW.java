/*
 * Copyright (c) 1999 Dustin Sallings <dustin@spy.net>
 *
 * $Id: SetPW.java,v 1.1 1999/10/10 08:03:59 dustin Exp $
 */

import java.sql.*;
import java.io.*;

public class SetPW {
	public static void main(String args[]) {
		try {
			BufferedReader rd=
				new BufferedReader(new InputStreamReader(System.in));
			PhotoConfig config = new PhotoConfig();
			Class.forName(config.dbDriverName);
			Connection db = DriverManager.getConnection(config.dbSource,
				config.dbUser, config.dbPass);
			PhotoSecurity security=new PhotoSecurity(null);

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
