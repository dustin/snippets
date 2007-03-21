// Copyright (c) 2001  Dustin Sallings <dustin@spy.net>
//
// $Id$

import java.io.*;
import java.sql.*;

/**
 * 
 */
public class GetBlob extends Object {

	/**
	 * Get an instance of GetBlob.
	 */
	public GetBlob() {
		super();
	}

	/**
	 * Testing and what not.
	 */
	public static void main(String args[]) throws Exception {
		Class.forName(args[0]);
		Connection conn=DriverManager.getConnection(args[1], args[2], args[3]);
		conn.setAutoCommit(false);

		PreparedStatement pst=conn.prepareStatement(
			"select fileoid from shit where name=?");
		pst.setString(1, args[4]);
		ResultSet rs=pst.executeQuery();

		while(rs.next()) {
			InputStream is=rs.getBinaryStream(1);
			FileOutputStream fos=new FileOutputStream(args[4]);

			byte buffer[]=new byte[8192];
			int bread=is.read(buffer);
			while(bread>0) {
				fos.write(buffer, 0, bread);
				bread=is.read(buffer);
			}
			fos.close();
			is.close();
		}

		rs.close();
		pst.close();

		conn.commit();
	}

}

