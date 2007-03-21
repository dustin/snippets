// Copyright (c) 2001  Dustin Sallings <dustin@spy.net>
//
// $Id$

import java.io.*;
import java.sql.*;

/**
 * 
 */
public class AddBlob extends Object {

	/**
	 * Get an instance of AddBlob.
	 */
	public AddBlob() {
		super();
	}

	/**
	 * Testing and what not.
	 */
	public static void main(String args[]) throws Exception {
		Class.forName(args[0]);
		Connection conn=DriverManager.getConnection(args[1], args[2], args[3]);
		conn.setAutoCommit(false);

		File file=new File(args[4]);
		FileInputStream fis=new FileInputStream(file);
		PreparedStatement pst=conn.prepareStatement(
			"insert into shit values(?,?)");
		pst.setString(1, file.getName());
		pst.setBinaryStream(2, fis, (int)file.length());
		pst.executeUpdate();
		pst.close();
		fis.close();

		conn.commit();
	}

}

