// Copyright (c) 1999  Dustin Sallings <dustin@spy.net>

import java.lang.*;
import java.sql.*;

public class DBDescribe {
	static Connection db;

	public static void main(String args[])
		throws SQLException, ClassNotFoundException
	{
		if(args.length < 4) {
			System.out.println("Usage:  DBDescribe driver source user pass");
		} else {
			describe(args[0], args[1], args[2], args[3]);
		}
	}

	public static void describe(String driver, String source,
		String u, String p) throws SQLException, ClassNotFoundException {
		System.out.println("Driver:  " + driver + "\n"
			+ "Source:  " + source + "\n"
			+ "User:  " + u + "\n");

		Class.forName(driver);
		db = DriverManager.getConnection(source, u, p);
		DatabaseMetaData meta = db.getMetaData();
		ResultSet rs = meta.getTables("", "", null, null);

		while(rs.next()) {
			System.out.println("Cat:\t" + rs.getString(1));
			System.out.println("Schem:\t" + rs.getString(2));
			System.out.println("Name:\t" + rs.getString(3));
			System.out.println("Type:\t" + rs.getString(4));
			System.out.println("Remarks:\t" + rs.getString(5));
			System.out.println();
		}
	}
}
