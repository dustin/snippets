import java.io.*;
import java.sql.*;
import java.util.*;
import java.text.*;
import java.rmi.Naming;

import net.spy.*;
import net.spy.rmi.*;
import net.spy.util.*;

public class CachePhoto {

	static Connection events;
	static ImageServer server=null;

	public static void main(String args[]) throws Exception
	{
		String source;
		server = (ImageServer)Naming.lookup("//localhost/ImageServer");

		// Load the postgres driver.
		Class.forName("postgresql.Driver");
		source="jdbc:postgresql://localhost/photo";
		events = DriverManager.getConnection(source, "dustin", "");

		try {
			getit();
		} catch(Exception e) {
			System.err.println("Got an exception:  " + e);
		}
		System.exit(0);
	}

	private static void getit() throws Exception {
		String query;
		Statement st;
		st = events.createStatement();
		Vector v = new Vector();

		query = "select id from album order by ts desc";

		ResultSet rs = st.executeQuery(query);
		while(rs.next()) {
			int id = rs.getInt(1);
			ImageData data=server.getImage(id, true);
			System.out.println("Caching " + id);
		}
	}
}
