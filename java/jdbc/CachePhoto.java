import java.io.*;
import java.sql.*;
import java.util.*;
import java.text.*;
import java.rmi.Naming;

import net.spy.*;
import net.spy.rmi.*;
import net.spy.photo.*;

public class CachePhoto {

	static Connection events;
	static ImageServer server=null;

	public static void main(String args[]) throws Exception
	{
		String source;
		server = (ImageServer)Naming.lookup("//dhcp-104/ImageServer");

		// Load the postgres driver.
		Class.forName("org.postgresql.Driver");
		source="jdbc:postgresql://dhcp-104/photo";
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

		query = "select id from album order by ts desc";

		ResultSet rs = st.executeQuery(query);
		while(rs.next()) {
			int id = rs.getInt(1);
			try {
				// true == thumbnail
				PhotoImage data=server.getImage(id, true);
			} catch(Exception e) {
				System.err.println("Fuck:  " + e);
			}
			System.out.println("Caching " + id);
		}
	}
}
