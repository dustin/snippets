import java.io.*;
import java.sql.*;
import java.util.*;
import java.text.*;

public class EventCount {

	static Connection events;

	public static void main(String args[])
		throws SQLException, ClassNotFoundException, IOException
	{
		String source;

		// Load the postgres driver.
		Class.forName("postgresql.Driver");
		source="jdbc:postgresql://poe.software.net/events";
		events = DriverManager.getConnection(source, "dustin", "");

		getit();
		System.exit(0);
	}

	private static void getit() throws SQLException, IOException {
		String query;
		Statement st;
		st = events.createStatement();
		RHash r = new RHash("//verde.software.net/RObjectServer");
		Vector v = new Vector();

		query = "select * from counts order by count";

		ResultSet rs = st.executeQuery(query);
		while(rs.next()) {
			Hashtable h = new Hashtable();

			h.put("tag_id", rs.getString(1));
			h.put("tag_name", rs.getString(2));
			h.put("count", rs.getString(3));

			v.addElement(h);

			System.out.println(rs.getString(2) + " (" + rs.getString(3) + ")");
		}

		r.put("event_count", v);
	}
}
