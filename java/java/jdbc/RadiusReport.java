import java.io.*;
import java.sql.*;
import java.util.*;
import java.text.*;

public class RadiusReport {

	static Connection radius;

	public static void main(String args[])
		throws SQLException, ClassNotFoundException, IOException
	{
		String source;

		// Load the postgres driver.
		Class.forName("postgresql.Driver");
		source="jdbc:postgresql://poe.software.net/radius";
		radius = DriverManager.getConnection(source, "dustin", "");

		createTmpTables();

		getHeavyUsers();
		getHeavyUsers2();
		getHeavyUsers3();

		// getUserReport();

		dropTmpTables();

	}

	public static void drop(Statement st, String what) {
		try {
			String query="drop " + what;
			st.executeUpdate(query);
		} catch(Exception e) {
			System.err.print(e.getMessage());
		}
	}

	public static void createTmpTables() throws SQLException, IOException {
		Statement st = radius.createStatement();

		drop(st, "table radiustmp");
		st.executeUpdate("select * into table radiustmp from radius where\n"
			+ "  start > now() - '1 month'::timespan");

		drop(st, "table tmp1");
		st.executeUpdate("select distinct username into table tmp1 "
			+ "from radiustmp");

		st.executeUpdate("alter table tmp1 add column times int");
		st.executeUpdate("alter table tmp1 add column totaltime timespan");
		st.executeUpdate("alter table tmp1 add column avgtime timespan");
		st.executeUpdate("alter table tmp1 add column avgbpsin int");
		st.executeUpdate("alter table tmp1 add column avgbpsout int");

		drop(st, "function numtimes(text)");
		st.executeUpdate("create function numtimes(text) returns int4 as\n"
			+ "  'select count(*) from radiustmp where username= $1'\n"
			+ "  language 'sql'");

		drop(st, "function ttime(text)");
		st.executeUpdate("create function ttime(text) returns timespan as\n"
			+ "  'select timespan(sum(sesstime))\n"
			+ "    from radiustmp where username= $1'\n"
			+ "  language 'sql'");

		drop(st, "function atime(text)");
		st.executeUpdate("create function atime(text) returns timespan as\n"
			+ "  'select timespan(avg(sesstime))\n"
			+ "    from radiustmp where username= $1'\n"
			+ "  language 'sql'");

		drop(st, "function avgoctin(text)");
		st.executeUpdate("create function avgoctin(text) returns int4 as\n"
			+ "  'select sum(inoct)/sum(sesstime)\n"
			+ "    from radiustmp where username= $1'\n"
			+ "  language 'sql'");

		drop(st, "function avgoctout(text)");
		st.executeUpdate("create function avgoctout(text) returns int4 as\n"
			+ "  'select sum(outoct)/sum(sesstime)\n"
			+ "    from radiustmp where username= $1'\n"
			+ "  language 'sql'");

		st.executeUpdate("update tmp1 set times=numtimes(username),\n"
			+ " totaltime=ttime(username), avgtime=atime(username),\n"
			+ " avgbpsin=avgoctin(username), avgbpsout=avgoctout(username)");

		st.executeUpdate("create unique index tmp1_un on tmp1 (username)");

	}

	public static void dropTmpTables() throws SQLException, IOException {
		Statement st = radius.createStatement();
		drop("table radiustmp");
		drop("table tmp1");
	}

	// Get heavy users by total time
	private static void getHeavyUsers() throws SQLException, IOException {
		String query;
		Statement st = radius.createStatement();

		st.executeUpdate("set QUERY_LIMIT to '10'");

		query = "select username, totaltime, times "
			  + "  from tmp1 order by totaltime desc";

		ResultSet rs = st.executeQuery(query);

		System.out.println("Top 10 High Users for the Month:");
		while(rs.next()) {
			System.out.println("\t"
				+ rs.getString(1)
				+ " (" + rs.getString(2) + " in "
				+ rs.getString(3) + " calls)");
		}

		System.out.println("");
	}

	// Top ten average dialup times
	private static void getHeavyUsers2() throws SQLException, IOException {
		String query;
		Statement st = radius.createStatement();

		st.executeUpdate("set QUERY_LIMIT to '10'");

		query = "select username, avgtime, times "
			  + "  from tmp1 order by avgtime desc";

		ResultSet rs = st.executeQuery(query);

		System.out.println("Top 10 People Who Stay Dialed Up Too Long:");
		while(rs.next()) {
			System.out.println("\t"
				+ rs.getString(1)
				+ " (" + rs.getString(2) + " in "
				+ rs.getString(3) + " calls)");
		}

		System.out.println("");
	}

	// Top ten by number of connections
	private static void getHeavyUsers3() throws SQLException, IOException {
		String query;
		Statement st = radius.createStatement();

		st.executeUpdate("set QUERY_LIMIT to '10'");

		query = "select username, times "
			  + "  from tmp1 order by times desc";

		ResultSet rs = st.executeQuery(query);

		System.out.println("Top 10 People Who Dialed Up a Lot:");
		while(rs.next()) {
			System.out.println("\t"
				+ rs.getString(1)
				+ " (" + rs.getString(2) + " calls)");
		}

		System.out.println("");
	}

	private static void getUserReport() throws SQLException, IOException {
		String query;
		Statement st = radius.createStatement();
		Vector users = new Vector();

		st.executeUpdate("set QUERY_LIMIT to '0'");

		query = "select distinct username from radius\n"
			  + "  where start > now() - '1 month'::timespan";

		ResultSet rs = st.executeQuery(query);
		while(rs.next()) {
			users.addElement(rs.getString(1));
		}
	}
}
