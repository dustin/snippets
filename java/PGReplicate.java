import java.io.*;
import java.sql.*;
import java.text.*;

// Java port of my PG_REPLICATE Eiffel object.

public class PGReplicate
{
	Connection source, destination;
	Statement sst, dst;

	// Constructor
	public PGReplicate(Connection s, Connection d) throws SQLException {
		this.source=s;
		this.destination=d;

		this.sst=source.createStatement();
		this.dst=destination.createStatement();
	}

	// Replicate a table
	public void rep_table(String table) throws SQLException {
		String query;

		// Begin transaction on destination, and delete
		dst.executeUpdate("begin transaction");
		dst.executeUpdate("delete from " + table);

		// Begin transaction on source, and declare cursor
		sst.executeUpdate("begin transaction");
		// sst.executeUpdate("declare c cursor for select * from " + table);
		// sst.executeUpdate("declare c cursor for select * from " + table);

		// ResultSet rs = sst.executeQuery("fetch 1000 in c");
		ResultSet rs = sst.executeQuery("select * from " + table);
		while(rs.next()) {
			query = new String("insert into " + table + " values(" +
				join(", ", rs) + ")");
			System.out.println(query);
		}

		// Commit our transactions.
		sst.executeUpdate("commit");
		dst.executeUpdate("commit");
	}

	protected String join(String with, ResultSet r) throws SQLException {
		ResultSetMetaData rsmd = r.getMetaData();
		int cols = rsmd.getColumnCount();
		StringBuffer s = new StringBuffer("");
		int i;

		for(i=1 ; i < cols - 1 ; i ++) {
			s.append(dbquote(r.getString(i)));
			s.append(with);
		}
		s.append(dbquote(r.getString(i+1)));
		return(s.toString());
	}

	protected String dbquote(String what) {
		StringBuffer s = new StringBuffer("'");
		int i;

		for(i=0; i<what.length(); i++) {
			if(what.charAt(i) == '\'')
				s.append("'");
			s.append(what.charAt(i));
		}
		s.append("'");
		return(s.toString());
	}

	protected void finalize() throws Throwable {
		System.out.println("Replication destructor called.");
		sst.close();
		dst.close();
	}
}
