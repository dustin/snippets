import java.io.*;
import java.sql.*;
import java.text.*;
import PGReplicate;

public class DoIt
{
	public static void main(String args[])
		throws SQLException, ClassNotFoundException
	{
		Connection s, d;
		PGReplicate rep;
		String source, destination;

		// Load the postgres driver.
		Class.forName("postgresql.Driver");

		source="jdbc:postgresql://bleu/photo";
		destination="jdbc:postgresql://dhcp-104/photo";

		System.out.println("Java sucks.");
		System.out.println("Getting source connection: " + source);
		s = DriverManager.getConnection(source, "dustin", "");
		System.out.println("Getting destination connection: " + destination);
		d = DriverManager.getConnection(destination, "dustin", "");

		rep=new PGReplicate(s, d);

		rep.rep_table("image_store");
	}
}
