import java.io.*;
import java.sql.*;
import java.text.*;
import sun.misc.*;

public class GetImage
{

	static Connection photo;
	static Statement st;

	public static void main(String args[])
		throws SQLException, ClassNotFoundException, IOException
	{
		String source;

		// Load the postgres driver.
		Class.forName("postgresql.Driver");

		source="jdbc:postgresql://dhcp-104/photo";

		photo = DriverManager.getConnection(source, "dustin", "");
		st = photo.createStatement();

		getit(Integer.valueOf(args[0]));
	}

	private static void getit(Integer which) throws SQLException, IOException {
		String query;
		BASE64Decoder base64 = new BASE64Decoder();

		query = "select * from image_store where id = " + which +
			" order by line";

		ResultSet rs = st.executeQuery(query);
		while(rs.next()) {
			byte data[];
			data=base64.decodeBuffer(rs.getString(3));
			System.out.write(data);
		}
	}
}
