import java.io.*;
import java.util.*;
import java.sql.*;

import net.spy.netinfo.*;

public class NetInfo {

	public static void main(String args[]) throws Exception {
		process(args[0]);
	}

	public static void process(String file) throws Exception {
		BufferedReader f = new BufferedReader(new FileReader(file));
		String line;
		Stack stack=new Stack();
		Connection netinfodb = null;
		String source="jdbc:postgresql://localhost/netinfo";

		while( (line=f.readLine()) != null) {
			try {
				String host, sport;
				int port;
				StringTokenizer st = new StringTokenizer(line);
				host=st.nextToken();
				sport=st.nextToken();
				port = Integer.valueOf(sport).intValue();

				// System.out.println(host + ":" + port);
				// Info i = new Info(host, port);
				// System.out.println(host + ":" + port + ":  " + i.summary());

				stack.push( new Service(host, port));

			} catch(Exception e) {
				System.err.println("Line exception:  " + line + " " + e);
			}
		}

		Class.forName("postgresql.Driver");
		netinfodb=DriverManager.getConnection(source, "dustin", "");

		Thread t=null;
		for(int i=0; i<200; i++) {
			t=new Getit(stack, netinfodb);
			t.run();
		}
	}

}
