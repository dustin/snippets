import java.io.*;
import java.sql.*;
import java.util.*;
import java.text.*;
import java.rmi.Naming;

import net.spy.*;
import net.spy.rmi.*;
import net.spy.photo.*;

public class CachePhoto {

	private static Connection events;
	private static ImageServer server=null;

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

		ResultSet rs=st.executeQuery("select count(*) from album");
		rs.next();
		int num=rs.getInt(1);
		rs.close();
		st.close();

		Stats stats=new Stats(num);

		query = "select id from album order by ts desc";

		st = events.createStatement();
		rs = st.executeQuery(query);
		while(rs.next()) {
			int id = rs.getInt(1);
			stats.click();
			stats.start();
			try {
				// true == thumbnail
				PhotoImage data=server.getImage(id, true);
			} catch(Exception e) {
				e.printStackTrace();
			}
			stats.stop();
			System.out.println("Cached " + id
				+ " in " + stats.getLastTime() + " - " + stats.getStats());
		}
	}

	// Inner class for stats
	private static class Stats extends Object {
		private int done=0;
		private int left=0;
		private long startTime=0;
		private long totalTime=0;
		private long lastTime=0;
		private long lastProcessTime=0;

		public Stats(int size) {
			super();

			this.startTime=System.currentTimeMillis();
			this.left=size;
		}

		public void click() {
			left--;
		}

		public void start() {
			lastTime=System.currentTimeMillis();
		}

		public void stop() {
			long thistime=System.currentTimeMillis();
			lastProcessTime=thistime-lastTime;
			done++;
			totalTime+=lastProcessTime;
		}

		public String getLastTime() {
			long lt=lastProcessTime/1000;
			return("" + lt + "s");
		}

		public String getStats() {
			String rv=null;
			try {
				double avgProcessTime=((double)totalTime/(double)done)/1000.0;
				double estimate=avgProcessTime*(double)left;

				java.text.NumberFormat nf=java.text.NumberFormat.getInstance();
				nf.setMaximumFractionDigits(2);

				rv="Avg=" + nf.format(avgProcessTime)
					+ "s, Estimate=" + nf.format(estimate) + "s"
					+ " ("
					+ new java.util.Date(
						System.currentTimeMillis()+(1000*(long)estimate)
						)
					+ ")";
			} catch(Exception e) {
				rv="Error getting stats:  " + e;
			}
			return(rv);
		}
	}
}
