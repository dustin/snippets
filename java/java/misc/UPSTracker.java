// Copyright (c) 2000  Dustin Sallings <dustin@spy.net>
//
// $Id: UPSTracker.java,v 1.2 2000/05/01 04:32:26 dustin Exp $

import java.sql.*;
import java.util.*;

import net.spy.net.SNPP;
import net.spy.info.*;

public class UPSTracker extends Object {
	Connection conn=null;

	public UPSTracker() {
		super();
	}

	protected void openDB() throws Exception {
		if(conn==null) {
			String source="jdbc:postgresql://dhcp-104/misc";
			conn=DriverManager.getConnection(source, "dustin", "");
		}
	}

	protected Hashtable getCurrentStatus() throws Exception {
		Hashtable ret=new Hashtable();
		Statement st = conn.createStatement();
		String query = "select * from ups_status";
		ResultSet rs = st.executeQuery(query);
		while(rs.next()) {
			ret.put(rs.getString("tracking_number"),
				rs.getString("status_str"));
		}
		return(ret);
	}

	protected Hashtable getChanges(Hashtable stati) throws Exception {
		Hashtable ret=new Hashtable();
		Statement st = conn.createStatement();
		String query = "select distinct tracking_number from ups_packages";
		ResultSet rs = st.executeQuery(query);
		while(rs.next()) {
			String n=rs.getString("tracking_number");
			UPS u = new UPS(n);
			String status=u.toString();
			String current=(String)stati.get(n);
			if(current==null) {
				current="";
			}

			// If they're not the same, add the current UPS object to the
			// changes hash.
			if(!current.equals(status)) {
				ret.put(n, u);
			}
		}
		return(ret);
	}

	protected void saveChanges(Hashtable changes) throws Exception {
		SNPP snpp=new SNPP("snpp.skytel.com", 444);
		for(Enumeration e=changes.keys(); e.hasMoreElements(); ) {
			String key=(String)e.nextElement();

			// Delete it just in case.
			PreparedStatement st = conn.prepareStatement(
				"delete from ups_status where tracking_number = ?"
			);
			st.setString(1, key);
			st.executeUpdate();
			st = conn.prepareStatement(
				"insert into ups_status values(?, ?, ?)"
			);

			// Add it if it's new
			UPS u=(UPS)changes.get(key);
			st.setString(1, key);
			st.setString(2, u.get("Package Status"));
			st.setString(3, u.toString());
			st.executeUpdate();

			st = conn.prepareStatement(
				"select * from ups_packages where tracking_number = ?"
			);
			st.setString(1, key);
			ResultSet rs = st.executeQuery();
			while(rs.next()) {
				String id=rs.getString("pager_id");
				String descr=rs.getString("descr");
				// This is our message.
				String msg="UPS Tracking:\r\n" + key
					+ " (" + descr + ")\r\n" + u;
				snpp.message(msg);
				snpp.pagerID(id);
				snpp.send();
				// Reset so we can reuse this connection.
				snpp.cmd("reset");
			}
		}
		snpp.close();
	}

	protected void cleanup() throws Exception {
		try {
			conn.setAutoCommit(false);
			Statement st = conn.createStatement();
			String query="delete from ups_packages where tracking_number in "
				+ "(select tracking_number from ups_status where "
				+   "status='Delivered')";
			st.executeUpdate(query);
			query="delete from ups_status where status='Delivered'";
			st.executeUpdate(query);
			conn.commit();
		} catch(Exception e) {
			conn.rollback();
		} finally {
			conn.setAutoCommit(true);
		}
	}

	protected void process() throws Exception {
		// Ensure we have an open database connection.
		openDB();
		cleanup();
		Hashtable stati=getCurrentStatus();
		Hashtable changed = getChanges(stati);
		saveChanges(changed);
	}

	public void run() {
		// Loop forever
		for(;;) {
			try {
				process();
				// Run once per hour
				Thread.sleep(60 * 60 * 1000);
			} catch(Exception e) {
				System.err.println("UPSTracker exception:  " + e);
				// Clear the db connection
				conn=null;
			}
		}
	}

	public static void main(String args[]) throws ClassNotFoundException {
		// Load our driver up-front
		Class.forName("org.postgresql.Driver");
		UPSTracker t = new UPSTracker();
		t.run();
	}
}
