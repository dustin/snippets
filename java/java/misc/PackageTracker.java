// Copyright (c) 2000  Dustin Sallings <dustin@spy.net>
//
// $Id: PackageTracker.java,v 1.2 2000/06/15 22:26:01 dustin Exp $

import java.sql.*;
import java.util.*;

import net.spy.net.SNPP;
import net.spy.info.*;

public abstract class PackageTracker extends Object {
	Connection conn=null;

	protected int carrier_id=0;

	public PackageTracker(int carrier_id) {
		super();
		this.carrier_id=carrier_id;
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
		String query = "select * from package_status\n"
			+ " where carrier_id = " + carrier_id;
		ResultSet rs = st.executeQuery(query);
		while(rs.next()) {
			ret.put(rs.getString("tracking_number"),
				rs.getString("status_str"));
		}
		return(ret);
	}

	protected abstract Info getInfoObj(String id);

	protected Hashtable getChanges(Hashtable stati) throws Exception {
		Hashtable ret=new Hashtable();
		Statement st = conn.createStatement();
		String query = "select distinct tracking_number from packages\n"
			+ " where carrier_id = " + carrier_id;
		ResultSet rs = st.executeQuery(query);
		while(rs.next()) {
			String n=rs.getString("tracking_number");
			Info i = getInfoObj(n);
			String status=i.toString();
			String current=(String)stati.get(n);
			if(current==null) {
				current="";
			}

			// If they're not the same, add the current Info object to the
			// changes hash.
			if(!current.equals(status)) {
				ret.put(n, i);
			}
		}
		return(ret);
	}

	protected void saveChanges(Hashtable changes) throws Exception {
		SNPP snpp=new SNPP("snpp.skytel.com", 444);
		for(Enumeration e=changes.keys(); e.hasMoreElements(); ) {
			String key=(String)e.nextElement();
			String short_status=null;

			// Delete it just in case.
			PreparedStatement st = conn.prepareStatement(
				"delete from package_status\n"
					+ " where carrier_id = " + carrier_id
					+ " and tracking_number = ?"
			);
			st.setString(1, key);
			st.executeUpdate();
			st = conn.prepareStatement(
				"insert into package_status values(?, ?, ?, ?)"
			);

			// Add it if it's new
			Info u=(Info)changes.get(key);
			st.setString(1, key);
			st.setInt(2, carrier_id);
			short_status = u.get("Package Status");
			if(short_status==null) {
				short_status="Unknown";
			}
			st.setString(3, short_status);
			st.setString(4, u.toString());
			st.executeUpdate();

			st = conn.prepareStatement(
				"select * from packages\n"
					+ " where carrier_id = " + carrier_id
					+ " and tracking_number = ?"
			);
			st.setString(1, key);
			ResultSet rs = st.executeQuery();
			while(rs.next()) {
				String id=rs.getString("pager_id");
				String descr=rs.getString("descr");
				// This is our message.
				String msg="Package tracking:\r\n" + key
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
			String query="delete from packages\n"
				+ " where carrier_id = " + carrier_id
				+ " and tracking_number in "
				+ "(select tracking_number from package_status where "
				+   "status='Delivered')";
			st.executeUpdate(query);
			query="delete from package_status where status='Delivered'";
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
				System.err.println("PackageTracker exception:  " + e);
				// Clear the db connection
				conn=null;
			}
		}
	}
}
