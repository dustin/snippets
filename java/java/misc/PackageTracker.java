// Copyright (c) 2000  Dustin Sallings <dustin@spy.net>
//
// $Id: PackageTracker.java,v 1.4 2000/06/16 21:23:56 dustin Exp $

import java.sql.*;
import java.util.*;

import net.spy.net.SNPP;
import net.spy.info.PackageInfo;

import net.spy.SpyConfig;

public class PackageTracker extends Object {
	Connection conn=null;

	protected int carrier_id=0;
	protected String infoclassname=null;

	// This is *only* used for the main application
	protected static Vector trackers=null;

	protected static SpyConfig conf=null;

	public PackageTracker() {
		super();
	}

	public void setCarrierID(int carrier_id) {
		this.carrier_id=carrier_id;
	}

	public void setClassName(String classname) {
		this.infoclassname=classname;
	}

	/**
	 * Give the object a database connection to use...Be *very* careful
	 * with this.  If there will be multiple instances of this object running
	 * on different threads, they *must* have their own DB connections.  This
	 * problem is solved very easily by not giving them one.
	 */
	public void setConnection(Connection conn) {
		this.conn=conn;
	}

	protected void openDB() throws Exception {
		if(conn==null) {
			Class.forName(conf.get("jdbc_driver"));
			String source=conf.get("jdbc_source");
			conn=DriverManager.getConnection(source,
				conf.get("jdbc_username"), conf.get("jdbc_password"));
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

	protected PackageInfo getInfoObj(String id) throws Exception {
		Class c=Class.forName(infoclassname);
		PackageInfo infoObj=(PackageInfo)c.newInstance();
		infoObj.setArg(id);
		return(infoObj);
	}

	protected Hashtable getChanges(Hashtable stati) throws Exception {
		Hashtable ret=new Hashtable();
		Statement st = conn.createStatement();
		String query = "select distinct tracking_number from packages\n"
			+ " where carrier_id = " + carrier_id;
		ResultSet rs = st.executeQuery(query);
		while(rs.next()) {
			String n=rs.getString("tracking_number");
			PackageInfo i = getInfoObj(n);
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
			PackageInfo u=(PackageInfo)changes.get(key);
			st.setString(1, key);
			st.setInt(2, carrier_id);
			if(u.isDelivered()) {
				short_status="Delivered";
			} else {
				short_status="Waiting";
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
			Statement st = conn.createStatement();
			String query=null;
			query="delete from packages\n"
				+ " where carrier_id = 2 and exists\n"
				+ "  (select carrier_id from package_status s\n"
				+ "   where status='Delivered' and carrier_id=2\n"
				+ "    and packages.tracking_number=s.tracking_number)";
			st.executeUpdate(query);
			query="delete from package_status where status='Delivered'";
			st.executeUpdate(query);
		} catch(Exception e) {
			System.err.println("Error cleaning up:  " + e);
			e.printStackTrace();
		}
	}

	protected void process() throws Exception {
		System.out.println("Processing package carrier " + carrier_id + "...");
		// Ensure we have an open database connection.
		openDB();
		cleanup();
		Hashtable stati=getCurrentStatus();
		Hashtable changed = getChanges(stati);
		saveChanges(changed);
		System.out.println("Completed package carrier " + carrier_id);
	}

	public static void debug(String what) {
		// System.out.println("DEBUG:  " + what);
	}

	// /////////////////////////////////////////////////////////////////////
	// Below this line is application-mode stuff
	// /////////////////////////////////////////////////////////////////////

	public static void run(int naptime) {
		// Loop forever
		for(;;) {
			for(Enumeration en=trackers.elements(); en.hasMoreElements(); ) {
				PackageTracker pt=(PackageTracker)en.nextElement();
				try {
					pt.process();
				} catch(Exception e) {
					System.err.println("PackageTracker exception:  " + e);
					e.printStackTrace();
				}
			} // package loop
			try {
				Thread.sleep(naptime);
			} catch(Exception e) {
				// Ignore, we'll just go faster
			}
		} // Infinite loop
	}

	public static void getObjs() {
		// Initialize the vector
		trackers=new Vector();

		String tracker_list=conf.get("trackers");
		StringTokenizer st=new StringTokenizer(tracker_list);
		while(st.hasMoreTokens()) {
			String type=st.nextToken();
			try {
				// Add the new object to the list
				PackageTracker tracker=new PackageTracker();
				String key=null;
				key=type+ ".carrier_id";
				tracker.setCarrierID(Integer.parseInt(conf.get(key)));
				key=type+ ".infoobj";
				tracker.setClassName(conf.get(key));
				trackers.addElement(tracker);
			} catch(Exception e) {
				System.err.println("Error instantiating " + type + " tracker");
				System.out.println("Message:  " + e);
				e.printStackTrace();
			}
		}
	}

	public static void main(String args[]) throws Exception {
		conf=new SpyConfig(args[0]);
		int naptime=0;

		getObjs();
		try {
			naptime=Integer.parseInt(conf.get("naptime"));
		} catch(Exception e) {
			naptime=3600000;
		}
		run(naptime);
	}
}
