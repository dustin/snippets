// Copyright (c) 2001  Dustin Sallings <dustin@spy.net>
//
// $Id: Cleanse.java,v 1.1 2001/03/15 11:24:28 dustin Exp $

import java.io.*;
import java.util.*;
import java.sql.*;
import java.util.zip.*;

public class Cleanse extends Object {

	Vector wordList=null;
	Vector toremove=null;

	public Cleanse() {
		super();
		this.wordList=new Vector(250000);
		this.toremove=new Vector(100000);
	}

	private void remove(String a, String b) {
		System.out.println("Removing " + a + " and " + b);
		wordList.removeElement(a);
		wordList.removeElement(b);
		toremove.addElement(a);
		toremove.addElement(b);
	}

	private void killWords() {
		CleanseStats stats=new CleanseStats(wordList.size());
		for(Enumeration e=wordList.elements(); e.hasMoreElements(); ) {
			String current=(String)e.nextElement();

			System.out.println("Examining " + current);

			stats.click();
			stats.start();
			for(Enumeration e2=wordList.elements(); e2.hasMoreElements(); ) {
				String checking=(String)e2.nextElement();

				if(! current.equals(checking)) {
					if(current.indexOf(checking)>0 ||
						checking.indexOf(current)>0) {
						remove(current, checking);
					}
				}
			} // inner loop
			stats.stop();
			System.out.println("Processed in " + stats.getLastTime()
				+ " - " + stats.getStats());
		} // outer loop
	}

	public Vector cleanse() throws Exception {

		Class.forName("org.postgresql.Driver");
		String source="jdbc:postgresql://dhcp-104/dustin";
		Connection conn=DriverManager.getConnection(source, "dustin", "");
		Statement st=conn.createStatement();
		System.out.println("Executing query.");
		ResultSet rs=st.executeQuery(
			"select celeb_key, item2 from celeb_parse\n"
			+ "  where item2 is not null order by item2");
		// A dummy object to store.
		System.out.println("Fetching results.");
		int i=0;
		while(rs.next()) {
			int id=rs.getInt("celeb_key");
			String tmp=rs.getString("item2");
			wordList.addElement(tmp.substring(0, tmp.indexOf(':')));
			if(i % 100 == 0) {
				System.out.println("Read " + i + " thingies.");
			}
			i++;
		}
		rs.close();
		st.close();
		conn.close();

		// Don't need a DB connection during this
		killWords();

		// OK, need it again
		conn=DriverManager.getConnection(source, "dustin", "");

		PreparedStatement pst=conn.prepareStatement(
			"update celeb_parse set item2=null where item2=?"
			);
		for(Enumeration e=toremove.elements(); e.hasMoreElements(); ) {
			String s=(String)e.nextElement() + ":";

			pst.setString(1, s);
			pst.executeUpdate();
		}

		return(toremove);
	}

	private class CleanseStats extends Object {
		private int done=0;
		private int left=0;
		private long startTime=0;
		private long totalTime=0;

		private long lastTime=0;
		private long lastProcessTime=0;

		public CleanseStats(int size) {
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
			double avgProcessTime=((double)totalTime/(double)done)/1000.0;
			double estimate=avgProcessTime*(double)left;

			java.text.NumberFormat nf=java.text.NumberFormat.getInstance();
			nf.setMaximumFractionDigits(2);

			return("Avg=" + nf.format(avgProcessTime)
				+ "s, Estimate=" + nf.format(estimate) + "s"
				+ " ("
				+ new java.util.Date(
					System.currentTimeMillis()+(1000*(long)estimate)
					)
				+ ")");
		}

	}

	public static void main(String args[]) throws Exception {

		Cleanse c=new Cleanse();
		Vector r=c.cleanse();

		System.out.println("The following thingies need to be removed:");
		for(Enumeration e=r.elements(); e.hasMoreElements(); ) {
			String s=(String)e.nextElement();
			System.out.println("\t" + s);
		}

	}

}
