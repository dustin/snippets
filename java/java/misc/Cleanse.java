// Copyright (c) 2001  Dustin Sallings <dustin@spy.net>
//
// $Id: Cleanse.java,v 1.4 2001/03/16 09:31:37 dustin Exp $

import java.util.*;
import java.sql.*;

public class Cleanse extends Object {

	private Vector wordList=null;
	private Vector toremove=null;
	private Hashtable removed=null;
	private int nremoved=0;
	private static final String SOURCE="jdbc:postgresql://dhcp-104/dustin";

	// How many passes before we run flush()
	private static final int BATCHSIZE=1000;

	private static final String REMOVE="REMOVE";

	public Cleanse() {
		super();
		this.wordList=new Vector(250000);
		this.toremove=new Vector(BATCHSIZE);
		this.removed=new Hashtable(100000);
	}

	private void remove(String a, String b) {
		System.out.println("Removing " + a + " and " + b);
		wordList.removeElement(a);
		wordList.removeElement(b);
		toremove.addElement(a);
		toremove.addElement(b);
		removed.put(a, REMOVE);
		removed.put(b, REMOVE);
	}

	// Save the state permanently
	private void flush() throws SQLException {

		if(toremove.size() > 0) {
			System.out.println("Flushing " + toremove.size() + " items");
			// OK, need it again
			Connection conn=DriverManager.getConnection(SOURCE, "dustin", "");
			// Get rid of all the stuff we don't want anymore.
			PreparedStatement pst=conn.prepareStatement(
				"insert into badwords(badword) values(?)"
				);
			for(Enumeration e=toremove.elements(); e.hasMoreElements(); ) {
				String s=(String)e.nextElement() + ":";
				pst.setString(1, s);
				pst.executeUpdate();
			}
			nremoved+=toremove.size();
			pst.close();
			conn.close();
			toremove.removeAllElements();
		}
	}

	private void killWords() throws Exception {
		CleanseStats stats=new CleanseStats(wordList.size());
		for(Enumeration e=wordList.elements(); e.hasMoreElements(); ) {
			String current=(String)e.nextElement();

			System.out.println("Examining " + current);

			stats.click();
			stats.start();
			int i=0;
			for(Enumeration e2=wordList.elements(); e2.hasMoreElements(); ) {
				String checking=(String)e2.nextElement();

				// Make sure it's not the word we're looking at, and that
				// the words both still exist (haven't been added to the
				// removed hash)
				if(! current.equals(checking)
					&& (removed.get(current)==null)
					&& (removed.get(checking)==null) ) {
					if(current.indexOf(checking)>0 ||
						checking.indexOf(current)>0) {
						remove(current, checking);
					}

					i++;

					if( (i % BATCHSIZE) == 0) {
						flush();
					}
				}
			} // inner loop
			stats.stop();
			System.out.println("Processed in " + stats.getLastTime()
				+ " - " + stats.getStats());
		} // outer loop
		// A final flush, as we're done now.
	}

	public int cleanse() throws Exception {

		Class.forName("org.postgresql.Driver");
		Connection conn=DriverManager.getConnection(SOURCE, "dustin", "");
		Statement st=conn.createStatement();
		System.out.println("Executing query.");
		ResultSet rs=st.executeQuery(
			"select word from wordlist\n"
			+ "  order by word");
		System.out.println("Fetching results.");
		int i=0;
		while(rs.next()) {
			String tmp=rs.getString("word");
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

		// Make sure everything's stored
		flush();

		return(nremoved);
	}

	// Private inner class used for managing statistics.
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
		int removed=c.cleanse();

		System.out.println("A total of " + removed + " words removed.");

	}

}
