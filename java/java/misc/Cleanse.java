// Copyright (c) 2001  Dustin Sallings <dustin@spy.net>
//
// $Id: Cleanse.java,v 1.7 2001/03/16 19:11:08 dustin Exp $

import java.util.*;
import java.sql.*;

public class Cleanse extends Object {

	private Word wordList[]=null;
	private Vector toremove=null;
	private int nremoved=0;
	private static final String SOURCE="jdbc:postgresql://dhcp-104/dustin";

	// How many passes before we run flush()
	private static final int BATCHSIZE=500;

	private static final String REMOVE="REMOVE";

	public Cleanse() {
		super();
		this.toremove=new Vector(BATCHSIZE);
	}

	private void remove(Word w, Word because) {
		System.out.println("Removing " + w + " because of " + because);
		boolean done=false;
		for(int i=0; i<wordList.length && !done; i++) {
			if(wordList[i]!=null && wordList[i].equals(w)) {
				wordList[i]=null;
				done=true;
			}
		}
		toremove.addElement(w);
	}

	// Save the state permanently
	private void flush() throws SQLException {

		if(toremove.size() > 0) {
			System.out.println("Flushing " + toremove.size() + " items");
			// OK, need it again
			Connection conn=DriverManager.getConnection(SOURCE, "dustin", "");
			// Get rid of all the stuff we don't want anymore.
			PreparedStatement pst=conn.prepareStatement(
				"insert into badwords(celeb_key, badword) values(?, ?)"
				);
			for(Enumeration e=toremove.elements(); e.hasMoreElements(); ) {
				Word w=(Word)e.nextElement();
				String s=w.getWord() + ":";
				pst.setInt(1, w.getKey());
				pst.setString(2, s);
				pst.executeUpdate();
			}
			nremoved+=toremove.size();
			pst.close();
			conn.close();
			toremove.removeAllElements();
		}
	}

	private void killWords() throws Exception {
		CleanseStats stats=new CleanseStats(wordList.length);
		System.out.println("Operating on " + wordList.length + " words.");
		for(int i=0; i<wordList.length; i++) {
			Word current=wordList[i];
			String current_s=current.getWord();
			boolean current_removed=false;

			System.out.println("Examining " + current);

			stats.click();
			stats.start();
			for(int j=i+1; j<wordList.length; j++) {
				Word checking=wordList[j];

				// Make sure the word we're checking still exists, and that
				// they're not from the same celeb_row
				if( (!current_removed)
					&& current.getKey()!=checking.getKey() ) {

					String checking_s=checking.getWord();

					/*
					System.out.println("Comparing " + current_s
						+ " to " + checking_s);
					*/

					// Only need to check in one direction
					if( checking_s.indexOf(current_s) >= 0) {
						remove(current, checking);
						current_removed=true;
					}

				}
			} // inner loop

			if( (i % BATCHSIZE) == 0) {
				flush();
			}

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
			"select celeb_key, word, length(word) as l\n"
			+ " from wordlist\n"
			+ " where not exists (\n"
			+ "  select badword from badwords\n"
			+ "   where badwords.badword=wordlist.word\n"
			+ " )\n"
			+ " order by l, word\n"
			);
		System.out.println("Fetching results.");
		int i=0;
		Vector vtmp=new Vector(250000);
		while(rs.next()) {
			int key=rs.getInt("celeb_key");
			String tmp=rs.getString("word");
			String word=tmp.substring(0, tmp.indexOf(':'));
			vtmp.addElement(new Word(key, word));
			if(i % 100 == 0) {
				System.out.println("Read " + i + " thingies.");
			}
			i++;
		}
		rs.close();
		st.close();
		conn.close();

		System.out.println("Copying vector to array.");
		wordList=new Word[vtmp.size()];
		for(i=0; i<vtmp.size(); i++) {
			wordList[i]=(Word)vtmp.elementAt(i);
		}

		// Clean this up real quick.
		vtmp.removeAllElements();
		vtmp=null;

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

	private class Word extends Object {
		private int key=0;
		private String word=null;

		public Word(int key, String word) {
			this.key=key;
			this.word=word;
		}

		public int getKey() {
			return(key);
		}

		public String getWord() {
			return(word);
		}

		public String toString() {
			return(word + "@" + key);
		}

		public boolean equals(Word w) {
			return( (this.getKey() == w.getKey())
				&& (this.getWord().equals(w.getWord()))
			);
		}
	}

	public static void main(String args[]) throws Exception {

		Cleanse c=new Cleanse();
		int removed=c.cleanse();

		System.out.println("A total of " + removed + " words removed.");

	}

}
