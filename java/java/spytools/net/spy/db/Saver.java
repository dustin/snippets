// Copyright (c) 2001  Dustin Sallings <dustin@spy.net>
//
// $Id: Saver.java,v 1.3 2002/08/15 04:20:46 dustin Exp $

package net.spy.db;

import java.util.HashSet;
import java.util.Enumeration;
import java.sql.Connection;
import java.sql.SQLException;

import net.spy.SpyDB;
import net.spy.SpyConfig;

/**
 * Transactional object saver.
 */
public class Saver extends Object {

	private static final int MAX_RECURSION_DEPTH=100;

	private SaveContext context=null;
	private SpyConfig config=null;
	private int rdepth=0;

	// Make sure we don't deal with the same object more than once
	private HashSet listedObjects=null;

	private SpyDB db=null;
	private Connection conn=null;

	/**
	 * Get an instance of Saver with the given database config.
	 */
	public Saver(SpyConfig config) {
		this(config, new SaveContext());
	}

	/**
	 * Get an instance of saver with the given database config and context.
	 */
	public Saver(SpyConfig config, SaveContext context) {
		super();
		this.context=context;
		this.config=config;
		this.listedObjects=new HashSet();
	}

	/**
	 * Save this Savabale and everything it contains.
	 */
	public void save(Savable o) throws SaveException {
		boolean complete=false;

		try {
			db=new SpyDB(config);
			conn=db.getConn();
			conn.setAutoCommit(false);

			// Begin recursion
			rsave(o);

			complete=true;
		} catch(SQLException se) {
			throw new SaveException("Error saving object", se);
		} catch(SaveException se) {
			throw se;
		} finally {
			// figure out whether we need to commit or roll back
			if(conn!=null) {
				if(complete==false) {
					try {
						conn.rollback();
					} catch(SQLException se) {
						se.printStackTrace();
					}
				} else {
					try {
						conn.commit();
					} catch(SQLException se) {
						throw new SaveException("Error committing", se);
					}
				}

				// Reset autocommit state
				try {
					conn.setAutoCommit(true);
				} catch(SQLException sqe) {
					sqe.printStackTrace();
				}
			} // Dealt with opened connection

			// Return a connection to the pool
			if(db!=null) {
				db.close();
			}
		}
	}

	// Deal with individual saves.
	private void rsave(Savable o) throws SaveException, SQLException {
		rdepth++;

		// watch recursion depth
		if(rdepth>MAX_RECURSION_DEPTH) {
			throw new SaveException("Recursing too deep!  Max depth is  "
				+ MAX_RECURSION_DEPTH);
		}

		// Save this object if it needs saving.
		if(o.isNew() || o.isModified()) {
			o.save(conn, context);
		}

		// Only go through the savables if we haven't gone through the
		// savables for this exact object
		if(!listedObjects.contains(o)) {
			// Add this to the set to keep us from doing it again
			listedObjects.add(o);

			// Get the savables
			Enumeration e=o.getSavables(context);
			if(e==null) {
				throw new NullPointerException(
					"Got a null savable list from " + o.getClass().getName());
			}

			// Deal with the savables
			while(e.hasMoreElements()) {
				Savable s=(Savable)e.nextElement();
				// verify the object isn't null, if it is, report it here
				// so we can figure out what gave it to us
				if(s==null) {
					throw new NullPointerException("Got a null object from "
						+ o);
				}

				rsave(s);
			}
		}

		rdepth--;
	}

}
