//
// $Id: JDBCPoolAble.java,v 1.3 2001/02/07 06:31:35 dustin Exp $

package net.spy.pool;

import java.util.*;
import java.sql.*;
import net.spy.SpyConfig;

/**
 * PoolAble object for containing a JDBC object.
 */

public class JDBCPoolAble extends PoolAble {

	public JDBCPoolAble(Object the_object) {
		super(the_object);
	}

	public JDBCPoolAble(Object the_object, long max_age) {
		super(the_object, max_age);
	}

	public void discard() {
		try {
			Connection c=(Connection)intGetObject();
			if(c!=null) {
				c.close();
			}
		} catch(Exception e) {
			System.err.println("Error on finalize!  " + e);
			e.printStackTrace();
		}
		// Tell the parent to do the same.
		super.discard();
	}

	/**
	 * Find out of the JDBCPoolAble represents a usable object.  This is
	 * done by doing a select of a constant and verifying it gets the same
	 * value in the return.
	 *
	 * @return true if the object will be usable
	 */
	public boolean isAlive() {
		boolean ret=false;
		try {
			Connection c=(Connection)intGetObject();
			Statement st=c.createStatement();
			ResultSet rs=st.executeQuery("select 7");
			rs.next();
			int r=rs.getInt(1);
			// Set the value to true only if r==7
			ret=(r==7);
		} catch(Exception e) {
			// Turn off availability
			available=false;
		}
		return(ret);
	}
}
