//
// $Id: JDBCPoolAble.java,v 1.2 2000/07/25 07:11:17 dustin Exp $

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
		available=false;
		if(the_object!=null) {
			try {
				Connection c=(Connection)the_object;
				c.close();
				the_object=null;
			} catch(Exception e) {
				System.err.println("Error on finalize!  " + e);
				e.printStackTrace();
			}
		}
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
			Connection c=(Connection)the_object;
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
