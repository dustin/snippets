//
// $Id: JDBCPoolFiller.java,v 1.2 2000/07/03 07:57:16 dustin Exp $

package net.spy.pool;

import java.util.*;
import java.sql.*;
import net.spy.SpyConfig;

public class JDBCPoolFiller extends PoolFiller {

	public JDBCPoolFiller(String name, SpyConfig conf) {
		super(name, conf);
	}

	public PoolAble getObject() throws PoolException {
		PoolAble p = null;
		try {
			String classname=null, source=null, user=null, pass=null;

			// Load the JDBC driver
			classname=getProperty("dbDriverName");
			if(classname==null) {
				throw new Exception("No dbDriverName property given");
			}
			Class.forName(classname);

			source=getProperty("dbSource");
			if(source==null) {
				throw new Exception("No dbSource property given");
			}

			user=getProperty("dbUser", "");
			pass=getProperty("dbPass", "");

			long max_age=(long)getPropertyInt("max_age", 0);

			// Grab a connection.
			Connection db = DriverManager.getConnection(source, user, pass);
			// Create the PoolAble object
			p=new PoolAble(db, max_age);
		} catch(Exception e) {
			throw new PoolException(
				"Error getting new DB object for the "
					+ name + " pool:  " + e
				);
		}

		return(p);
	}
}
