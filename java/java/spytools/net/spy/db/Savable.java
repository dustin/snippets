// Copyright (c) 2001  Dustin Sallings <dustin@spy.net>
//
// $Id: Savable.java,v 1.2 2002/07/22 06:41:41 dustin Exp $

package net.spy.db;

import java.util.Enumeration;
import java.sql.Connection;
import java.sql.SQLException;

/**
 * Interface for transactionally savable objects.
 */
public interface Savable {

	/**
	 * Is this a new object?
	 */
	boolean isNew();

	/**
	 * Has this object been modified?
	 */
	boolean isModified();

	/**
	 * Save this object's state over the given connection.
	 *
	 * @param conn the connection to use for saving
	 * @param context SaveContext being used in this Saver session
	 */
	void save(Connection conn, SaveContext context)
		throws SaveException, SQLException;

	/**
	 * Get a list of all of the Savables this Savable is holding that will
	 * need to be saved after this object.
	 */
	Enumeration getSavables(SaveContext context);

}
