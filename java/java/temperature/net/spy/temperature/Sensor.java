// Copyright (c) 2000  Dustin Sallings <dustin@spy.net>
//
// $Id: Sensor.java,v 1.1 2000/10/15 10:04:47 dustin Exp $

package net.spy.temperature;

import net.spy.*;
import java.sql.*;
import java.util.*;

public class Sensor extends Object {

	protected int sensor_id=-1;
	protected String serial=null;
	protected String name=null;

	/**
	 * Get a new Sensor object with the given sensor_id.
	 *
	 * @exception Exception on stuff like invalid or uknown ids or other
	 * failures.
	 */
	public Sensor(int sensor_id) throws Exception {
		super();
		getSensor(sensor_id);
	}

	/**
	 * Get a new Sensor object from a ResultSet containing the following
	 * values:
	 * <ul>
	 *  <li>sensor_id - integer sensor ID</li>
	 *  <li>serial - String - sensor serial number</li>
	 *  <li>name - String - name of sensor</li>
	 * </ul>
	 *
	 * @exception Exception on stuff like an invalid result set or other
	 * types of errors.
	 */
	public Sensor(ResultSet rs) throws Exception {
		super();
		initFromResultSet(rs);
	}

	/**
	 * Get the sensor ID.
	 */
	public int getSensorID() {
		return(sensor_id);
	}

	/**
	 * Get the sensor's serial number.
	 */
	public String getSerial() {
		return(serial);
	}

	/**
	 * Get the sensor's descriptive name.
	 */
	public String getName() {
		return(name);
	}

	/**
	 * Get an Enumeration of Sensor objects representing all known sensors.
	 *
	 * @exception Exception when DB problems arrise
	 */
	public static Enumeration enumerate() throws Exception {
		Vector v=new Vector();
		SpyDB db=new SpyDB(new TemperatureConf());
		ResultSet rs=db.executeQuery("select * from sensor");

		while(rs.next()) {
			v.addElement(new Sensor(rs));
		}

		return(v.elements());
	}

	/**
	 * Printable representation of this object.
	 */
	public String toString() {
		return(sensor_id + "\t" + serial + ":  " + name);
	}

	protected void getSensor(int sensor_id) throws Exception {
		SpyDB db=new SpyDB(new TemperatureConf());
		PreparedStatement pst=db.prepareStatement(
			"select * from sensor where sensor_id = ? "
			);
		pst.setInt(1, sensor_id);
		ResultSet rs=pst.executeQuery();
		rs.next();
		initFromResultSet(rs);
		db.close();
	}

	protected void initFromResultSet(ResultSet rs) throws Exception {
		this.sensor_id = rs.getInt("sensor_id");
		this.serial = rs.getString("serial");
		this.name = rs.getString("name");
	}

	public static void main(String args[]) throws Exception {
		int id=Integer.parseInt(args[0]);

		Sensor s=new Sensor(id);

		System.out.println("Found this:  " + s);
	}
}
