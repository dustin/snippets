// Copyright (c) 2000  Dustin Sallings <dustin@spy.net>
//
// $Id: Sensor.java,v 1.2 2001/06/01 08:52:02 dustin Exp $

package net.spy.temperature;

import net.spy.*;
import java.sql.*;
import java.util.*;

public class Sensor extends Object {

	private int sensor_id=-1;
	private String serial=null;
	private String name=null;
	private boolean active=true;
	private int low=0;
	private int high=0;

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

	private Sensor(ResultSet rs) throws Exception {
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
	 * Is this sensor active?
	 */
	public boolean isActive() {
		return(active);
	}

	/**
	 * What's the low threshold for this sensor?
	 */
	public int getLow() {
		return(low);
	}

	/**
	 * What's the high threshold for this sensor?
	 */
	public int getHigh() {
		return(high);
	}

	/**
	 * Get an Enumeration of Sensor objects representing all known sensors.
	 *
	 * @exception Exception when DB problems arrise
	 */
	public static Enumeration enumerate() throws Exception {
		Vector v=new Vector();
		SpyDB db=new SpyDB(new TemperatureConf());
		ResultSet rs=db.executeQuery("select * from sensors order by name");

		while(rs.next()) {
			v.addElement(new Sensor(rs));
		}

		return(v.elements());
	}

	/**
	 * Printable representation of this object.
	 */
	public String toString() {
		StringBuffer sb=new StringBuffer();
		sb.append(sensor_id);
		sb.append("\t");
		sb.append(serial);
		sb.append(":  ");
		sb.append(name);
		if(!active) {
			sb.append(" (not active)");
		}

		return(sb.toString());
	}

	private void getSensor(int sensor_id) throws Exception {
		SpyDB db=new SpyDB(new TemperatureConf());
		PreparedStatement pst=db.prepareStatement(
			"select * from sensors where sensor_id = ? "
			);
		pst.setInt(1, sensor_id);
		ResultSet rs=pst.executeQuery();
		rs.next();
		initFromResultSet(rs);
		db.close();
	}

	private void initFromResultSet(ResultSet rs) throws Exception {
		sensor_id = rs.getInt("sensor_id");
		serial = rs.getString("serial");
		name = rs.getString("name");
		active=rs.getBoolean("active");
		low=rs.getInt("low");
		high=rs.getInt("high");
	}

	public static void main(String args[]) throws Exception {
		if(args.length>0) {
			int id=Integer.parseInt(args[0]);
			Sensor s=new Sensor(id);
			System.out.println("Found this:  " + s);
		} else {
			for(Enumeration e=Sensor.enumerate(); e.hasMoreElements(); ) {
				System.out.println(e.nextElement());
			}
		}
	}
}
