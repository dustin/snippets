// Copyright (c) 1999  Dustin Sallings <dustin@spy.net>
//
// $Id: Loader.java,v 1.2 2000/05/01 04:32:48 dustin Exp $

package net.spy.temperature;

import java.lang.*;
import java.util.*;
import java.text.*;
import java.sql.*;
import java.io.*;

public class Loader {

	static Hashtable sensors = null;

	public static void main(String args[]) throws Exception {
		System.out.println("Loading logfile.");
		Class.forName("org.postgresql.Driver");
		String source="jdbc:postgresql://dhcp-104/temperature";
		Connection temp = DriverManager.getConnection(source, "dustin", "");
		initSensors(temp);
		Statement st = temp.createStatement();
		int i = 1;
		BufferedReader bin =
			new BufferedReader(new InputStreamReader(System.in));
		String in = null;

		while( (in=bin.readLine()) != null) {
			// System.out.print(in);
			try {
				TempSet t = parseEntry(in);
				String s = t.toSQL();
				System.out.print(i++ + " " + s);
				st.executeUpdate(s);
				System.out.println("------------------------------");
			} catch(Exception e) {
				System.out.println("Parse error:  " + e);
			}
		}

	}

	protected static TempSet parseEntry(String in) throws Exception {
		StringTokenizer st = new StringTokenizer(in, "\t");
		String date_str = st.nextToken();
		String serial_str = st.nextToken();
		String sample_str = st.nextToken();

		SimpleDateFormat f = new SimpleDateFormat("yyyy/MM/dd HH:mm:ss");

		java.util.Date  date=f.parse(date_str);
		int   id=lookupSerial(serial_str);
		float sample=Float.valueOf(sample_str).floatValue();

		return(new TempSet(date, id, sample));
	}

	protected static void initSensors(Connection conn) throws Exception {
		sensors = new Hashtable();
		try {
			Statement st = conn.createStatement();
			ResultSet rs =
				st.executeQuery("select sensor_id, serial from sensor");
			while(rs.next()) {
				int id=rs.getInt("sensor_id");
				String serial=rs.getString("serial");
				Integer i=new Integer(id);
				sensors.put(serial, i);
			}
		} catch(Exception e) {
			throw new Exception("Exception:  " + e);
		}
	}

	protected static int lookupSerial(String serial) throws Exception {
		int ret=0;
		try {
			Integer i=(Integer)sensors.get(serial);
			ret=i.intValue();
		} catch(Exception e) {
			throw new Exception("Unknown serial:  " + serial);
		}
		return(ret);
	}
}
