// Copyright (c) 2000  Dustin Sallings <dustin@spy.net>
//
// $Id: ReportBean.java,v 1.2 2000/10/15 10:09:12 dustin Exp $

package net.spy.temperature;

import net.spy.*;
import java.sql.*;
import java.util.*;

/**
 * Bean used for Temperature reports.
 */
public class ReportBean extends Object {

	// Report types
	/**
	 * Histogram report.
	 */
	public final static int HISTOGRAM=1;
	/**
	 * Hourly averages
	 */
	public final static int AVG_HOUR=2;
	/**
	 * Hourly minimums.
	 */
	public final static int MIN_HOUR=3;
	/**
	 * Hourly maximums
	 */
	public final static int MAX_HOUR=4;
	/**
	 * Hourly stuff (min, avg, and max)
	 */
	public final static int HOURLY_VITALITY=5;

	// The start and stop dates for our report
	protected String start_date=null;
	protected String stop_date=null;
	protected Sensor sensor=null;
	protected int report_num=-1;

	// Results go here
	protected Vector results=null;
	protected Vector columns=null;

	public ReportBean() {
		super();
	}

	/**
	 * Returns a list of Sensor objects describing the known sensors.
	 */
	public Enumeration listSensors() throws Exception {
		return(Sensor.enumerate());
	}

	/**
	 * Get an Enumeration of Sensor objects describing all the known sensors.
	 *
	 * @exception Exception when stuff breaks.
	 */
	public static void main(String args[]) throws Exception {
		ReportBean rb=new ReportBean();

		for(Enumeration e=rb.listSensors(); e.hasMoreElements(); ) {
			System.out.println(e.nextElement().toString());
		}
	}

	/**
	 * Set the start date for the report.
	 */
	public void setStartdate(String to) {
		this.start_date=to;
	}

	/**
	 * Set the stop date for the report.
	 */
	public void setStopdate(String to) {
		this.stop_date=to;
	}

	/**
	 * Set the sensor number we're reporting on.
	 *
	 * @exception Exception when something breaks.
	 */
	public void setSensor(int sensor_id) throws Exception {
		sensor=new Sensor(sensor_id);
	}

	/**
	 * Set the report number.
	 *
	 * @exception Exception if the report number is invalid
	 */
	public void setReport(int report_id) throws Exception {
		if(invalidReportNum(report_id)) {
			throw new Exception("Invalid report number:  " + report_id);
		}

		this.report_num=report_id;
	}

	/**
	 * Get the report results.
	 */
	public Enumeration getResults() throws Exception {
		if(results==null) {
			throw new Exception("No results exist!");
		}
		return(results.elements());
	}

	/**
	 * Get the report result column names.
	 */
	public Enumeration getResultColumns() throws Exception {
		if(columns==null) {
			throw new Exception("No results exist!");
		}
		return(columns.elements());
	}

	// Is the report number valid?
	protected boolean invalidReportNum(int num) {
		return(num < 1 || num > 5);
	}

	/**
	 * Run the report.
	 *
	 * @exception Exception for various reasons
	 */
	public void runReport() throws Exception {

		if(
			start_date==null ||
			start_date==null ||
			sensor==null ||
			invalidReportNum(report_num) ) {
			throw new Exception("Missing fields.");
		}

		doReport();
	}

	// This really does the report, now that we know we have ok-ish data
	protected void doReport() throws Exception {

		String query=null;

		// Get the report's SQL
		switch(report_num) {
			case HISTOGRAM:
				query="select round(sample,-1) as sample_10,count(*) as count\n"
					+ " from samples\n"
					+ " where ts between ? and ?\n"
					+ " and sensor_id = ?\n"
					+ " group by sample_10\n"
					+ " order by count\n";
				break;

			case MAX_HOUR:
				query="select date_part('hour',ts) as hour,max(sample) as temp"
					+ "\n"
					+ "  from samples"
					+ "  where ts between ? and ?\n"
					+ "  and sensor_id = ?\n"
					+ "  group by hour\n"
					+ "  order by hour\n";
				break;

			case AVG_HOUR:
				query="select date_part('hour',ts) as hour,avg(sample) as temp"
					+ "\n"
					+ "  from samples"
					+ "  where ts between ? and ?\n"
					+ "  and sensor_id = ?\n"
					+ "  group by hour\n"
					+ "  order by hour\n";
				break;

			case MIN_HOUR:
				query="select date_part('hour',ts) as hour,min(sample) as temp"
					+ "\n"
					+ "  from samples"
					+ "  where ts between ? and ?\n"
					+ "  and sensor_id = ?\n"
					+ "  group by hour\n"
					+ "  order by hour\n";
				break;

			case HOURLY_VITALITY:
				query="select date_part('hour',ts) as hour,\n"
					+ " min(sample) as min_temp,\n"
					+ " avg(sample) as avg_temp,\n"
					+ " max(sample) as max_temp\n"
					+ "  from samples"
					+ "  where ts between ? and ?\n"
					+ "  and sensor_id = ?\n"
					+ "  group by hour\n"
					+ "  order by hour\n";
				break;

			default:
				throw new Exception("Impossible!  No such report");
		}

		SpyDB db=new SpyDB(new TemperatureConf());
		PreparedStatement pst=db.prepareStatement(query);
		pst.setString(1, start_date);
		pst.setString(2, stop_date);
		pst.setInt(3, sensor.getSensorID());
		ResultSet rs=pst.executeQuery();
		ResultSetMetaData rmd=rs.getMetaData();

		// This is where the results go.
		results=new Vector();
		columns=new Vector();

		// Figure out how many columns we've got
		int cols=rmd.getColumnCount();

		// Get the column names.
		for(int i=1; i<=cols; i++) {
			columns.addElement(rmd.getColumnName(i));
		}

		// Get the results
		while(rs.next()) {
			Vector v=new Vector();
			for(int i=1; i<=cols; i++) {
				v.addElement(rs.getString(i));
			}
			results.addElement(v);
		}
	}
}
