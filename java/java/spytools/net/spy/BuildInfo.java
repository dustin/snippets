// Copyright (c) 2001  Dustin Sallings <dustin@spy.net>
//
// $Id: BuildInfo.java,v 1.2 2002/08/21 00:52:54 dustin Exp $

package net.spy;

import java.util.Properties;
import java.util.Date;
import java.text.SimpleDateFormat;
import java.text.ParseException;
import java.io.InputStream;
import java.io.IOException;
import java.net.URL;

import net.spy.util.NestedRuntimeException;

/**
 * Information regarding this spy.jar build.
 *
 * The following properties will be set at build time:
 *
 * <ul>
 *  <li>build.number - Number of this build</li>
 *  <li>build.date - Date of this build (yyyy/MM/dd HH:mm)</li>
 *  <li>java.vendor - Java vendor who made the VM that built this jar</li>
 *  <li>java.version - Java version of the the VM that built this jar</li>
 *  <li>os.name - Name of the OS on which this jar was built</li>
 *  <li>os.version - Version of the OS on which this jar was built</li>
 * </ul>
 */
public class BuildInfo extends Properties {

	/**
	 * Get an instance of BuildInfo that describes the spy.jar build.
	 */
	public BuildInfo() throws IOException {
		this("net/spy/build.properties");
	}

	/**
	 * Get an instance of BuildInfo that describes the build info found in
	 * the given resource.
	 */
	protected BuildInfo(String resource) throws IOException {
		super();
		// Grab the build properties
		ClassLoader cl=getClass().getClassLoader();
		InputStream is=cl.getResourceAsStream(resource);
		if(is==null) {
			throw new IOException("No resources found for " + resource);
		}
		load(is);
		is.close();
	}

	/**
	 * Get the date of this build.
	 */
	public Date getBuildDate() {
		SimpleDateFormat sdf=new SimpleDateFormat("yyyy/MM/dd HH:mm");
		Date rv=null;

		try {
			rv=sdf.parse(getProperty("build.date"));
		} catch(ParseException pe) {
			throw new NestedRuntimeException(
				"Invalid date from build properties file", pe);
		}
		return(rv);
	}

	/**
	 * String me.
	 */
	public String toString() {
		StringBuffer sb=new StringBuffer(256);

		sb.append("build ");
		sb.append(getProperty("build.number"));
		sb.append(" on ");
		sb.append(getBuildDate());
		sb.append("\nBuild platform:  java ");
		sb.append(getProperty("java.version"));
		sb.append(" from ");
		sb.append(getProperty("java.vendor"));
		sb.append(" on ");
		sb.append(getProperty("os.name"));
		sb.append(" version ");
		sb.append(getProperty("os.version"));

		return(sb.toString());
	}

	/**
	 * Print out the build properties.
	 */
	public static void main(String args[]) throws Exception {
		BuildInfo bi=new BuildInfo();

		System.out.println("spy.jar " + bi);
	}

}
