// Copyright (c) 2001  Dustin Sallings <dustin@spy.net>
//
// $Id: Geo.java,v 1.1 2002/03/06 06:23:04 dustin Exp $

package net.spy.rpc.services;

import java.util.*;

import net.spy.geo.*;

/**
 * XML RPC services for geography data.
 */
public class Geo extends Object {

	/**
	 * Get an instance of Geo.
	 */
	public Geo() {
		super();
	}

	/**
	 * Get the info for the given point.
	 */
	public Vector getPointInfo(double lat, double lon) throws Exception {

		Point p=new Point(lat, lon);
		Enumeration polys=Polygon.getAreasForPoint(p);

		if(!polys.hasMoreElements()) {
			throw new Exception("No information found for that point.");
		}

		Vector rv=new Vector();
		for(; polys.hasMoreElements(); ) {
			DBPolygon poly=(DBPolygon)polys.nextElement();
			Point center=poly.getCenter();

			Double height=new Double(poly.getHeight());
			Double width=new Double(poly.getWidth());

			Hashtable h=new Hashtable();
			h.put("width", width);
			h.put("height", height);
			h.put("name", poly.getName());
			h.put("center_longitude", new Double(center.getLongitude()));
			h.put("center_latitude", new Double(center.getLatitude()));

			rv.addElement(h);
		}

		return(rv);
	}

	/**
	 * Testing and what not.
	 */
	public static void main(String args[]) throws Exception {
		Geo geo=new Geo();
		System.out.println(geo.getPointInfo(
			Double.parseDouble(args[0]),
			Double.parseDouble(args[1])));
	}

}
