// Copyright (c) 2001  Dustin Sallings <dustin@spy.net>
//
// $Id: Geo.java,v 1.4 2002/11/11 18:22:20 dustin Exp $

package net.spy.rpc.services;

import java.util.*;

import net.spy.geo.*;

/**
 * XML RPC services for geography data.
 */
public class Geo extends Remote {

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

			// Boundary1 is minx, miny
			// Boundary2 is maxx, maxy
			Point boundary1=poly.getBoundary1();
			Point boundary2=poly.getBoundary2();

			double western=Math.min(boundary1.getLongitude(),
				boundary2.getLongitude());
			double eastern=Math.max(boundary1.getLongitude(),
				boundary2.getLongitude());
			double northern=Math.max(boundary1.getLatitude(),
				boundary2.getLatitude());
			double southern=Math.min(boundary1.getLatitude(),
				boundary2.getLatitude());

			h.put("eastern_border", new Double(eastern));
			h.put("northern_border", new Double(northern));
			h.put("western_border", new Double(western));
			h.put("southern_border", new Double(southern));

			h.put("center_longitude", new Double(center.getLongitude()));
			h.put("center_latitude", new Double(center.getLatitude()));

			String source=poly.getSource();
			String type="unknown";
			if(source.startsWith("zt")) {
				type="zipcode";
			} else if(source.startsWith("co")) {
				type="county";
			} else if(source.startsWith("st")) {
				type="state";
			}

			h.put("type", type);

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
