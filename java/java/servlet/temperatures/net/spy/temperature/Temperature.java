/*
 * Copyright (c) 1999 Dustin Sallings
 *
 * $Id: Temperature.java,v 1.21 2003/06/28 22:55:45 dustin Exp $
 */

package net.spy.temperature;

import java.io.*;
import java.util.*;
import java.net.*;
import java.text.SimpleDateFormat;

import javax.servlet.*;
import javax.servlet.http.*;

import net.spy.*;
import net.spy.net.*;

import java.awt.*;
import java.awt.image.*;

import net.spy.cache.*;
import net.spy.png.*;

// The class
public class Temperature extends PngServlet {

	private SpyCache cache=null;
	private Gatherer gatherer=null;

	// Image stuff.
	private Image baseImage=null;
	private Color black=null;
	private Color white=null;
	private Font font=null;

	// The once only init thingy.
	public void init(ServletConfig config) throws ServletException {
		super.init(config);

		try {
			// TODO:  make this configurable
			InetAddress ia=InetAddress.getByName("225.0.0.37");
			int port=6789;
			log("Initializing gatherer on " + ia + ":" + port);
			gatherer=new Gatherer(ia, port);
		} catch(IOException e) {
			throw new ServletException("Could not initialize the gatherer.",e);
		}

		try {
			log("Getting the base image.");
			getBaseImage();
			log("Got the base image.");
			cache=SpyCache.getInstance();

		} catch(Exception e) {
			throw new ServletException("Error getting base image", e);
		}

		// Initializing image stuff.
		black=new Color(0, 0, 0);
		white=new Color(255, 255, 255);
		font=new Font("SanSerif", Font.PLAIN, 10);
	}

	/**
	 * Shut down the gatherer.
	 */
	public void destroy() {
		log("Shutting down gatherer.");
		gatherer.stopRunning();
		super.destroy();
	}

	// Do a GET request
	public void doGet (
		HttpServletRequest request, HttpServletResponse response
	) throws ServletException {

		String which=request.getParameter("temp");
		String therm=request.getParameter("therm");
		String out="";
		if(which==null) {
			// If there's no which, check for a therm
			if(therm!=null) {
				// Make sure the temperature updates keep coming back...
				long l=new java.util.Date().getTime();
				l+=300000;
				response.setDateHeader("Expires", l);
				// Show the graphical representation of the temperature
				try {
					writeImage(request, response, getTherm(therm));
				} catch(Exception e) {
					throw new ServletException("Error sending gif", e);
				}
			} else {
				// If there's no therm, and no out, list the temps
				// If readings is provided, show the readings with the
				// thermometers

				String encodings=request.getHeader("Accept");
				// If the wml parameter is given, or we figure out the
				// browser supports it
				if( (request.getParameter("wml")!=null) ||
					(encodings!=null && encodings.indexOf("text/vnd.wap.wml")>0)
					) {

					// WML support, let's do the WML page.
					out=getWML();
					log("Sending wml response (" + encodings + ")");
					send_response(response, out, "text/vnd.wap.wml");
				} else {
					log("Sending plain response (" + encodings + ")");
					// No WML support, do the HTML things.
					if(request.getParameter("readings") != null) {
						out=listReadings();
						send_response(response, out);
					} else {
						out=listTemps();
						send_response(response, out);
					}
				}
			}
		} else {
			// Show the non-graphical representation of the temperature
			out=getTemp(which);
			send_response(response, out);
		}
	}

	private String listTemps() {
		String ret="";

		for(Iterator i=gatherer.getSeen().keySet().iterator(); i.hasNext();) {
			ret+=i.next() + "\n";
		}

		return(ret);
	}

	private String listReadings() {
		String ret="";

		for(Iterator i=gatherer.getSeen().entrySet().iterator(); i.hasNext();) {
			Map.Entry me=(Map.Entry)i.next();
			ret+=me.getKey() + "=" + me.getValue() + "\n";
		}

		return(ret);
	}

	private String getWML() {
		StringBuffer sb=new StringBuffer(64);

		SimpleDateFormat sdf=new SimpleDateFormat("yyyy/MM/dd-hh:mm:ss");

		sb.append("<?xml version=\"1.0\"?>\n");
		sb.append("<!DOCTYPE wml PUBLIC ");
		sb.append("\"-//WAPFORUM//DTD WML 1.1//EN\" ");
		sb.append("\"http://www.wapforum.org/DTD/wml_1.1.xml\">\n");
		sb.append("<wml>\n");
		sb.append("<card title=\"Temperatures\">\n");
		sb.append("<p>");
		sb.append(sdf.format(new java.util.Date()));
		sb.append("</p>\n");
		sb.append("<p>\n");
		// Add the contents
		for(Iterator i=gatherer.getSeen().entrySet().iterator(); i.hasNext();) {
			Map.Entry me=(Map.Entry)i.next();
			sb.append(me.getKey());
			sb.append("=");
			Sample s=(Sample)me.getValue();
			sb.append(s.getSample());
			sb.append("<br/>\n");
		}
		sb.append("</p>\n");
		sb.append("</card>\n");
		sb.append("</wml>");

		return(sb.toString());
	}

	private String getTemp(String which)
		throws ServletException {

		Double dv=gatherer.getSeen(which);
		if(dv == null) {
			throw new ServletException("No value for " + which);
		}
		double t=dv.doubleValue();
		int temptmp=(int)(t*100.0);
		t=(double)temptmp/100;
		return("" + t);
	}

	private void send_response(HttpServletResponse response, String o,
		String type) {

		try {
			response.setContentType(type);
			PrintWriter out=response.getWriter();
			out.print(o);
			out.close();
		} catch(Exception e) {
			e.printStackTrace();
		}
	}

	private void send_response(HttpServletResponse response, String o) {
		send_response(response, o, "text/plain");
	}

	// Graphical representation of the image.
	private Image getTherm(String which)
		throws ServletException {

		int width=133;
		int height=132;

		// Stuff we need to calculate the temperature
		double x2, y2;
		int trans;
		double rad, angle;
		double temp=getTempD(which);

		// Create an image the size we want
		Image image = createImage(width, height);

		// Get a graphics object to draw on the image
		Graphics g = image.getGraphics();
		// Prepare the drawing stuff.
		g.setFont(font);
		g.setColor(black);

		// Draw the base thermometer
		g.drawImage(baseImage, 0, 0, new StupidImageObserver());

		// Translate the angle because we're a little crooked
		trans=-90;

		// Calculate the angle based on the temperature
		angle=(temp*1.8)+trans;
		// Calculate the angle in radians
		rad=( (angle/360) * 2 * 3.14159265358979d );
		// Find the extra points
		x2=Math.sin(rad)*39;
		y2=Math.cos(rad)*39;
		// Negate the y, we're upside-down
		y2=-y2;
		// Move over to the starting points.
		x2+=66; y2+=65;
		// Draw the line.
		g.drawLine(66, 65,  (int)x2, (int)y2);
		// Draw the temperature
		g.drawString("" + temp, 52, 82);

		return(image);
	}

	// Get the temperature as a double (For the image)
	private double getTempD(String which)
		throws ServletException {

		Double d=gatherer.getSeen(which);
		if(d == null) {
			throw new ServletException("No value for " + which);
		}
		double rv = d.doubleValue();
		return(rv);
	}

	// Get the base image loaded
	private Image getBaseImage() throws Exception {
		if(baseImage==null) {
			String therm=getServletConfig().getInitParameter("baseImage");
			URL url=null;

			// Try to get this locally first.
			try {
				url=getServletContext().getResource(therm);
			} catch(MalformedURLException me) {
				// Nothing, it will stay null, and the next thing will be
				// tried.
			}
			// If not, try to make a URL out of it.
			if(url==null) {
				url=new URL(therm);
			}
			log("Getting image (" + url + ")");

			ImageLoader il=new ImageLoader(url);
			baseImage=il.getImage();
		}
		return(baseImage);
	}
}
