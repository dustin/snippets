/*
 * Copyright (c) 1999 Dustin Sallings
 *
 * $Id: Temperature.java,v 1.16 2002/08/22 07:37:54 dustin Exp $
 */

package net.spy.temperature;

import java.io.*;
import java.util.*;
import java.net.*;
import sun.misc.*;

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
	private File configFile=null;

	// Image stuff.
	private Image baseImage=null;
	private Color black=null;
	private Color white=null;
	private Font font=null;

	// The once only init thingy.
	public void init(ServletConfig config) throws ServletException {
		super.init(config);

		// find the config file
		String configFileS
			=getServletConfig().getInitParameter("temperatureProps");
		// If it's in WEB-INF, map it
		if(configFileS.startsWith("/WEB-INF")) {
			configFileS=getServletContext().getRealPath(configFileS);
		}
		configFile=new File(configFileS);
		log("Using the following config:  " +  configFile);

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

	// Do a GET request
	public void doGet (
		HttpServletRequest request, HttpServletResponse response
	) throws ServletException {

		SpyConfig temps=new SpyConfig(configFile);
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
					writeImage(request, response, getTherm(temps, therm));
				} catch(Exception e) {
					throw new ServletException("Error sending gif", e);
				}
			} else {
				// If there's no therm, and no out, list the temps
				out=listTemps(temps);
				send_response(response, out);
			}
		} else {
			// Show the non-graphical representation of the temperature
			out=getTemp(temps, which);
			send_response(response, out);
		}
	}

	private String listTemps(SpyConfig temps) {
		String ret="";

		for(Enumeration e = temps.keys(); e.hasMoreElements();) {
			ret+=e.nextElement() + "\n";
		}

		return(ret);
	}

	private String getTemp(SpyConfig temps, String which)
		throws ServletException {

		String key="therm_" + which;
		Double rv=(Double)cache.get(key);
		if(rv==null) {
			String url=(String)temps.get(which);
			double t;
			if(url==null) {
				throw new ServletException("Unknown location: " + which);
			}
			try {
				URLWatcher uw=URLWatcher.getInstance();
				String s=uw.getContent(new URL(url));
				int temptmp;
				t=Double.valueOf(s).doubleValue();
				temptmp=(int)(t*100.0);
				t=(double)temptmp/100;
				// log("Fetching " + which + " from " + url);
			} catch(Exception e) {
				throw new ServletException("Error getting temperature", e);
			}
			rv=new Double(t);
			cache.store(key, rv, 60000);
		}
		return(rv.toString());
	}

	private void send_response(HttpServletResponse response, String o) {
		try {
			response.setContentType("text/plain");
			PrintWriter out=response.getWriter();
			out.print(o);
			out.close();
		} catch(Exception e) {
			e.printStackTrace();
		}
	}

	// Graphical representation of the image.
	private Image getTherm(SpyConfig temps, String which)
		throws ServletException {

		int width=133;
		int height=132;

		// Stuff we need to calculate the temperature
		double x2, y2;
		int trans;
		double rad, angle;
		double temp=getTempD(temps, which);

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
	private double getTempD(SpyConfig temps, String which)
		throws ServletException {

		double t = 0.0;
		// Get the temperature, make it a double.
		String s=getTemp(temps, which) + "d";
		t=Double.valueOf(s).doubleValue();
		return(t);
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
