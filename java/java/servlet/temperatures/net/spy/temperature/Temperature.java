/*
 * Copyright (c) 1999 Dustin Sallings
 *
 * $Id: Temperature.java,v 1.5 2000/05/01 04:32:39 dustin Exp $
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

import com.mongus.servlet.GifServlet;

// The class
public class Temperature extends GifServlet implements ImageObserver
{
	Properties temps = null;

	// Image stuff.
	static Image baseImage=null;
	Color black=null;
	Color white=null;
	Font font=null;

	// The once only init thingy.
	public void init(ServletConfig config) throws ServletException {
		super.init(config);

		temps=new Properties();
		try {
			temps.load(new
				FileInputStream("/afs/spy.net/misc/web/etc/temperature.properties"));
		} catch(Exception e) {
			throw new ServletException("Error loading properties:  " + e);
		}

		log("Loaded the following:\n" + listTemps());

		try {
			log("Getting the base image.");
			getBaseImage();
			log("Got the base image.");
		} catch(Exception e) {
			throw new ServletException("Error getting base image:  " + e);
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
					writeGif(response, getTherm(therm));
				} catch(Exception e) {
					throw new ServletException("Error sending gif:  " + e);
				}
			} else {
				// If there's no therm, and no out, list the temps
				out=listTemps();
				send_response(response, out);
			}
		} else {
			// Show the non-graphical representation of the temperature
			out=getTemp(which);
			send_response(response, out);
		}
	}

	protected String listTemps() {
		String ret="";

		for(Enumeration e = temps.keys(); e.hasMoreElements();) {
			ret+=e.nextElement() + "\n";
		}

		return(ret);
	}

	protected String getTemp(String which) throws ServletException {
		String url=(String)temps.get(which);
		double t;
		if(url==null) {
			throw new ServletException("Unknown location: " + which);
		}
		try {
			HTTPFetch f = new HTTPFetch(url);
			String s;
			int temptmp;
			s=f.getData();
			t=Double.valueOf(s).doubleValue();
			temptmp=(int)(t*100.0);
			t=(double)temptmp/100;
			log("Fetching " + which + " from " + url);
		} catch(Exception e) {
			throw new ServletException("Error getting temperature:  " + e);
		}
		return("" + t);
	}

	protected void send_response(HttpServletResponse response, String o) {
		try {
			response.setContentType("text/plain");
			PrintWriter out=response.getWriter();
			out.print(o);
			out.close();
		} catch(Exception e) {
			// Ignore.
		}
	}

	// Graphical representation of the image.
	protected Image getTherm(String which) throws ServletException {
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
		g.drawImage(baseImage, 0, 0, this);

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
	protected double getTempD(String which) throws ServletException {
		double t = 0.0;
		// Get the temperature, make it a double.
		String s=getTemp(which) + "d";
		t=Double.valueOf(s).doubleValue();
		return(t);
	}

	// Get the base image loaded
	protected Image getBaseImage() throws Exception {
		if(baseImage==null) {
			String therm=
				"http://bleu.west.spy.net/~dustin/images/therm.gif";
			URL url=new URL(therm);
			baseImage=Toolkit.getDefaultToolkit().getImage(url);
			log("Getting image...");
		}
		return(baseImage);
	}

	// When imageUpdate is called...yeah, this sucks.
	public boolean imageUpdate(Image img, int infoflags,
		int x, int y, int width, int height) {
		log("imageUpdate called");
		return(true);
	}
}
