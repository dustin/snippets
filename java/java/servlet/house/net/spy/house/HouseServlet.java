/*
 * Copyright (c) 1999 Dustin Sallings
 *
 * $Id: HouseServlet.java,v 1.10 2002/05/04 08:50:07 dustin Exp $
 */

package net.spy.house;

import java.io.*;
import java.util.*;
import java.net.*;

import javax.servlet.*;
import javax.servlet.http.*;

import net.spy.*;
import net.spy.net.*;
import net.spy.temperature.*;

import java.awt.*;
import java.awt.image.*;

import net.spy.png.*;

// The class
public class HouseServlet extends PngServlet implements ImageObserver
{
	// Colors we'll be using
	private Color white=null;
	private Color red=null;
	private Color blue=null;
	private Color black=null;

	// The base house image.
	private Image baseImage=null;
	private boolean imageLoaded=false;

	// The house config file path
	private String houseConfig=null;

	// The once only init thingy.
	public void init(ServletConfig config) throws ServletException {
		super.init(config);

		// Find the house config
		houseConfig=config.getInitParameter("houseConfig");
		if(houseConfig.startsWith("/WEB-INF")) {
			houseConfig=getServletContext().getRealPath(houseConfig);
		}
		log("Using the following config file:  " + houseConfig);

		String bi=config.getInitParameter("baseImage");

		// If the base image begins with a /, look up the real path
		if(bi.startsWith("/")) {
			try {
				URL biurl=config.getServletContext().getResource(bi);
				if(biurl!=null) {
					bi=biurl.toString();
				}
			} catch(MalformedURLException me) {
				me.printStackTrace();
			}
		}

		getBaseImage(bi);

		white=new Color(255, 255, 255);
		red=new Color(255, 0, 0);
		blue=new Color(0, 0, 255);
		black=new Color(0, 0, 0);
	}

	// Do a GET request
	public void doGet (
		HttpServletRequest request, HttpServletResponse response
	) throws ServletException, IOException {
		try {
			writeImage(request, response, getHouseImage());
		} catch(Exception e) {
			e.printStackTrace();
			throw new ServletException("Error getting image", e);
		}
	}

	// Graphical representation of the image.
	private Image getHouseImage() throws Exception {
		Image img=createImage(307, 223);
		Graphics g=img.getGraphics();
		g.drawImage(baseImage, 0, 0, this);

		ServletConfig sconf=getServletConfig();

		// Get the temperature servlet
		String tempUrl=sconf.getInitParameter("tempServlet");
		SpyTemp spytemp=new SpyTemp(tempUrl);

		// The description of what we're drawing.
		SpyConfig conf=new SpyConfig(new File(houseConfig));

		// Find all the things we need to colorize
		String things[]=SpyUtil.split(" ", conf.get("colorize", ""));
		for(int i=0; i<things.length; i++) {
			int x, y, w, h;

			String rstring=null;

			x=conf.getInt(things[i] + ".rect.x", 0);
			y=conf.getInt(things[i] + ".rect.y", 0);
			w=conf.getInt(things[i] + ".rect.w", 0);
			h=conf.getInt(things[i] + ".rect.h", 0);
			// Default color is white
			g.setColor(white);

			try {
				double reading=spytemp.getTemp(things[i]);
				rstring="" + reading;

				// Draw a black border
				g.setColor(black);
				g.fillRect(x-1, y-1, w+2, h+2);
				g.setColor(white);

				// Set the color based on the temperature reading.
				if(reading< conf.getInt(things[i] + ".min", 0)) {
					g.setColor(blue);
				} else if(reading> conf.getInt(things[i] + ".max", 0)) {
					g.setColor(red);
				}

				// Stick the color all up in there.
				g.fillRect(x, y, w, h);
			} catch(Exception e) {
				rstring="??.??";
			}
			// Put the reading in there.
			g.setColor(black);
			int stringx=conf.getInt(things[i] + ".reading.x", (x+(w/2)-18));
			int stringy=conf.getInt(things[i] + ".reading.y", (y+(h/2)+4));
			g.drawString(rstring, stringx, stringy);
		}

		return(img);
	}

	private void getBaseImage(String url) {
		try {
			if(baseImage==null) {
				log("Loading image... (" + url + ")");
				baseImage=Toolkit.getDefaultToolkit().getImage(new URL(url));
				Toolkit.getDefaultToolkit().prepareImage(baseImage,-1,-1,this);
				if(!imageLoaded) {
					synchronized(this) {
						wait(15000);
					}
				}
				log("Image loaded (or timed out).");
			}
		} catch(Exception e) {
			System.err.println("Error fetching base image:  " +e);
			e.printStackTrace();
		}
	}

	// When imageUpdate is called...yeah, this sucks.
	public boolean imageUpdate(Image img, int infoflags,
		int x, int y, int width, int height) {

		if((infoflags&ALLBITS) != 0) {
			imageLoaded=true;
			log("Actually loaded.");
			synchronized(this) {
				notify();
			}
		}

		return(!imageLoaded);
	}
}
