/*
 * Copyright (c) 1999 Dustin Sallings
 *
 * $Id: BarGraphServlet.java,v 1.1 2000/10/19 08:55:52 dustin Exp $
 */

package net.spy.chart;

import java.io.*;
import java.util.*;
import java.net.*;
import sun.misc.*;

import javax.servlet.*;
import javax.servlet.http.*;

import net.spy.*;

import java.awt.*;
import java.awt.image.*;

import com.mongus.servlet.GifServlet;

// The class
public class BarGraphServlet extends GifServlet {

	protected Color black=null;
	protected Color white=null;
	protected Font font=null;

	// The once only init thingy.
	public void init(ServletConfig config) throws ServletException {
		super.init(config);

		// Initializing image stuff.
		black=Color.black;
		white=Color.white;
		font=new Font("SanSerif", Font.PLAIN, 10);
	}

	// Do a GET request
	public void doGet (
		HttpServletRequest request, HttpServletResponse response
	) throws ServletException, IOException {

		int width=640;
		int height=480;

		String tmp=request.getParameter("width");
		if(tmp!=null) {
			width=Integer.parseInt(tmp);
		}
		tmp=request.getParameter("height");
		if(tmp!=null) {
			height=Integer.parseInt(tmp);
		}

		String tmpa[]=SpyUtil.split(",", request.getParameter("data"));
		double d[]=new double[tmpa.length];
		for(int i=0; i<d.length; i++) {
			d[i]=Double.valueOf(tmpa[i]).doubleValue();
		}
		String l[]=SpyUtil.split(",", request.getParameter("labels"));
		BarGraph bg=new BarGraph(width, height);

		// Prepare the data
		bg.setBGColor(white);
		bg.setFGColor(black);
		bg.setData(d);
		bg.setLabels(l);
		Image i=createImage(width, height);
		Graphics g=i.getGraphics();
		bg.paint(g);

		writeGif(response, i);
	}
}
