// Copyright (c) 2000  Dustin Sallings <dustin@spy.net>
// $Id: Thermometer.java,v 1.1 2000/03/10 08:46:58 dustin Exp $

package net.spy.temperature;

import java.awt.*;
import java.awt.image.*;
import java.io.*;
import java.net.*;

import javax.servlet.*;
import javax.servlet.http.*;

import com.mongus.servlet.GifServlet;

public class Thermometer extends GifServlet implements ImageObserver
{
	static Image baseImage=null;

	public void init(ServletConfig config) throws ServletException {
		super.init(config);
		getBaseImage();
	}

	public void doGet(HttpServletRequest request,
		HttpServletResponse response) throws IOException
	{
		// Read the parameters
		int width=133;
		int height=132;
		String which=request.getParameter("which");
		Color black=new Color(0, 0, 0);
		Color white=new Color(255, 255, 255);
		Font font=new Font("SanSerif", Font.PLAIN, 10);

		// Stuff we need to calculate the temperature
		double x2, y2;
		int trans;
		double rad, angle;
		double temp=getTemp(which);;

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

		// Make sure the temperature updates keep coming back...
		long l=new java.util.Date().getTime();
		l+=300000;
		response.setDateHeader("Expires", l);

		// Send the GIF to the browser
		writeGif(response, image);
	}

	protected Image getBaseImage() {
		if(baseImage==null) {
			try {
				String therm=
					"http://bleu.west.spy.net/~dustin/images/therm.gif";
				URL url=new URL(therm);
				baseImage=Toolkit.getDefaultToolkit().getImage(url);
				System.out.println("Getting image...");
			} catch(Exception e) {
				// Just return null
			}
		}
		return(baseImage);
	}

	protected double getTemp(String which) {
		double t = 0.0;
		try {
			SpyTemp spytemp=new SpyTemp();
			t=spytemp.getTemp(which);
		} catch(Exception e) {
			// Nothing yet.
		}
		return(t);
	}

	public boolean imageUpdate(Image img, int infoflags,
		int x, int y, int width, int height) {
		System.out.println("imageUpdate called");
		return(true);
	}
}
