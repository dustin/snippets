// Copyright 1996 SPY Networking
import java.applet.*;
import java.awt.*;
import java.awt.event.*;
import java.net.*;
import java.util.*;
import java.lang.*;

import net.spy.*;
import net.spy.net.*;

public class GetTemp extends Applet implements Runnable, MouseListener
{
	Image therm=null;
	Image image=null;
	URL url=null;
	URL linkto=null;
	String linkdesc=null;
	Thread timer=null;
	Font font=null;
	double temp;

	public String getAppletInfo() {
		return "GetTemp 1.0 by Dustin Sallings\n"
			+ "For more information, contact dustin@spy.net";
	}

	// When the mouse is over the applet
	public void mouseEntered(MouseEvent e) {
		showStatus(linkdesc);
	}

	// When the mouse button is released in the applet
	public void mouseReleased(MouseEvent e) {
		int x=e.getX();
		int y=e.getY();
		Dimension d = getSize();
		// As long as it's within the applet and we have  URL
		if(x>=0 && x<=d.width && y>=0 && y<=d.height && url!=null) {
			showStatus("Going to " + linkdesc);
			getAppletContext().showDocument(linkto);
		}
	}

	public void mousePressed(MouseEvent e) {
	}

	public void mouseClicked(MouseEvent e) {
	}

	public void mouseExited(MouseEvent e) {
	}

	// init function creates the double buffer
	public void init() {
		try {
			url=new URL(getParameter("image"));
		} catch(Exception e) {
			// Nothin'
		}

		String tmp=getParameter("linkto");
		if(tmp!=null) {
			try {
				linkto=new URL(tmp);
				linkdesc=getParameter("linkdesc");
				if(linkdesc==null) {
					linkdesc=tmp;
				}
			} catch(Exception e) {
				// Leave it null, it just won't work.
				System.err.println("Exception preparing " + e);
			}
		}

		font=new Font("SanSerif", Font.PLAIN, 10);
		getTemp();
		therm=getImage(url);
		image=therm;
		addMouseListener(this);
	}

	public void stop() {
		timer=null;
	}

	public void start() {
		if(timer==null) {
			timer = new Thread(this);
			timer.start();
		}
	}

	public void run() {
		while(timer!=null) {
			try {
				Thread.sleep(60*1000);
			} catch(Exception e) {
				System.err.println("Sleep exception:  " + e);
			}
			// Fetch and flush
			getTemp();
			repaint();
		}
	}

	protected void getTemp() {
		double t;
		int temptmp;
		String tempservlet=getParameter("tempservlet");
		String whichtemp=getParameter("temp");
		String tempurl=tempservlet + "?temp=" + whichtemp;
		String s=null;

		try {
			HTTPFetch f = new HTTPFetch(tempurl);
			s=f.getData();
		} catch(Exception e) {
			s="0.0";
		}
		s=s.trim();
		s+="d";

		t=Double.valueOf(s).doubleValue();

		// Ugly way to round to two digits
		temptmp=(int)(t*100.0);
		t=(double)temptmp/100;


		this.temp=t;
	}

	protected void drawTemp(Graphics g) {
		double x2, y2;
		int trans;
		double rad, angle;

		// Our display min and max
		if(temp>140) {
			temp=140;
		} else if(temp<-40) {
			temp=-40;
		}

		// Translate the angle
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
		g.drawString("" + temp, 52, 80);
	}

	// Update does about the same thing here.
	public void update(Graphics g) {
		super.update(g);
		g.drawImage(image, 0, 0, this);
		drawTemp(g);
	}

	// Paint.
	public void paint(Graphics g) {
		super.paint(g);
		g.setFont(font);
		g.drawImage(image, 0, 0, this);
		drawTemp(g);
	}
}
