// Copyright 1996 SPY Networking
import java.applet.*;
import java.awt.*;
import java.net.*;
import java.util.*;
import java.lang.*;

public class GetTemp extends Applet implements Runnable
{
	Image therm=null;
	Image image=null;
	URL url=null;
	Thread timer=null;
	Font font=null;

	// init function creates the double buffer
	public void init() {
		try {
			url=new URL(getParameter("image"));
		} catch(Exception e) {
			// Nothin'
		}
		font=new Font("SanSerif", java.awt.Font.PLAIN, 10);
		therm=getImage(url);
		image=therm;
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
				Thread.sleep(5*60*1000);
			} catch(Exception e) {
				System.err.println("Sleep exception:  " + e);
			}
			// Fetch and flush
			// image=getImage(url);
			// image.flush();
			// repaint();
		}
	}

	protected double getTemp() {
		double temp;
		int temptmp;
		String s = getParameter("temp");
		temp=Double.valueOf(s).doubleValue();

		// Ugly way to round to two digits
		temptmp=(int)(temp*100.0);
		temp=(double)temptmp/100;

		return(temp);
	}

	protected void drawTemp(Graphics g) {
		double temp=getTemp();
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
		x2=java.lang.Math.sin(rad)*39;
		y2=java.lang.Math.cos(rad)*39;
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
