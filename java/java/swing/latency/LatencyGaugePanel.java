import javax.swing.*;          //This is the final package name.
//import com.sun.java.swing.*; //Used by JDK 1.2 Beta 4 and all
                               //Swing releases before Swing 1.1 Beta 3.
import java.lang.*;
import java.awt.*;
import java.awt.event.*;
import java.util.*;

import net.spy.temperature.*;

class LatencyGaugePanel extends JPanel {
	Image image;
	Font font=null;
	String which=null;
	Hashtable hash=null;

	Color black=null;
	Color white=null;
	Color blue=null;

	Dimension size=null;

	double max_latency=0.0;

	public LatencyGaugePanel(Image image, String which, Hashtable hash) {
		super();

		this.image=image;
		this.hash=hash;
		this.which=which;

		black=new Color(0, 0, 0);
		white=new Color(255, 255, 255);
		blue=new Color(0, 0, 255);

		setBackground(white);

		// We add a little bit on the end so that we can label it.
		size=new Dimension(241,261);

		font=new Font("SanSerif", Font.PLAIN, 10);
	}

	public void paintComponent(Graphics g) {
		super.paintComponent(g);

		boolean gotLatency=false, toohigh=false;
		double latency=0.00;

		try {
			Double t=(Double)hash.get(which);
			latency=t.doubleValue();
			gotLatency=true;
		} catch(Exception e) {
			// Just needed to know
		}

		g.setFont(font);

		// Go ahead and draw the thermometer now.
		g.drawImage(image, 0, 0, this);

		if(gotLatency) {
			// We are limited to -40 to 140
			if(latency>=6975) {
				latency=6975;
				toohigh=true;
			} else if(latency < 0) {
				latency=-0;
			}

			drawLatency(g, black, latency);

			if(latency >= max_latency) {
				max_latency=latency;
			} else {
				drawLatency(g, blue, max_latency);
			}

			// Further operations will be black.
			g.setColor(black);

			String drawstring="" + (int)(latency/60) + "min";
			if(toohigh) {
				drawstring=">" + drawstring;
			}
			g.drawString(drawstring, 129, 68);
		} else {
			g.drawString("Error", 52, 82);
		}
		g.drawString(which, 5, size.height-8);
	}

	public void drawLatency(Graphics g, Color c, double latency) {

		double x2, y2;
		int trans;
		double rad, angle;

		g.setColor(c);

		// Translate the angle because we're a little crooked
		trans=-90;

		// Calculate the angle based on the temperature
		angle=(latency/20)+trans;

		rad=( (angle/360) * 2 * 3.14159265358979d );
		// Find the extra points
		x2=Math.sin(rad)*85;
		y2=Math.cos(rad)*85;
		// Negate the y, we're upside-down
		y2=-y2;
		// Move over to the starting points.
		x2+=120; y2+=120;
		// Draw the line.
		g.drawLine(120, 120,  (int)x2, (int)y2);
	}

	public Dimension getPreferredSize() {
		return(size);
	}
}
