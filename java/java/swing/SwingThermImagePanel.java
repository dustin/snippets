import javax.swing.*;          //This is the final package name.
//import com.sun.java.swing.*; //Used by JDK 1.2 Beta 4 and all
                               //Swing releases before Swing 1.1 Beta 3.
import java.lang.*;
import java.awt.*;
import java.awt.event.*;
import java.util.*;

import net.spy.temperature.*;

class SwingThermImagePanel extends JPanel {
	Image image;
	Font font=null;
	String which=null;
	Hashtable hash=null;

	Color black=null;
	Color white=null;

	Dimension size=null;

	public SwingThermImagePanel(Image image, String which, Hashtable hash) {
		super();

		this.image=image;
		this.which=which;
		this.hash=hash;

		black=new Color(0, 0, 0);
		white=new Color(255, 255, 255);

		setBackground(white);

		size=new Dimension(133,146);

		font=new Font("SanSerif", Font.PLAIN, 10);
	}

	public void paintComponent(Graphics g) {
		super.paintComponent(g);

		double x2, y2;
		int trans;
		double rad, angle;
		boolean gotTemp=false;
		double temp=0.0;

		try {
			Double t=(Double)hash.get(which);
			temp=t.doubleValue();
			gotTemp=true;
		} catch(Exception e) {
			// Just needed to know
		}

		g.setFont(font);

		// Further operations will be black.
		g.setColor(black);

		// Go ahead and draw the thermometer now.
		g.drawImage(image, 0, 0, this);

		if(gotTemp) {
			// We are limited to -40 to 140
			if(temp>140) {
				temp=140;
			} else if(temp < -40) {
				temp=-40;
			}

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
		} else {
			g.drawString("Error", 52, 82);
		}
		g.drawString(which, 5, 142);
	}

	public Dimension getPreferredSize() {
		return(size);
	}
}
