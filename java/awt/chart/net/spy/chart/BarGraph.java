// Copyright (c) 2000 Dustin Sallings <dustin@spy.net>
//
// $Id: BarGraph.java,v 1.2 2002/01/18 00:12:53 dustin Exp $

package net.spy.chart;

import java.awt.*;
import java.util.*;

public class BarGraph extends TwoPointGraph {

	public BarGraph(int width, int height) {
		super(width, height);
	}

	public void paint(Graphics g) {

		g.setColor(foreground);

		double yfactor=((double)(height-20))/(double)getMax();
		double xfactor=(double)((width-20))/(double)data.length;
		double bwidth=(xfactor/2)-(xfactor*.10);

		for(int i=0; i<data.length; i++) {
			int x=(int)(i*xfactor)-10;
			int y=(int)(data[i]*yfactor)-40;
			String label=labels[i];

			int x1=(int)(x+bwidth);
			int y1=(int)((height-25)-y);
			int width1=(int)(bwidth*2);
			int height1=(int)((height-25)-y1);

			// Make sure the height is at least 1
			if(height1<1) {
				height1=1;
			}

			// Draw the bar
			g.fillRect(x1, y1, width1, height1);

			// Label the bottom
			g.drawString(label, labelPos(label, g, x, bwidth), height-10);

			// Label the value (and make sure it's short enough)
			label="" + data[i];
			int lwidth=g.getFontMetrics().stringWidth(label);
			while(lwidth>(bwidth*2)) {
				label=label.substring(0, label.length()-1);
				lwidth=g.getFontMetrics().stringWidth(label);
			}

			g.drawString(label, labelPos(label, g, x, bwidth), y1-10);
		}
	}

	private static double[] getNumbers() {
		double a[]=new double[5];
		Random r=new Random();

		for(int i=0; i<a.length; i++) {
			a[i]=(double)Math.abs(r.nextFloat());
		}

		return(a);
	}

	public static void main(String args[]) throws Exception {
		String labels[]={
			"One", "Two", "Three", "Four", "Five", "Six", "Seven", "Eight",
			"Nine", "Ten"
		};
		Frame f=new Frame();
		BarGraph bg=new BarGraph(640, 480);
		bg.setData(getNumbers());
		bg.setLabels(labels);
		// bg.setBGColor(Color.red);
		// bg.setFGColor(Color.green);
		f.add(bg);
		f.pack();
		f.show();
	}
}
