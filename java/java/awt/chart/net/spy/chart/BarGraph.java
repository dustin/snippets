// Copyright (c) 2000 Dustin Sallings <dustin@spy.net>
//
// $Id: BarGraph.java,v 1.1 2000/10/19 08:55:51 dustin Exp $

package net.spy.chart;

import java.awt.*;
import java.util.*;

public class BarGraph extends Canvas {

	protected int width=0;
	protected int height=0;
	protected double data[]=null;
	protected String labels[]=null;

	protected Color background=null;
	protected Color foreground=null;

	protected boolean bgset=false;

	public BarGraph(int width, int height) {
		super();
		this.width=width;
		this.height=height;
		setSize(width, height);
		background=Color.white;
		foreground=Color.black;
	}

	public void setData(double data[]) {
		this.data=data;
	}

	public void setData(int data[]) {
		double d[]=new double[data.length];
		for(int i=0; i<d.length; i++) {
			d[i]=(double)data[i];
		}
		this.data=d;
	}

	public void setLabels(String labels[]) {
		this.labels=labels;
	}

	public void setFGColor(Color c) {
		foreground=c;
	}

	public void setBGColor(Color c) {
		background=c;
	}

	protected int labelPos(String label, Graphics g, int x, double bwidth) {
		FontMetrics fm=g.getFontMetrics();
		int width=fm.stringWidth(label);

		// X has to take into account the item width
		x+=(int)(bwidth*2);
		// Make sure the string starts at the right place
		x-=(int)(width/2);

		return(x);
	}

	public void paint(Graphics g) {
		double max=0;

		if(!bgset) {
			setBackground(background);
			bgset=true;
		}

		g.setColor(foreground);

		// Find the max
		for(int i=0; i<data.length; i++) {
			if(data[i]>max) {
				max=data[i];
			}
		}

		double yfactor=((double)(height-20))/(double)max;
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

	public static double[] getNumbers() {
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
		bg.setBGColor(Color.red);
		bg.setFGColor(Color.green);
		f.add(bg);
		f.pack();
		f.show();
	}
}
