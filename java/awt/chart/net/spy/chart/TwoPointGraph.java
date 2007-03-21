// Copyright (c) 2000 Dustin Sallings <dustin@spy.net>
//
// $Id: TwoPointGraph.java,v 1.1 2002/01/18 00:12:54 dustin Exp $

package net.spy.chart;

import java.awt.*;

/**
 * Superclass for all two point graphs (bars, lines, etc...).
 */
public abstract class TwoPointGraph extends Canvas {

	/**
	 * Width of this graph.
	 */
	protected int width=0;
	/**
	 * Height of this graph.
	 */
	protected int height=0;
	/**
	 * The data to graph.
	 */
	protected double data[]=null;
	/**
	 * The data to graph.
	 */
	protected String labels[]=null;

	/**
	 * The background color.
	 */
	protected Color background=null;
	/**
	 * The foreground color.
	 */
	protected Color foreground=null;

	private double max=0;
	private double min=0;
	
	private boolean bgset=false;

	/**
	 * Get a two point graph at the given width and height.
	 */
	public TwoPointGraph(int width, int height) {
		super();
		this.width=width;
		this.height=height;
		setSize(width, height);
		setFGColor(Color.black);
		setBGColor(Color.white);
	}

	/**
	 * Set the data to graph.
	 */
	public void setData(double data[]) {
		this.data=data;
		calcMinAndMax();
	}

	/**
	 * Set the data to graph (ints instead of double).
	 */
	public void setData(int data[]) {
		double d[]=new double[data.length];
		for(int i=0; i<d.length; i++) {
			d[i]=(double)data[i];
		}
		setData(d);
	}

	/**
	 * Set the labels.  There should be one entry in this array for each
	 * entry in the data array.
	 */
	public void setLabels(String labels[]) {
		this.labels=labels;
	}

	/**
	 * Set the foreground color.
	 */
	public void setFGColor(Color c) {
		foreground=c;
	}

	/**
	 * Set the background color.
	 */
	public void setBGColor(Color c) {
		background=c;
		setBackground(background);
	}

	private void calcMinAndMax() {
		max=data[0];
		min=data[0];

		for(int i=0; i<data.length; i++) {
			if(data[i]>max) {
				max=data[i];
			} else if(data[i]<min) {
				min=data[i];
			}
		}
	}

	/**
	 * Get the maximum value in the data set.
	 */
	protected double getMax() {
		return(max);
	}

	/**
	 * Get the minimum value in the data set.
	 */
	protected double getMin() {
		return(min);
	}

	/**
	 * Place a label at the given position.
	 */
	protected int labelPos(String label, Graphics g, int x, double bwidth) {
		FontMetrics fm=g.getFontMetrics();
		int width=fm.stringWidth(label);
		
		// X has to take into account the item width
		x+=(int)(bwidth*2);
		// Make sure the string starts at the right place
		x-=(int)(width/2);

		return(x);
	}

	public abstract void paint(Graphics g);
}
