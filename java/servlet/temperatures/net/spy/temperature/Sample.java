// Copyright (c) 2003  Dustin Sallings <dustin@spy.net>
//
// $Id: Sample.java,v 1.1 2003/02/23 08:05:24 dustin Exp $

package net.spy.temperature;

/**
 * A sample read via multicast.
 */
public class Sample extends Object {

	private String name=null;
	private long birthday=0;
	private long modtime=0;
	private Double sample=null;

	/**
	 * Get a new sample with the given name.
	 *
	 * @param name
	 */
	public Sample(String name) {
		super();
		this.name=name;
		birthday=System.currentTimeMillis();
	}

	/**
	 * String me.
	 *
	 * @return
	 */
	public String toString() {
		return("{Sample:  " + name + " val=" + sample
			+ " age=" + age() + "}");
	}

	/**
	 * Get the age of this sample.
	 */
	public long age() {
		return(System.currentTimeMillis()-modtime);
	}

	/**
	 * Get the sample.
	 *
	 * @return
	 */
	public Double getSample() {
		return(sample);
	}

	/**
	 * Set the sample.
	 */
	public void setSample(Double s) {
		sample=s;
		modtime=System.currentTimeMillis();
	}
}

