// Copyright (c) 2001  Dustin Sallings <dustin@spy.net>
//
// $Id: RingBuffer.java,v 1.4 2002/08/21 00:53:18 dustin Exp $

package net.spy.util;

import java.util.Collection;
import java.util.Iterator;
import java.util.ArrayList;

/**
 * A circular buffer.
 */
public class RingBuffer extends Object {

	private Object buf[]=null;
	private int start=0;
	private int end=0;
	private boolean wrapped=false;
	private int size=0;

	/**
	 * Get an instance of RingBuffer.
	 */
	public RingBuffer(int size) {
		super();
		buf=new Object[size];
		for(int i=0; i<size; i++) {
			buf[i]=null;
		}
	}

	/**
	 * Add an object to the ring buffer (if it's full, it'll cycle the
	 * oldest one out).
	 */
	public synchronized void addObject(Object o) {
		if(end>=buf.length) {
			// Will get set to 0
			end=0;
			wrapped=true;
		}
		buf[end]=o;
		end++;
		if(wrapped) {
			start++;
			if(start>=buf.length) {
				start=0;
			}
		} else {
			size++;
		}
	}

	/**
	 * Check to see if the ring buffer has wrapped.
	 *
	 * @return true if the ring buffer has wrapped
	 */
	public boolean hasWrapped() {
		return(wrapped);
	}

	/**
	 * Get the sequenced data that exists in the RingBuffer.
	 */
	public synchronized Collection getData() {
		ArrayList a=new ArrayList();

		if(end<=start) {
			for(int i=start; i<buf.length; i++) {
				a.add(buf[i]);
			}
			for(int i=0; i<end; i++) {
				a.add(buf[i]);
			}
		} else {
			for(int i=start; i<end; i++) {
				a.add(buf[i]);
			}
		}
		return(a);
	}

	/**
	 * String me.
	 */
	public String toString() {
		StringBuffer sb=new StringBuffer(256);
		sb.append("{RingBuffer cap=");
		sb.append(getCapacity());
		sb.append(" s=");
		sb.append(start);
		sb.append(", e=");
		sb.append(end);
		sb.append(" [");
		for(int i=0; i<buf.length; i++) {
			sb.append(buf[i]);
			sb.append(" ");
		}
		sb.append("]\n\t");
		ArrayList a=new ArrayList();
		for(Iterator i=getData().iterator(); i.hasNext(); ) {
			a.add(i.next());
		}
		sb.append(a);
		sb.append("}");
		return(sb.toString());
	}

	/**
	 * Get the number of objects in this RingBuffer.
	 */
	public int getSize() {
		return(size);
	}

	/**
	 * Get the total capacity of this RingBuffer.
	 * @return the number of objects this RingBuffer will hold
	 */
	public int getCapacity() {
		return(buf.length);
	}

}
