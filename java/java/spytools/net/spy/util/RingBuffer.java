// Copyright (c) 2001  Dustin Sallings <dustin@spy.net>
//
// $Id: RingBuffer.java,v 1.2 2002/07/10 04:26:40 dustin Exp $

package net.spy.util;

import java.util.Enumeration;
import java.util.Vector;

/**
 * A circular buffer.
 */
public class RingBuffer extends Object {

	private Object buf[]=null;
	private int start=0;
	private int end=0;
	private boolean wrapped=false;

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
		}
	}

	/**
	 * Get the sequenced data that exists in the RingBuffer.
	 */
	public synchronized Enumeration getData() {
		Vector v=new Vector();

		if(end<=start) {
			for(int i=start; i<buf.length; i++) {
				v.addElement(buf[i]);
			}
			for(int i=0; i<end; i++) {
				v.addElement(buf[i]);
			}
		} else {
			for(int i=start; i<end; i++) {
				v.addElement(buf[i]);
			}
		}
		return(v.elements());
	}

	/**
	 * String me.
	 */
	public String toString() {
		StringBuffer sb=new StringBuffer();
		sb.append(start);
		sb.append(",");
		sb.append(end);
		sb.append(" [");
		for(int i=0; i<buf.length; i++) {
			sb.append(buf[i]);
			sb.append(" ");
		}
		sb.append("]\n\t");
		Vector v=new Vector();
		for(Enumeration e=getData(); e.hasMoreElements(); ) {
			v.addElement(e.nextElement());
		}
		sb.append(v);
		return(sb.toString());
	}

	/**
	 * Get the size of this RingBuffer (total size, not number stored).
	 */
	public int getSize() {
		return(buf.length);
	}

	// This is only done for testing from main()
	private static void verify(RingBuffer rb) throws Exception {

		// OK, now verify it's correct.
		Vector v=new Vector();
		for(Enumeration e=rb.getData(); e.hasMoreElements(); ) {
			v.addElement(e.nextElement());
		}
		// Verify the size.
		if(v.size()!=rb.getSize()) {
			throw new Exception("It's not full "
				+ "got " + v.size() + " instead of " + rb.getSize());
		}

		int i=((Integer)v.elementAt(0)).intValue();
		for(Enumeration e=v.elements(); e.hasMoreElements(); i++) {
			Integer itmp=(Integer)e.nextElement();
			int tmp=itmp.intValue();
			if(tmp!=i) {
				throw new Exception("Out of sequence, expected "
					+ i + " but got " + tmp + ":  " + rb);
			}
		}
	}

	/**
	 * Testing and what not.
	 */
	public static void main(String args[]) throws Exception {
		RingBuffer rb=new RingBuffer(Integer.parseInt(args[0]));

		// Fill it.
		for(int i=0; i<rb.getSize(); i++) {
			rb.addObject(new Integer(i));
			System.out.println(rb);
		}

		// Do more extensive testing.
		for(int i=rb.getSize(); i<1000; i++) {
			rb.addObject(new Integer(i));
			System.out.println(rb);
			verify(rb);
			Thread.sleep(125);
		}

	}

}
