// Copyright (c) 2002  Dustin Sallings <dustin@spy.net>
//
// $Id: Gatherer.java,v 1.2 2002/11/25 01:54:19 dustin Exp $

package net.spy.temperature;

import java.io.IOException;

import java.util.Collections;
import java.util.Map;
import java.util.HashMap;
import java.util.ResourceBundle;
import java.util.MissingResourceException;
import java.util.StringTokenizer;

import java.net.DatagramPacket;
import java.net.InetAddress;
import java.net.MulticastSocket;

import net.spy.SpyThread;

/**
 * Gather temperatures from a multicast socket.
 */
public class Gatherer extends SpyThread {

	private MulticastSocket s=null;
	private InetAddress group=null;
	private int port=-1;
	private boolean running=true;

	private int updates=0;
	private Map seen=null;
	private ResourceBundle serials=null;

	private static Gatherer instance=null;

	/**
	 * Get an instance of Gatherer.
	 */
	public Gatherer(InetAddress group, int port) throws IOException {
		super();

		this.group=group;
		this.port=port;
		seen=Collections.synchronizedMap(new HashMap());
		// Serial number -> name mapping
		serials=ResourceBundle.getBundle("net.spy.temperature.therms");

		s=new MulticastSocket(port);
		s.joinGroup(group);

		setName("Temperature Gatherer");
		setDaemon(true);
		start();
	}

	/**
	 * String me.
	 */
	public String toString() {
		StringBuffer sb=new StringBuffer(128);

		sb.append(super.toString());
		sb.append(" - has read ");
		sb.append(updates);
		sb.append(" updates, currently tracking ");
		sb.append(seen.size());
		sb.append(" thermometers");

		return(sb.toString());
	}

	/**
	 * Shut down the socket.
	 */
	public void stopRunning() {
		running=false;

		try {
			s.leaveGroup(group);
			s.close();
		} catch(IOException e) {
			getLogger().warn("Problem leaving multicast group.", e);
		}
	}

	/**
	 * Get a read-only view of the current readings.
	 *
	 * @return an unmodifiable view of the seen map
	 */
	public Map getSeen() {
		return(Collections.unmodifiableMap(seen));
	}

	/**
	 * Get the current value for the named thermometer.
	 *
	 * @param name name of the thermometer
	 * @return the value, or null if there's no mapping or the value is
	 * unavailable
	 */
	public Double getSeen(String name) {
		Double rv=(Double)seen.get(name);

		return(rv);
	}

	private void process(DatagramPacket recv) throws IOException {
		byte data[]=recv.getData();
		byte tmp[]=new byte[recv.getLength()];
		System.arraycopy(data, 0, tmp, 0, tmp.length);
		String entry=new String(tmp);

		StringTokenizer st = new StringTokenizer(entry, "\t");
		if(st.countTokens() < 3) {
			throw new IOException("This message doesn't make sense:  " + entry);
		}
		String date_str = st.nextToken();
		String serial = st.nextToken();
		String sample_str = st.nextToken();

		Double sample=new Double(sample_str);

		try {
			seen.put(serials.getString(serial), sample);
			if(getLogger().isDebugEnabled()) {
				getLogger().debug("Got sample for "
					+ serials.getString(serial) + " " + sample);
			}
		} catch(MissingResourceException e) {
			getLogger().warn("Unknown serial number seen:  " + serial, e);
			seen.put(serial, sample);
		}

		updates++;
	}

	/**
	 * Watch for incoming temperature updates and record them.
	 */
	public void run() {
		while(running) {
			try {
				byte data[]=new byte[1500];
				DatagramPacket recv = new DatagramPacket(data, data.length);
				s.receive(recv);
				process(recv);

			} catch(IOException e) {
				getLogger().error("Exception processing temperature packet", e);
			} catch(Throwable t) {
				getLogger().fatal("UNEXPECTED ERROR IN GATHERER", t);
			}
		}
	}

}
