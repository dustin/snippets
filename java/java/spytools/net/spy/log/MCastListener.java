// Copyright (c) 2001  Dustin Sallings <dustin@spy.net>
//
// $Id: MCastListener.java,v 1.4 2002/07/10 05:41:35 dustin Exp $

package net.spy.log;

import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.io.ObjectInputStream;

import java.net.DatagramPacket;
import java.net.InetAddress;
import java.net.MulticastSocket;

/**
 * Listen for multicast messages.
 */
public class MCastListener extends Object {

	private InetAddress ia=null;
	private int port=0;
	private MulticastSocket socket=null;

	/**
	 * Get an instance of MCastListener listening on the given group and
	 * port.
	 */
	public MCastListener(InetAddress ia, int port) throws IOException {
		super();
		this.ia=ia;
		this.port=port;
		if(!ia.isMulticastAddress()) {
			throw new IOException("Not a multicast address.");
		}
		socket=new MulticastSocket(port);
		socket.joinGroup(ia);
	}

	/**
	 * Get the next message in the multicast group.
	 */
	public SpyMessage getNextMessage() throws IOException {
		byte buf[]=new byte[8192];
		DatagramPacket recv = new DatagramPacket(buf, buf.length);
		socket.receive(recv);
		ByteArrayInputStream bis=new ByteArrayInputStream(buf);
		ObjectInputStream is=new ObjectInputStream(bis);
		SpyMessage sm=null;
		try {
			sm=(SpyMessage)is.readObject();
		} catch(ClassNotFoundException cnfe) {
			cnfe.printStackTrace();
		}
		is.close();
		bis.close();
		return(sm);
	}

	/**
	 * Close up the multicast socket and free any other resources this
	 * thing is using.
	 */
	public void close() {
		if(socket!=null) {
			socket.close();
			socket=null;
		}
	}

	/**
	 * Testing and what not.
	 */
	public static void main(String args[]) throws Exception {
		MCastListener mcl=new MCastListener(
			InetAddress.getByName("227.227.227.227"), 3432);
		while(true) {
			SpyMessage sm=mcl.getNextMessage();
			System.out.println(sm);
		}
		// NOT REACHED
		// mcl.close();
	}

}

