// Copyright (c) 2001  Dustin Sallings <dustin@spy.net>
//
// $Id: MCastLog.java,v 1.3 2002/07/10 04:25:45 dustin Exp $

package net.spy.log;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.ObjectOutputStream;
import java.io.OutputStream;

import java.net.DatagramPacket;
import java.net.InetAddress;
import java.net.MulticastSocket;

/**
 * Multicast log sender.
 */
public class MCastLog extends Object {

	private MulticastSocket socket=null;
	private InetAddress ia=null;
	private int port=0;

	/**
	 * Get an instance of MCastLog that will send messages to the given
	 * Multicast address on the given port.
	 */
	public MCastLog(InetAddress ia, int port) throws IOException {
		super();
		if(!ia.isMulticastAddress()) {
			throw new IOException("Not a multicast address.");
		}
		this.port=port;
		this.ia=ia;
		socket=new MulticastSocket();
	}

	/**
	 * Send a SpyMessage on this multicast socket.
	 */
	public void sendMessage(SpyMessage sm) throws IOException {
		ByteArrayOutputStream bos=new ByteArrayOutputStream();
		ObjectOutputStream os=new ObjectOutputStream(bos);
		os.writeObject(sm);
		os.close();
		bos.close();

		byte data[]=bos.toByteArray();
		DatagramPacket packet=new DatagramPacket(data, data.length, ia, port);
		socket.send(packet);
	}

	/**
	 * Close this log thingy.
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
		MCastLog mcl=new MCastLog(InetAddress.getByName("227.227.227.227"),
			3432);
		for(int i=0; i<1000; i++) {
			System.out.println("Sending...");
			SpyMessage sm=new SpyMessage("Message " + i);
			mcl.sendMessage(sm);
			// Thread.sleep(100);
		}
		mcl.close();
	}

}

