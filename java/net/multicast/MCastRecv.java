// Copyright (c) 2001  Dustin Sallings <dustin@spy.net>
//
// $Id: MCastRecv.java,v 1.1 2001/07/28 02:03:10 dustin Exp $

import java.net.*;

/**
 * 
 */
public class MCastRecv extends Object {

	/**
	 * Testing and what not.
	 */
	public static void main(String args[]) throws Exception {
		InetAddress group = InetAddress.getByName(args[0]);
		MulticastSocket s = new MulticastSocket(6789);
		s.joinGroup(group);

		byte data[]=new byte[1500];
		DatagramPacket recv = new DatagramPacket(data, data.length);
		s.receive(recv);

		byte tmp_d[]=new byte[recv.getLength()];
		System.arraycopy(data, 0, tmp_d, 0, tmp_d.length);

		String tmp=new String(tmp_d);
		System.out.println("Got:\n\t" + tmp
			+ "\n (" + tmp.length() + " bytes)");

		s.leaveGroup(group);
	}

}

