// Copyright (c) 2001  Dustin Sallings <dustin@spy.net>
//
// $Id: MCastSend.java,v 1.1 2001/07/28 02:03:11 dustin Exp $

import java.net.*;

/**
 *
 */
public class MCastSend extends Object {

	/**
	 * Testing and what not.
	 */
	public static void main(String args[]) throws Exception {
		String msg=args[1];
		InetAddress group = InetAddress.getByName(args[0]);
		MulticastSocket s = new MulticastSocket(16789);
		// s.joinGroup(group);
		DatagramPacket hi = new DatagramPacket(msg.getBytes(),
			msg.length(), group, 6789);
		s.send(hi);
		// s.leaveGroup(group);
	}

}

