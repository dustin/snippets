/*
 * Copyright (c) 2001  Dustin Sallings <dustin@spy.net>
 *
 * $id$
 */

package net.spy.net;

import java.net.*;
import java.io.*;

/**
 * Because java doesn't have the ability to timeout on a connection, this
 * does.
 */
public abstract class TimedSocket extends Object {

	// Polling delay for socket checks (in milliseconds)
	private static final int POLL_DELAY = 100;

	/**
	 * Connect to a given host and port for a bit.
	 *
	 * @param	addr  Address of host
	 * @param	port  Port of service
	 * @param	delay Delay in milliseconds
	 *
	 * @exception IOTimeoutException if there's a timeout connecting
	 * @exception IOException for other failures connecting
	 */
	public static Socket getSocket(InetAddress addr, int port, int delay)
		throws IOTimeoutException, IOException {

		// Start the stopwatch
		SocketThread st=new SocketThread(addr, port);
		st.start();

		int timer = 0;
		Socket sock = null;

		for (;;) {
			// Check to see if a connection is established
			if (st.isConnected()) {
				// Yes ...  assign to sock variable, and break out of loop
				sock = st.getSocket();
				break;
			} else {
				// Check to see if an error occurred
				if (st.isError()) {
					// No connection could be established
					throw (st.getException());
				}
				try {
					// Sleep for a short period of time
					Thread.sleep ( POLL_DELAY );
				} catch (InterruptedException ie) {
					// This is OK.
				}

				// Increment timer
				timer += POLL_DELAY;

				// Check to see if time limit exceeded
				if (timer > delay) {
					st.interrupt();
					// Can't connect to server
					throw new IOTimeoutException("Could not connect for "
						+ delay + " milliseconds");
				}
			} // else (not connected)
		} // for loop

		return(sock);
    }

	// Test case
	public static void main(String args[]) throws Exception {
		if (args.length < 3 ) {
			System.out.println("Usage: TimedSocket server port delay(ms)");
			return;
		}

		Socket s = TimedSocket.getSocket(InetAddress.getByName(args[0]),
			new Integer(args[1]).intValue(), new Integer(args[2]).intValue());
		s.close();
		System.out.println ("connected to " + args[0] + ":" + args[1]);
	}

	// Inner class for establishing a socket thread
	// within another thread, to prevent blocking.
	static class SocketThread extends Thread {
		// Socket connection to remote host
		volatile private Socket conn=null;
		// Internet Address to connect to
		private InetAddress addr=null;
		// Port number to connect to
		private int port=0;
		// Exception in the event a connection error occurs
		private IOException exception=null;

		// Connect to the specified host IP and port number
		public SocketThread (InetAddress addr, int port) {
			// Assign to member variables
			this.addr = addr;
			this.port = port;
		}

		public void run() {
			// Socket used for establishing a connection
			Socket sock = null;

			try {
				// Try to get the socket.
				sock=new Socket (addr, port);
			} catch (IOException ioe) {
				// Assign to our exception member variable
				exception=ioe;
				return;
			}

			// If socket constructor returned without error,
			// then connection finished
			conn = sock;
		}

		// Are we connected?
		public boolean isConnected() {
			return(conn != null);
		}

		// Did an error occur?
		public boolean isError() {
			return(exception!=null);
		}

		// Get socket
		public Socket getSocket() {
			return(conn);
		}

		// Get exception
		public IOException getException() {
			return(exception);
		}
	}
}
