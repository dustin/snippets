package net.spy.netinfo;

import java.net.*;
import java.io.*;

public class Port extends Object {
	public static String driver_info = "Undefined driver";
	String host=null;
	int port=-1;
	BufferedReader bin=null;
	PrintWriter prout;
	Socket s=null;

	public Port() {
		super();
	}

	public void set(String h, int p) {
		host=h;
		port=p;
	}

	public String info() {
		return("NO DRIVER!!!");
	}

	protected void connect() throws Exception {
		InputStream in;
		OutputStream out;
		BufferedReader din;
		s = new Socket(host, port);
		in=s.getInputStream();
		bin = new BufferedReader(new InputStreamReader(in));
		out=s.getOutputStream();
		prout=new PrintWriter(out);
	}

	public void close() throws Exception {
		s.close();
		bin=null;
		prout=null;
	}

	protected String getline() throws Exception {
		return(bin.readLine());
	}

	protected void send(String msg) throws Exception {
		prout.println(msg);
		prout.flush();
	}
}
