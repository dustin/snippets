package net.spy.netinfo.ports;

import net.spy.netinfo.*;
import java.util.*;

public class Port80 extends Port {

	public Port80() {
		super();
		driver_info="SMTP driver";
	}

	public String info() {
		String ret=null;
		try {
			connect();
			send("HEAD / HTTP/1.0");
			send("");
			String s;
			while( (s=getline())!= null ) {
				if(s.startsWith("Server")) {
					// System.out.println(s);
					ret=s.substring(8);
				}
			}
			close();
		} catch(Exception e) {
			ret="Error:  " + e;
		}
		return(ret);
	}
}
