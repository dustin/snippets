package net.spy.netinfo.ports;

import net.spy.netinfo.*;
import java.util.*;

public class Port22 extends Port {

	public Port22() {
		super();
		driver_info="SSH driver";
	}

	public String info() {
		String ret=null;
		try {
			connect();
			String s=getline();
			ret=s;
			send("");
			close();
		} catch(Exception e) {
			ret="Error:  " + e;
		}
		return(ret);
	}
}
