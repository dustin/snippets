package net.spy.netinfo.ports;

import net.spy.netinfo.*;
import java.util.*;

public class Port25 extends Port {

	public Port25() {
		super();
		driver_info="SMTP driver";
	}

	public String info() {
		String ret=null;
		try {
			connect();
			String s=getline();
			ret=parse(s);
			send("QUIT");
			close();
		} catch(Exception e) {
			ret="Error:  " + e;
		}
		return(ret);
	}

	protected String parse(String in) throws Exception {
		StringTokenizer st = new StringTokenizer(in);
		String ret="";
		try {
			int status=Integer.valueOf(st.nextToken()).intValue(); // Status
			String hostname=st.nextToken(); // Hostname
			ret+=hostname + " ";
			String protocol=st.nextToken(); // SMTP/ESMTP
			String software=st.nextToken(); // ServerSoftware
			ret+=software + " ";
			// External mail servers are configured to not send banners.
			try {
				String version=st.nextToken(); // Version
				ret+=version + " ";
			} catch(Exception e) {
				// No big deal.
			}
		} catch(Exception e) {
			// If the above didn't work, just give back the input.
			ret=in;
		}
		return(ret);
	}
}
