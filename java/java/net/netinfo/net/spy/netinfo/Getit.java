package net.spy.netinfo;

import java.lang.*;
import java.util.*;

public class Getit extends Thread {
	Stack stuff=null;

	public Getit(Stack v) {
		super();
		stuff=v;
	}

	public boolean getone() {
		boolean ret=false;
		try {
			Service s = (Service)stuff.pop();
			Info i = new Info(s.host, s.port);
			System.out.println(s.host + ":" + s.port + ":  " + i.summary());
				ret=true;
		} catch(Exception e) {
		}
		return(ret);
	}

	public void run() {
		while(getone());
	}
}
