package net.spy.netinfo;

public class Service extends Object {
	String host=null;
	int port=-1;

	public Service() {
	}

	public Service(String h, int p) {
		host=h;
		port=p;
	}
}
