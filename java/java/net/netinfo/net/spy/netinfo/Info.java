package net.spy.netinfo;

public class Info extends Object {
	Port port = null;

	public Info(String h, int p) throws Exception {
		String classname = "net.spy.netinfo.ports.Port" + p;
		port = (Port)Class.forName(classname).newInstance();
		port.set(h, p);
	}

	public String summary() {
		return(port.info());
	}
}
