import java.rmi.Naming;
import java.rmi.RemoteException;

import java.util.*;

public class RObjectClient {

	public RObjectClient(String args[]) {
		allthework(args);
	}

	public static void main(String args[])  {
		RObjectClient r = new RObjectClient(args);
	}

	private void allthework(String args[]) {
		try {
			RObject obj = new RObjectImpl();
			Hashtable h = new Hashtable();
			String url = "//localhost/RObjectServer";
			obj = (RObject)Naming.lookup(url);
			h = obj.getHash();

			dumphash(h);

		} catch (Exception e) {
			System.out.println("RObject exception: " +
						e.getMessage());
			e.printStackTrace();
		}
		System.exit(0);
	}

	private void dumphash(Hashtable h) {
		System.out.println(h.toString());
	}

}
