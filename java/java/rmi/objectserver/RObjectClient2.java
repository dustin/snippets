import java.rmi.Naming;
import java.rmi.RemoteException;

import java.util.*;

public class RObjectClient2 {

	public RObjectClient2(String args[]) {
		allthework(args);
	}

	public static void main(String args[])  {
		RObjectClient2 r = new RObjectClient2(args);
	}

	private void allthework(String args[]) {
		try {
			RObject obj = new RObjectImpl();
			Hashtable h = new Hashtable();
			String url = "//localhost/RObjectServer";
			obj = (RObject)Naming.lookup(url);

			h.put("Noelani", "Hunt");
			h.put("Dustin", "Sallings");
			h.put("Sidney", "Sallings");

			obj.storeHash(h);

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
