import java.util.*;
import java.io.*;

public class CachePK {
	static Stack urls;

	public static void main(String args[]) throws Exception {
		Fetch f=null;
		BufferedWriter log_file=null;

		// Set up the log file.
		log_file = new BufferedWriter( new FileWriter("cache.log", false), 1);

		urls=new Stack();

		f = new Fetch("http://darwin/cgi-bin/stats/gettoporders.cgi");
		Vector topOrders = f.getLines();
		log_file.write("# Got " + topOrders.size() + " skus\n");

		f = new Fetch("http://verde.software.net/~dustin/ld/is/beyond:80");
		Vector machines = f.getLines();
		log_file.write("# Got " + machines.size() + " servers\n");
		log_file.flush();

		for(int j=topOrders.size()-1; j>=0; j--) {
			String end = "/PK" + (String)topOrders.elementAt(j) + "/prod.htm";
			for(int i=0; i<machines.size(); i++) {
				Fetch f2=null;
				String host=(String)machines.elementAt(i);
				String url="http://localhost:8000/http://" + host + end;
				urls.push(url);
			}
		}

		System.out.println("Initializing threads...");

		Thread t = new Flusher(log_file);
		t.start();
		for(int i=0; i<10; i++) {
			t=new Getit(urls, log_file);
			t.start();
		}
	}
}
