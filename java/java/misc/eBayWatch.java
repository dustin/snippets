import java.util.*;
import net.spy.info.eBay;

public class eBayWatch extends Object {
	String descr="", price="", who="", time="";
	int request_no=0;
	String item_no=null;

	public String getDiff() throws Exception {
		String toprint="";
		request_no++;

		eBay e=new eBay(item_no);

		// Every twenty requests, print the whole thing again...
		if(request_no==20) {
			toprint+="Full Description:\n";
			descr=""; price=""; who=""; time="";
			request_no=0;
		}

		if(!descr.equals(e.get("Description"))) {
			descr=e.get("Description");
			toprint+=descr + "\n";
		}
		if(!price.equals(e.get("Currently"))) {
			price=e.get("Currently");
			toprint+=price + "\n";
		}
		if(!who.equals(e.get("High bid"))) {
			who=e.get("High bid");
			toprint+=who + "\n";
		}
		if(!time.equals(e.get("Time left"))) {
			time=e.get("Time left");
			toprint+=time + "\n";
		}
		if(toprint.length() > 0) {
			toprint+="\n";
		}
		return(toprint);
	}

	public String getItemNo() {
		return(item_no);
	}

	public String describe() {
		String ret="";
		if(descr.length()>0) {
			ret=descr;
		} else {
			ret="Item " + item_no;
		}
		return(ret);
	}

	public eBayWatch(String item_no) {
		super();
		this.item_no=item_no;
	}

	public static void main(String args[]) throws Exception {
		Vector v = new Vector();

		for(int i=0; i<args.length; i++) {
			v.addElement(new eBayWatch(args[i]));
		}

		for(;;) {
			for(Enumeration e=v.elements(); e.hasMoreElements();) {
				eBayWatch ebw=(eBayWatch)e.nextElement();
				System.out.println("Info for " + ebw.describe());
				System.out.println(ebw.getDiff());
			}

			try {
				Thread.sleep(60000);
			} catch(Exception ex) {
				// Blah
			}
		}
	}
}
