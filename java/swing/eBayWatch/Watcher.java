import java.util.*;
import net.spy.info.eBay;

public class Watcher extends Object {
	protected String descr="", price="", who="", time="";
	protected String item_no=null;
	protected String old_status="";
	protected String current_status="";
	protected boolean changed=true;

	public String toString() {
		return("{Watcher " + descr + "}");
	}

	public boolean hasChanged() throws Exception {
		old_status=current_status;

		changed=false;

		eBay e=new eBay(item_no);

		time=e.get("Time left");
		descr=e.get("Description");
        if(descr==null) {
            descr="Item " + item_no;
        }
		price=e.get("Currently");
		who=e.get("High bid");

		current_status=descr + "\n" + price + "\n" + who + "\n";
		if(!current_status.equals(old_status)) {
			changed=true;
		}

		return(changed);
	}

    public double getPrice() {
        double rv=0;
        try {
            String tmp=price;
            if(price.indexOf("$")>=0) {
                tmp=price.substring(price.indexOf("$")+1);
            }
            rv=Double.parseDouble(tmp);
        } catch(NumberFormatException e) {
            e.printStackTrace();
        }
        return(rv);
    }

    /**
     * Who's bidding?
     */
    public String getWho() {
        return(who);
    }

	public String getItemNo() {
		return(item_no);
	}

	public String describe() {
		String ret="";
		if((descr!=null) && descr.length()>0) {
			ret=descr;
		} else {
			ret="Item " + item_no;
		}
		return(ret);
	}

	public String currentStatus() {
		return(current_status);
	}

	public String oldStatus() {
		return(old_status);
	}

	public String getTime() {
		return(time);
	}

	public Watcher(String item_no) {
		super();
		this.item_no=item_no;
	}

	public static void main(String args[]) throws Exception {
		Vector v = new Vector();

		for(int i=0; i<args.length; i++) {
			v.addElement(new Watcher(args[i]));
		}

		for(;;) {
			for(Enumeration e=v.elements(); e.hasMoreElements();) {
				Watcher ebw=(Watcher)e.nextElement();
				if(ebw.hasChanged()) {
					System.out.println("Info for " + ebw.describe());
					System.out.println(ebw.currentStatus());
				}
			}

			try {
				Thread.sleep(60000);
			} catch(Exception ex) {
				// Blah
			}
		}
	}
}
