import java.util.*;
import net.spy.*;

public class SiemensToMot extends Object {

	protected String mot_data=null;
	int current_octave=0;
	int octave_translate=2;
	String current_length="";
	static Hashtable lengths=null;

	public SiemensToMot(String input) throws Exception {
		super();
		mot_data="";
		StringTokenizer st=new StringTokenizer(input);

		initLengths();

		while(st.hasMoreTokens()) {
			String in=st.nextToken();
			mot_data+=getThing(in);
		}
	}

	protected synchronized void initLengths() {
		if(lengths==null) {
			lengths=new Hashtable();
			lengths.put("(1/1)",  "w");
			lengths.put("(1/2)",  "h");
			lengths.put("(1/4)",  "q");
			lengths.put("(1/8)",  "e");
			lengths.put("(1/16)", "s");
			lengths.put("(1/32)", "t");
		}
	}

	protected String getThing(String s) {
		String ret="";
		String l, r, length;
		int paren=0;

		paren=s.indexOf("(");
		// If there's no paren, we can't use this.
		if(paren<0) {
			return("");
		}

		l=s.substring(0, paren);
		r=s.substring(paren);

		if(l.length() == 1) {
			if(!l.equals("P")) {
				return("");
			} else {
				ret+="R";
			}
		} else if(l.length()==2) {
			String note;
			int octave;
			note=l.substring(0, 1);
			octave=Integer.parseInt(l.substring(1)) + octave_translate;

			if(current_octave != octave) {
				current_octave=octave;
				ret+=octave;
			}

			ret+=note;
		} else if(l.length()==4) {
			String note;
			int octave;
			note=l.substring(0, 1);
			octave=Integer.parseInt(l.substring(3)) + octave_translate;

			if(current_octave!=octave) {
				current_octave=octave;
				ret+=octave;
			}

			ret+=note + "#";
		}

		length=(String)lengths.get(r);

		if(!current_length.equals(length)) {
			current_length=length;
			ret+=length;
		}

		ret+=" ";

		return(ret);
	}

	public String toString() {
		return(mot_data);
	}
	
	public static void main(String args[]) throws Exception {
		String name=args[0];
		String fromfile=SpyUtil.getFileData(args[1]);
		SiemensToMot stm=new SiemensToMot(fromfile);

		System.out.println("[!A0][D32801]!*"
			+ name + "," + stm + "*!");
	}
}
