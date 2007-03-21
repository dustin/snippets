import java.util.*;
import net.spy.*;

public class SiemensToSpeaker extends Object {

	protected String mot_data=null;
	int current_octave=0;
	int octave_translate=2;
	String current_length="";
	static Hashtable lengths=null;

	public SiemensToSpeaker(String input) throws Exception {
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
			lengths.put("(1/1)",  "L1");
			lengths.put("(1/2)",  "L2");
			lengths.put("(1/4)",  "L4");
			lengths.put("(1/8)",  "L8");
			lengths.put("(1/16)", "L16");
			lengths.put("(1/32)", "L32");
		}
	}

	protected String getThing(String s) {
		String ret="";
		String l="", r="", length="", note="";
		int paren=0;

		paren=s.indexOf("(");
		// If there's no paren, we can't use this.
		if(paren<0) {
			return("");
		}

		l=s.substring(0, paren);
		r=s.substring(paren);

		if(l.length() == 1) {
			// May be a rest.
			if(!l.equals("P")) {
				return("");
			} else {

				note="P";

				// ret+="P";
			}
		} else if(l.length()==2) {
			// It's a note.  Format:  NO  Where N is Note, and O is octave
			int octave;
			note=l.substring(0, 1);
			octave=Integer.parseInt(l.substring(1)) + octave_translate;

			if(current_octave != octave) {
				current_octave=octave;
				ret+="O" + octave;
			}
		} else if(l.length()==4) {
			int octave;
			note=l.substring(0, 1);
			octave=Integer.parseInt(l.substring(3)) + octave_translate;

			if(current_octave!=octave) {
				current_octave=octave;
				ret+=octave;
			}

			note+="#";
		}

		length=(String)lengths.get(r);

		if(!current_length.equals(length)) {
			current_length=length;
			ret+=length;
		}

		ret+= note;
		ret+=" ";

		return(ret);
	}

	public String toString() {
		return(mot_data);
	}
	
	public static void main(String args[]) throws Exception {
		String fromfile=SpyUtil.getFileData(args[0]);
		SiemensToSpeaker sts=new SiemensToSpeaker(fromfile);

		if(args.length>1) {
			System.out.println(args[1]);
		}

		System.out.println(sts);
	}
}
