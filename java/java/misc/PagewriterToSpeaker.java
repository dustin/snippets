// Copyright (c) 2001  Dustin Sallings <dustin@spy.net>
//
// $Id: PagewriterToSpeaker.java,v 1.1 2001/02/03 22:41:22 dustin Exp $

import java.util.*;
import net.spy.*;

public class PagewriterToSpeaker extends Object {
	private char input[]=null;
	private String input_s=null;
	private String out=null;

	public PagewriterToSpeaker(String input) throws Exception {
		super();
		extract(input);
		parse();
	}

	private boolean isLength(char c) {
		boolean rv=false;

		switch(c) {
			case 'w':
			case 'h':
			case 'q':
			case 'e':
			case 's':
			case 't':
				rv=true;
		}

		return(rv);
	}

	private String getLength(char c) {
		String rv=null;

		switch(c) {
			case 'w':
				rv="1";
				break;
			case 'h':
				rv="2";
				break;
			case 'q':
				rv="4";
				break;
			case 'e':
				rv="8";
				break;
			case 's':
				rv="16";
				break;
			case 't':
				rv="32";
				break;
		}

		return(rv);
	}

	private void parse() throws Exception {
		int current=0;
		out="";
		Hashtable special=new Hashtable();

		while(current<input.length) {
			switch(input[current]) {
				case 'T':
					int end=input_s.indexOf("%", current);
					String tmp=input_s.substring(current+1, end);
					System.err.println("Tempo:  " + tmp);
					current=end;
					out+="T" + tmp;
					break;
				case 'R':
					if(getLength(input[current+1])!=null) {
						out+="L" + getLength(input[current+1]);
						current++;
					}
					out+="P";
					break;
				case 'A':
				case 'B':
				case 'C':
				case 'D':
				case 'E':
				case 'F':
				case 'G':
					String note="" + input[current];
					String sharpflat=(String)special.get(note);
					if(input[current+1]=='#' || input[current+1]=='S') {
						sharpflat="+";
						current++;
					}
					if(input[current+1]=='#') {
						sharpflat="-";
						current++;
					}
					if(input[current+1]=='n') {
						sharpflat="";
						current++;
					}
					if(getLength(input[current+1])!=null) {
						out+="L" + getLength(input[current+1]);
						current++;
					}

					out+=note;
					if(sharpflat!=null) {
						out+=sharpflat;
					}

					break;
				case 'S':
					note="" + input[current+1];
					special.put(note, "+");
					current++;
					break;
				case 'f':
					note="" + input[current+1];
					special.put(note, "-");
					current++;
					break;
				case '0':
				case '1':
				case '2':
				case '3':
				case '4':
				case '5':
				case '6':
				case '7':
				case '8':
					out+="O" + input[current];
					break;
				case 'L':
					current++;
					break;
				case 'l':
					current++;
					break;
				case 'V':
				case 'v':
					break;
				// Ignore whitespace
				case ' ':
					break;
				default:
					System.err.println("Unknown character:  " + input[current]);
			}
			current++;
			out+=" ";
		}
	}

	public String toString() {
		return(out);
	}

	private void extract(String input) throws Exception {
		int start=input.indexOf("!*");
		if(start<0) {
			throw new Exception("Invalid Pagewriter alert (1).");
		}
		start=input.indexOf(";2,", start+2);
		if(start<0) {
			throw new Exception("Invalid Pagewriter alert (2).");
		}
		int end=input.indexOf("*!", start);
		if(end<0) {
			throw new Exception("Invalid Pagewriter alert (3).");
		}

		String tmp=input.substring(start+3, end);

		StringTokenizer st=new StringTokenizer(tmp);
		StringBuffer sb=new StringBuffer();
		while(st.hasMoreTokens()) {
			sb.append(st.nextToken());
		}

		// Leave some space on the end to make parsing eaiser.
		input_s=sb.toString().trim() + "    ";
		this.input=input_s.toCharArray();
	}

	public static void main(String args[]) throws Exception {
		String fromfile=SpyUtil.getFileData(args[0]);

		PagewriterToSpeaker pts=new PagewriterToSpeaker(fromfile);

		System.out.println(pts);
	}

}
