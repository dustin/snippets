// Copyright (c) 2001  Dustin Sallings <dustin@spy.net>
//
// $Id: Base64.java,v 1.1 2001/03/30 09:20:56 dustin Exp $

package net.spy.util;

public class Base64 extends Object {

	private static final char charmap[]={
		'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L', 'M',
		'N', 'O', 'P', 'Q', 'R', 'S', 'T', 'U', 'V', 'W', 'X', 'Y', 'Z',
		'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm',
		'n', 'o', 'p', 'q', 'r', 's', 't', 'u', 'v', 'w', 'x', 'y', 'z',
		'0', '1', '2', '3', '4', '5', '6', '7', '8', '9', '+', '/', '=',
		'"'
	};

	/**
	 * Get a base64 encode/decoder.
	 */
	public Base64() {
		super();
	}

	/**
	 * Encode some bytes to a base64 string.
	 */
	public String encode(byte data[]) {
		// Go ahead and guess how big it needs to be.
		StringBuffer sb=new StringBuffer((data.length*4/3));

		int o=0;
		// Flip through the input and encode the shite.
		for(int i=0; i<data.length; i+=3) {
			byte a, b, c, tmpa, tmpb;

			a=data[i];
			sb.append(charmap[(int)(a>>2)]);
			tmpa=(byte)((a&0x03)<<4);

			// If there's another byte, grab it and process it
			if(data.length > i+1) {
				b=data[i+1];
				tmpb=(byte)(b>>4);
				sb.append(charmap[(int)(tmpa|tmpb)]);
				tmpa=(byte)((b&0x0f)<<2);
				if(data.length>i+2) {
					c=data[i+2];
					tmpb=(byte)((c&0xc0)>>6);
					sb.append(charmap[(int)(tmpa|tmpb)]);
					sb.append(charmap[(int)(c&0x3f)]);
				} else {
					sb.append(charmap[(int)tmpa]);
					sb.append('=');
				}
			} else {
				// Only one byte in this block.
				sb.append(charmap[(int)tmpa]);
				sb.append('=');
				sb.append('=');
			}

			o+=4;
			if( (o%76)==0) {
				sb.append("\r\n");
			}
		}

		return(sb.toString());
	}

	/**
	 * Decode a string back into the bytes.
	 */
	public byte[] decode(String input) {
		// Get enough room to store the output.
		int size=(input.length()*3/4);
		int insize=input.length();
		if(input.endsWith("=")) {
			size--;
			insize--;
			if(input.endsWith("==")) {
				size--;
				insize--;
			}
		}
		byte rv[]=new byte[size];
		int pos=0;
		int count=0;
		int packer=0;

		for(int i=0; i<insize; i++) {
			int x=mapIndex(input.charAt(i));

			if(x<0) {
				continue;
			}

			// Count valid chars.
			count++;

			// Pack them.
			packer = packer << 6 | x;

			// Every four bytes, we've got three valid output bytes.
			if(count==4) {
				rv[pos++]=(byte)((packer >> 16)&0xFF);
				rv[pos++]=(byte)((packer >>  8)&0xFF);
				rv[pos++]=(byte)(packer&0xFF);
				count=0; packer=0;
			}
		}

		// Any remainders?
		if(count==2) {
			System.err.println("Remainder 2");
			rv[pos++]=(byte)(( (packer << 12) >> 16)&0xFF);
		} else if(count==3) {
			System.err.println("Remainder 3");
			rv[pos++]=(byte)(( (packer << 6) >> 16)&0xFF);
			rv[pos++]=(byte)(( (packer << 6) >> 8) & 0xFF);
		}

		return(rv);
	}

	private int mapIndex(char c) {
		int rv=-1;

		for(int i=0; i<charmap.length && rv==-1; i++) {
			if(charmap[i]==c) {
				rv=i;
			}
		}

		return(rv);
	}

	private static void test(String input) throws Exception {
		Base64 b=new Base64();
		System.out.println("Input:    " + input);
		String encoded=b.encode(input.getBytes());
		System.out.println("Encoded:  " + encoded);
		String decoded=new String(b.decode(encoded));
		System.out.println("Decoded:  " + decoded);
		if(decoded.equals(input)) {
			System.err.println("Success");
		} else {
			System.err.println("The input and the decoded are different.");
			System.err.println("Input:    " + input.length() + " bytes");
			System.err.println("Decoded:  " + decoded.length() + " bytes");
			throw new Exception("Damnit.");
		}
	}

	public static void main(String args[]) throws Exception {
		String testStr="";
		for(int i=0; i<100; i++) {
			test(testStr);
			testStr+=(i%10);
		}
	}

}
