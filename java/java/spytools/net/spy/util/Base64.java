// Copyright (c) 2001  Dustin Sallings <dustin@spy.net>
//
// $Id: Base64.java,v 1.6 2001/03/31 10:01:38 dustin Exp $

package net.spy.util;

import java.io.*;

public class Base64 extends Object {

	private static final char charmap[]={
		'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L', 'M',
		'N', 'O', 'P', 'Q', 'R', 'S', 'T', 'U', 'V', 'W', 'X', 'Y', 'Z',
		'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm',
		'n', 'o', 'p', 'q', 'r', 's', 't', 'u', 'v', 'w', 'x', 'y', 'z',
		'0', '1', '2', '3', '4', '5', '6', '7', '8', '9', '+', '/'
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
			int a, b, c, tmpa, tmpb;

			a=((int)data[i] & 0xff);
			sb.append(charmap[(a>>2)]);
			tmpa=((a&0x03)<<4);

			// If there's another byte, grab it and process it
			if(data.length > i+1) {
				b=((int)data[i+1] & 0xff);
				tmpb=(b>>4);
				sb.append(charmap[(tmpa|tmpb)]);
				tmpa=((b&0x0f)<<2);
				if(data.length>i+2) {
					c=((int)data[i+2] & 0xff);
					tmpb=((c&0xc0)>>6);
					sb.append(charmap[(tmpa|tmpb)]);
					sb.append(charmap[(c&0x3f)]);
				} else {
					sb.append(charmap[tmpa]);
					sb.append('=');
				}
			} else {
				// Only one byte in this block.
				sb.append(charmap[tmpa]);
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
		int invalid=0;

		for(int i=0; i<insize; i++) {
			int x=mapIndex(input.charAt(i));

			// Skip over invalid characters.
			if(x<0) {
				invalid++;
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
			rv[pos++]=(byte)(( (packer << 12) >> 16)&0xFF);
		} else if(count==3) {
			rv[pos++]=(byte)(( (packer << 6) >> 16)&0xFF);
			rv[pos++]=(byte)(( (packer << 6) >> 8) & 0xFF);
		}

		// If there were any invalid characters, our size was wrong.
		if(invalid>0) {
			byte tmp[]=new byte[pos];
			System.arraycopy(rv, 0, tmp, 0, pos);
			rv=tmp;
		}

		return(rv);
	}

	/**
	 * Is this character a valid Base64 character?
	 *
	 * @return true if this character is in our Base64 character map.
	 */
	public boolean isValidBase64Char(char c) {
		return(mapIndex(c)>=0);
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

	public static void main(String args[]) throws Exception {
		File f=new File(args[0]);
		System.out.println("Working on " + f + " (" + f.length() + " bytes).");
		FileInputStream fis=new FileInputStream(f);
		byte data[]=new byte[(int)f.length()];
		int size=fis.read(data);
		fis.close();
		if(size!=f.length()) {
			throw new Exception("Didn't read all the data.");
		}

		Base64 b=new Base64();
		String tmp=b.encode(data);

		System.out.println(tmp);

		FileOutputStream fos=new FileOutputStream(args[1]);
		fos.write(b.decode(tmp));
		fos.close();
	}

}
