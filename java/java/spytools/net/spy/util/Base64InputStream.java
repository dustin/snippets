// Copyright (c) 2001  Dustin Sallings <dustin@spy.net>
//
// $Id: Base64InputStream.java,v 1.2 2001/03/31 10:37:08 dustin Exp $

package net.spy.util;

import java.io.*;

/**
 * A filter stream for decoding Base64 data on an InputStream.
 */
public class Base64InputStream extends FilterInputStream {

	private Base64 base64=null;
	// Properly initialized, this will be zero
	private int currentOut=9;
	private byte outbuffer[]=null;

	/**
	 * Get a new Base64InputStream decoding the given InputStream.
	 */
	public Base64InputStream(InputStream is) {
		super(is);
		base64=new Base64();
	}

	/**
	 * Get the next decoded byte in this stream.
	 */
	public int read() throws IOException {
		int rv=-1;

		if(outbuffer==null || currentOut>=outbuffer.length) {
			decodeMore();
		}

		if(outbuffer.length>0) {
			rv=outbuffer[currentOut++];
		}

		return(rv);
	}

	private void decodeMore() throws IOException {
		byte tmp[]=new byte[4];
		boolean more=true;

		int bytesread=0;
		for(bytesread=0; bytesread<4 && more; bytesread++) {
			int input=in.read();
			if(input<0) {
				more=false;
			} else {
				if(base64.isValidBase64Char( (char)input) ) {
					tmp[bytesread]=(byte)input;
				} else {
					// Skip this byte
					bytesread--;
				} // Deal with the read character
			} // Got input
		} // Getting input

		String todecode=null;

		if(bytesread==0) {
			outbuffer=new byte[0];
		} else {
			if(bytesread<4) {
				byte tmptmp[]=new byte[bytesread];
				System.arraycopy(tmp, 0, tmptmp, 0, bytesread);
				todecode=new String(tmptmp);
			} else {
				todecode=new String(tmp);
			}
			outbuffer=base64.decode(todecode);
		}

		currentOut=0;
	}

	/**
	 * Reads up to len bytes of data from this input stream into an array
	 * of bytes.  See the FilterInputStream documentation for more details.
	 *
	 * @see FilterInputStream
	 */
	public int read(byte data[], int offset, int len) throws IOException {
		byte tmpbuf[]=new byte[len];

		int lastread=0;
		int bytesread=0;

		for(bytesread=0; bytesread<len && lastread>=0; bytesread++) {
			lastread=read();
			if(lastread>=0) {
				tmpbuf[bytesread]=(byte)lastread;
			} else {
				bytesread--;
			}
		}

		System.arraycopy(tmpbuf, 0, data, offset, bytesread);

		return(bytesread);
	}

	/**
	 * Marking and resetting are not supported in this filter.
	 */
	public boolean markSupported() {
		return(false);
	}

	/**
	 * Return the number of bytes that may be read without blocking.
	 * This is kind of a guess based on the number of bytes available.  It
	 * probably works.
	 */
	public int available() throws IOException {
		int rv=in.available();
		rv=(rv*3)/4;
		return(rv);
	}

	public static void main(String args[]) throws Exception {
		FileInputStream fis=new FileInputStream(args[0]);
		Base64InputStream bis=new Base64InputStream(fis);
		FileOutputStream fos=new FileOutputStream(args[1]);

		byte buffer[]=new byte[8192];
		int bytesread=bis.read(buffer);
		while(bytesread>0) {
			System.out.println("Read " + bytesread);
			fos.write(buffer, 0, bytesread);
			bytesread=bis.read(buffer);
		}

		bis.close();
		fos.close();
	}

}
