// Copyright (c) 2001  Dustin Sallings <dustin@spy.net>
//
// $Id: Base64OutputStream.java,v 1.4 2001/04/26 07:21:58 dustin Exp $

package net.spy.util;

import java.io.*;

/**
 * A FilterOutputStream that encodes data into Base64.
 */
public class Base64OutputStream extends FilterOutputStream {

	private Base64 base64=null;
	private int currentByte=0;
	private int currentOutput=0;
	private byte buffer[]=null;
	private byte crlf[]=null;

	/**
	 * Get a new Base64OutputStream encoding the given OutputStream.
	 */
	public Base64OutputStream(OutputStream os) {
		super(os);
		base64=new Base64();
		buffer=new byte[3];
		crlf=new byte[2];
		crlf[0]=(byte)'\r';
		crlf[1]=(byte)'\n';
	}

	/**
	 * Writes len bytes from the specified byte array starting at offset off
	 * to this output stream.  See the documentation for FilterOutputStream
	 * for more details.
	 *
	 * @see FilterOutputStream
	 */
	public void write(byte data[], int offset, int length) throws IOException {
		for(int i=offset; i<offset+length; i++) {
			write(data[i]);
		}
	}

	/**
	 * Close this stream and finish up the Base64.
	 */
	public void close() throws IOException {
		if(currentByte>0) {
			byte tmp[]=new byte[currentByte];
			System.arraycopy(buffer, 0, tmp, 0, currentByte);
			out.write(base64.encode(tmp).getBytes());
			out.write(crlf);
		} else {
			// Unless this is a new line, add a newline.
			if(currentOutput!=0) {
				out.write(crlf);
			}
		}
		super.close();
	}

	/**
	 * Write the given byte to the underlying OutputStream.
	 */
	public void write(byte datum) throws IOException {
		buffer[currentByte++]=datum;
		if(currentByte==3) {
			out.write(base64.encode(buffer).getBytes());
			currentByte=0;
			currentOutput+=4;
		}
		if(currentOutput==76) {
			currentOutput=0;
			out.write(crlf);
		}
	}

	public static void main(String args[]) throws Exception {
		FileInputStream fis=new FileInputStream(args[0]);
		FileOutputStream fos=new FileOutputStream(args[1]);
		Base64OutputStream bos=new Base64OutputStream(fos);

		byte buffer[]=new byte[8192];
		int bytesread=fis.read(buffer);
		while(bytesread>0) {
			bos.write(buffer, 0, bytesread);
			bytesread=fis.read(buffer);
		}

		fis.close();
		bos.close();
	}

}
