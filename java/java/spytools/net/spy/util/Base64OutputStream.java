// Copyright (c) 2001  Dustin Sallings <dustin@spy.net>
//
// $Id: Base64OutputStream.java,v 1.1 2001/03/31 10:01:40 dustin Exp $

package net.spy.util;

import java.io.*;

public class Base64OutputStream extends FilterOutputStream {

	Base64 base64=null;
	int currentByte=0;
	int currentOutput=0;
	byte buffer[]=null;
	byte crlf[]=null;

	public Base64OutputStream(OutputStream os) {
		super(os);
		base64=new Base64();
		buffer=new byte[3];
		crlf=new byte[2];
		crlf[0]='\r';
		crlf[1]='\n';
	}

	public void write(byte b) throws IOException {
		byte ba[]=new byte[1];
		write(ba, 0, 1);
	}

	public void write(byte data[], int offset, int length) throws IOException {
		for(int i=offset; i<offset+length; i++) {
			writeByte(data[i]);
		}
	}

	public void close() throws IOException {
		if(currentByte>0) {
			byte tmp[]=new byte[currentByte];
			System.arraycopy(buffer, 0, tmp, 0, currentByte);
			out.write(base64.encode(buffer).getBytes());
			out.write(crlf);
		} else {
			// Unless this is a new line, add a newline.
			if(currentOutput!=0) {
				out.write(crlf);
			}
		}
		super.close();
	}

	private void writeByte(byte datum) throws IOException {
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
