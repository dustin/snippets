// Copyright (c) 2001  Dustin Sallings <dustin@spy.net>
//
// $Id: UUInputStream.java,v 1.1 2001/08/08 09:38:36 dustin Exp $

package net.spy.util;

import java.io.*;

/**
 * This class decodes uuencoded files.
 */
public class UUInputStream extends FilterInputStream {

	private static byte[] outputBuffer = null;
	private int currentOut=0;
	private static int count = 0;

	private boolean started=false;
	private boolean finished=false;

	/**
	 * Get a UUInputStream decoding the given InputStream.
	 */
	public UUInputStream(InputStream is) {
		super(is);
	}

	/**
	 * @see FilterInputStream
	 */
	public int read() throws IOException {
		int rv=-1;

		if(outputBuffer == null || currentOut >= count) {
			if(!finished) {
				decodeMore();
			}
		}

		if(count>=currentOut) {
			rv=(int)(outputBuffer[currentOut++] & 0xffff);
		}

		return(rv);
	}

	/**
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
	 * Marking and resetting are not supported.
	 */
	public boolean markSupported() {
		return(false);
	}

	/**
	 * Guess how many bytes are available.
	 */
	public int available() throws IOException {
		int rv=count - currentOut;
		return(rv);
	}

	// get a line
	private String readLine() throws IOException {
		String rv=null;

		StringBuffer sb=new StringBuffer();
		int r=0;

		while(r!=-1 && (r=='\n' || r=='\r')) {
			r=in.read();
		}

		while(r!=-1 && r!='\n' && r!='\r') {
			sb.append((char)r);
			r=in.read();
		}

		if(sb.length()>0) {
			rv=sb.toString().trim();
		}

		return(rv);
	}

	private void decodeMore() throws IOException {

		while(!started) {
			String temp=readLine();

			if(temp.startsWith("begin")) {
				started=true;
			}
		}

		String temp = readLine();

		if (temp == null) {
			System.err.println("uudecode warning: finished due to null line");
			finished=true;
		} else if (!temp.equalsIgnoreCase("end")
			&& !temp.equals("") && !temp.equals("`")) {

			byte[] b = temp.getBytes();
			outputBuffer = new byte[100];
			count = 0;
			int length = (int) ((b[0] - ' ') & 0x3F);

			if (!Float.toString(((float) length / 3)).endsWith(".0")) {
				for (int i = 1, j = 0; j < length; j += 3, i += 4) {

					if (j + 3 > length) {
						for (int x = i; x < length; j++, x++) {
							makeSBuf(b[x]);
						}
					} else {
						makeBuf(b, i);
					}
				}
			} else {
				for (int i = 1; i < temp.length(); i +=4) {
					makeBuf(b, i);
				}
			}
			currentOut=0;
		} else {
			count--;
			finished=true;
		}
	}

	private void makeSBuf(byte b) {
		outputBuffer[count++] = (byte) ((b - ' ') & 0x3F);
	}

	private void makeBuf(byte[] b, int i) {
		outputBuffer[count++] = (byte) (((b[i] - ' ') & 0x3F) << 2 |
					((b[i + 1] - ' ') & 0x3F) >> 4);
		outputBuffer[count++] = (byte) (((b[i + 1] - ' ') & 0x3F) << 4 |
					((b[i + 2] - ' ') & 0x3F) >> 2);
		outputBuffer[count++] = (byte) (((b[i + 2] - ' ') & 0x3F) << 6 |
					((b[i + 3] - ' ') & 0x3F));
	}

	/**
	 * Test UU decode.
	 */
	public static void main(String args[]) throws Exception {
		FileInputStream fis=new FileInputStream(args[0]);
		UUInputStream uu=new UUInputStream(fis);
		FileOutputStream fos=new FileOutputStream(args[1]);

		byte buffer[]=new byte[8192];
		int bytesread=uu.read(buffer);
		while(bytesread>0) {
			System.err.println("Read " + bytesread + " bytes");
			fos.write(buffer, 0, bytesread);
			bytesread=uu.read(buffer);
		}

		uu.close();
		fos.close();
	}
}
