import java.io.*;

public class JpegSize {

	public static void dumpShit(byte[] shit, int start, int lines) {
		int i=0, j=0;

		for(i=0; i<lines; i++) {
			System.out.print(Integer.toHexString(i*16) + "\t");
			for(j=0; j<16; j++) {
				int ivalue=(int)(shit[(i*16)+j+start]&0xff);

				if(ivalue<0x10) {
					System.out.print("0");
				}

				System.out.print(Integer.toHexString(ivalue) + " ");
			}

			// Now let's do the character display.
			System.out.print("    ");
			for(j=0; j<16; j++) {
				char c=(char)(shit[(i*16)+j+start]&0xff);
				if(Character.isLetterOrDigit(c)) {
					System.out.print(c);
				} else {
					System.out.print(".");
				}
			}

			System.out.println("");
		}
	}

	private void parseExif(byte data[]) {
		System.out.println("Parsing Exif of " + data.length + " bytes");
	}

	public void doit(String filename) throws Exception {
		File f=new File(filename);
		long size=f.length();
		FileInputStream is=new FileInputStream(f);
		byte data[]=new byte[(int)size];
		int read=is.read(data);
		System.out.println("Read " + read + " bytes.");
		int ch=-1, i=0;
		boolean done=false;
		int width=0, height=0;

		// Make sure it looks kinda like a jpeg.
		if( ( (int)(data[0]&0xff) != 0xff)
			|| ( (int)(data[1]&0xff) != 0xD8 ) ) {
			throw(new Exception("That's not a JPEG!"));
		}

		i+=2;

		// Look around.
		while(!done && ch != 0xDA && i<data.length) {
			// Look for the next marker.
			while(ch!=0xFF) { ch=(int)(data[i++]&0xff); }
			// Look for the end of this marker.
			while(ch==0xFF) { ch=(int)(data[i++]&0xff); }

			System.out.println("Working on tag " + Integer.toHexString(ch) );
			dumpShit(data, i, 8);

			// OK, What's the marker?
			if(ch>=0xC0 && ch<= 0xC3) {
				i+=3;
				width=((data[i+2]&0xff)<<8) | ((data[i+3]&0xff));
				height=((data[i+0]&0xff)<<8) | (data[i+1]&0xff);
				System.out.println("Found size!  " + width + "x" + height);
				// done=true;
			} else {
				int length=((data[i]&0xff)<<8)|(data[i+1]&0xff);
				if(ch==0xe1) {
					System.out.println("That's an Exif tag of "
						+ length + " bytes.");
					byte tmp[]=new byte[length];
					System.arraycopy(data, i, tmp, 0, length);
					parseExif(tmp);
				} else {
					System.out.println(" (that was " + length + " bytes)");
				}
				i+=length;
			}
		}
	}

	public static void main(String args[]) throws Exception {
		JpegSize jps=new JpegSize();
		jps.doit(args[0]);
	}
}
