import java.io.*;

public class JpegSize {

	public static void dumpShit(char[] shit, int start, int lines) {
		int i=0, j=0;

		for(i=0; i<lines; i++) {
			System.out.print(Integer.toHexString(i*16) + "\t");
			for(j=0; j<16; j++) {
				int ivalue=(int)shit[(i*16)+j+start];

				if(ivalue<0x10) {
					System.out.print("0");
				}

				System.out.print(Integer.toHexString(ivalue) + " ");
			}

			// Now let's do the character display.
			System.out.print("    ");
			for(j=0; j<16; j++) {
				char c=shit[(i*16)+j+start];
				if(Character.isLetterOrDigit(c)) {
					System.out.print(c);
				} else {
					System.out.print(".");
				}
			}

			System.out.println("");
		}
	}

	public void doit(String filename) throws Exception {
		long size=-1;
		File f=new File(filename);
		size=f.length();
		FileReader is=new FileReader(f);
		char data[]=new char[(int)size];
		int read=is.read(data);
		int ch=-1, i=0;
		boolean done=false;
		int width=0, height=0;

		// Make sure it looks kinda like a jpeg.
		if( ( (int)data[0] != 0xFF) || ( (int)data[1] != 0xD8 ) ) {
			throw(new Exception("That's not a JPEG!"));
		}

		i+=2;

		// Look around.
		while(!done && ch != 0xDA && i<data.length) {
			// Look for the next marker.
			while(ch!=0xFF) { ch=(int)data[i++]; }
			// Look for the end of this marker.
			while(ch==0xFF) { ch=(int)data[i++]; }

			System.out.println("Working on tag " + Integer.toHexString(ch) );
			dumpShit(data, i, 4);

			// OK, What's the marker?
			if(ch>=0xC0 && ch<= 0xC3) {
				i+=3;
				width=(data[i+2]<<8) | (data[i+3]);
				height=(data[i+0]<<8) | (data[i+1]);
				System.out.println("Found size!  " + width + "x" + height);
				done=true;
			} else {
				int length=(data[i]<<8)|data[i+1];
				i+=length;
			}
		}
	}

	public static void main(String args[]) throws Exception {
		JpegSize jps=new JpegSize();
		jps.doit(args[0]);
	}
}
