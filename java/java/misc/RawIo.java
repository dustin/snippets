// Raw io

import java.io.*;

public class RawIo {

	public static void main(String args[]) throws Exception {
		BufferedInputStream bis=new BufferedInputStream(System.in, 1);
		System.out.print("Press a key:  ");
		int r=bis.read();
		System.out.println("You pressed " + r);
	}

}
