// Copyright (c) 2000  Dustin Sallings <dustin@spy.net>
// $Id: DEM.java,v 1.2 2001/02/03 22:41:23 dustin Exp $

import java.io.*;
import java.util.*;
import java.util.zip.*;

public class DEM extends Object implements Serializable {

	DEM_A a_block=null;
	DEM_B b_blocks=null;
	InputStream in=null;

	public DEM(InputStream in) throws Exception {
		super();

		this.in=in;
		grokDatFile();
	}

	public String toString() {
		return(a_block.toString());
	}

	public DEM_A getABlock() {
		return(a_block);
	}

	public DEM_B getBBlocks() {
		if(b_blocks==null) {
			try {
				b_blocks=new DEM_B(a_block, in);
			} catch(Exception e) {
				System.err.println("Ahh!  Problem getting B blocks: " + e);
				e.printStackTrace();
			}
		}
		return(b_blocks);
	}

	protected void grokDatFile() throws Exception {
		byte data[]=new byte[1024];
		in.read(data);
		String type_a=new String(data);

		a_block=new DEM_A(type_a);
	}

	public static void main(String args[]) throws Exception {
		FileInputStream f=new FileInputStream(args[0]);
		// GZIPInputStream gz=new GZIPInputStream(f);
		DEM d=new DEM(f);
		f.close();
		System.out.println("" + d);

		if(args.length>1) {
			d.getBBlocks();
			FileOutputStream ostream = new FileOutputStream(args[1]);
			ObjectOutputStream p = new ObjectOutputStream(ostream);
			p.writeObject(d);
			p.flush();
			ostream.close();
		}
	}
}
