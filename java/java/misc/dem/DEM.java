// Copyright (c) 2000  Dustin Sallings <dustin@spy.net>
// $Id: DEM.java,v 1.1 2000/07/30 10:42:22 dustin Exp $

import java.io.*;
import java.util.*;
import java.util.zip.*;

public class DEM extends Object {

	DEM_A a_block=null;

	public DEM(InputStream in) throws Exception {
		super();

		grokDatFile(in);
	}

	public String toString() {
		return(a_block.toString());
	}

	protected void grokDatFile(InputStream in) throws Exception {
		byte data[]=new byte[1024];
		in.read(data);
		String type_a=new String(data);

		a_block=new DEM_A(type_a);
	}

	public static void main(String args[]) throws Exception {
		FileInputStream f=new FileInputStream(args[0]);
		GZIPInputStream gz=new GZIPInputStream(f);
		DEM d=new DEM(gz);

		System.out.println("" + d);
	}
}
