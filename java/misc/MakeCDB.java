// Copyright (c) 2003  Dustin Sallings <dustin@spy.net>
//
// $Id: MakeCDB.java,v 1.1 2003/06/11 08:06:23 dustin Exp $

import java.io.*;
import java.util.Random;

import net.spy.util.ProgressStats;

import com.strangegizmo.cdb.CdbMake;

/**
 * Make a big cdb.
 *
 * This uses the cdb library found in the following location:
 * http://www.strangegizmo.com/software/sg-cdb/
 */
public class MakeCDB extends Object {

	private static final int HOWMANY=5000000;
	private Random r=null;

	private static final String FORMAT=
		"Did {3} of {4}, {5} todo.  Avg={0,number,#.######}s, "
			+ "Est={1,number,#,###.##}s ({2,date,EEE MMMdd HH:mm})";

	/**
	 * Get an instance of MakeCDB.
	 */
	public MakeCDB() {
		super();
		r=new Random();
	}

	private void populateOne(CdbMake cdb) throws Exception {
		// Get a random twelve digit number
		long i=Math.abs(r.nextLong());

		String key=String.valueOf(i);

		cdb.add(key.getBytes(), getRandomObject(String.valueOf(i)));
	}
	
	private byte[] getRandomObject(String name) throws Exception {
		ByteArrayOutputStream bos=new ByteArrayOutputStream(512);
		ObjectOutputStream os= new ObjectOutputStream(bos);
		os.writeObject(new RandomObject(name));
		os.close();
		bos.close();
		byte b[]=bos.toByteArray();
		return(b);
	}

	private void populate(CdbMake cdb) throws Exception {
		ProgressStats ps=new ProgressStats(HOWMANY);
		for(int i=0; i<HOWMANY; i++) {
			ps.start();
			if(i%10000 == 0) {
				System.out.println(ps.getStats(FORMAT));
			}
			populateOne(cdb);
			ps.stop();
		}
	}

	public void go(File f) throws Exception {
		CdbMake cdb=new CdbMake();
		cdb.start(f.toString());

		populate(cdb);
	}

	public static void main(String args[]) throws Exception {
		MakeCDB mzf=new MakeCDB();
		// Test the creation of the stuff.
		byte b[]=mzf.getRandomObject("000000000000");
		mzf.go(new File(args[0]));
	}

}

class RandomObject extends Object implements Serializable {

	private static Random r=new Random();

	private int id=0;
	private int someOtherId=0;
	private String name=null;
	private byte authData[]=null;
	private java.util.Date modDate=null;
	private String model=null;
	private String pca=null;
	private String otherThing=null;
	private String yetAnotherThing=null;
	private boolean b=false;

	public RandomObject(String name) {
		this.name=name;

		this.id=r.nextInt(10000000);
		this.someOtherId=r.nextInt(10000000);
		authData=new byte[16];
		r.nextBytes(authData);
		modDate=new java.util.Date();

		StringBuffer sb=new StringBuffer(16);
		sb.append("HomePortal/");
		sb.append(r.nextInt(10) * 100);
		model=sb.toString();
		assert(model.length() <= 16);

		sb=new StringBuffer(16);
		sb.append(r.nextInt(9999) + 10000);
		sb.append("-");
		sb.append(r.nextInt(999) + 1000);
		sb.append("-");
		sb.append(r.nextInt(99) + 100);
		pca=sb.toString();
		assert(pca.length() <= 16);

		sb=new StringBuffer(16);
		sb.append("Some");
		sb.append(r.nextInt(9999) + 10000);
		sb.append("-XxxxXX");
		otherThing=sb.toString();
		assert(otherThing.length() <= 16);

		sb=new StringBuffer(64);
		sb.append("Here is some more text for this thing");
		sb.append(r.nextInt(9999999) + 10000000);
		sb.append("-XxxxXXxX-");
		sb.append(r.nextInt(9999999) + 10000000);
		yetAnotherThing=sb.toString();
		assert(yetAnotherThing.length() <= 64
			&& yetAnotherThing.length() > 60);
	}
}
