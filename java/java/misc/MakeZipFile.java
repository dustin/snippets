// Copyright (c) 2003  Dustin Sallings <dustin@spy.net>
//
// $Id: MakeZipFile.java,v 1.2 2003/06/11 08:06:23 dustin Exp $

import java.io.*;
import java.util.Random;
import java.util.zip.ZipEntry;
import java.util.zip.ZipOutputStream;

import net.spy.util.ProgressStats;

/**
 * Make a big zipfile.
 */
public class MakeZipFile extends Object {

	private static final int HOWMANY=5000000;
	private Random r=null;

	/**
	 * Get an instance of MakeZipFile.
	 */
	public MakeZipFile() {
		super();
		r=new Random();
	}

	private void populateOne(ZipOutputStream zos) throws Exception {
		// Get a random twelve digit number
		long i=Math.abs(r.nextLong());

		ZipEntry ze=new ZipEntry("s/" + i);
		ByteArrayOutputStream bos=new ByteArrayOutputStream(512);
		ObjectOutputStream os= new ObjectOutputStream(bos);
		os.writeObject(new RandomObject(String.valueOf(i)));
		os.close();
		bos.close();
		byte b[]=bos.toByteArray();
		ze.setSize(b.length);
		zos.putNextEntry(ze);
		zos.write(b, 0, b.length);
	}

	private void populate(ZipOutputStream zos) throws Exception {
		ProgressStats ps=new ProgressStats(HOWMANY);
		for(int i=0; i<HOWMANY; i++) {
			ps.start();
			if(i%100 == 0) {
				System.out.println(ps.getStats());
			}
			populateOne(zos);
			ps.stop();
		}
	}

	public void go(File f) throws Exception {
		FileOutputStream fos=new FileOutputStream(f);
		ZipOutputStream zos=new ZipOutputStream(fos);
		zos.setLevel(9);

		populate(zos);

		zos.finish();
		zos.close();
		fos.close();
	}

	public static void main(String args[]) throws Exception {
		MakeZipFile mzf=new MakeZipFile();
		mzf.go(new File(args[0]));
	}

}

class RandomObject extends Object implements Serializable {
	private int id=0;
	private String name=null;
	private byte authData[]=null;
	private java.util.Date modDate=null;
	private String model=null;
	private String pca=null;

	public RandomObject(String name) {
		this.name=name;
		Random r=new Random();
		this.id=r.nextInt(10000000);
		authData=new byte[16];
		r.nextBytes(authData);
		modDate=new java.util.Date();
		model="HomePortal/" + (r.nextInt(10) * 100);
		pca = (r.nextInt(9999) + 10000) + "-" + (r.nextInt(999) + 1000)
			+ "-" + (r.nextInt(99) + 100);
	}
}
