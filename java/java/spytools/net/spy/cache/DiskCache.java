// Copyright (c) 1999 Dustin Sallings <dustin@spy.net>
// $Id: DiskCache.java,v 1.3 2002/07/10 04:25:01 dustin Exp $

package net.spy.cache;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.InputStream;
import java.io.IOException;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.io.OutputStream;

import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;

/**
 * Simple local disk caching.
 *
 * This is used for terribly simple caches with no expiration dates on
 * objects.  Things go in and they stay in.
 */
public class DiskCache extends Object {

	private static final String DEFAULT_DIR="/tmp/diskcache";

	// Base directory for hashing
	private String basedir = null;

	/**
	 * Get a DiskCache object using the default directory.
	 */
	public DiskCache() {
		this(DEFAULT_DIR);
	}

	/**
	 * Get an DiskObject using the given directory.
	 */
	public DiskCache(String basedir) {
		super();
		this.basedir=basedir;
	}

	// Get the path of an object for the given key
	private String getPath(String key) {
		MessageDigest md=null;
		try {
			md=MessageDigest.getInstance("SHA");
		} catch(NoSuchAlgorithmException e) {
			throw new Error("There's no SHA?");
		}
		md.update(key.getBytes());

		String hashed=net.spy.SpyUtil.byteAToHexString(md.digest());

		String base=basedir+"/"+hashed.substring(0,2);
		String path=basedir+"/"+hashed.substring(0,2)+"/"+hashed;

		File f=new File(base);
		if(!f.isDirectory()) {
			f.mkdirs();
		}

		return(path);
	}

	/**
	 * Store an object in the cache.
	 */
    public void storeObject(String name, Object o) throws IOException {
		String pathto=getPath(name);

		FileOutputStream ostream = new FileOutputStream(pathto);
		ObjectOutputStream p = new ObjectOutputStream(ostream);
		p.writeObject(o);
		p.flush();
		ostream.close();
	}

	/**
	 * Get an object from the cache.
	 *
	 * @return the object, or null if there's no such object
	 */
    public Object getObject(String name) {
		Object rv=null;
		String pathto=getPath(name);

		try {
			Object o;
			FileInputStream istream = new FileInputStream(pathto);
			ObjectInputStream p = new ObjectInputStream(istream);
			o = p.readObject();
			rv=o;
		} catch(FileNotFoundException e) {
            // If it's file not found, just print the path, no need for a
            // full stack.
            System.err.println(e.toString());
		} catch(Exception e) {
			e.printStackTrace();
		}
		return(rv);
	}
}
