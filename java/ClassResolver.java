// Copyright (c) 2004  Dustin Sallings <dustin@spy.net>

/*
package net.spy.util;
*/

import java.io.InputStream;
import java.io.IOException;

import java.util.Collection;
import java.util.Iterator;
import java.util.ArrayList;

/**
 * ClassLoader implementation that keeps a list of all loaded classes.  This is
 * useful, for example, to see what classes a particular class requires.
 */
public class ClassResolver extends ClassLoader {

	private static final int START_SIZE=65536;
	private static final int INCR_SIZE=16384;

	private Collection loaded=null;

	/**
	 * Get an instance of ClassResolver with the system classloader as a
	 * parent.
	 */
	public ClassResolver() {
		this(getSystemClassLoader());
	}

	/**
	 * Get an instance of ClassResolver.
	 */
	public ClassResolver(ClassLoader parent) {
		super(parent);
		loaded=new ArrayList();
	}

	/**
	 * Get the collection of loaded class names.
	 */
	public Collection getLoaded() {
		return(loaded);
	}

	/**
	 * Find the named class.
	 */
	public Class findClass(String name) {
		String nm=name.replace('.', '/') + ".class";
		Class rv=null;
		try {
			InputStream is=getResourceAsStream(nm);
			if(is == null) {
				is=getSystemResourceAsStream(nm);
				if(is == null) {
					throw new NoClassDefFoundError(name);
				}
			}
			rv=loadFromStream(name, is);
			is.close();
		} catch(IOException e) {
			NoClassDefFoundError ne=new NoClassDefFoundError(name);
			ne.initCause(e);
			throw ne;
		}
		loaded.add(rv);
		return(rv);
	}

	private Class loadFromStream(String name, InputStream is)
		throws IOException {

		byte b[]=new byte[START_SIZE];
		int readsize=0;
		int size=1;
		while(size > 0) {
			size=is.read(b, readsize, b.length - readsize);
			if(size >= 0) {
				readsize += size;
				if(readsize == b.length) {
					byte tmpbuf[]=new byte[b.length + INCR_SIZE];
					System.arraycopy(b, 0, tmpbuf, 0, b.length);
					b=tmpbuf;
				} // adjusted buffer size
			} // read something
		} // reading
		Class rv=defineClass(name, b, 0, readsize, null);
		return(rv);
	} // loadFromStream

	/**
	 * Load all of the classes given as arguments and print what classes got
	 * loaded after all of them.  This is a pretty much minimal demonstration.
	 * To be more useful, you'd want to *do* something with these classes to
	 * get them going as they would in the real world.
	 */
	public static void main(String args[]) throws Exception {
		ClassResolver cl=new ClassResolver(null);
		try {
			for(int i=0; i<args.length; i++) {
				System.out.println("Loading " + args[i]);
				Class c=cl.loadClass(args[i]);
				System.out.println("Got " + c + " from " + c.getClassLoader());
				c.newInstance();
			}
		} catch(Throwable t) {
			t.printStackTrace();
		}
		System.out.println("Loaded classes:");
		for(Iterator i=cl.getLoaded().iterator(); i.hasNext();) {
			Class c=(Class)i.next();
			System.out.println(" " + c.getName() + " " + c.getClassLoader());
		}
	}

}
