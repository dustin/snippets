// Copyright (c) 2002  Dustin Sallings <dustin@spy.net>
// $Id: MTTaskFactory.java,v 1.3 2002/07/12 04:43:03 dustin Exp $

package net.spy.test;

import java.lang.reflect.Constructor;
import java.lang.reflect.InvocationTargetException;

/**
 * Factory for creating MTTask instances.
 *
 * Due to the way inner classes are constructed, the MTTaskFactory may not
 * be used to construct inner classes at this time.
 */
public class MTTaskFactory extends Object {

	private Constructor cons=null;
	private Object consArgs[]=null;

	/**
	 * Get an instance of MTTaskFactory.
	 */
	protected MTTaskFactory() {
		super();
	}

	/**
	 * Get an instance of MTTaskFactory that will create instances of the
	 * given class that will each perform the given number of iterations of
	 * its test and have the given stopOnFailure setting.
	 *
	 * @param mtTaskClass a Class instance that represents a subclass of
	 * 	MTTask
	 * @param iterations the number of iterations the instance will loop
	 * @param stopOnFailure if true, failures will cause the loop to stop
	 */
	public MTTaskFactory(Class mtTaskClass, int iterations,
		boolean stopOnFailure) {

		super();

		if(mtTaskClass == null) {
			throw new NullPointerException("Provided class is null.");
		}

		if(!MTTask.class.isAssignableFrom(mtTaskClass)) {
			throw new IllegalArgumentException(
				"mtTaskClass must be a subclass of MTTask "
                + mtTaskClass.getName() + " is not");
		}

		if(MTTask.class.equals(mtTaskClass)) {
			throw new IllegalArgumentException(
				"mtTaskClass must be a subclass of MTTask");
		}

		// OK, go find the constructor

		Class argTypes[]=new Class[2];
		argTypes[0]=Integer.TYPE;
		argTypes[1]=Boolean.TYPE;

		try {
			cons=mtTaskClass.getConstructor(argTypes);
		} catch(NoSuchMethodException nsme) {
			nsme.printStackTrace();
            // Find constructors:
            Constructor list[]=mtTaskClass.getConstructors();
            for(int i=0; i<list.length; i++) {
                System.err.println("Constructor:  " + list[i]);
            }
			throw new Error(mtTaskClass.getName()
				+ " appears to be a subclass of " + MTTask.class.getName()
				+ " but couldn't find a valid constructor.");
		}

		consArgs=new Object[2];
		consArgs[0]=new Integer(iterations);
		consArgs[1]=new Boolean(stopOnFailure);
	}

	/**
	 * Get a new instance of an MTTask.
	 *
	 * @return a freshly baked subclass of MTTask, as requested
	 */
	public MTTask newInstance()
		throws InstantiationException, IllegalAccessException,
				InvocationTargetException {
		return(MTTask)cons.newInstance(consArgs);
	}

}
