/*
 * Java Interface Implementor.  :)
 *
 * Copyright (c) 2000  Dustin Sallings <dustin@spy.net>
 */

package net.spy.util;

import java.lang.reflect.*;
import java.io.*;
import java.util.*;

/**
 * Think Java Interfaces are even more stupid than the lack of multiple
 * inheritence?  SO DO I!  That's why this class exists.
 * <p>
 * Using InterfaceImplementor, you can write code that's portable among
 * different API specs.  You simply implement what you need out of an
 * interface, and InterfaceImplementor writes a class that extends from the
 * class you wrote, and fills in all the blanks for you.
 * <p>
 * Here's the way you use it:
 * <p>
 * java net.spy.util.InterfaceImplementor -interface java.sql.ResultSet
 * -superclass test.TestSet -outputclass test.TestSetImpl
 */

public class InterfaceImplementor extends Object {

	// Functions that are already defined.
	private Hashtable definedFunctions=null;
	private Class interfaceClass=null;
	private Class superClass=null;
	private String outpackage=null;
	private String outclass="BLAH";

	/**
	 * Get a new InterfaceImplementor to implement the passed in interface.
	 *
	 * @param intf The interface to implement.
	 *
	 * @exception Exception if the passed in class is not an interface.
	 */
	public InterfaceImplementor(Class c) throws Exception {
		super();

		// Verify the interface isn't null
		if(c==null) {
			throw new Exception("Null interface is invalid.");
		}

		// Verify that it's an interface
		if(!c.isInterface()) {
			throw new Exception("Passed in class is not an interface.");
		}

		// Go ahead and initialize this here.  That way we don't have to
		// worry about it later.
		definedFunctions=new Hashtable();
		this.interfaceClass=c;
	}

	/**
	 * Get the name of the class we'll be generating
	 */
	public String getOutClassName() {
		return(outclass);
	}

	/**
	 * Get the name of the package containing class we'll be generating
	 */
	public String getOutPackageName() {
		return(outpackage);
	}

	/**
	 * Set the name of the output class.
	 */
	public void setOutputClass(String outclass) {
		int lastdot=outclass.lastIndexOf(".");
		if(lastdot==-1) {
			this.outclass=outclass;
		} else {
			outpackage=outclass.substring(0, lastdot);
			this.outclass=outclass.substring(lastdot+1);
		}
	}

	/**
	 * Set an optional superclass that defines some of the methods for the
	 * implementation.
	 *
	 * @param c Superclass
	 *
	 * @exception Exception if the passed in class isn't valid for this
	 * operation.
	 */
	public void setSuperClass(Class c) throws Exception {
		if(c==null) {
			throw new Exception("Null class is invalid.");
		}

		int modifiers=c.getModifiers();

		if(Modifier.isFinal(modifiers)) {
			throw new Exception("You can't extend from final classes.");
		}
		if(Modifier.isInterface(modifiers)) {
			throw new Exception("Interfaces aren't valid here.");
		}

		superClass=c;
		// Extract the methods
		getMethods(c);
	}

	// Extract the methods from the above.
	private void getMethods(Class c) throws Exception {
		Method methods[]=c.getDeclaredMethods();
		for(int i=0; i<methods.length; i++) {
			int modifiers=methods[i].getModifiers();
			// Ignore abstract methods
			if(!Modifier.isAbstract(modifiers)) {
				definedFunctions.put(getSignature(methods[i]), "defined");
			}
		}
	}

	private String decodeType(Class type) {
		String rv=null;
		if(type.isArray()) {
			rv=decodeType(type.getComponentType()) + "[]";
		} else {
			rv=type.getName();
		}
		return(rv);
	}

	// Get the method signature with exceptions
	private String getSignature(Method method) throws Exception {
		return(getSignature(method, true));
	}

	// Get the method signature
	private String getSignature(Method method, boolean needExceptions)
		throws Exception {

		String ret="";

		// Get the modifiers
		int modifiers=method.getModifiers();
		// Clear the abstract flag
		modifiers&=~(Modifier.ABSTRACT);
		// Add the modifiers to our string
		ret=Modifier.toString(modifiers);

		// Get the return type
		Class rt=method.getReturnType();
		ret+=" " + decodeType(rt) + " ";

		// Add the method name
		String name=method.getName();

		// OK, now deal with parameters
		Class types[]=method.getParameterTypes();

		ret+=name + "(";
		if(types.length > 0) {
			for(int i=0; i<types.length; i++) {
				ret+=decodeType(types[i]) + " a" + i + ", ";
			}
			// Strip off the last comma
			ret=ret.substring(0, ret.length()-2);
		}
		// Get rid of the last comma and add a paren
		ret+=") ";
		if(needExceptions) {
			ret+=getExSignature(method);
		}
		return(ret.trim());
	}

	private String getExSignature(Method method) throws Exception {
		String ret="";
		// Now flip through the exceptions
		Class e[]=method.getExceptionTypes();
		if(e.length>0) {
			ret+="\n\t\tthrows ";
			for(int i=0; i<e.length; i++) {
				ret+=e[i].getName() + ",";
			}
			// Strip off the last comma
			ret=ret.substring(0, ret.length()-1);
		}

		return(ret);
	}

	// Get the constructor signature
	private String getSignature(Constructor con) throws Exception {
		String ret=null;

		// Get the modifiers
		int modifiers=con.getModifiers();
		// Add the modifiers to our string
		ret=Modifier.toString(modifiers) + " " + outclass;

		Class types[]=con.getParameterTypes();
		ret+="(";
		if(types.length > 0) {
			for(int i=0; i<types.length; i++) {
				ret+=decodeType(types[i]) + " a" + i + ", ";
			}
			// Strip off the last ,space
			ret=ret.substring(0, ret.length()-2);
		}
		ret+=") ";

		// Now flip through the exceptions
		Class e[]=con.getExceptionTypes();
		if(e.length>0) {
			ret+="\n\t\tthrows ";
			for(int i=0; i<e.length; i++) {
				ret+=e[i].getName() + ", ";
			}
			// Strip off the last ,space
			ret=ret.substring(0, ret.length()-2);
		}

		return(ret);
	}

	// Implement a constructor that tries to error out as best as possible
	private String implement(Method method) throws Exception {
		// Start
		String ret=null;
		ret="\t// InterfaceImplementor added " + method.getName() + "\n";
		ret+="\t" + getSignature(method) + " {\n";

		Class e[]=method.getExceptionTypes();
		// If we can throw an exception, do so.
		if(e.length>0) {
			ret+="\t\tthrow new "
				+ e[0].getName() + "(\""
				+ getSignature(method, false)
				+ " not implemented yet.\");\n";
		} else {
			// OK, let's check the return value...
			Class rt=method.getReturnType();
			if(rt.isPrimitive()) {
				if(rt == Boolean.TYPE) {
					ret+="\t\treturn false;\n";
				} else if(rt == Void.TYPE) {
					// Do nothing (nothing to do!)
				} else {
					ret+="\t\treturn 0;\n";
				}
			} else {
				ret+="\t\treturn null;\n";
			}
		}

		ret+="\t}\n\n";
		return(ret);
	}

	// Implement a constructor that calls the super constructor
	private String implementConstructor(Constructor con) throws Exception {
		String ret=null;
		ret="\t// InterfaceImplementor added constructor\n";
		ret+="\t" + getSignature(con) + " {\n";

		ret+="\t\tsuper(";
		Class params[]=con.getParameterTypes();
		if(params.length>0) {
			for(int i=0; i<params.length; i++) {
				ret+="a" + i + ", ";
			}
			// Get rid of the trailing ,space
			ret=ret.substring(0, ret.length()-2);
		}
		ret+=");\n\t}\n\n";
		return(ret);
	}

	/**
	 * Generate the source code for the class this object represents.
	 *
	 * @exception Exception now and then
	 */
	public String makeSource() throws Exception {
		String ret="";

		// If there's a package, declare it
		if(outpackage!=null) {
			ret+="package " + outpackage + ";\n\n";
		}

		ret+="public class " + outclass + " ";

		// If there's a superclass, extend it
		if(superClass!=null) {
			ret+="extends " + superClass.getName() + " ";
		}

		ret+="implements " + interfaceClass.getName() + " {\n\n";

		// If there's a superclass, grab all of the constructors from that
		// superclass and make sure they all get called.
		if(superClass!=null) {
			Constructor constructors[]=superClass.getConstructors();
			for(int i=0; i<constructors.length; i++) {
				ret+=implementConstructor(constructors[i]);
			}
		}

		// Now, implement the methods of the interface
		Method methods[]=interfaceClass.getMethods();
		for(int i=0; i<methods.length; i++) {
			String sig=getSignature(methods[i]);
			if(!definedFunctions.containsKey(sig)) {
				ret+=implement(methods[i]);
			}
		}
		ret+=("}\n");
		return(ret);
	}

	// A method com

	public static void usage() {
		System.err.println("Usage:  InterfaceImplementor"
			+ " -interface className [-superclass className]\n"
			+ "\t[-outputclass className]");
	}

	public static void main(String args[]) throws Exception {

		String superclassName=null;
		String interfaceName=null;
		String outclass=null;

		// parse the arguments
		for(int i=0; i<args.length; i++) {
			if(args[i].equals("-superclass")) {
				superclassName=args[++i];
			} else if(args[i].equals("-interface")) {
				interfaceName=args[++i];
			} else if(args[i].equals("-outputclass")) {
				outclass=args[++i];
			} else {
				System.err.println("Unknown argument:  " + args[i]);
				usage();
				throw new Exception("Unknown argument:  " + args[i]);
			}
		}

		// Make sure an interface was given.
		if(interfaceName == null) {
			System.err.println("No superinterface given.");
			usage();
			throw new Exception("No superinterface given.");
		}

		// Get an interface implementor
		InterfaceImplementor i=
			new InterfaceImplementor(Class.forName(interfaceName));

		// Set the superclass
		if(superclassName!=null) {
			System.out.println("Loading super class:  " + superclassName);
			i.setSuperClass(Class.forName(superclassName));
		}

		// If the user specified an output class, create the .java file to
		// make it, else send it to stdout with the class name BLAH
		if(outclass!=null) {
			// Set the output class name
			i.setOutputClass(outclass);

			String fn="";
			String op=i.getOutPackageName();
			String oc=i.getOutClassName();
			// Figure out if there's a package name, if so, make sure the
			// dirs exist and all that.
			if(op!=null) {
				char sep=File.separatorChar;
				File packagepath= new File(op.replace('.', sep));
				packagepath.mkdirs();
				fn+=packagepath + "/";
			}
			// Stick the classname.java to the end
			fn+=oc + ".java";

			// Write it out...
			System.out.println("Writing output to " + fn);
			FileOutputStream fout=new FileOutputStream(fn);
			fout.write(i.makeSource().getBytes());
		} else {
			System.out.print(i.makeSource());
		}
	}
}
