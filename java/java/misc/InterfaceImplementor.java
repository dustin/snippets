/*
 * Java Interface Implementor.  :)
 *
 * Copyright (c) 2000  Dustin Sallings <dustin@spy.net>
 */

import java.lang.reflect.*;
import java.util.*;

public class InterfaceImplementor extends Object {

	protected String decodeType(Class type) {
		String rv=null;
		if(type.isArray()) {
			rv=decodeType(type.getComponentType()) + "[]";
		} else {
			rv=type.getName();
		}
		return(rv);
	}

	// Display the method signature
	protected String displayMethodName(Method method) throws Exception {
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
				ret+=decodeType(types[i]) + " a" + i + ",";
			}
			// Strip off the last comma
			ret=ret.substring(0, ret.length()-1);
		}
		// Get rid of the last comma and add a paren
		ret+=") ";

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

	protected String implement(Method method) throws Exception {
		// Start
		String ret=null;
		ret="\t// InterfaceImplementor added " + method.getName() + "\n";
		ret+="\t" + displayMethodName(method) + " {" + "\n";

		Class e[]=method.getExceptionTypes();
		// If we can throw an exception, do so.
		if(e.length>0) {
			ret+="\t\tthrow new "
				+ e[0].getName() + "(\"Not Implemented yet.\");\n";
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

	public String makeSource(String className) throws Exception {
		String ret=null;
		Class c=Class.forName(className);
		Method methods[]=c.getDeclaredMethods();

		// Whether it extends or implements (is it a class or Interface)
		String extimp="extends";
		if(c.isInterface()) {
			extimp="implements";
		}

		ret="public class BLAH " + extimp + " " + className + " {\n\n";
		for(int i=0; i<methods.length; i++) {
			String dc=methods[i].getDeclaringClass().getName();
			ret+=implement(methods[i]);
		}
		ret+=("}\n");
		return(ret);
	}

	public static void main(String args[]) throws Exception {
		String className=args[0];
		InterfaceImplementor i=new InterfaceImplementor();

		System.out.print(i.makeSource(className));
	}
}
