/*
 * Java Interface Implementor.  :)
 *
 * Copyright (c) 2000  Dustin Sallings <dustin@spy.net>
 */

import java.lang.reflect.*;
import java.util.*;

public class InterfaceImplementor extends Object {

	public static String displayMethodName(Method method) throws Exception {
		String ret="";

		// Get the modifiers
		int modifiers=method.getModifiers();
		// Clear the abstract flag
		modifiers&=~(Modifier.ABSTRACT);

		// Get the return type
		Class rt=method.getReturnType();

		String name=method.getName();

		Class types[]=method.getParameterTypes();

		ret=Modifier.toString(modifiers);
		ret+=" " + rt.getName() + " " + name + "(";
		if(types.length > 0) {
			for(int i=0; i<types.length; i++) {
				ret+=types[i].getName() + " a" + i + ",";
			}
			ret=ret.substring(0, ret.length()-1);
		}
		// Get rid of the last comma and add a paren
		ret+=")";

		return(ret);
	}

	public static void implement(Method method) throws Exception {
		// Start
		System.out.println("\t// InterfaceImplementor added "
			+ method.getName());
		System.out.println("\t" + displayMethodName(method) + " {");

		Class e[]=method.getExceptionTypes();
		// If we can throw an exception, do so.
		if(e.length>0) {
			System.out.println("\t\tthrow new "
				+ e[0].getName() + "(\"Not Implemented yet.\");");
		} else {
			// OK, let's check the return value...
			Class rt=method.getReturnType();
			if(rt.isPrimitive()) {
				if(rt == Boolean.TYPE) {
					System.out.println("\t\treturn false;");
				} else if(rt == Void.TYPE) {
					// Do nothing (nothing to do!)
				} else {
					System.out.println("\t\treturn 0;");
				}
			} else {
				System.out.println("\t\treturn null;");
			}
		}

		System.out.println("\t}");
	}

	public static void main(String args[]) throws Exception {
		String className=args[0];
		Class c=Class.forName(className);
		Method methods[]=c.getDeclaredMethods();

		System.out.println("public class BLAH implements " + className
			+ " {");
		for(int i=0; i<methods.length; i++) {
			String dc=methods[i].getDeclaringClass().getName();
			implement(methods[i]);
		}
		System.out.println("}");
	}
}
