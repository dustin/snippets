// Copyright (c) 2001  Dustin Sallings <dustin@spy.net>
//
// $Id: PKTest.java,v 1.1 2002/08/23 07:46:26 dustin Exp $

package net.spy.test;

import java.sql.SQLException;

import java.math.BigDecimal;

import junit.framework.Test;
import junit.framework.TestCase;
import junit.framework.TestSuite;

import net.spy.SpyConfig;
import net.spy.db.GetPK;

/**
 * Test the primary key implementation.
 */
public class PKTest extends TestCase {

	private SpyConfig conf=null;

	/**
	 * Get an instance of PKTest.
	 */
	public PKTest(String name) {
		super(name);
		conf=new SpyConfig(new java.io.File("test.conf"));
	}

	/**
	 * Get this test.
	 */
	public static Test suite() {
		return new TestSuite(PKTest.class);
	}

	/**
	 * Run this test.
	 */
	public static void main(String args[]) {
		junit.textui.TestRunner.run(suite());
	}

	/**
	 * Test basic primary key functionality.
	 */
	public void testPrimaryKeyByConf() throws SQLException {
		GetPK getpk=GetPK.getInstance();
		BigDecimal previous=getpk.getPrimaryKey(conf, "test_table");
		BigDecimal one=new BigDecimal(1);

		for(int i=0; i<1000; i++) {
			BigDecimal newKey=getpk.getPrimaryKey(conf, "test_table");

			assertEquals("Keys not in sequence", previous.add(one), newKey);

			previous=newKey;
		}
	}

}
