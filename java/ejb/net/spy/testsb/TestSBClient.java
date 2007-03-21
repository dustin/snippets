package net.spy.testsb;

import java.io.FileInputStream;
import java.util.Hashtable;
import java.util.Properties;
import javax.naming.InitialContext;
import javax.naming.Context;
import javax.transaction.UserTransaction;

public class TestSBClient {

    public static void main(String args[]) { 

	// Set environment properties
	Properties env = System.getProperties();
	// JNDI is above rmiregistry
	env.put("java.naming.factory.initial", "com.sun.jndi.rmi.registry.RegistryContextFactory");
	// java.naming.provider.url can be set with a -D option on java command.
	// you can choose instead to uncomment the following line, set with your own url.
	// env.put("java.naming.provider.url", "rmi://hostname:1099");

	// get JNDI initial context
	Context initialContext = null;
	try {
	    initialContext = new InitialContext(env);
	} catch (Exception e) {
	    System.err.println("Cannot get initial context for JNDI: " + e);
	    System.exit(2);
	}

	// We want to start transactions from client: get UserTransaction
	UserTransaction utx = null;
	try {
	    utx = (UserTransaction) initialContext.lookup("javax.transaction.UserTransaction");
	} catch (Exception e) {
	    System.err.println("Cannot lookup UserTransaction: "+e);
	    System.exit(2);
	}

	// Connecting to TestSBHome thru JNDI
	TestSBHome home = null;
	try {
	    home = (TestSBHome) initialContext.lookup("TestSBHome");
	} catch (Exception e) {
	    System.err.println( "Cannot lookup TestSBHome: " + e);
	    System.exit(2);
	}

	// TestSBBean creation
	TestSB t1 = null;
	try {
	    System.out.println("Create a bean");
	    t1 = home.create("User1");
		System.out.println("Done.");
	} catch (Exception e) {
	    System.err.println("Cannot create TestSB: " + e);
	    System.exit(2);
	}

	// First transaction (committed)
	try {
	    System.out.println("Start a first transaction");
	    utx.begin();
	    System.out.println("First request on the new bean");
	    t1.buy(10);
	    System.out.println("Second request on the bean");
	    t1.buy(20);
	    System.out.println("Commit the transaction");
	    utx.commit();
	} catch (Exception e) {
	    System.err.println("exception during 1st Tx: " + e);
	    System.exit(2);
	}

	// Start another transaction (rolled back)
	try {
	    System.out.println("Start a second transaction");
	    utx.begin();
	    t1.buy(50);
	    System.out.println("Rollback the transaction");
	    utx.rollback();
	} catch (Exception e) {
	    System.err.println("exception during 2nd Tx: " + e);
	    System.exit(2);
	}

	// Get the total bought, outside the transaction
	int val = 0;
	try {
	    System.out.println("Request outside any transaction");
	    val = t1.read();
	} catch (Exception e) {
	    System.err.println("Cannot read value on t1 : "+e);  
	    System.exit(2);
	}
	if (val != 10+20) {
	    System.err.println("Bad value read: "+val);
	    System.exit(2);
	}

	// Remove Session bean
	try {
	    t1.remove();
	} catch (Exception e) {
	    System.out.println("Exception on buy: "+e);
	    System.exit(2);
	}
	
	System.out.println("TestSBClient OK. Exiting.");
    }
}

    
