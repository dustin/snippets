// Copyright (c) 2002  Dustin Sallings <dustin@spy.net>
//
// $Id: Remote.java,v 1.2 2002/11/15 11:13:19 dustin Exp $

package net.spy.rpc.services;

import java.util.Vector;
import java.util.Enumeration;

import org.apache.xmlrpc.XmlRpcHandler;
import org.apache.xmlrpc.Invoker;

import net.spy.SpyUtil;

import net.spy.log.Logger;
import net.spy.log.LoggerFactory;

/**
 * Superclass for all XML-RPC services.
 */
public abstract class Remote extends Object implements XmlRpcHandler {

	private Logger logger=null;
	private Invoker invoker=null;

    /**
     * Get an instance of Remote.
     */
    public Remote() {
        super();
		invoker=new Invoker(this);
		logger=LoggerFactory.getLogger(getClass());
    }

	/**
	 * Invoker based execute implementation that logs a bunch more.
	 *
	 * @param method the name of the method to execute
	 * @param params the parameters to the method
	 * @return whatever the method returns
	 * @throws Exception if the method throws an exception
	 */
	public Object execute(String method, Vector params) throws Exception {
		Object rv=null;
		try {
			if(logger.isDebugEnabled()) {
				logger.debug("Executing " + method + " with " + params);
			} else if(logger.isInfoEnabled()) {
				logger.debug("Executing " + method);
			}
			rv=invoker.execute(method, params);
		} catch(NoSuchMethodException ex) {
			Vector v=new Vector(params.size());
			for(Enumeration e=params.elements(); e.hasMoreElements(); ) {
				v.addElement(e.nextElement().getClass().getName());
			}
			String args=SpyUtil.join(v.iterator(), ", ");
			logger.error("Could not find method " + method + "("
				+ args + ")", ex);
			throw ex;
		} catch(Exception e) {
			// Log the error on our end
			logger.error("XMLRPC Error", e);
			// Let the other side know
			throw e;
		} catch(Throwable t) {
			// Deal with the case where someone threw a non-Exception
			// Log the error on our end
			logger.error("XMLRPC Error", t);
			// Let the other side know
			throw new Exception(t.getMessage());
		}
		return(rv);
	}

}
