// Copyright (c) 2000  Dustin Sallings <dustin@spy.net>
//
// $Id: ObjectPool.java,v 1.1 2000/06/20 23:41:57 dustin Exp $

package net.spy.pool;

import java.util.*;
import net.spy.SpyConfig;

public class ObjectPool extends Object {
	protected Vector pool=null;
	protected SpyConfig conf=null;
	protected Exception objectException=null;

	public ObjectPool(SpyConfig conf) {
		super();
		this.conf=conf;

		initialize();
	}

	protected synchronized void initialize() {
		pool=new Vector();
		for(int i=0; i<conf.getInt("pool.startSize", 1); i++) {
			getNewObject();
		}
	}

	protected synchronized getNewObject() {
		objectException=null;
		try {
			Class c=Class.forName(conf.get("pool.object"));
			Poolable p=(Poolable)c.getInstance();
			p.poolConfigure(p);
			pool.addElement(p);
		} catch(Exception e) {
			System.err.println("ObjectPool exception:  " + e);
		}
	}
}
