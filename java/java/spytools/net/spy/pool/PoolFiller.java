//
// $Id: PoolFiller.java,v 1.1 2000/07/01 09:44:32 dustin Exp $

package net.spy.pool;

import java.util.*;
import net.spy.SpyConfig;

public abstract class PoolFiller extends Object {
	protected SpyConfig conf=null;
	protected String name=null;

	public PoolFiller() {
		super();
	}

	public PoolFiller(String name, SpyConfig conf) {
		super();
		this.conf=conf;
		this.name=name;
	}

	public void setName(String name) {
		this.name=name;
	}

	public void setConfig(SpyConfig conf) {
		this.conf=conf;
	}

	public abstract PoolAble getObject() throws PoolException;

	protected int getPropertyInt(String what, int def) {
		return(conf.getInt(name + "." + what, def));
	}

	protected String getProperty(String what, String def) {
		return(conf.get(name + "." + what, def));
	}

	protected String getProperty(String what) {
		return(conf.get(name + "." + what));
	}
}
