// Copyright (c) 2000  Dustin Sallings
//
// $Id: PackageInfo.java,v 1.3 2002/07/10 04:25:33 dustin Exp $

package net.spy.info;

/**
 * Abstract class for Info classes that describe shipping information.
 */
public abstract class PackageInfo extends Info {
	protected boolean delivered=false;

	public boolean isDelivered() {
		return(delivered);
	}
}

