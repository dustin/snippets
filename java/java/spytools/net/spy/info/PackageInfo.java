// Copyright (c) 2000  Dustin Sallings
//
// $Id: PackageInfo.java,v 1.1 2000/06/16 20:08:37 dustin Exp $

package net.spy.info;

public abstract class PackageInfo extends Info {
	protected boolean delivered=false;

	public boolean isDelivered() {
		return(delivered);
	}
}
