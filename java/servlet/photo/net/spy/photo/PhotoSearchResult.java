/*
 * Copyright (c) 2000  Dustin Sallings <dustin@spy.net>
 *
 * $Id: PhotoSearchResult.java,v 1.1 2000/03/17 09:41:20 dustin Exp $
 */

package net.spy.photo;

import javax.servlet.*;
import javax.servlet.http.*;

import net.spy.*;

public class PhotoSearchResult extends Object {
	public String keywords=null;
	public String descr=null;
	public String cat=null;
	public String size=null;
	public String taken=null;
	public String ts=null;
	public String image=null;
	public String catnum=null;
	public String addedby=null;

	public int id=-1;
}
