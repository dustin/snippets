/*
 * Copyright (c) 2000  Dustin Sallings <dustin@spy.net>
 *
 * $Id: PhotoSearchResults.java,v 1.1 2000/03/17 09:41:23 dustin Exp $
 */

package net.spy.photo;

import java.util.*;

import javax.servlet.*;
import javax.servlet.http.*;

import net.spy.*;

public class PhotoSearchResults extends Object {
	protected Vector results=null;
	protected int current=0;
	protected int max=0;

	public PhotoSearchResults() {
		results=new Vector();
	}

	// Add a search result to the list.
	public void add(PhotoSearchResult r) {
		// Set the result id
		r.id=max;
		results.addElement(r);

		max++;
	}

	// Set the search result we're lookin' at.
	public void set(int to) {
		if(to>max) {
			current=max;
		} else {
			current=to;
		}
	}

	// Get the current entry
	public PhotoSearchResult get() {
		return((PhotoSearchResult)results.elementAt(current));
	}

	// Get the next result, or null if we're done
	public PhotoSearchResult next() {
		PhotoSearchResult r=null;

		if(current<max) {
			r=(PhotoSearchResult)results.elementAt(current);
			current++;
		}
		return(r);
	}

	// Get the previous result, or null if we're at the beginning
	public PhotoSearchResult prev() {
		PhotoSearchResult r=null;

		if(current>0) {
			current--;
			r=(PhotoSearchResult)results.elementAt(current);
		}
		return(r);
	}

	// Find out how many results total are in this result set
	public int nResults() {
		return(max);
	}

	// Find out how many results are remaining
	public int nRemaining() {
		return(max-current);
	}
}
