/*
 * Copyright (c) 2000  Dustin Sallings <dustin@spy.net>
 *
 * $Id: PhotoSearchResults.java,v 1.3 2000/06/24 08:03:13 dustin Exp $
 */

package net.spy.photo;

import java.util.*;

import javax.servlet.*;
import javax.servlet.http.*;

import net.spy.*;

public class PhotoSearchResults extends Object {
	protected Vector _results=null;
	protected int _current=0;
	protected int _max=0;

	public PhotoSearchResults() {
		super();
		_results=new Vector();
	}

	// Add a search result to the list.
	public void add(PhotoSearchResult r) {
		// Set the result id
		r.id=_max;
		_results.addElement(r);

		_max++;
	}

	// Set the search result we're lookin' at.
	public void set(int to) {
		if(to>_max) {
			_current=_max;
		} else {
			_current=to;
		}
	}

	// Get the current entry
	public PhotoSearchResult get() {
		return((PhotoSearchResult)_results.elementAt(_current));
	}

	// Get a specific
	public PhotoSearchResult get(int which) {
		return((PhotoSearchResult)_results.elementAt(which));
	}

	// Get the next result, or null if we're done
	public PhotoSearchResult next() {
		PhotoSearchResult r=null;

		if(_current<_max) {
			r=(PhotoSearchResult)_results.elementAt(_current);
			_current++;
		}
		return(r);
	}

	// Get the previous result, or null if we're at the beginning
	public PhotoSearchResult prev() {
		PhotoSearchResult r=null;

		if(_current>0) {
			_current--;
			r=(PhotoSearchResult)_results.elementAt(_current);
		}
		return(r);
	}

	// Find out how many results total are in this result set
	public int nResults() {
		return(_max);
	}

	// Find out how many results are remaining
	public int nRemaining() {
		return(_max-_current);
	}

	// Find out which one we're on
	public int current() {
		return(_current);
	}
}
