// Copyright (c) 2001  Dustin Sallings <dustin@spy.net>
//
// $Id: StupidImageObserver.java,v 1.1 2002/08/22 07:32:53 dustin Exp $

package net.spy.png;

import java.awt.image.ImageObserver;
import java.awt.Image;

/**
 * The most stupid impementation of ImageObserver
 */
public class StupidImageObserver extends Object implements ImageObserver {

	private boolean imageLoaded=false;

	/**
	 * Get an instance of StupidImageObserver.
	 */
	public StupidImageObserver() {
		super();
	}

	/**
	 * @see ImageObserver
	 */
	public boolean imageUpdate(Image img, int infoflags, int x, int y,
		int width, int height) {

		if((infoflags&ALLBITS) != 0) {
			imageLoaded=true;
		}

		return(!imageLoaded);
	}

}
