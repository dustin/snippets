// Copyright (c) 2001  Dustin Sallings <dustin@spy.net>
//
// $Id: ImageLoader.java,v 1.1 2002/08/22 07:22:59 dustin Exp $

package net.spy.png;

import java.awt.*;
import java.awt.image.*;
import java.net.URL;
import java.io.IOException;

/**
 * Load images.
 */
public class ImageLoader extends Object implements ImageObserver {

	private boolean imageLoaded=false;
	private Image image=null;

	/**
	 * Get an instance of ImageLoader.
	 * @param pathToImage the URL to the image
	 */
	public ImageLoader(URL pathToImage) throws IOException {
		super();

		loadImage(pathToImage);
	}

	/**
	 * Get the image.
	 *
	 * @return the image
	 */
	public Image getImage() {
		return(image);
	}

	private void loadImage(URL u) throws IOException {
		image=Toolkit.getDefaultToolkit().getImage(u);
		Toolkit.getDefaultToolkit().prepareImage(image,-1,-1,this);
		if(!imageLoaded) {
			try {
				synchronized(this) {
					wait(15000);
				}
			} catch(InterruptedException e) {
				e.printStackTrace();
			}
		}
	}

	/**
	 * @see ImageObserver
	 */
	public boolean imageUpdate(Image img, int infoflags, int x, int y,
		int width, int height) {

		if((infoflags&ALLBITS) != 0) {
			imageLoaded=true;
			synchronized(this) {
				notifyAll();
			}
		}

		return(!imageLoaded);
	}

}
