// Copyright (c) 1999  Dustin Sallings <dustin@spy.net>
//
// $Id: ImageData.java,v 1.3 1999/11/25 09:37:00 dustin Exp $

package net.spy.util;

import java.lang.*;
import java.util.*;
import java.io.Serializable;

public class ImageData extends Object implements Serializable {
	// Meta stuff
	public int format_version=1;
	public boolean thumbnail=false;

	// Image data.
	public Vector image_data = null;
	public int image_width = 0;
	public int image_height = 0;

	public ImageData() {
		super();
	}

	public ImageData(Vector v) {
		super();
		image_data = v;
	}
}
