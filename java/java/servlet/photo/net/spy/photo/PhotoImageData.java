// Copyright (c) 1999 Dustin Sallings <dustin@spy.net>
//
// $Id: PhotoImageData.java,v 1.1 1999/10/20 03:42:58 dustin Exp $

package net.spy.photo;

import java.util.*;
import java.lang.*;
import java.io.Serializable;

import net.spy.*;

// Store the actual data for a PhotoImage object.
public class PhotoImageData extends Object implements Serializable {

	// Version number so we don't jack shite up.
	int format_version = 1;

	// Full size image
	Vector image_data = null;
	int image_width = 0;
	int image_height = 0;

	// Thumbnail stuff.
	Vector thumbnail_data = null;
	int thumbnail_width = 0;
	int thumbnail_height = 0;
}
