// Copyright (c) 1999  Dustin Sallings <dustin@spy.net>
//
// $Id: ImageData.java,v 1.1 1999/11/24 08:49:34 dustin Exp $

public class ImageData {
	// Meta stuff
	int format_version=1;
	boolean thumbnail=false;

	// Image data.
	Vector image_data = null;
	int image_width = 0;
	int image_height = 0;
}
