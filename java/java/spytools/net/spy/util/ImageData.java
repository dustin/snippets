// Copyright (c) 1999  Dustin Sallings <dustin@spy.net>
//
// $Id: ImageData.java,v 1.2 1999/11/24 08:50:43 dustin Exp $

import java.lang.*;
import java.util.*;

public class ImageData {
	// Meta stuff
	int format_version=1;
	boolean thumbnail=false;

	// Image data.
	Vector image_data = null;
	int image_width = 0;
	int image_height = 0;
}
