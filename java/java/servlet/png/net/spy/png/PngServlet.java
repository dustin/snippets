// Copyright (c) 2001  Dustin Sallings <dustin@spy.net>
//
// $Id: PngServlet.java,v 1.3 2002/04/12 08:52:15 dustin Exp $

package net.spy.png;

import java.awt.*;
import java.awt.image.*;
import java.io.*;

import javax.servlet.*;
import javax.servlet.http.*;

// GIF support
import Acme.JPM.Encoders.GifEncoder;

/**
 * A servlet that allows one to create PNGs easily.
 */
public class PngServlet extends HttpServlet {

	private Frame frame=null;

	/**
	 * Get the frame for making images and what-not.
	 */
	public void init(ServletConfig conf) throws ServletException {
		super.init(conf);

		frame=new Frame();
		frame.addNotify();
	}

	/**
	 * Get an Image to play with.
	 *
	 * @param width the width of the Image.
	 * @param height the height of the Image.
	 */
	protected Image createImage(int width, int height) {
		return(frame.createImage(width, height));
	}

	/**
	 * Abastract image writing.  Will write a PNG (preferred) or a GIF,
	 * depending on what the browser wants.
	 *
	 * @param request the HTTP Request.
	 * @param response the HTTP Response.
	 * @param image the Image to write.
	 */
	protected void writeImage(HttpServletRequest request,
		HttpServletResponse response, Image image) throws IOException {

		String encodings=request.getHeader("Accept");
		// log("Encodings:  " + encodings);
		if(encodings != null && encodings.indexOf("png")>=0) {
			log("Sending PNG");
			writePng(request, response, image);
		} else {
			log("Sending GIF");
			writeGif(request, response, image);
		}
	}

	/**
	 * Write the PNG.
	 *
	 * @param request the HTTP Request.
	 * @param response the HTTP Response.
	 * @param image the Image to write.
	 *
	 * @deprecated use the abstract writeImage instead
	 */
	protected void writePng(HttpServletRequest request,
		HttpServletResponse response, Image image) throws IOException {

		// Set the content type for PNG
		response.setContentType("image/png");

		// Get the output stream.
		OutputStream out=response.getOutputStream();

		// Encode it and write it.
		PngEncoder pnge=new PngEncoder(image, false, PngEncoder.FILTER_NONE, 9);
		out.write(pnge.pngEncode());

		out.flush();
		out.close();
	}

	/**
	 * Write the GIF.
	 *
	 * @param request the HTTP Request.
	 * @param response the HTTP Response.
	 * @param image the Image to write.
	 */
	private void writeGif(HttpServletRequest request,
		HttpServletResponse response, Image image) throws IOException {

		// Set the content type for PNG
		response.setContentType("image/gif");

		// Get the output stream.
		OutputStream out=response.getOutputStream();

		// Encode it and write it.
		GifEncoder gife=new GifEncoder(image, out);
		gife.encode();

		out.flush();
		out.close();
	}

}
