// Copyright 1996 SPY Networking
import java.applet.*;
import java.awt.*;
import java.net.*;
import java.util.*;

public class GetTempFromCGI extends Applet implements Runnable
{
	Image image=null;
	URL url=null;
	Thread timer=null;

	// init function creates the double buffer
	public void init() {
		try {
			url=new URL(getParameter("image"));
		} catch(Exception e) {
			// Nothin'
		}
		image=getImage(url);
	}

	public void start() {
		if(timer==null) {
			timer = new Thread(this);
			timer.start();
		}
	}

	public void run() {
		while(timer!=null) {
			try {
				Thread.sleep(5*60*1000);
			} catch(Exception e) {
				System.err.println("Sleep exception:  " + e);
			}
			// Fetch and flush
			image=getImage(url);
			image.flush();
			repaint();
		}
	}

	// Update does about the same thing here.
	public void update(Graphics g) {
		super.update(g);
		g.drawImage(image, 0, 0, this);
	}

	// Paint.
	public void paint(Graphics g) {
		super.paint(g);
		g.drawImage(image, 0, 0, this);
	}
}
