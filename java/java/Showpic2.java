// Copyright 1996 SPY Networking
import java.applet.*;
import java.awt.*;

public class Showpic2 extends Applet
{
Image what;
Image Ibuf;
Graphics Gbuf;

	// init function creates the double buffer
	public void init()
	{
		String tmp;
		Ibuf=createImage(size().width, size().height);
		Gbuf=Ibuf.getGraphics();

		tmp=getParameter("image");
		System.out.println("Getting " + tmp);
		what=getImage(getDocumentBase(), tmp);
		what.flush();

		Gbuf.drawImage(what, 0, 0, this);
	}

	public void update(Graphics g)
	{
		Gbuf.drawImage(what, 0, 0, this);
		g.drawImage(Ibuf, 0, 0, this);
	}

	public void paint(Graphics g)
	{
		Gbuf.drawImage(what, 0, 0, this);
		g.drawImage(Ibuf, 0, 0, this);
	}
}
