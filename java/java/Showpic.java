// Copyright 1996 SPY Networking
import java.applet.*;
import java.awt.*;

public class Showpic extends Applet
{
Image what;

	// init function creates the double buffer
	public void init()
	{
		String tmp;
		tmp=getParameter("image");
		System.out.println("Getting " + tmp);
		what=getImage(getDocumentBase(), tmp);
		what.flush();
	}

	public void paint(Graphics g)
	{
		g.drawImage(what, 0, 0, this);
	}
}
