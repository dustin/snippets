import javax.swing.*;          //This is the final package name.
                               //Swing releases before Swing 1.1 Beta 3.
import java.awt.*;
import java.awt.event.*;
import java.net.*;
import java.lang.*;
import java.util.*;

import net.spy.temperature.*;

public class SwingTherm implements Runnable {

	JFrame base=null;
	Hashtable hash=null;
	SpyTemp spytemp=null;
	String which=null;

	protected Component createComponents() throws Exception {
		String therm="http://bleu.west.spy.net/~dustin/images/therm.gif";
		URL url=new URL(therm);
		Image image=Toolkit.getDefaultToolkit().getImage(url);
		SwingThermImagePanel p=
			new SwingThermImagePanel(image, which, hash);
		return(p);
	}

    public static void main(String args[]) throws Exception {

		if(args.length < 1) {
			SpyTemp st=new SpyTemp();
			String list[] = st.listTherms();
			System.out.println("Which thermometer(s) would you like to watch?");
			System.out.println("The following are currently available:");
			for(int i=0; i<list.length; i++) {
				System.out.println("\t" + list[i]);
			}
			System.exit(1);
		}

		for(int i=0; i<args.length; i++) {
		SwingTherm app = new SwingTherm();
			app.realmain(args[i]);
		}
	}

	protected void realmain(String which) throws Exception {
        try {
            UIManager.setLookAndFeel(
                UIManager.getCrossPlatformLookAndFeelClassName());
        } catch (Exception e) { }

		spytemp = new SpyTemp();

		this.which=which;
		hash=new Hashtable();
		hash.put(which, new Double(0.0));

        //Create the top-level container and add contents to it.
        JFrame frame = new JFrame("SwingTherm");
		base=frame;
        Component contents = createComponents();
        frame.getContentPane().add(contents, BorderLayout.CENTER);

        frame.pack();
        frame.setVisible(true);

		Thread t = new Thread(this);
		t.start();
    }

	public void run() {
		while(true) {

			try {
				double t=spytemp.getTemp(which);;
				hash.put(which, new Double(t));
				System.out.println("Got new temperature for "
					+ which + ":  " + t);
				base.repaint();
			} catch(Exception e) {
				// Nothin'
			}

			// Wait
			try {
				Thread.sleep(60*1000);
			} catch(Exception e) {
				// Don't care
				System.err.println("Error!  " + e);
			}

		}
	}
}
