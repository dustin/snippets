import javax.swing.*;          //This is the final package name.
                               //Swing releases before Swing 1.1 Beta 3.
import java.awt.*;
import java.awt.event.*;
import java.net.*;
import java.lang.*;
import java.util.*;

import net.spy.*;
import net.spy.net.*;

public class Latency implements Runnable {

	JFrame base=null;
	Hashtable hash=null;

	String args[]=null;

	protected Component createComponents() throws Exception {
		String
			therm="http://verde.software.net/~dustin/images/latency_gauge.gif";
		URL url=new URL(therm);
		Image image=Toolkit.getDefaultToolkit().getImage(url);

		JPanel pane=new JPanel();
		// Grid-like layout
		pane.setLayout(new GridLayout(1, 0));
		// Background should be white.
		pane.setBackground(new Color(255, 255, 255));
		// We want a five pixel border around everything
		pane.setBorder(BorderFactory.createEmptyBorder(
			5, 5, 5, 5));

		for(int i=0; i<args.length; i++) {
			pane.add(new LatencyGaugePanel(image, args[i], hash));
		}

		return(pane);
	}

    public static void main(String args[]) throws Exception {

       	Latency app = new Latency();
		app.realmain(args);
	 }

	protected void realmain(String args[]) throws Exception {
        try {
            UIManager.setLookAndFeel(
                UIManager.getCrossPlatformLookAndFeelClassName());
        } catch (Exception e) { }

		this.args=args;

		hash=new Hashtable();

		for(int i=0; i<args.length; i++) {
			hash.put(args[i], new Double(0.0));
		}

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
		double t=0.00;
		while(true) {

			try {
				String url="http://lame/~brandonk/status/db/dbStatus.current";
				HTTPFetch f = new HTTPFetch(url);
				Vector v = f.getLines();
				for(int i=1; i<v.size(); i++) {
					String a[]=SpyUtil.split(":", (String)v.elementAt(i));
					String toparse=a[1].trim();
					t=Double.valueOf(toparse).doubleValue();
					hash.put(a[0], new Double(t*60));
					System.out.println("Got new latency reading for "
						+ a[0] + ": " + a[1] + " (" + t + ")");
				}
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
