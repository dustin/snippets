import javax.swing.*;
import java.awt.*;
import java.awt.event.*;
import java.net.*;
import java.lang.*;
import java.util.*;

import net.spy.net.*;

public class Pager implements Runnable {

	JFrame base=null;
	JTextField snpp_id = null;
	JTextField message = null;
	JCheckBox twoway = null;

	static Hashtable queue=null;

	public Pager() {
		super();
	}

	protected void sendPage() throws Exception {
		boolean goes_both_ways=twoway.isSelected();

		SNPP snpp=new SNPP("snpp.skytel.com", 444);
		snpp.debug=true;

		if(goes_both_ways) {
			snpp.twoWay();
		}

		snpp.pagerID(snpp_id.getText());
		snpp.message(message.getText());

		snpp.send();

		if(goes_both_ways) {
			String tag=snpp.getTag();
			queue.put(tag, message.getText());
		}

	}

	protected Component bottom() throws Exception {
		JPanel bottom = new JPanel();
		bottom.setLayout( new BoxLayout(bottom, BoxLayout.X_AXIS));
		bottom.setBorder(BorderFactory.createEmptyBorder(14,14,14,14));
		JButton send = new JButton("Send Page");
		JButton quit = new JButton("Quit");

		send.addActionListener ( new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				System.out.println(snpp_id.getText());
				System.out.println(message.getText());
				System.out.println("Sending Page...");
				System.out.println("Two-way is " + twoway.isSelected());
				try {
					sendPage();
				} catch(Exception ception) {
					System.out.println("Damn:  " + ception);
				}
			}
		} );

		quit.addActionListener ( new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				System.exit(0);
			}
		} );

		bottom.add(send);
		bottom.add(quit);
		return(bottom);
	}
	protected Component level1() throws Exception {
		JPanel pane=new JPanel();
		pane.setBorder(BorderFactory.createEmptyBorder(14,14,14,14));
		pane.setLayout( new BoxLayout(pane, BoxLayout.Y_AXIS));

		JLabel label=new JLabel("SNPP ID:  ");
		snpp_id = new JTextField(16);

		JLabel label2=new JLabel("Message:  ");
		message = new JTextField(30);

		pane.add(label);
		pane.add(snpp_id);
		pane.add(label2);
		pane.add(message);

		twoway = new JCheckBox("Two-Way");
		pane.add(twoway);

		return(pane);
	}

    public static void main(String args[]) throws Exception {

	Pager app = new Pager();
		app.realmain(args);
	}

	protected void realmain(String args[]) throws Exception {
        try {
            UIManager.setLookAndFeel(
                UIManager.getCrossPlatformLookAndFeelClassName());
        } catch (Exception e) { }

        //Create the top-level container and add contents to it.
        JFrame frame = new JFrame("Pager");
        frame.setVisible(true);
		base=frame;
	Component pane_1 = level1();
		Component bottom = bottom();
		Container c = frame.getContentPane();
		Container c_1 = frame.getContentPane();
		c.add(pane_1,"North");
		c.add(bottom,"South");

		WindowListener l =  new WindowAdapter() {
			public void windowClosing(WindowEvent e) {
				System.exit(0);
			}
		};


		frame.addWindowListener(l);

		// no wonder it won't resize!
        frame.pack();

		Thread t = new Thread(this);
		t.start();
    }

	protected void popUp(String original, String response) {
		String rr="";
		rr=response + "\n\nIn reply to:  " + original;
		System.out.println(rr);

		final JFrame jf = new JFrame();
		jf.setBackground(new Color(255, 255, 255));
		Container c = jf.getContentPane();

		JPanel pane=new JPanel();
		JTextArea jta=new JTextArea(rr, 10, 30);
		jta.setLineWrap(true);
		jta.setWrapStyleWord(true);
		pane.setBackground(new Color(255, 255, 255));
		pane.add(jta);

		JPanel pane2=new JPanel();
		pane2.setBackground(new Color(255, 255, 255));

		JButton ok = new JButton("OK");
		ok.addActionListener ( new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				jf.dispose();
			}
		});

		pane2.add(ok);

		c.add(pane, "North");
		c.add(pane2, "South");

		jf.pack();
		jf.setVisible(true);
	}

	// This will eventually do the polling.
	public void run() {

		queue=new Hashtable();

		while(true) {

			SNPP s = null;

			// Wait
			try {
				Thread.sleep(5*1000);
			} catch(Exception e) {
				// Don't care
				System.err.println("Error!  " + e);
			}

			if(queue.size() > 0) {
				try {
					s=new SNPP("snpp.skytel.com", 444);
				} catch(Exception e) {
					// Blah
				}
				s.debug=true;
			}

			for(Enumeration e=queue.keys(); e.hasMoreElements(); ) {
				String msta = (String)e.nextElement();
				String text = (String)queue.get(msta);
				String msg=null;
				try {
					msg=s.getResponse(msta);
					if(msg!=null) {
						queue.remove(msta);
						popUp(text, msg);
					}
				} catch(Exception cept) {
					// Damn
				}
			}
			// Kill the SNPP connection
			s=null;
		}
	}
}
