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
	static JLabel status_bar = null;

	static Hashtable queue=null;

	public Pager() {
		super();
	}

	protected synchronized void setStatus(String to) {
		int size = 0;
		
		if(queue!=null) {
			size = queue.size();
		}

		String status=size + " left.  " + to;
		status_bar.setText(status);
		status_bar.updateUI();
	}

	protected void sendPage() throws Exception {
		boolean goes_both_ways=twoway.isSelected();

		setStatus("Connecting to SNPP server");
		SNPP snpp=new SNPP("snpp.skytel.com", 444);

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
		bottom.setBorder(BorderFactory.createEmptyBorder(4,4,4,4));
		JButton send = new JButton("Send Page");
		JButton quit = new JButton("Quit");

		send.addActionListener ( new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				try {
					sendPage();
				} catch(Exception ception) {
					System.err.println("Damn:  " + ception);
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
		pane.setBorder(BorderFactory.createEmptyBorder(4,4,4,4));
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

	protected Component status() throws Exception {
		JPanel pane=new JPanel();
		pane.setBorder(BorderFactory.createEmptyBorder(4,4,4,4));
		pane.setLayout( new BoxLayout(pane, BoxLayout.Y_AXIS));
		status_bar = new JLabel("STATUS");
		pane.add(status_bar);
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
		Component status = status();
		Component bottom = bottom();
		Container c = frame.getContentPane();
		// c.add(pane_1,"North");
		// c.add(status,"South");
		// c.add(bottom,"South");
		c.add(pane_1, "North");
		c.add(bottom, "Center");
		c.add(status, "South");

		WindowListener l =  new WindowAdapter() {
			public void windowClosing(WindowEvent e) {
				System.exit(0);
			}
		};


		frame.addWindowListener(l);

		// no wonder it won't resize!
        frame.pack();

		setStatus("Ready!");

		Thread t = new Thread(this);
		t.start();
    }

	protected void popUp(String original, String response) {
		String rr="";
		rr=response + "\n\nIn reply to:  " + original;

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
			}

			for(Enumeration e=queue.keys(); e.hasMoreElements(); ) {
				String msta = (String)e.nextElement();
				String text = (String)queue.get(msta);
				String msg=null;
				setStatus("Checking on " + msta);
				try {
					msg=s.getResponse(msta);
					if(msg!=null) {
						queue.remove(msta);
						setStatus("Got response for " + msta);
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
