import javax.swing.*;
import java.awt.*;
import java.awt.event.*;
import java.net.*;
import java.lang.*;
import java.util.*;
import java.io.*;

import net.spy.net.*;

public class eBayWatch implements Runnable {

	JFrame base=null;
	JTextField item_no = null;
	// JCheckBox twoway = null;
	static JLabel status_bar = null;

	static Hashtable queue=null;

	static Properties prop=null;

	final static String prop_file="ebaywatch.prp";

	public eBayWatch() {
		super();
	}

	protected void readProperties() {
		if(prop==null) {
			prop=new Properties();
			try {
				prop.load( new FileInputStream(prop_file));
			} catch(Exception e) {
				System.err.println("Error loading properties.");
			}
		}
	}

	protected void saveProperties() {
		if(prop!=null) {
			try {
				prop.save( new FileOutputStream(prop_file),
					" Properties for eBayWatch, please don't edit");
			} catch(Exception e) {
				System.err.println("Error saving properties.");
			}
		}
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

	// When the user clicks on addItem, do this.
	protected void addItem() throws Exception {
		// boolean goes_both_ways=twoway.isSelected();

		setStatus("Adding an item.");

		String item = item_no.getText();
		prop.put("item." + item, "unknown");
		queue.put(item, new Watcher(item));
		saveProperties();
	}

	protected Component bottom() throws Exception {
		JPanel bottom = new JPanel();
		bottom.setLayout( new BoxLayout(bottom, BoxLayout.X_AXIS));
		bottom.setBorder(BorderFactory.createEmptyBorder(4,4,4,4));
		JButton send = new JButton("Add Item");
		JButton quit = new JButton("Quit");

		send.addActionListener ( new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				try {
					addItem();
				} catch(Exception ception) {
					System.err.println("Damn:  " + ception);
				}
			}
		} );

		quit.addActionListener ( new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				saveProperties();
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

		JLabel label=new JLabel("Item No:  ");
		item_no = new JTextField(32);

		pane.add(label);
		pane.add(item_no);

		// twoway = new JCheckBox("Two-Way");
		// pane.add(twoway);

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
	eBayWatch app = new eBayWatch();
		app.realmain(args);
	}

	// Load the items from the properties file
	protected void addItems() {
		for(Enumeration en = prop.keys(); en.hasMoreElements();) {
			String p=(String)en.nextElement();
			if(p.startsWith("item.")) {
				String item=p.substring(5);
				queue.put(item, new Watcher(item));
			}
		}
	}

	protected void realmain(String args[]) throws Exception {
		queue=new Hashtable();

		// Read the properties, and configure in whatever we're watching.
		readProperties();
		addItems();

        try {
            UIManager.setLookAndFeel(
                UIManager.getCrossPlatformLookAndFeelClassName());
        } catch (Exception e) { }

        //Create the top-level container and add contents to it.
        JFrame frame = new JFrame("eBayWatch");
        frame.setVisible(true);
		base=frame;
	Component pane_1 = level1();
		Component status = status();
		Component bottom = bottom();
		Container c = frame.getContentPane();

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

		// Start the queue watcher.
		Thread t = new Thread(this);
		t.start();
    }

	protected void popUp(String msg) {
		String rr=msg;

		final JFrame jf = new JFrame();
		jf.setBackground(new Color(255, 255, 255));
		Container c = jf.getContentPane();

		JPanel pane=new JPanel();
		JTextArea jta=new JTextArea(rr, 13, 30);
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

		while(true) {

			for(Enumeration e=queue.keys(); e.hasMoreElements(); ) {
				try {

					String key = (String)e.nextElement();
					Watcher ebw = (Watcher)queue.get(key);
					String tl=ebw.getTime();

					if(tl.length() > 0) {
						tl += " left";
					}

					// Go ahead and remove stuff first.
					if(tl.startsWith("Auction has ended")) {
						setStatus("Removing " + key);
						queue.remove(key);
						prop.remove("item." + key);
					} else {
						setStatus("Checking on " + key + " " + tl);

						if(ebw.hasChanged()) {
							setStatus(key + " has changed.");

							// Figure out if we need a Was line
							String was="";
							was=ebw.oldStatus();
							if(was.length() > 0) {
								was="\n\nWas:\n" + was + "\n";
							} else {
								// If we didn't have a was, we didn't have
								// a description for this item in the
								// properties file (maybe)
								prop.put("item." + key, ebw.describe());
							}

							String msg="Item " + key + " has changed:\n"
								+ ebw.currentStatus() + was + tl;
							popUp(msg);
						} // Item info has changed
					} // Auction for item has not ended

				} catch(Exception cept) {
					System.err.println("Error checking stuff:  " + cept);
					cept.printStackTrace();
				}
			} // Looking through all of the queued items

			// Wait
			try {
				Thread.sleep(30*1000);
			} catch(Exception e) {
				// Don't care
				System.err.println("Error!  " + e);
			} // sleep (and exception)
		} // Infinite Loop(tm)
	} // Thread run
}
