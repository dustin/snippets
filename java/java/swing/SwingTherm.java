import javax.swing.*;          //This is the final package name.
                               //Swing releases before Swing 1.1 Beta 3.
import java.awt.*;
import java.awt.event.*;
import java.net.*;

import net.spy.temperature.*;

public class SwingTherm {

	Component createComponents(String which) throws Exception {
		String therm="http://bleu.west.spy.net/~dustin/images/therm.gif";
		URL url=new URL(therm);
		Image image=Toolkit.getDefaultToolkit().getImage(url);
		SwingThermImagePanel p=new SwingThermImagePanel(image, which);
		return(p);
	}

    public static void main(String[] args) throws Exception {
        try {
            UIManager.setLookAndFeel(
                UIManager.getCrossPlatformLookAndFeelClassName());
        } catch (Exception e) { }

		if(args.length < 1) {
			SpyTemp st = new SpyTemp();
			String list[] = st.listTherms();
			System.out.println("Which thermometer would you like to watch?");
			for(int i=0; i<list.length; i++) {
				System.out.println("\t" + list[i]);
			}
			System.exit(1);
		}

        //Create the top-level container and add contents to it.
        JFrame frame = new JFrame("SwingTherm");
        SwingTherm app = new SwingTherm();
        Component contents = app.createComponents(args[0]);
        frame.getContentPane().add(contents, BorderLayout.CENTER);

        frame.pack();
        frame.setVisible(true);
    }
}
