import java.lang.*;
import java.util.*;
import java.io.*;

public class Getit extends Thread {
	Stack urls;
	BufferedWriter log_file;

	public Getit(Stack v, BufferedWriter f) {
		super();
		urls=v;
		log_file=f;
	}

	protected boolean getone() {
		Fetch f=null;
		String url;
		boolean got=false;
		int i;

		try {
			url=(String)urls.pop();
		} catch(EmptyStackException e) {
			return(false);
		}
		System.out.println("Going to get " + url);
		// Retry on error up to three times.
		for(i=0; i<3 && got==false; i++) {
			try {
				long start, stop;
				start=System.currentTimeMillis();
				f=new Fetch(url);
				f.getLines();
				stop=System.currentTimeMillis();
				log_file.write(url + " " + (stop-start) + "\n");
				got=true;
			} catch(Exception e) {
				try {
					log_file.write("# Exception on " + url + " " + e + "\n");
					log_file.flush();
				} catch(Exception e2) {
					// Nothing, this sucks.
				}
			}
		}
		return(true);
	}

	public void run() {
		// Two modes of behavior, depending on whether we're a daemon or
		// not.
		if(isDaemon()) {
			// If we're a daemon thread, we'll just sleep when we run out.
			for(;;) {
				if(!getone()) {
					try {
						// Sleep five seconds if the stack's empty.
						sleep(5000);
					} catch(Exception e) {
						// We don't care.
					}
				}
			}
		} else {
			// If we're not a daemon, we're done when the stack's empty.
			while(getone());
		}
	}
}
