import java.lang.*;
import java.util.*;
import java.io.*;

// This is just to flush the log_file every couple of seconds.
public class Flusher extends Thread {
	BufferedWriter log_file;

	public Flusher(BufferedWriter f) {
		super();
		this.setDaemon(true);
		log_file=f;
	}

	public void run() {
		for(;;) {
			try {
				sleep(5000);
				log_file.flush();
			} catch(Exception e) {
			}
		}
	}
}
