import java.lang.*;
import java.util.*;
import java.io.*;

public class LoggingFlusher extends Thread {

	public boolean is_running = false;

	private static BufferedWriter log_file=null;
	private static Logging log_object;

	public LoggingFlusher(ThreadGroup t) {
		super(t, "log_flusher");
		this.setDaemon(true);
	}

	public synchronized void doFlush() {
		Vector v = log_object.flush();
		System.out.println("Flushing!");
		for(int i = 0; i<v.size(); i++) {
			try {
				LogEntry l = (LogEntry)v.elementAt(i);
				log_file.write( l.toString() + "\n");
			} catch(IOException e) {
				System.err.println("Error writing log:  " + e.getMessage());
			}
		}
		try {
			log_file.flush();
		} catch(IOException e) {
			System.err.println("Error writing log:  " + e.getMessage());
		}
	}

	public void run() {
		is_running = true;

		try {
			log_file = new BufferedWriter(
				new FileWriter("/tmp/log.boy", true));
		} catch(IOException e) {
			System.err.println("Error:  " + e.getMessage());
		}
		log_object = new Logging();

		System.out.println("Running thread...");

		for(;;) {
			try {
				// Wait a second before continuing
				sleep(1000);
				System.out.println("Flusher thread sleep thing is over.");
			} catch(Exception e) {
			} finally {
				System.out.println("Flushing.");
				doFlush();
			}
		}
	}

	public void finalize() throws Throwable {
		doFlush();
		super.finalize();
	}
}
