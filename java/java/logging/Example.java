import java.lang.*;

public class Example {
	public static void main(String args[]) {
		Logging l = new Logging();

		for(int i = 0; i<10; i++) {
			afunction();
			try {
				System.runFinalization();
			} catch(Exception e) {
			}
		}
		try {
			Thread.sleep(100);
		} catch(Exception e) {
		}
		System.out.println("Done!!!");
	}

	public static void afunction() {
		Logging l = new Logging();
		l.log(new LogEntry("This is a message."));

		anotherfunction();
	}

	public static void anotherfunction() {
		Logging l = new Logging();

		for(int i = 0 ; i<100; i++ ) {
			l.log(new LogEntry("This is sub-message " + i) );
		}
	}
}
