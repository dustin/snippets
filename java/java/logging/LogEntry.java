// Object to store the shite properly.

import java.util.*;
import java.io.*;

public class LogEntry extends Object {

	private String message;
	private int priority;
	private Date timestamp;
	private Exception exception;

	private String getStack() {
		String r;
		ByteArrayOutputStream bytes = new ByteArrayOutputStream();
		PrintWriter writer = new PrintWriter(bytes, true);
		exception.printStackTrace(writer);

		StringTokenizer t = new StringTokenizer(bytes.toString(), "\n");
		// The first three are generated here.
		t.nextToken();
		t.nextToken();
		t.nextToken();
		r = new String();
		while(t.hasMoreTokens()) {
			r+=t.nextToken().substring(4) + ",";
		}

		return(r);
	}

	public LogEntry(String msg) {
		super();
		message = msg;
		timestamp = new Date();
		exception = new Exception("dummy");
	}

	public LogEntry() {
		super();
		message = null;
		timestamp = new Date();
		exception = new Exception("dummy");
	}

	public String toString() {
		String r;

		r="[" + timestamp + "] " + message + "\n{ " + getStack() + "\n}" ;
		return(r);
	}

	public String getMessage() {
		return(message);
	}
	public void setMessage(String msg) {
		message = msg;
	}

	public int getPriority() {
		return(priority);
	}
	public void setPriority(int p) {
		priority=p;
	}

	public Date getDate() {
		return(timestamp);
	}
	public void setDate(Date d) {
		timestamp = d;
	}
}
