import java.io.*;
import java.net.*;
import java.util.*;

public class Server {
	public static Stack urls;

	public static void main(String args[]) throws Exception {
		urls = new Stack();
		int i;
		BufferedWriter log_file =
			new BufferedWriter(new FileWriter("server.log", false), 1);

		// Do the threads.
		Thread t = new Flusher(log_file);
		t.start();
		for(i=0; i<23; i++) {
			t=new Getit(urls, log_file);
			t.setDaemon(true);
			t.start();
		}

		startServer(Integer.parseInt(args[0]));
	}

	public static void startServer(int port) {
		ServerSocket ss=null;
		try {
			ss = new ServerSocket(port);
		} catch (Exception e) {
			System.err.println("Got exception:  " + e);
		}
		for(;;) {
			Socket client=null;
			BufferedReader in=null;
			PrintWriter out=null;
			try {
				client = ss.accept();
				in = new BufferedReader(
					new InputStreamReader(client.getInputStream()));
				out = new PrintWriter(
					new OutputStreamWriter(client.getOutputStream()));

				processRequest(in, out);
			} catch(Exception e2) {
				System.err.println("Got exception:  " + e2);
			} finally {
				try { client.close();
				} catch(Exception whatever) {
				}
				try { in.close();
				} catch(Exception whatever) {
				}
				try { out.close();
				} catch(Exception whatever) {
				}
			}
		}
	}

	public static void processRequest(BufferedReader in, PrintWriter out)
		throws Exception {
		String request = in.readLine();
		String method=null, path=null, version=null;
		int i;

		StringTokenizer st = new StringTokenizer(request, " ");
		method=st.nextToken();
		path=st.nextToken();
		version=st.nextToken();

		out.println("HTTP/1.0 200 OK");
		out.println("Content-type: text/plain");
		out.println();

		if(path.startsWith("/http")) {
			path = path.substring(1);
		}

		// Push it into the stack.
		out.println("Adding " + path + " to request list");
		urls.push(path);

		out.flush();
	}
}
