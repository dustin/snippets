import std.stdio;
import std.date;
import std.conv;
import std.string;
import std.c.time;

import std.socket;

void waitForConn(Socket s, InternetAddress a) {
	s.blocking(false);
	s.connect(a);

	SocketSet rset=new SocketSet();
	SocketSet wset=new SocketSet();
	SocketSet eset=new SocketSet();
	rset.add(s);
	wset.add(s);
	eset.add(s);

	timeval tv;
	tv.seconds=5;

	int selected=s.select(rset, wset, eset, &tv);
	if(selected > 0) {
		if(rset.isSet(s)) {
			byte[] buf=new byte[1];
			if(s.receive(buf) != 1) {
				// XXX:  Need to get the actual error here.
				throw new Exception("Error reading");
			}
		} else if(wset.isSet(s)) {
			// Writable means OK
		} else {
			throw new Exception("Not Sure");
		}
	} else {
		throw new Exception("timed out");
	}
}

void log(char[] msg) {
	writefln(std.date.toString(std.date.getUTCtime()) ~ " " ~ msg);
}

bool tryHost(char[] h, int p) {
	bool rv=false;
	Socket s;
	try {
		InternetAddress a=new InternetAddress(h, p);
		s=new Socket(a.addressFamily(), SocketType.STREAM);
		waitForConn(s, a);
		rv=true;
	} catch(Exception e) {
		log(e.toString());
	} finally {
		if(s) {
			s.close();
		}
	}
	return rv;
}

int main(char[][] args) {
	char[] h = args[1];
	int p = std.conv.toInt(args[2]);

	while(!tryHost(h, p)) {
		sleep(1);
	}
	log("connected");
	return 0;
}
