import java.io.*;
import java.net.*;

class Readthread implements Runnable
{
DataInputStream din;
	public void run()
	{
		String blah=new String();

		System.out.println("Little thread has started");
		while(true)
		{
			System.out.println("Little man loop");
			try
			{
				blah=din.readLine();
			}
			catch (IOException e)
			{
				System.out.println("Error reading from socket");
				return;
			}
			if(blah!=null)
				System.out.println(blah);
		}
	}

	public Readthread(Socket s)
	{
		System.out.println("Constructing");
		try
		{
			din=new DataInputStream(s.getInputStream());
		}
		catch (IOException e)
		{
			System.out.println("Can't open read socket");
			return;
		}
		System.out.println("End Construction");
	}
}

class GeTit
{
static Socket socket;
	public static void main(String args[])
	{
		int port=80;
		boolean going=true;
		DataInputStream Blah;
		PrintStream prout;
		String blah=new String();
		Readthread t;

		System.out.println("Attempting to connect to "+args[0]);
		System.out.flush();
		try
		{
			socket=new Socket(args[0], port);
		}
		catch (IOException e)
		{
			System.out.println("Error opening socket");
			return;
		}

		try
		{
			prout=new PrintStream(socket.getOutputStream());

			Blah=new DataInputStream(System.in);
		}
		catch (IOException e)
		{
			System.out.println("Error opening streams");
			return;
		}

		prout.println("GET / HTTP/1.0\n\n");
		prout.flush();

		t=new Readthread(socket);
		new Thread(t).start();
		System.out.println("Regular program is here too");


/*
		while(going)
		{
			try
			{
				blah=din.readLine();
			}
			catch (IOException e)
			{
				System.out.println("Error reading from socket");
				return;
			}
			if(blah == null)
				going=false;
			else
				System.out.println(blah);
		}
*/

		while(true)
		{
			System.out.println("Big man loop");
			try
			{
				blah=Blah.readLine();
				prout.println(blah);
				prout.flush();
			}
			catch (IOException e)
			{
				System.out.println("Error writing to socket.");
				return;
			}
		}
	}
}
