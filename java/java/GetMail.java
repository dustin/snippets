
/**
 * Copyright (c)1996 Harm Verbeek, All Rights Reserved.
 *
 * Description: Retrieve E-mail (using POP) from within Java applet,
 *              won't work with applets in WWW-browser
 *              (security violation !!!).
 *
 * Usage Notes: Java 2.0 Beta
 *
 * Date       : 01/23/96
 *
 *
 * Permission to use, copy, modify, and distribute this software
 * and its documentation for NON-COMMERCIAL or COMMERCIAL purposes and
 * without fee is hereby granted.
 *
 * YOUR COMPANY MAKES NO REPRESENTATIONS OR WARRANTIES ABOUT THE SUITABILITY OF
 * THE SOFTWARE, EITHER EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED
 * TO THE IMPLIED WARRANTIES OF MERCHANTABILITY, FITNESS FOR A
 * PARTICULAR PURPOSE, OR NON-INFRINGEMENT. YOUR COMPANY SHALL NOT BE LIABLE FOR
 * ANY DAMAGES SUFFERED BY LICENSEE AS A RESULT OF USING, MODIFYING OR
 * DISTRIBUTING THIS SOFTWARE OR ITS DERIVATIVES.
 *
 * THIS SOFTWARE IS NOT DESIGNED OR INTENDED FOR USE OR RESALE AS ON-LINE
 * CONTROL EQUIPMENT IN HAZARDOUS ENVIRONMENTS REQUIRING FAIL-SAFE
 * PERFORMANCE, SUCH AS IN THE OPERATION OF NUCLEAR FACILITIES, AIRCRAFT
 * NAVIGATION OR COMMUNICATION SYSTEMS, AIR TRAFFIC CONTROL, DIRECT LIFE
 * SUPPORT MACHINES, OR WEAPONS SYSTEMS, IN WHICH THE FAILURE OF THE
 * SOFTWARE COULD LEAD DIRECTLY TO DEATH, PERSONAL INJURY, OR SEVERE
 * PHYSICAL OR ENVIRONMENTAL DAMAGE ("HIGH RISK ACTIVITIES").  YOUR COMPANY
 * SPECIFICALLY DISCLAIMS ANY EXPRESS OR IMPLIED WARRANTY OF FITNESS FOR
 * HIGH RISK ACTIVITIES.
 *
**/


import java.io.*;
import java.net.*;
import java.lang.*;
import java.applet.*;


public class GetMail extends Applet
{
  static Socket          socket;
  static InputStream     in;
  static OutputStream    out;
  static DataInputStream din;
  static PrintStream     prout;



  static int getNumberofMessagesfromString(String m)
  {
    int i,j;
    String s;

    i = m.indexOf(" mes");
    s = m.substring(4, i);

    i = s.lastIndexOf(" ");
    s = s.substring(i+1);

    i = Integer.parseInt(s);

    return i;
  }


  public static void main (String args[])
  {
    int             POPport  = 110;
    String          incoming = new String();

    String          MailHost = "dragon.axs.net";
    String          user     = "dustin";
    String          password = "biAtch13";
    String          USER     = "USER " + user;
    String          PASSWORD = "PASS " + password;
    String          STAT     = "STAT";
    String          RETR     = "RETR ";
    String          DELE     = "DELE ";
    String          QUIT     = "QUIT";

    /* send this command to the server if you
       want to keep connected while you're busy
       doing something else
    */
    String          DONTQUIT = "NOOP";

    /* mailhost returns string that starts with
       "+OK" to indicate everything went OK.
    */
    String          OK       = "+OK";

    /* mailhost returns string that starts with
       "-ERR" to indicate something went wrong.
    */
    String          Err      = "-ERR";


/* connect to the mail server */
    System.out.println("Connecting to " + MailHost + "...");
    System.out.flush();

    try {
      socket = new Socket(MailHost, POPport);
    } catch (IOException e) {
      System.out.println("Error opening socket.");
      return;
    }

    try {
      in    = socket.getInputStream();
      din   = new DataInputStream(in);

      out   = socket.getOutputStream();
      prout = new PrintStream(out);
    }
    catch (IOException e) {
      System.out.println("Error opening inputstream.");
      return;
    }

 /* let server know who's calling... */
    prout.println(USER);
    prout.flush();
    System.out.println("Sent: " + USER);

    try {
      incoming = din.readLine();
    }
    catch (IOException e) {
      System.out.println("Error reading from socket.");
      return;
    }
    System.out.println("Received: " + incoming);

/* give server your password... */
    prout.println(PASSWORD);
    prout.flush();
    System.out.println("Sent: " + PASSWORD);

    try {
      incoming = din.readLine();
    }
    catch (IOException e) {
      System.out.println("Error reading from socket.");
      return;
    }
    System.out.println("Received: " + incoming);


/* ask server if there's mail for you... */
    prout.println(STAT);
    prout.flush();
    System.out.println("Sent: " + STAT);

    try {
      incoming = din.readLine();
    }
    catch (IOException e) {
      System.out.println("Error reading from socket.");
      return;
    }
    System.out.println("Received: " + incoming);

    int n = getNumberofMessagesfromString(incoming);

/* retrieve mail... */
    if (n>0) {
      for (int i=1; i<=n; i++) {
        prout.println(RETR + i);
        prout.flush();
        System.out.println("Sent: " + RETR + i);

        try {
          incoming = din.readLine();

          while (!incoming.equals(".")) {
            System.out.println("Received: " + incoming);
            incoming = din.readLine();
          }
        }
        catch (IOException e) {
          System.out.println("Error reading from socket.");
          return;
        }

        /* delete retrieved mail from server... */
        prout.println(DELE + i);
        prout.flush();
        System.out.println("Sent: " + DELE + i);

        try {
          incoming = din.readLine();
        }
        catch (IOException e) {
          System.out.println("Error reading from socket.");
          return;
        }
        System.out.println("Received: " + incoming);
      }
    }

/* ready, let's quit... */
    prout.println(QUIT);
    prout.flush();
    System.out.println("Sent: " + QUIT);

    try {
      incoming = din.readLine();
    }
    catch (IOException e) {
      System.out.println("Error reading from socket.");
      return;
    }
    System.out.println("Received: " + incoming);

/* we're done, disconnect from server */
   System.out.print("Disconnecting...");
    try {
      socket.close();
    }
    catch (IOException e) {
      System.out.println("Error closing socket.");
    }

    System.out.println("done.");
  }

}


