// -----------------------------------------------------------------------
//
// 2 W I R E ,   I N C .
//
// -----------------------------------------------------------------------
//
// (C) Copyright 1998-2001 2Wire, Inc.  All rights reserved.
//
// NOTICE:      All information contained herein or attendant hereto is,
//              and remains, the property of 2Wire, Inc.  Many of the
//              intellectual and technical concepts contained herein are
//              proprietary to 2Wire, Inc. and may be covered
//              by U.S. and Foreign Patents or Patents Pending, or are
//              protected as trade secrets.  Any dissemination of this
//              information or reproduction of this material is strictly
//              forbidden unless prior written permission is obtained
//              from 2Wire, Inc.
//
// -----------------------------------------------------------------------
//
// Perforce File Info
//
// $Id: $
// $Author: $
// $DateTime: $
//
// -----------------------------------------------------------------------

import java.net.*;
import java.util.*;

/**
 *
 */
public class ResolvingRunnable extends Object implements Runnable {

    private Hashtable results=null;
    private String hostname=null;

	/**
	 * Get an instance of ResolvingRunnable.
	 */
	public ResolvingRunnable(Hashtable results, String hostname) {
		super();

        this.results=results;
        this.hostname=hostname;
	}

    public void run() {
        try {
            if(!results.containsKey(hostname)) {
                InetAddress ia=InetAddress.getByName(hostname);
                String h=ia.getHostName();
                results.put(hostname, h);
            }
        } catch(UnknownHostException uhe) {
            // uhe.printStackTrace();
            results.put(hostname, hostname);
        }
    }

	/**
	 * Testing and what not.
	 */
	public static void main(String args[]) throws Exception {
        Hashtable h=new Hashtable();
        ResolvingRunnable rr=new ResolvingRunnable(h, args[0]);
        rr.run();

        System.out.println(h);
	}

}
