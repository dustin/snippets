// Copyright (c) 2000  Dustin Sallings <dustin@spy.net>
//
// $Id: Stocks.java,v 1.1 2000/03/20 06:20:42 dustin Exp $

package net.spy.info;

import java.util.*;

import net.spy.*;
import net.spy.net.*;

/**
 * Get stock quotes.
 */

public class Stocks extends Object {

	protected Hashtable quotes=null;

	/**
	 * Get a stock quote gettin' object.
	 */
	public Stocks() {
		super();
		quotes=new Hashtable();
	}

	/**
	 * Get a StockQuote for a given symbol.
	 *
	 * @param symbol which stock to look up.
	 *
	 * @exception Exception when a lookup fails.
	 */
	public StockQuote getQuote(String symbol) throws Exception {
		String url="http://quote.yahoo.com/d/quotes.csv?s="
			+ symbol + "&f=sl1d1t1c1ohgv&e=.csv";
		HTTPFetch f = new HTTPFetch(url);
		Vector v = f.getLines();
		StockQuote sq=new StockQuote((String)v.elementAt(0));
		return(sq);
	}

	public static void main(String args[]) throws Exception {
		Stocks s=new Stocks();
		StockQuote sq = s.getQuote(args[0]);
		System.out.println("Quote:  " + sq);
	}
}
