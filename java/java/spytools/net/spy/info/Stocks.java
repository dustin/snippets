// Copyright (c) 2000  Dustin Sallings <dustin@spy.net>
//
// $Id: Stocks.java,v 1.3 2000/06/16 21:46:06 dustin Exp $

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

	/**
	 * Get multiple stock quotes given an array of ticker symbols.
	 *
	 * @param symbols list of symbols to look up
	 *
	 * @exception Exception when there's a problem looking up symbols
	 */
	public Hashtable getQuotes(String symbols[]) throws Exception {
		quotes=new Hashtable();
		String sym="";

		for(int i=0; i<symbols.length; i++) {
			// We're only doing five at a time.
			if( (i%5) ==0) {
				if(sym.length() > 1) {
					// trim the last +
					sym=sym.substring(0, sym.length()-1);
					addQuotes(sym);
				}
				sym="";
			}
			sym+= symbols[i] + "+";
		}
		// If there are any leftover symbols, trim it and add them.
		if(sym.length() > 1) {
			sym=sym.substring(0, sym.length()-1);
			addQuotes(sym);
		}
		return(quotes);
	}

	protected void addQuotes(String sym) throws Exception {
		String url="http://quote.yahoo.com/d/quotes.csv?s="
			+ sym + "&f=sl1d1t1c1ohgv&e=.csv";
		HTTPFetch f = new HTTPFetch(url);
		Vector v = f.getLines();
		for(int j=0; j<v.size(); j++) {
			StockQuote sq=new StockQuote( (String)v.elementAt(j));
			quotes.put(sq.getSymbol(), sq);
		}
	}

	/**
	 * Test program, call it with ticker symbols on the commandline.
	 *
	 * @exception Exception when something goes terribly wrong.
	 */
	public static void main(String args[]) throws Exception {
		Stocks s=new Stocks();
		Hashtable q = s.getQuotes(args);
		System.out.println("Quotes:  " + q);
	}
}
