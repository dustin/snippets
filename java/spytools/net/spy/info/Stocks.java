// Copyright (c) 2000  Dustin Sallings <dustin@spy.net>
//
// $Id: Stocks.java,v 1.6 2002/08/16 07:27:04 dustin Exp $

package net.spy.info;

import java.util.Map;
import java.util.HashMap;
import java.util.List;
import java.util.Iterator;

import net.spy.net.HTTPFetch;

/**
 * Get stock quotes.
 */

public class Stocks extends Object {

	private HashMap quotes=null;

	/**
	 * Get a stock quote gettin' object.
	 */
	public Stocks() {
		super();
		quotes=new HashMap();
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
		List l = f.getLines();
		StockQuote sq=new StockQuote((String)l.get(0));
		return(sq);
	}

	/**
	 * Get multiple stock quotes given an array of ticker symbols.
	 *
	 * @param symbols list of symbols to look up
	 *
	 * @exception Exception when there's a problem looking up symbols
	 */
	public Map getQuotes(String symbols[]) throws Exception {
		quotes=new HashMap();
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

	private void addQuotes(String sym) throws Exception {
		String url="http://quote.yahoo.com/d/quotes.csv?s="
			+ sym + "&f=sl1d1t1c1ohgv&e=.csv";
		HTTPFetch f = new HTTPFetch(url);
		List l = f.getLines();
		for(Iterator i=l.iterator(); i.hasNext();) {
			StockQuote sq=new StockQuote( (String)i.next());
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
		Map q = s.getQuotes(args);
		System.out.println("Quotes:  " + q);
	}
}
