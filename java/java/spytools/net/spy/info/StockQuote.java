// Copyright (c) 2000  Dustin Sallings <dustin@spy.net>
//
// $Id: StockQuote.java,v 1.3 2000/03/21 19:06:35 dustin Exp $

package net.spy.info;

import net.spy.*;

/**
 * An individual stock quote.
 */

public class StockQuote extends Object {

	protected String symbol=null;
	protected double price=0.0;
	protected String date=null;
	protected String time=null;
	protected double change=0.0;
	protected double open=0.0;
	protected double high=0.0;
	protected double low=0.0;
	protected int volume=0;

	/**
	 * Create a StockQuote object from a yahoo CSV output.
	 *
	 * @param csv line of csv from yahoo
	 *
	 * @throws Exception for an invalid cvs line
	 */
	public StockQuote(String csv) throws Exception {
		super();

		String f[]=SpyUtil.split(",", csv);

		// Process each individual part...
		int current=0;
		String tmp=null;

		// Symbol
		symbol=f[current]; current++;
		symbol=symbol.substring(1, symbol.length()-1);

		// price
		tmp=f[current];    current++;
		tmp+="d";
		price=Double.valueOf(tmp).doubleValue();

		// date
		date=f[current]; current++;
		date=date.substring(1, date.length()-1);

		// time
		time=f[current]; current++;
		time=time.substring(1, time.length()-1);

		// change
		tmp=f[current];    current++;
		tmp+="d";
		change=Double.valueOf(tmp).doubleValue();

		// open
		tmp=f[current];    current++;
		tmp+="d";
		open=Double.valueOf(tmp).doubleValue();

		// high
		tmp=f[current];    current++;
		tmp+="d";
		high=Double.valueOf(tmp).doubleValue();

		// low
		tmp=f[current];    current++;
		tmp+="d";
		low=Double.valueOf(tmp).doubleValue();

		// volume
		tmp=f[current];    current++;
		volume=Integer.parseInt(tmp);
	}

	/**
	 * produces a readable version of the StockQuote
	 */
	public String toString() {
		String ret="";
		ret=symbol + ": " + price + " ";
		if(change>0.0) {
			ret+="+";
		}
		ret+=change;
		return(ret);
	}

	/**
	 * gets the ticket symbol this object describes.
	 */
	public String getSymbol() {
		return(symbol);
	}

	/**
	 * gets the current stock price.
	 */
	public double getPrice() {
		return(price);
	}

	/**
	 * gets the date of the trade that set this price.
	 */
	public String getDate() {
		return(date);
	}

	/**
	 * gets the time of the trade that set this price.
	 */
	public String getTime() {
		return(time);
	}

	/**
	 * gets the amount of change since market opening.
	 */
	public double getChange() {
		return(change);
	}

	/**
	 * gets the opening price of the stock.
	 */
	public double getOpen() {
		return(open);
	}

	/**
	 * gets today's high value of the stock.
	 */
	public double getHigh() {
		return(high);
	}

	/**
	 * gets today's low value of the stock.
	 */
	public double getLow() {
		return(low);
	}

	/**
	 * gets the amount of volume of trade.
	 */
	public int getVolume() {
		return(volume);
	}
}
