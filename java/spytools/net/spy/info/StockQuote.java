// Copyright (c) 2000  Dustin Sallings <dustin@spy.net>
//
// $Id: StockQuote.java,v 1.7 2002/07/10 04:25:35 dustin Exp $

package net.spy.info;

import net.spy.SpyUtil;

/**
 * An individual stock quote.
 */

public class StockQuote extends Object {

	private String symbol=null;
	private double price=0.0;
	private String date=null;
	private String time=null;
	private double change=0.0;
	private double open=0.0;
	private double high=0.0;
	private double low=0.0;
	private int volume=0;

	private String error=null;

	/**
	 * Create a StockQuote object from a yahoo CSV output.
	 *
	 * @param csv line of csv from yahoo
	 *
	 * @exception Exception for an invalid cvs line
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

		// Figure out real quick whether the stock is valid or not...
		if(f[4].equals("N/A")) {
			error="Could not stock info for "
				+ symbol + ", sure this symbol is valid?";
			return;
		}

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
		if(error==null) {
			ret=symbol + ": " + price + " ";
			if(change>0.0) {
				ret+="+";
			}
			ret+=change;
		} else {
			ret=error;
		}
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
