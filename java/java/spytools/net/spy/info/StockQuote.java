// Copyright (c) 2000  Dustin Sallings <dustin@spy.net>
//
// $Id: StockQuote.java,v 1.1 2000/03/20 06:20:41 dustin Exp $

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
	protected double ask=0.0;
	protected double bid=0.0;
	protected double something=0.0;
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

		// ask
		tmp=f[current];    current++;
		tmp+="d";
		ask=Double.valueOf(tmp).doubleValue();

		// bid
		tmp=f[current];    current++;
		tmp+="d";
		bid=Double.valueOf(tmp).doubleValue();

		// something
		tmp=f[current];    current++;
		tmp+="d";
		something=Double.valueOf(tmp).doubleValue();

		// volume
		tmp=f[current];    current++;
		volume=Integer.parseInt(tmp);
	}

	/**
	 * produces a readable version of the StockQuote
	 */
	public String toString() {
		String ret="";
		ret=symbol + ":  " + price;
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
	 * gets the ask price of the stock.
	 */
	public double getAsk() {
		return(ask);
	}

	/**
	 * gets the bid price of the stock.
	 */
	public double getBid() {
		return(bid);
	}

	/**
	 * gets the amount of volume of trade.
	 */
	public int getVolume() {
		return(volume);
	}
}
