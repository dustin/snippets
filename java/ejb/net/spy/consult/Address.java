// Copyright (c) 2000  Dustin Sallings <dustin@spy.net>
//
// $Id: Address.java,v 1.1 2000/01/22 09:45:14 dustin Exp $

package net.spy.consult;

import java.lang.*;

public class Address extends Object {

	protected String _address[]=null;

	protected String _city=null;
	protected String _state=null;
	protected String _country=null;

	protected String _email=null;

	// Constructor
	public Address() {
		super();
	}

	public String[] address() {
		return(_address);
	}

	public void setAddress(String to[]) {
		_address=to;
	}

	public String city() {
		return(_city);
	}

	public void setCity(String to) {
		_city=to;
	}

	public String state() {
		return(_state);
	}

	public void setState(String to) {
		_state=to;
	}

	public String country() {
		return(_country);
	}

	public void setCountry(String to) {
		_country=to;
	}

	public String email() {
		return(_email);
	}

	public void setEmail(String to) {
		_email=to;
	}
}
