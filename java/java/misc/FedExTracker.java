// Copyright (c) 2000  Dustin Sallings <dustin@spy.net>
//
// $Id: FedExTracker.java,v 1.1 2000/06/15 22:14:06 dustin Exp $

import java.sql.*;
import java.util.*;

import net.spy.net.SNPP;
import net.spy.info.*;

public class FedExTracker extends PackageTracker {

	public FedExTracker() {
		super(1);
	}

	protected Info getInfoObj(String id) {
		return(new FedEx(id));
	}

	public static void main(String args[]) throws ClassNotFoundException {
		// Load our driver up-front
		Class.forName("org.postgresql.Driver");
		FedExTracker t = new FedExTracker();
		t.run();
	}
}
