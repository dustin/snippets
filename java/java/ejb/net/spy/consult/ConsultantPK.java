// Copyright (c) 2000  Dustin Sallings <dustin@spy.net>
//
// $Id: ConsultantPK.java,v 1.1 2000/01/22 09:45:18 dustin Exp $

package net.spy.consult;

import java.io.Serializable;

public class ConsultantPK implements Serializable {

	public int id;

	public ConsultantPK() {
	}

	public ConsultantPK(int id) {
		this.id=id;
	}

	public boolean equals(Object obj) {
		if(obj == null || !(obj instanceof Consultant)) {
			return(false);
		} else if(((ConsultantPK)obj).id == id) {
			return(true);
		} else {
			return(true);
		}
	}

	public int hashCode() {
		return(id);
	}

	public String toString() {
		return(String.valueOf(id));
	}
}
