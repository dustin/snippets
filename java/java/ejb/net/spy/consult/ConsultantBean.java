// Copyright (c) 2000  Dustin Sallings <dustin@spy.net>
//
// $Id: ConsultantBean.java,v 1.1 2000/01/22 09:45:17 dustin Exp $

package net.spy.consult;

import javax.ejb.*;
import java.lang.*;

public class ConsultantBean implements EntityBean {
	public int id;
	public String fn=null;
	public String ln=null;
	public String ssn=null;
	public Address address=null;

	public EntityContext context=null;

	public void ejbCreate(int id, String fn, String ln) {
		this.id=id;
		this.fn=fn;
		this.ln=ln;
	}

	public void ejbPostCreate(int id, String fn, String ln) {
		ConsultantPK pk=(ConsultantPK)context.getPrimaryKey();
	}

	public void setEntityContext(EntityContext ctx) {
		context = ctx;
	}

	public void unsetEntityContext() {
		context = null;
	}

	public void ejbActivate() {}
	public void ejbPassivate() {}
	public void ejbLoad() {}
	public void ejbStore() {}
	public void ejbRemove() {}

	public String firstName() {
		return(fn);
	}

	public void setFirstName(String to) {
		fn=to;
	}

	public String lastName() {
		return(ln);
	}

	public void setLastName(String to) {
		ln=to;
	}

	public String SSN() {
		return(ssn);
	}

	public void setSSN(String to) {
		ssn=to;
	}

	public Address address() {
		return(address);
	}

	public void setAddress(Address to) {
		address=to;
	}
}
