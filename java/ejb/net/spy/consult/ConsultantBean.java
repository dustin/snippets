// Copyright (c) 2000  Dustin Sallings <dustin@spy.net>
//
// $Id: ConsultantBean.java,v 1.3 2000/01/22 11:46:39 dustin Exp $

package net.spy.consult;

import javax.ejb.*;
import java.lang.*;
import java.util.*;
import java.sql.*;
import java.rmi.*;
import javax.sql.*;
import javax.naming.*;

public class ConsultantBean implements EntityBean {
	protected ConsultantPK pk = null;

	public int id;
	public String fn=null;
	public String ln=null;
	public String ssn=null;
	public Address address=null;

	protected EntityContext context=null;
	protected DataSource dataSource=null;

	public ConsultantPK ejbCreate(String fn, String ln)
		throws CreateException, RemoteException {
		this.fn=fn;
		this.ln=ln;

		Connection con=null;
		PreparedStatement ps=null;

		try {
			// Get the next ID from the database.
			con=getConnection();
			ps=con.prepareStatement(
				"select nextval('consultant_consultant_id_seq')");
			ResultSet rs = ps.executeQuery();
			rs.next();
			pk=new ConsultantPK();
			pk.id=rs.getInt(1);
			id=pk.id;
		} catch(SQLException e) {
			throw new CreateException("Error creating Consultant:  " + e);
		} finally {
			try {
				if(ps!=null) ps.close();
				if(con!=null) con.close();
			} catch(SQLException e) {
				e.printStackTrace();
			}
		}
		return(pk);
	}

	public void ejbPostCreate(String fn, String ln) {
		// ConsultantPK pk=(ConsultantPK)context.getPrimaryKey();
		// id=pk.id;
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

	protected Connection getConnection()
		throws SQLException, RemoteException {
		Properties p=context.getEnvironment();
		Context initialContext = null;
		if(dataSource==null) {
			String dataSourceName = p.getProperty("datasource.name");
			try {
				initialContext = new InitialContext(p);
				dataSource = (DataSource)initialContext.lookup(dataSourceName);
			} catch(Exception e) {
				throw new RemoteException("Can't get a context:  " + e);
			}
		}
		return(dataSource.getConnection());
	}
}
