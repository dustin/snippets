// Copyright (c) 2001  SPY internetworking <dustin@spy.net>
//
// $Id: DBSP.java,v 1.13 2002/08/26 05:39:58 dustin Exp $

package net.spy.db;

import java.lang.reflect.Constructor;

import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.ResultSetMetaData;
import java.sql.SQLException;
import java.sql.SQLWarning;
import java.sql.Time;
import java.sql.Timestamp;
import java.sql.Types;

import java.math.BigDecimal;

import java.util.Collections;
import java.util.Collection;
import java.util.Iterator;
import java.util.HashMap;
import java.util.ArrayList;
import java.util.List;

import net.spy.SpyConfig;

/**
 * Super class for all stored procedure calls.
 */
public abstract class DBSP extends SpyCacheDB {

	// The arguments themselves
	private HashMap args=null;
	// The data type of this argument
	private HashMap types=null;

	/**
	 * Required fields and their types.
	 */
	private HashMap required=null;
	/**
	 * Required fields in order.
	 */
	private ArrayList requiredInorder=null;
	/**
	 * Optional fields and their types.
	 */
	private HashMap optional=null;
	/**
	 * Optional fields in order.
	 */
	private ArrayList optionalInorder=null;

	// SP name
	private String spname=null;

	// Caching info
	private long cachetime=0;

	private boolean debug=false;

	// The query
	private String query=null;

	// My prepared statement
	private PreparedStatement pst=null;

	/**
	 * Get a new DBSP object with a given config.
	 */
	public DBSP(SpyConfig conf) throws SQLException {
		super(conf);
		initsp();
	}

	/**
	 * Get a new DBSP object using the given Connection
	 */
	public DBSP(Connection conn) throws SQLException {
		super(conn);
		initsp();
	}

	// Initialize hashtables
	private void initsp() {
		this.args=new HashMap();
		this.types=new HashMap();
		this.required=new HashMap();
		this.optional=new HashMap();
		this.requiredInorder=new ArrayList();
		this.optionalInorder=new ArrayList();
	}

	/**
	 * Execute the query.
	 */
	public ResultSet executeQuery() throws SQLException {
		prepare();

		ResultSet rs=null;
		rs=pst.executeQuery();

		if (debug) {
			System.err.print("Returned: ");
			ResultSetMetaData rsmd=rs.getMetaData();
			String cols="";
			for (int x=1; x<=rsmd.getColumnCount(); x++) {
				if (x>1) {
					cols+=", ";
				}
				cols+=rsmd.getColumnName(x)+"="+rsmd.getColumnTypeName(x);
			}
			System.err.println(cols);
		}

		return(rs);
	}

	/**
	 * Execute a query for update only.
	 */
	public int executeUpdate() throws SQLException  {
		int rv=0;
		prepare();
		rv=pst.executeUpdate();
		return(rv);
	}

	/**
	 * Get the next result set.
	 *
	 * @return null if there are no more result sets.
	 */
	public ResultSet nextResults() throws SQLException  {
		ResultSet rs=null;

		// Skip over the updates
		pst.getUpdateCount();

		if(pst.getMoreResults()) {
			rs=pst.getResultSet();
		}

		return(rs);
	}

	/**
	 * Get the warnings.
	 */
	public SQLWarning getWarnings() throws SQLException {
		return(pst.getWarnings());
	}

	/**
	 * Set the number of seconds the results of this SP will be valid.
	 * This option uses {@link SpyCacheDB#prepareStatement(String,long)}
	 * instead of the native driver's version.
	 *
	 * @param time time (in seconds) to keep the results around
	 */
	public void setCacheTime(long time) {
		this.cachetime=time;
	}

	/**
	 * Define a field to be required.
	 *
	 * @param name the name of the field
	 * @param type the type
	 * @throws SQLException if the type has already been added
	 * @see java.sql.Types
	 */
	protected void setRequired(String name, int type) throws SQLException {
		Object tmp=required.put(name, new Integer(type));
		if(tmp!=null) {
			throw new SQLException("required parameter ``"
				+ name + "'' already provided.");
		}
		requiredInorder.add(name);
	}

	/**
	 * Define a field to be optional.
	 *
	 * @param name the name of the field
	 * @param type the type
	 * @throws SQLException if the type has already been added
	 * @see java.sql.Types
	 */
	protected void setOptional(String name, int type) throws SQLException {
		Object tmp=optional.put(name, new Integer(type));
		if(tmp!=null) {
			throw new SQLException("required parameter ``"
				+ name + "'' already provided.");
		}
		optionalInorder.add(name);
	}

	/**
	 * Set the named argument to the value contained in the given object
	 * for the given type.
	 *
	 * @param which which variable to set
	 * @param what what value to set it to
	 * @param type type of variable this is
	 */
	protected void setArg(String which, Object what, int type)
			throws SQLException {
		if (what!=null) {
			args.put(which, what);
			types.put(which, new Integer(type));
		} else { // it was a null object
			DBNull n=new DBNull(type);
			args.put(which, n);
			types.put(which, new Integer(Types.NULL));
		}
	}

	/**
	 * Set the stored procedure to the given value.
	 */
	protected void setSPName(String to) {
		this.spname=to;
	}

	/**
	 * Verify that the arguments, as given, are acceptable.
	 */
	protected void checkArgs() throws SQLException {

		if(debug) {
			System.err.println("Checking");
			System.err.println("Required:  "+ required);
			System.err.println("Optional:  "+ optional);
			System.err.println("Args:  "+ args);
		}

		// First, verify all of the arguments we have are correctly typed.
		for(Iterator e=args.keySet().iterator(); e.hasNext(); ) {
			String key=(String)e.next();

			boolean checked=false;

			// Check required type
			Integer typei=(Integer)required.get(key);
			if(typei!=null) {
				Integer mytype=(Integer)types.get(key);

				// Check if the type is set right, or is null
				if(! mytype.equals(typei)
					&& ! mytype.equals(new Integer(Types.NULL))) {
					throw new SQLException("Invalid type for arg " + key
						+ " type was "
						+ mytype + " ("
							+ TypeNames.getTypeName(mytype.intValue()) + ")"
						+ " should be "
						+ typei
							+ " (" + TypeNames.getTypeName(typei.intValue())
							+ ")"
					);
				}
				checked=true;
			}

			// Check optional type
			typei=(Integer)optional.get(key);
			if(typei!=null) {
				Integer mytype=(Integer)types.get(key);
				if(! typei.equals(mytype)) {
					throw new SQLException("Invalid type for arg " + key);
				}
				checked=true;
			}

			// If it's not required or optional, it's invalid.
			if(!checked) {
				throw new SQLException("Invalid argument:  " + key);
			}
		}

		// Next, verify all of the required arguments are there.
		for(Iterator e=required.keySet().iterator(); e.hasNext(); ) {
			String key=(String)e.next();

			if(args.get(key) == null) {
				throw new SQLException("Required argument "
					+ key + " missing.");
			}
		}

		// Check complete.  :)
	}

	/**
	 * Prepare the statement for execution.
	 */
	protected void prepare() throws SQLException {

		// Make sure all the arguments are there.
		checkArgs();

		// Get ready to build our query.
		StringBuffer querySb=new StringBuffer(256);
		querySb.append("exec ");
		querySb.append(spname);
		querySb.append(" ");

		// Get the keys in a vector so we can make sure they come out in
		// the right order.
		ArrayList v=new ArrayList();
		for(Iterator e=args.keySet().iterator(); e.hasNext(); ) {
			String param=(String)e.next();
			v.add(param);

			querySb.append("\t@");
			querySb.append(param);
			querySb.append("=?,\n");
		}

		// Remove the last comma if we had params
		if(v.size()>0) {
			querySb=new StringBuffer(querySb.toString().substring(0,
										querySb.length()-2));
		}
		String query=querySb.toString().trim();

		// Get a prepared statement, varies whether it's cachable or not.
		if(cachetime>0) {
			pst=prepareStatement(query, cachetime);
		} else {
			pst=prepareStatement(query);
		}

		// Fill in the arguments.
		setQuery(query);
		applyArgs(v);
	}

	/**
	 * Set the value of debug for this instance.
	 */
	protected void setDebug(boolean db) {
		this.debug=db;
	}

	/**
	 * Set the SQL query to call
	 */
	protected void setQuery(String query) {
		this.query=query;

		if(debug) {
			System.err.println("DBSP Query:  " + query);
			System.err.println("\t-");

			for(Iterator e=requiredInorder.iterator(); e.hasNext(); ) {
				String key=(String)e.next();
				String val=args.get(key).toString();
				System.err.println("\t" + key + "=" + val);
			}

			for(Iterator e=optionalInorder.iterator(); e.hasNext(); ) {
				String key=(String)e.next();
				Object o=args.get(key);
				if(o!=null) {
					String val=o.toString();
					System.err.println("\t" + key + "=" + val);
				}
			}

		}
	}

	/**
	 * Fill in the arguments (with types) for the given list of parameters.
	 *
	 * @param query the query we'll be calling
	 * @param v the list of named parameters we need to add, in order
	 */
	protected void applyArgs(Collection v) throws SQLException {

		// Get the statement
		if(cachetime>0) {
			pst=prepareStatement(query, cachetime);
		} else {
			pst=prepareStatement(query);
		}

		// Use this iterator for the now positional arguments
		int i=1;
		for(Iterator e=v.iterator(); e.hasNext(); ) {
			String key=(String)e.next();
			Object o=args.get(key);

			Integer typeInt=(Integer)types.get(key);

			// Check the type
			if(typeInt == null) {
				throw new SQLException ("No argument given for " + key);
			}

			// Get it as an int so we can switch it
			int type=typeInt.intValue();

			try {
				switch(type) {
					case Types.BIT:
						pst.setBoolean(i, ((Boolean)o).booleanValue());
						break;
					case Types.DATE:
						pst.setDate(i, (java.sql.Date)o);
						break;
					case Types.DOUBLE:
						pst.setDouble(i, ((Double)o).doubleValue());
						break;
					case Types.FLOAT:
						pst.setFloat(i, ((Float)o).floatValue());
						break;
					case Types.INTEGER:
						pst.setInt(i, ((Integer)o).intValue());
						break;
					case Types.BIGINT:
						pst.setLong(i, ((Long)o).longValue());
						break;
					case Types.NUMERIC:
					case Types.DECIMAL:
						BigDecimal bd=((BigDecimal)o).setScale(4,
													BigDecimal.ROUND_HALF_UP);
						pst.setBigDecimal(i, bd);
						break;
					case Types.SMALLINT:
					case Types.TINYINT:
						pst.setShort(i, (short)((Integer)o).intValue());
						break;
					case Types.NULL:
						pst.setNull(i, ((DBNull)o).getType());
						break;
					case Types.OTHER:
						pst.setObject(i, o);
						break;
					case Types.VARCHAR:
						pst.setString(i, (String)o);
						break;
					case Types.TIME:
						pst.setTime(i, (Time)o);
						break;
					case Types.TIMESTAMP:
						pst.setTimestamp(i, (Timestamp)o);
						break;
					default:
						throw new SQLException("Whoops, type "
							+ TypeNames.getTypeName(type) + "(" + type + ")"
							+ " seems to have been overlooked.");
				}
			} catch(SQLException se) {
				throw se;
			} catch (Exception applyException) {
				applyException.printStackTrace();
				String msg="Problem setting " + key
					+ " in prepared statement for type "
					+ TypeNames.getTypeName(type) + " "
					+ o.toString() + " : " + applyException;
				throw new SQLException (msg);
			}

			i++;

		}
	}

	/**
	 * Set field <i>which</i> to the value a1 of the type java.math.BigDecimal
	 *
	 * @param which which field to set
	 * @param a1 the value to set
	 *
	 * @exception SQLException if there's an error setting this argument.
	 */
	public void set(String which,java.math.BigDecimal a1)
		throws SQLException {
		setArg(which, a1, Types.DECIMAL);
	}

	/**
	 * Set field <i>which</i> to the value a1 of the type boolean
	 *
	 * @param which which field to set
	 * @param a1 the value to set
	 *
	 * @exception SQLException if there's an error setting this argument.
	 */
	public void set(String which,boolean a1)
		throws SQLException {
		setArg(which, new Boolean(a1), Types.BIT);
	}

	/**
	 * Set field <i>which</i> to the value a1 of the type java.sql.Date
	 *
	 * @param which which field to set
	 * @param a1 the value to set
	 *
	 * @exception SQLException if there's an error setting this argument.
	 */
	public void set(String which,java.sql.Date a1)
		throws SQLException {
		setArg(which, a1, Types.DATE);
	}

	/**
	 * Set field <i>which</i> to the value a1 of the type double
	 *
	 * @param which which field to set
	 * @param a1 the value to set
	 *
	 * @exception SQLException if there's an error setting this argument.
	 */
	public void set(String which,double a1)
		throws SQLException {
		setArg(which, new Double(a1), Types.DOUBLE);
	}

	/**
	 * Set field <i>which</i> to the value a1 of the type float
	 *
	 * @param which which field to set
	 * @param a1 the value to set
	 *
	 * @exception SQLException if there's an error setting this argument.
	 */
	public void set(String which,float a1)
		throws SQLException {
		setArg(which, new Float(a1), Types.FLOAT);
	}

	/**
	 * Set field <i>which</i> to the value a1 of the type int
	 *
	 * @param which which field to set
	 * @param a1 the value to set
	 *
	 * @exception SQLException if there's an error setting this argument.
	 */
	public void set(String which,int a1)
		throws SQLException {
		setArg(which, new Integer(a1), Types.INTEGER);
	}

	/**
	 * Set field <i>which</i> to the value a1 of the type long
	 *
	 * @param which which field to set
	 * @param a1 the value to set
	 *
	 * @exception SQLException if there's an error setting this argument.
	 */
	public void set(String which,long a1)
		throws SQLException {
		setArg(which, new Long(a1), Types.BIGINT);
	}

	/**
	 * Set field <i>which</i> to a null of the given type.
	 *
	 * @param which which field to set
	 * @param a1 the value to set
	 *
	 * @exception SQLException if there's an error setting this argument.
	 */
	public void setNull(String which,int a1)
		throws SQLException {
		// This one works a bit different because we have to store the
		// original type
		setArg(which, new Integer(a1), Types.NULL);
	}

	/**
	 * Set field <i>which</i> to the value a1 of the type java.lang.Object
	 *
	 * @param which which field to set
	 * @param a1 the value to set
	 *
	 * @exception SQLException if there's an error setting this argument.
	 */
	public void set(String which,java.lang.Object a1)
		throws SQLException {
		setArg(which, a1, Types.OTHER);
	}

	/**
	 * Set field <i>which</i> to the value a1 of the type short
	 *
	 * @param which which field to set
	 * @param a1 the value to set
	 *
	 * @exception SQLException if there's an error setting this argument.
	 */
	public void setSmallInt(String which,short a1)
		throws SQLException {
		setArg(which, new Integer(a1), Types.SMALLINT);
	}

	/**
	 * Set field <i>which</i> to the value a1 of the type short
	 *
	 * @param which which field to set
	 * @param a1 the value to set
	 *
	 * @exception SQLException if there's an error setting this argument.
	 */
	public void set(String which,short a1)
		throws SQLException {
		setArg(which, new Integer(a1), Types.TINYINT);
	}

	/**
	 * Set field <i>which</i> to the value a1 of the type java.lang.String
	 *
	 * @param which which field to set
	 * @param a1 the value to set
	 *
	 * @exception SQLException if there's an error setting this argument.
	 */
	public void set(String which,java.lang.String a1)
		throws SQLException {
		setArg(which, a1, Types.VARCHAR);
	}

	/**
	 * Set field <i>which</i> to the value a1 of the type Time
	 *
	 * @param which which field to set
	 * @param a1 the value to set
	 *
	 * @exception SQLException if there's an error setting this argument.
	 */
	public void set(String which,Time a1)
		throws SQLException {
		setArg(which, a1, Types.TIME);
	}

	/**
	 * Set field <i>which</i> to the value a1 of the type Timestamp
	 *
	 * @param which which field to set
	 * @param a1 the value to set
	 *
	 * @exception SQLException if there's an error setting this argument.
	 */
	public void set(String which,Timestamp a1)
		throws SQLException {
		setArg(which, a1, Types.TIMESTAMP);
	}

	/**
	 * Get the names of all required arguments.
	 *
	 * @return a List of Strings representing the required arguments in the
	 * order in which they will be passed into the query
	 */
	public List getRequiredArgs() {
		return(Collections.unmodifiableList(requiredInorder));
	}

	/**
	 * Get the names of all optional arguments.
	 *
	 * @return a List of Strings representing the optional arguments in the
	 * order in which they will be passed into the query
	 */
	public List getOptionalArgs() {
		return(Collections.unmodifiableList(optionalInorder));
	}

	/**
	 * Get the type (java.sql.Types) of the given variable.
	 *
	 * @return the type, or -1 if there's no such variable
	 */
	public int getType(String var) {
		int rv=-1;

		Integer i=(Integer)required.get(var);
		if(i!=null) {
			rv=i.intValue();
		} else {
			i=(Integer)optional.get(var);
			if(i!=null) {
				rv=i.intValue();
			}
		}

		return(rv);
	}

	/**
	 * Set a field in the DBSP after coercing the String value to the
	 * required value for the given field.  This should <i>rarely</i> be
	 * used, but is useful for accessing DBSPs through web forms.
	 *
	 * @param var the field to set
	 * @param value the String representation of the value to set
	 * @throws SQLException if there's a problem with coersion
	 */
	public void setCoerced(String var, String value) throws SQLException {

		int type=getType(var);
		if(value == null) {
			// if the value is null, send in a null
			DBNull n=new DBNull(type);
		} else {
			// Value is not null, parse it and call the proper set method
			switch(type) {
				case Types.BIT:
					set(var, Boolean.valueOf(value).booleanValue());
					break;
				case Types.DOUBLE:
					set(var, new Double(value).doubleValue());
					break;
				case Types.FLOAT:
					set(var, new Float(value).floatValue());
					break;
				case Types.INTEGER:
					set(var, Integer.parseInt(value));
					break;
				case Types.BIGINT:
					set(var, Long.parseLong(value));
					break;
				case Types.NUMERIC:
				case Types.DECIMAL:
					set(var, new BigDecimal(value));
					break;
				case Types.TINYINT:
					set(var, (short)Integer.parseInt(value));
					break;
				case Types.OTHER:
					set(var, value);
					break;
				case Types.VARCHAR:
					set(var, value);
					break;
				case Types.DATE:
				case Types.TIME:
				case Types.TIMESTAMP:
					throw new SQLException("Date types not currently handled");
				default:
					throw new SQLException(
						"No known type for " + var + ", you sure it's valid?.");
			}
		}
	}

	/**
	 * Commandline test for SPs that return result sets.  Invoked thusly:
	 *
	 * <p/>
	 *
	 * java net.spy.db.DBSP net.spy.db.sp.SPClassName configpath
	 * key value [...]
	 */
	public static void main(String args[]) throws Exception {
		// Get a Config
		SpyConfig dbconfig=new SpyConfig(new java.io.File(args[1]));

		// Now, we have sit back and reflect on a way to instantiate this
		// thing.
		// Figure out what the argument types are
		Class argtypes[]={dbconfig.getClass()};
		// Make the actual argument list
		Object dargs[]={dbconfig};
		Class c=Class.forName(args[0]);
		Constructor cons=c.getConstructor(argtypes);
		DBSP dbsp=(DBSP)cons.newInstance(dargs);

		// Set the args
		for(int i=2; i<args.length; i+=2) {
			dbsp.setCoerced(args[i], args[i+1]);
		}

		ResultSet rs=dbsp.executeQuery();
		System.out.println("ResultSet type is " + rs.getClass().getName());

		int rsi=1;
		while(rs!=null) {
			int rowi=1;
			System.out.println("Result set " + rsi + ":");
			ResultSetMetaData rsmd=rs.getMetaData();
			int ncolumns=rsmd.getColumnCount();
			while(rs.next()) {
				System.out.println("\tRow " + rowi + ":");

				for(int i=1; i<=ncolumns; i++) {
					int type=rsmd.getColumnType(i);
					String extra=TypeNames.getTypeName(type);
					String data=rs.getString(i);
					if(type==Types.VARCHAR) {
						extra+="," + data.length();
					}
					System.out.println("\t\t" + rsmd.getColumnName(i)
						+ "("
						+ extra
						+ ")=" + data);
				}
				rowi++;
			}
			rs=dbsp.nextResults();
			rsi++;
		}

		// Print out the warnings.
		SQLWarning warn=dbsp.getWarnings();
		while(warn!=null) {
			System.out.println("Warning:  " + warn);
			warn=warn.getNextWarning();
		}
	}
}
