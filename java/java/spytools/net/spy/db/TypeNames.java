// Copyright (c) 2001  Dustin Sallings <dustin@spy.net>
//
// $Id: TypeNames.java,v 1.2 2002/07/10 04:25:25 dustin Exp $

package net.spy.db;

import java.sql.Types;

/**
 * Get names for sql data types.
 */
public class TypeNames extends Object {

	/**
	 * Get the name of the given java.sql.Types type
	 */
	public static String getTypeName(int type) {
		String rv=null;

		switch(type) {
			case Types.BIGINT:
				rv="BigInt";
				break;
			case Types.BINARY:
				rv="Binary";
				break;
			case Types.BIT:
				rv="Bit";
				break;
			case Types.CHAR:
				rv="Char";
				break;
			case Types.DATE:
				rv="Date";
				break;
			case Types.DECIMAL:
				rv="Decimal";
				break;
			case Types.NUMERIC:
				rv="Numeric";
				break;
			case Types.DOUBLE:
				rv="Double";
				break;
			case Types.FLOAT:
				rv="Float";
				break;
			case Types.INTEGER:
				rv="Integer";
				break;
			case Types.LONGVARBINARY:
				rv="LongVarBinary";
				break;
			case Types.LONGVARCHAR:
				rv="LongVarChar";
				break;
			case Types.NULL:
				rv="Null";
				break;
			case Types.OTHER:
				rv="Other";
				break;
			case Types.REAL:
				rv="Real";
				break;
			case Types.SMALLINT:
				rv="SmallInt";
				break;
			case Types.TIME:
				rv="Time";
				break;
			case Types.TIMESTAMP:
				rv="Timestamp";
				break;
			case Types.TINYINT:
				rv="TinyInt";
				break;
			case Types.VARBINARY:
				rv="VarBinary";
				break;
			case Types.VARCHAR:
				rv="VarChar";
				break;
			default:
				rv="Unknown#" + type;
				break;
		}

		return(rv);
	}

	public static void main(String args[]) throws Exception {
		int type=Integer.parseInt(args[0]);

		System.out.println("Type " + type + "=" + getTypeName(type));
	}

}
