/*
 * Copyright (c) 1999  Dustin Sallings <dustin@spy.net>
 *
 * $Id: PhotoDB.java,v 1.3 1999/10/20 02:14:47 dustin Exp $
 */

import java.sql.*;
import java.util.*;

import net.spy.*;

import com.javaexchange.dbConnectionBroker.*;

public class PhotoDB extends SpyDB {
	public PhotoDB() {
		super(new PhotoConfig());
	}
}
