// Copyright (c) 2004  2Wire, Inc.

package PACKAGE;

import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.Map;
import java.util.HashMap;
import java.util.Set;
import java.util.HashSet;
import java.util.Collection;

import net.spy.cache.SpyCache;

import com.twowire.TWObject;

/**
 *
 *
 * @author <a href="mailto:dsallings@2wire.com">Dustin Sallings</a>
 */
public class CLASSNAME extends TWObject {

    private static final String CACHE_KEY="PACKAGE.CLASSNAME.Map";
    private static final int CACHE_TIME=86400000;

    private int id=0;

    private CLASSNAME(ResultSet rs) throws SQLException {
        super();

        id=rs.getInt("MYKEY");
    }

    // Initialize the map from the DB
    private static Map initMap() throws EXC {
        Map rv=null;
		DBSP db=null;
        try {
			db=INITDB;

            rv=new HashMap();

            ResultSet rs=DOQUERY;

            while(rs.next()) {
                CLASSNAME inst=new CLASSNAME(rs);

                // Add it to the map
                rv.put(new Integer(pc.getId()), inst);
            }

            rs.close();

            db.close();
            db=null;

        } catch(SQLException e) {
            throw new EXC("Problem initializing category list", e);
        } finally {
            if(db!=null) {
                db.close();
            }
        }
        return(rv);
    }

    /**
     * Get a CLASSNAME by integer ID.
     *
     * @param id the integer ID
     * @return the CLASSNAME instance
     * @throws EXC if there's a problem loading the map
     * @throws NOSUCHEXC if the instance does not exist
     */
    public static CLASSNAME getCLASSNAME(int id) throws EXC {
        Map m=getMap();
        CLASSNAME rv=(CLASSNAME)m.get(new Integer(id));
        if(rv==null) {
            throw new NOSUCHEXC(id);
        }
        return(rv);
    }

    /**
     * Get a collection of all known CLASSNAME objects.
     * @return a collection of all CLASSNAME objects
     * @throws EXC if there's a problem creating this list
     */
    public static Collection getAllCLASSNAMEs() throws EXC {
        Map m=getMap();
        Set rv=new HashSet(m.values());
        return(rv);
    }

    // Get the map (which may cause initialization)
    private static synchronized Map getMap() throws EXC {
        SpyCache sc=SpyCache.getInstance();
        Map rv=(Map)sc.get(CACHE_KEY);
        if(rv==null) {
            rv=initMap();
            sc.store(CACHE_KEY, rv, CACHE_TIME);
        }
        return(rv);
    }

    /**
     * True if the object is an instance of CLASSNAME with the same ID.
     */
    public boolean equals(Object o) {
        boolean rv=false;
        if(o instanceof CLASSNAME) {
            CLASSNAME inst=(CLASSNAME)o;
            rv= (id == inst.id);
        }
        return(rv);
    }

    /**
     * Get the hashcode for this object.
     *
     * @return the id
     */
    public int hashCode() {
        return(id);
    }

    /**
     * Get the ID of this CLASSNAME.
     */
    public int getId() {
        return(id);
    }

    /**
     * String me.
     */
    public String toString() {
        StringBuffer sb=new StringBuffer(64);
        sb.append("{CLASSNAME id=");
        sb.append(id);
        sb.append("}");
        return(sb.toString());
    }

}
