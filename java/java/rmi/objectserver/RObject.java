import java.rmi.Remote;
import java.rmi.RemoteException;

import java.util.*;

public interface RObject extends Remote {
    void storeHash(Hashtable h) throws RemoteException;
    Hashtable getHash() throws RemoteException;
}
