#!/usr/bin/env python

import os
import time
import exceptions

class AlreadyLockedException(exceptions.Exception):
    """Exception raised when a resource is already locked."""
    pass

class CannotLockException(exceptions.Exception):
    """Exception raised when we can't lock a resource."""
    pass

class PidLock:
    """Basic resource locking based on PIDs."""

    def __init__(self, pidfile):
        """Atomically obtain a lock using the given pidfile.

        If the pidfile exists, verify the contents contain a valid PID.
        """
        self.pidfile=pidfile
        try:
            self.__doLock()
        except OSError:
            # If we failed to obtain a lock, verify it's still valid, and
            # if not, reclaim it.
            self.__checkLock()
            os.unlink(self.pidfile)
            self.__doLock()

    def __doLock(self):
        """Internal:  Store the lock."""
        fd=None
        f=None
        self.locked=None
        try:
            fd=os.open(self.pidfile, os.O_WRONLY|os.O_CREAT|os.O_EXCL, 0644)
            towrite=str(os.getpid()) + "\n"
            rv=os.write(fd, towrite)
            if rv!=len(towrite):
                self.unlock()
                raise CannotLockException
            self.locked=1
        finally:
            if f!=None:
                f.close()
            if fd!=None:
                os.close(fd)

    def __checkLock(self):
        """Internal:  Verify a lock and raise an exception if the resource
        is truly locked.
        """
        f=file(self.pidfile)
        d=f.readline()
        if d!='':
            pid=int(d)
            try:
                os.kill(pid, 0)
                raise AlreadyLockedException(pid)
            except OSError, oe:
                if oe[0] != 3:
                    print "kill error:  " + str(oe)
                    raise AlreadyLockedException(pid)

    def unlock(self):
        """Unlock the resource if it's not already unlocked (safe to call
        more than once).
        """
        if self.locked:
            os.unlink(self.pidfile)
            self.locked=None

    def __del__(self):
        """Verify the resource is unlocked on object destruct."""
        self.unlock()

if __name__ == '__main__':
    print "Obtaining lock..."
    lock=PidLock("test.pid")
    print "Got it, sleeping..."
    time.sleep(5)
    # print "Unlocking..."
    # lock.unlock()
    print "Done."
