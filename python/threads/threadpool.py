#!/usr/bin/env python
#
# Copyright (c) 2002  Dustin Sallings <dustin@spy.net>
# $Id: threadpool.py,v 1.3 2002/03/28 20:08:27 dustin Exp $

import threading
import exceptions
import traceback
import time

def dumpThreads():
    """Dump all threads to stdout."""
    print "Threads:"
    for t in threading.enumerate():
        print "  * " + str(t)

class Job:
    """Superclass for all jobs."""
    def run():
        """This method will be called when the job is ready to be executed."""
        raise exceptions.NotImplementedError

class JobQueue:
    """This is a basic thread-safe stack."""

    def __init__(self):
        """Get a new JobQueue."""
        self.queue=list()
        self.mutex=threading.Lock()
        self.condition=threading.Condition(self.mutex)

    def append(self, job):
        """Add a new job to the queue."""
        if not isinstance(job, Job):
            raise ValueError("Job must be of class Job, not " + str(type(job)))
        
        try:
            self.mutex.acquire()
            self.queue.append(job)
            self.condition.notify()
        finally:
            # This could raise an exception if the mutex isn't locked.  I'm
            # not sure what I'd do about that, since things are probably
            # broken by then, anyway.
            self.mutex.release()

    def pop(self):
        """Get the next job from the queue.

        If no jobs are available, an IndexError will be raised.
        """

        rv=None
        try:
            self.mutex.acquire()
            rv=self.queue.pop()
            self.condition.notify()
        finally:
            self.mutex.release()
        return rv

    def __len__(self):
        """Find out how many unclaimed jobs are in this queue."""
        rv=None
        try:
            self.mutex.acquire()
            rv=len(self.queue)
        finally:
            self.mutex.release()
        return rv

    def wait(self, timeout=None):
        """Wait for an object to be added to or removed from the queue."""
        self.condition.acquire()
        self.condition.wait(timeout)
        self.condition.release()
        # time.sleep(timeout)

class RunThread(threading.Thread):
    """This is a worker thread.

    Pay no attention to the thread behind the curtain.
    """

    thread_id=0

    def __init__(self, queue):
        """Get a RunThread"""

        # super.init
        threading.Thread.__init__(self)

        # A reference to the object that this is currently running
        self.running=None
        # Mark the start time of a job
        self.startTime=0

        # Our reference to the queue
        self.queue=queue
        # This reminds us to keep going
        self.going=1

        self.thread_id=RunThread.thread_id
        RunThread.thread_id+=1

        print "RunThread " + str(self.thread_id) + " going online."

        self.setName("RunThread#" + str(self.thread_id))
        
        self.start()

    def runJob(self, job):
        try:
            self.startTime=time.time()
            self.running=job
            job.run()
        except:
            traceback.print_exc()
        self.running=None

    def isProcessing(self):
        """Return true if this thread is currently processing something."""
        return(self.running != None)

    def __str__(self):
        # Get the regular string representation, minus the > on the end.
        rv=threading.Thread.__repr__(self)[0:-1]

        if self.isProcessing():
            rv+=" - running " + str(self.running.__class__) + " for "
            rv+="%.2fs" % (time.time() - self.startTime)
        else:
            rv+=" - idle"
        rv+=">"
        return rv

    def run(self):
        while self.going:
            try:
                j=self.queue.pop()
                self.runJob(j)
            except IndexError:
                # print "Got an index error on pop."
                # Wait up to ten seconds for a job to be added
                self.queue.wait(10)

    def shutdown(self):
        """Shutdown this thread."""
        self.going=None

class ThreadPool:
    """A pool of threads that are eagerly awaiting work."""

    def __init__(self, name="AThreadPool", num=5):
        """Get a new thread pool."""
        self.queue=JobQueue()
        self.threads=list()

        for i in range(num):
            t=RunThread(self.queue)
            self.threads.append(t)

    def waitForTaskCount(self, n=0):
        """Wait for the number of tasks in the queue to reach the given
        number."""
        while len(self.queue) > n:
            self.queue.wait(5)

    def addTask(self, job):
        """Add a job to the queue."""
        self.queue.append(job)

    def getTaskCount(self):
        """How many jobs are in the queue?"""
        return len(self.queue)

    def getThreadCount(self):
        """How many threads are in the pool?"""
        return len(self.threads)

    def shutdown(self):
        """Shutdown the pool."""
        for t in self.threads:
            t.shutdown()

    def __del__(self):
        """Make sure the pool is shut down."""
        self.shutdown()

class SampleTask(Job):
    """A sample task for test purposes."""
    def run(self):
        """Sleep a random amount of time up to 15 seconds."""
        import random
        r=random.Random()
        st=r.random() * 15
        print "Sleeping for " + str(st) + " seconds."
        time.sleep(st)
        print "Done."

def main():
    tp=ThreadPool("Test Pool", 15)
    dumpThreads()
    try:
        for i in range(100):
            dumpThreads()
            tp.addTask(SampleTask())
    
        dumpThreads()
        for i in range(100):
            tp.waitForTaskCount(50)
            print "Adding a new task."
            tp.addTask(SampleTask())

        tp.waitForTaskCount(0)
        print "All tasks have been accepted, shutting down."
        tp.shutdown()
        dumpThreads()

        print "Done."
    except:
        traceback.print_exc()
        tp.shutdown()

if __name__ == '__main__':
    main()
