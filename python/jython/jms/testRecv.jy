#!/usr/bin/env jython

import time
import java
import javax

class Listener(javax.jms.MessageListener):

    def __init__(self, sess):
        self.sess=sess

    def onMessage(self, msg):
        print "Got a message:", msg.getJMSMessageID(), msg.text
        # time.sleep(0.5)
        # self.sess.rollback()
        msg.acknowledge()

ht=java.util.Hashtable()

ht['java.naming.factory.initial']='org.jnp.interfaces.NamingContextFactory'
# ht['java.naming.provider.url']='jnp/localhost:1099'
ht['java.naming.provider.url']='jnp://lamer.dhcp.2wire.com:1099'
ht['java.naming.factory.url.pkgs']='org.jboss.namingrg.jnp.interfaces'

ic=javax.naming.InitialContext(ht)
qcf=ic.lookup("ConnectionFactory")
conn=qcf.createQueueConnection()
que=ic.lookup("queue/testQueue")
# session=conn.createQueueSession(0, javax.jms.QueueSession.AUTO_ACKNOWLEDGE)
session=conn.createQueueSession(1, javax.jms.QueueSession.CLIENT_ACKNOWLEDGE)
conn.start()

recv=session.createReceiver(que);
recv.setMessageListener(Listener(session))
