#!/usr/bin/env jython

import com
import java

import lookupSerial

# Serial number of the device
sn="400000000334"
# Event and command stuff for the heartbeat struct
event="SomeEvent"
command="SomeCommand"
# the noc
noc="13"

# Get the gateway serial object (contains all the info from the hpid table)
gatewayserial=lookupSerial.GatewaySerial(sn)

# Build the heartbeat struct
hbs=gatewayserial.getHeartbeatStruct(event, command)
print "HeartbeatStruct: ", hbs

# Build the gateway bean
gwb=com.twowire.gateway.GatewayBean.createGatewayFromBootstrap(sn, hbs)
print "Gateway bean: ", gwb

# Get an action
action=com.twowire.gateway.Command.CreateBootStrapRedirectCommand(gwb, noc)
print "Action: ", action

