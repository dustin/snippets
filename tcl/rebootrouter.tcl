#!/usr/bin/expect -f
#
# arch-id: 9D62927E-4646-11D8-8E0A-000A957659CC

# Get the router password from the environment.
set pw $env(ROUTER_PW)

spawn telnet apc1
expect "User Name"
send "dustin\r"
expect "Password"
send "$pw\r"
expect "Device Manager"
send "1\r"
expect "DSL Router"
send "1\r"
expect "DSL Router"
send "3\r"
expect "Immediate Reboot"
send "YES\r"
expect "successfully"
send "\r"
send ""
expect "MasterSwitch"
send "4\r"
