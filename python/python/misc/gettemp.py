#!/usr/bin/env python
#
# $Id: gettemp.py,v 1.6 2003/05/19 17:20:16 dustin Exp $

import xmlrpclib
import smtplib
import nntplib
import time
import sys
import fpformat
from email.MIMEText import MIMEText

class ReadableString:
	"""A string that acts like a stream beacuse nntplib is stupid"""

	def __init__(self, text):
		self.lines=text.split('\n')

	def readline(self):
		rv = ''
		if len(self.lines) > 0:
			rv = self.lines[0] + '\n'
			del self.lines[0]

		return rv

class Alerts:
	"""Manage alert notification on thermometers being out of range."""

	def __init__(self):
		self.alerts=[]

	def add(self, k, v=None):
		"""Add a temperature reading to the alert list (None if a reading
		was not available."""
		self.alerts.append( (k,v) )

	def alert(self):
		"""Send alerts if there are any."""
		if len(self.alerts) > 0:
			self.__real_alert()

	def sendMessage(self, subject, body):
		"""Alert on the set of alerts provided."""
		emails=(
			'dustin@spy.net',
			'noelani@spy.net',
			'knitterb@blandsite.org',
			'dsallings@tmomail.net',)
		sender='dustin+temperature@spy.net'

		# Construct a MIME message.
		msg=MIMEText(body)
		msg['From'] = sender
		msg['Subject'] = subject

		# Send to each recipient
		for addr in emails:
			msg['To'] = addr
			# Send the mail
			server = smtplib.SMTP('mail')
			server.sendmail(sender, addr, msg.as_string())
			server.quit()

	def __real_alert(self):
		# Get a useful message body
		body=''
		for k,v in self.alerts:
			body = body + k + ': '
			if v is None:
				body = body + '<not read>'
			else:
				body = body + fpformat.fix(v, 2)
			body = body + '\n'

		subj='Temperature Alert - ' \
			+ time.strftime("%Y/%m/%d %H:%M:%S", time.localtime())

		self.sendMessage(subj, body)

def reportNNTP(vals):
	# Get a useful message body
	body=''
	for k,v in vals.items():
		body = body + k + ': ' + fpformat.fix(v, 2) + '\n'

	# Construct a MIME message.
	msg=MIMEText(body)
	msg['From'] = 'dustin+temperature@spy.net'
	msg['Subject'] = 'SPY Temp - ' \
		+ time.strftime("%Y/%m/%d %H:%M:%S", time.localtime())
	msg['Newsgroups'] = 'spy.temp'

	server = nntplib.NNTP('news')
	server.post(ReadableString(msg.as_string()))
	server.quit()

def report(vals):
	"""Report on the name dictionary of thermometers."""

	# These are the ranges we care about:
	normal = {
		"newmachineroom": (13, 26),
		"bedroom": (10, 31),
		"livingroom": (13, 30),
		"guestroom": (8, 30),
		}

	# Get the alerts object
	alerts=Alerts()

	for k, r in normal.items():
		# Minimum and maximum values for this thermometer
		min, max = r

		# Make sure we have something before reading it.
		if vals.has_key(k):
			# Get the actual temperature reading
			temp=vals[k]
			# Got a reading
			if temp < min or temp > max:
				alerts.add(k, temp)
		else:
			# Didn't get a reading
			alerts.add(k)

	# Run the alerts
	alerts.alert()

	# Report normally
	reportNNTP(vals)

if __name__ == '__main__':

	try:
		# Get the proxy to XML-RPC
		server=xmlrpclib.Server(
			"http://bleu.west.spy.net/servlet/net.spy.rpc.XMLRPC")

		# Get all of the thermometers
		vals=server.therm.getTemperatures()

		report(vals)
	except:
		e=sys.exc_info()
		alert=Alerts()
		subj='TEMPERATURE PROBLEM'
		msg="Could not read temperatures!\n\n" + `e[0]` + ': ' + `e[1]`
		alert.sendMessage(subj, msg)
