#!/usr/bin/env python

class OidResolve:
	"""A simple OID map."""

	def lookupOid(self, name):
		"""Look up an SNMP variable by name.

		i.e.
			sysUptime.0        ->  1.3.6.1.2.1.1.3.0
			1.3.6.1.2.1.1.3.0  ->  1.3.6.1.2.1.1.3.0
			unknowncrap.0      ->  unknowncrap.0
		"""
		parts=name.split('.')
		if self.mib2oids.has_key(parts[0]):
			parts[0]=self.mib2oids[parts[0]]
		rv='.'+'.'.join(parts)
		return(rv)

	mib2oids={
		'iso': '1',
		'org': '1.3',
		'dod': '1.3.6',
		'internet': '1.3.6.1',
		'directory': '1.3.6.1.1',
		'mgmt': '1.3.6.1.2',
		'mib-2': '1.3.6.1.2.1',
		'system': '1.3.6.1.2.1.1',
		'sysDescr': '1.3.6.1.2.1.1.1',
		'sysObjectID': '1.3.6.1.2.1.1.2',
		'sysUpTime': '1.3.6.1.2.1.1.3',
		'sysUptime': '1.3.6.1.2.1.1.3',
		'sysContact': '1.3.6.1.2.1.1.4',
		'sysName': '1.3.6.1.2.1.1.5',
		'sysLocation': '1.3.6.1.2.1.1.6',
		'sysServices': '1.3.6.1.2.1.1.7',
		'interfaces': '1.3.6.1.2.1.2',
		'ifNumber': '1.3.6.1.2.1.2.1',
		'ifTable': '1.3.6.1.2.1.2.2',
		'ifEntry': '1.3.6.1.2.1.2.2.1',
		'ifIndex': '1.3.6.1.2.1.2.2.1.1',
		'ifInOctets': '1.3.6.1.2.1.2.2.1.10',
		'ifInUcastPkts': '1.3.6.1.2.1.2.2.1.11',
		'ifInNUcastPkts': '1.3.6.1.2.1.2.2.1.12',
		'ifInDiscards': '1.3.6.1.2.1.2.2.1.13',
		'ifInErrors': '1.3.6.1.2.1.2.2.1.14',
		'ifInUnknownProtos': '1.3.6.1.2.1.2.2.1.15',
		'ifOutOctets': '1.3.6.1.2.1.2.2.1.16',
		'ifOutUcastPkts': '1.3.6.1.2.1.2.2.1.17',
		'ifOutNUcastPkts': '1.3.6.1.2.1.2.2.1.18',
		'ifOutDiscards': '1.3.6.1.2.1.2.2.1.19',
		'ifDescr': '1.3.6.1.2.1.2.2.1.2',
		'ifOutErrors': '1.3.6.1.2.1.2.2.1.20',
		'ifOutQLen': '1.3.6.1.2.1.2.2.1.21',
		'ifSpecific': '1.3.6.1.2.1.2.2.1.22',
		'ifType': '1.3.6.1.2.1.2.2.1.3',
		'ifMtu': '1.3.6.1.2.1.2.2.1.4',
		'ifSpeed': '1.3.6.1.2.1.2.2.1.5',
		'ifPhysAddress': '1.3.6.1.2.1.2.2.1.6',
		'ifAdminHack': '1.3.6.1.2.1.2.2.1.7',
		'ifAdminStatus': '1.3.6.1.2.1.2.2.1.7',
		'ifOperHack': '1.3.6.1.2.1.2.2.1.8',
		'ifOperStatus': '1.3.6.1.2.1.2.2.1.8',
		'ifLastChange': '1.3.6.1.2.1.2.2.1.9',
		'ifName': '1.3.6.1.2.1.31.1.1.1.1',
		'ifInMulticastPkts': '1.3.6.1.2.1.31.1.1.1.2',
		'ifInBroadcastPkts': '1.3.6.1.2.1.31.1.1.1.3',
		'ifOutMulticastPkts': '1.3.6.1.2.1.31.1.1.1.4',
		'ifOutBroadcastPkts': '1.3.6.1.2.1.31.1.1.1.5',
		'ifHCInOctets': '1.3.6.1.2.1.31.1.1.1.6',
		'ifHCInUcastPkts': '1.3.6.1.2.1.31.1.1.1.7',
		'ifHCInMulticastPkts': '1.3.6.1.2.1.31.1.1.1.8',
		'ifHCInBroadcastPkts': '1.3.6.1.2.1.31.1.1.1.9',
		'ifHCOutOctets': '1.3.6.1.2.1.31.1.1.1.10',
		'ifHCOutUcastPkts': '1.3.6.1.2.1.31.1.1.1.11',
		'ifHCOutMulticastPkts': '1.3.6.1.2.1.31.1.1.1.12',
		'ifHCOutBroadcastPkts': '1.3.6.1.2.1.31.1.1.1.13',
		'ifLinkUpDownTrapEnable': '1.3.6.1.2.1.31.1.1.1.14',
		'ifHighSpeed': '1.3.6.1.2.1.31.1.1.1.15',
		'ifPromiscuousMode': '1.3.6.1.2.1.31.1.1.1.16',
		'ifConnectorPresent': '1.3.6.1.2.1.31.1.1.1.17',
		'ifAlias': '1.3.6.1.2.1.31.1.1.1.18',
		'ifCounterDiscontinuityTime': '1.3.6.1.2.1.31.1.1.1.19',
		'at': '1.3.6.1.2.1.3',
		'atTable': '1.3.6.1.2.1.3.1',
		'atEntry': '1.3.6.1.2.1.3.1.1',
		'atIfIndex': '1.3.6.1.2.1.3.1.1.1',
		'atPhysAddress': '1.3.6.1.2.1.3.1.1.2',
		'atNetAddress': '1.3.6.1.2.1.3.1.1.3',
		'ip': '1.3.6.1.2.1.4',
		'ipForwarding': '1.3.6.1.2.1.4.1',
		'ipOutRequests': '1.3.6.1.2.1.4.10',
		'ipOutDiscards': '1.3.6.1.2.1.4.11',
		'ipOutNoRoutes': '1.3.6.1.2.1.4.12',
		'ipReasmTimeout': '1.3.6.1.2.1.4.13',
		'ipReasmReqds': '1.3.6.1.2.1.4.14',
		'ipReasmOKs': '1.3.6.1.2.1.4.15',
		'ipReasmFails': '1.3.6.1.2.1.4.16',
		'ipFragOKs': '1.3.6.1.2.1.4.17',
		'ipFragFails': '1.3.6.1.2.1.4.18',
		'ipFragCreates': '1.3.6.1.2.1.4.19',
		'ipDefaultTTL': '1.3.6.1.2.1.4.2',
		'ipAddrTable': '1.3.6.1.2.1.4.20',
		'ipAddrEntry': '1.3.6.1.2.1.4.20.1',
		'ipAdEntAddr': '1.3.6.1.2.1.4.20.1.1',
		'ipAdEntIfIndex': '1.3.6.1.2.1.4.20.1.2',
		'ipAdEntNetMask': '1.3.6.1.2.1.4.20.1.3',
		'ipAdEntBcastAddr': '1.3.6.1.2.1.4.20.1.4',
		'ipAdEntReasmMaxSize': '1.3.6.1.2.1.4.20.1.5',
		'ipRouteTable': '1.3.6.1.2.1.4.21',
		'ipRouteEntry': '1.3.6.1.2.1.4.21.1',
		'ipRouteDest': '1.3.6.1.2.1.4.21.1.1',
		'ipRouteAge': '1.3.6.1.2.1.4.21.1.10',
		'ipRouteMask': '1.3.6.1.2.1.4.21.1.11',
		'ipRouteMetric5': '1.3.6.1.2.1.4.21.1.12',
		'ipRouteInfo': '1.3.6.1.2.1.4.21.1.13',
		'ipRouteIfIndex': '1.3.6.1.2.1.4.21.1.2',
		'ipRouteMetric1': '1.3.6.1.2.1.4.21.1.3',
		'ipRouteMetric2': '1.3.6.1.2.1.4.21.1.4',
		'ipRouteMetric3': '1.3.6.1.2.1.4.21.1.5',
		'ipRouteMetric4': '1.3.6.1.2.1.4.21.1.6',
		'ipRouteNextHop': '1.3.6.1.2.1.4.21.1.7',
		'ipRouteType': '1.3.6.1.2.1.4.21.1.8',
		'ipRouteProto': '1.3.6.1.2.1.4.21.1.9',
		'ipNetToMediaTable': '1.3.6.1.2.1.4.22',
		'ipNetToMediaEntry': '1.3.6.1.2.1.4.22.1',
		'ipNetToMediaIfIndex': '1.3.6.1.2.1.4.22.1.1',
		'ipNetToMediaPhysAddress': '1.3.6.1.2.1.4.22.1.2',
		'ipNetToMediaNetAddress': '1.3.6.1.2.1.4.22.1.3',
		'ipNetToMediaType': '1.3.6.1.2.1.4.22.1.4',
		'ipRoutingDiscards': '1.3.6.1.2.1.4.23',
		'ipInReceives': '1.3.6.1.2.1.4.3',
		'ipInHdrErrors': '1.3.6.1.2.1.4.4',
		'ipInAddrErrors': '1.3.6.1.2.1.4.5',
		'ipForwDatagrams': '1.3.6.1.2.1.4.6',
		'ipInUnknownProtos': '1.3.6.1.2.1.4.7',
		'ipInDiscards': '1.3.6.1.2.1.4.8',
		'ipInDelivers': '1.3.6.1.2.1.4.9',
		'icmp': '1.3.6.1.2.1.5',
		'icmpInMsgs': '1.3.6.1.2.1.5.1',
		'icmpInTimestamps': '1.3.6.1.2.1.5.10',
		'icmpInTimestampReps': '1.3.6.1.2.1.5.11',
		'icmpInAddrMasks': '1.3.6.1.2.1.5.12',
		'icmpInAddrMaskReps': '1.3.6.1.2.1.5.13',
		'icmpOutMsgs': '1.3.6.1.2.1.5.14',
		'icmpOutErrors': '1.3.6.1.2.1.5.15',
		'icmpOutDestUnreachs': '1.3.6.1.2.1.5.16',
		'icmpOutTimeExcds': '1.3.6.1.2.1.5.17',
		'icmpOutParmProbs': '1.3.6.1.2.1.5.18',
		'icmpOutSrcQuenchs': '1.3.6.1.2.1.5.19',
		'icmpInErrors': '1.3.6.1.2.1.5.2',
		'icmpOutRedirects': '1.3.6.1.2.1.5.20',
		'icmpOutEchos': '1.3.6.1.2.1.5.21',
		'icmpOutEchoReps': '1.3.6.1.2.1.5.22',
		'icmpOutTimestamps': '1.3.6.1.2.1.5.23',
		'icmpOutTimestampReps': '1.3.6.1.2.1.5.24',
		'icmpOutAddrMasks': '1.3.6.1.2.1.5.25',
		'icmpOutAddrMaskReps': '1.3.6.1.2.1.5.26',
		'icmpInDestUnreachs': '1.3.6.1.2.1.5.3',
		'icmpInTimeExcds': '1.3.6.1.2.1.5.4',
		'icmpInParmProbs': '1.3.6.1.2.1.5.5',
		'icmpInSrcQuenchs': '1.3.6.1.2.1.5.6',
		'icmpInRedirects': '1.3.6.1.2.1.5.7',
		'icmpInEchos': '1.3.6.1.2.1.5.8',
		'icmpInEchoReps': '1.3.6.1.2.1.5.9',
		'tcp': '1.3.6.1.2.1.6',
		'tcpRtoAlgorithm': '1.3.6.1.2.1.6.1',
		'tcpInSegs': '1.3.6.1.2.1.6.10',
		'tcpOutSegs': '1.3.6.1.2.1.6.11',
		'tcpRetransSegs': '1.3.6.1.2.1.6.12',
		'tcpConnTable': '1.3.6.1.2.1.6.13',
		'tcpConnEntry': '1.3.6.1.2.1.6.13.1',
		'tcpConnState': '1.3.6.1.2.1.6.13.1.1',
		'tcpConnLocalAddress': '1.3.6.1.2.1.6.13.1.2',
		'tcpConnLocalPort': '1.3.6.1.2.1.6.13.1.3',
		'tcpConnRemAddress': '1.3.6.1.2.1.6.13.1.4',
		'tcpConnRemPort': '1.3.6.1.2.1.6.13.1.5',
		'tcpInErrs': '1.3.6.1.2.1.6.14',
		'tcpOutRsts': '1.3.6.1.2.1.6.15',
		'tcpRtoMin': '1.3.6.1.2.1.6.2',
		'tcpRtoMax': '1.3.6.1.2.1.6.3',
		'tcpMaxConn': '1.3.6.1.2.1.6.4',
		'tcpActiveOpens': '1.3.6.1.2.1.6.5',
		'tcpPassiveOpens': '1.3.6.1.2.1.6.6',
		'tcpAttemptFails': '1.3.6.1.2.1.6.7',
		'tcpEstabResets': '1.3.6.1.2.1.6.8',
		'tcpCurrEstab': '1.3.6.1.2.1.6.9',
		'udp': '1.3.6.1.2.1.7',
		'udpInDatagrams': '1.3.6.1.2.1.7.1',
		'udpNoPorts': '1.3.6.1.2.1.7.2',
		'udpInErrors': '1.3.6.1.2.1.7.3',
		'udpOutDatagrams': '1.3.6.1.2.1.7.4',
		'udpTable': '1.3.6.1.2.1.7.5',
		'udpEntry': '1.3.6.1.2.1.7.5.1',
		'udpLocalAddress': '1.3.6.1.2.1.7.5.1.1',
		'udpLocalPort': '1.3.6.1.2.1.7.5.1.2',
		'egp': '1.3.6.1.2.1.8',
		'egpInMsgs': '1.3.6.1.2.1.8.1',
		'egpInErrors': '1.3.6.1.2.1.8.2',
		'egpOutMsgs': '1.3.6.1.2.1.8.3',
		'egpOutErrors': '1.3.6.1.2.1.8.4',
		'egpNeighTable': '1.3.6.1.2.1.8.5',
		'egpNeighEntry': '1.3.6.1.2.1.8.5.1',
		'egpNeighState': '1.3.6.1.2.1.8.5.1.1',
		'egpNeighStateUps': '1.3.6.1.2.1.8.5.1.10',
		'egpNeighStateDowns': '1.3.6.1.2.1.8.5.1.11',
		'egpNeighIntervalHello': '1.3.6.1.2.1.8.5.1.12',
		'egpNeighIntervalPoll': '1.3.6.1.2.1.8.5.1.13',
		'egpNeighMode': '1.3.6.1.2.1.8.5.1.14',
		'egpNeighEventTrigger': '1.3.6.1.2.1.8.5.1.15',
		'egpNeighAddr': '1.3.6.1.2.1.8.5.1.2',
		'egpNeighAs': '1.3.6.1.2.1.8.5.1.3',
		'egpNeighInMsgs': '1.3.6.1.2.1.8.5.1.4',
		'egpNeighInErrs': '1.3.6.1.2.1.8.5.1.5',
		'egpNeighOutMsgs': '1.3.6.1.2.1.8.5.1.6',
		'egpNeighOutErrs': '1.3.6.1.2.1.8.5.1.7',
		'egpNeighInErrMsgs': '1.3.6.1.2.1.8.5.1.8',
		'egpNeighOutErrMsgs': '1.3.6.1.2.1.8.5.1.9',
		'egpAs': '1.3.6.1.2.1.8.6',
		'transmission': '1.3.6.1.2.1.10',
		'frame-relay': '1.3.6.1.2.1.10.32',
		'frDlcmiTable': '1.3.6.1.2.1.10.32.1',
		'frDlcmiEntry': '1.3.6.1.2.1.10.32.1.1',
		'frDlcmiIfIndex': '1.3.6.1.2.1.10.32.1.1.1',
		'frDlcmiState': '1.3.6.1.2.1.10.32.1.1.2',
		'frDlcmiAddress': '1.3.6.1.2.1.10.32.1.1.3',
		'frDlcmiAddressLen': '1.3.6.1.2.1.10.32.1.1.4',
		'frDlcmiPollingInterval': '1.3.6.1.2.1.10.32.1.1.5',
		'frDlcmiFullEnquiryInterval': '1.3.6.1.2.1.10.32.1.1.6',
		'frDlcmiErrorThreshold': '1.3.6.1.2.1.10.32.1.1.7',
		'frDlcmiMonitoredEvents': '1.3.6.1.2.1.10.32.1.1.8',
		'frDlcmiMaxSupportedVCs': '1.3.6.1.2.1.10.32.1.1.9',
		'frDlcmiMulticast': '1.3.6.1.2.1.10.32.1.1.10',
		'frCircuitTable': '1.3.6.1.2.1.10.32.2',
		'frCircuitEntry': '1.3.6.1.2.1.10.32.2.1',
		'frCircuitIfIndex': '1.3.6.1.2.1.10.32.2.1.1',
		'frCircuitDlci': '1.3.6.1.2.1.10.32.2.1.2',
		'frCircuitState': '1.3.6.1.2.1.10.32.2.1.3',
		'frCircuitReceivedFECNs': '1.3.6.1.2.1.10.32.2.1.4',
		'frCircuitReceivedBECNs': '1.3.6.1.2.1.10.32.2.1.5',
		'frCircuitSentFrames': '1.3.6.1.2.1.10.32.2.1.6',
		'frCircuitSentOctets': '1.3.6.1.2.1.10.32.2.1.7',
		'frOutOctets': '1.3.6.1.2.1.10.32.2.1.7',
		'frCircuitReceivedFrames': '1.3.6.1.2.1.10.32.2.1.8',
		'frCircuitReceivedOctets': '1.3.6.1.2.1.10.32.2.1.9',
		'frInOctets': '1.3.6.1.2.1.10.32.2.1.9',
		'frCircuitCreationTime': '1.3.6.1.2.1.10.32.2.1.10',
		'frCircuitLastTimeChange': '1.3.6.1.2.1.10.32.2.1.11',
		'frCircuitCommittedBurst': '1.3.6.1.2.1.10.32.2.1.12',
		'frCircuitExcessBurst': '1.3.6.1.2.1.10.32.2.1.13',
		'frCircuitThroughput': '1.3.6.1.2.1.10.32.2.1.14',
		'frErrTable': '1.3.6.1.2.1.10.32.3',
		'frErrEntry': '1.3.6.1.2.1.10.32.3.1',
		'frErrIfIndex': '1.3.6.1.2.1.10.32.3.1.1',
		'frErrType': '1.3.6.1.2.1.10.32.3.1.2',
		'frErrData': '1.3.6.1.2.1.10.32.3.1.3',
		'frErrTime': '1.3.6.1.2.1.10.32.3.1.4',
		'frame-relay-globals': '1.3.6.1.2.1.10.32.4',
		'frTrapState': '1.3.6.1.2.1.10.32.4.1',
		'snmp': '1.3.6.1.2.1.11',
		'snmpInPkts': '1.3.6.1.2.1.11.1',
		'snmpInBadValues': '1.3.6.1.2.1.11.10',
		'snmpInReadOnlys': '1.3.6.1.2.1.11.11',
		'snmpInGenErrs': '1.3.6.1.2.1.11.12',
		'snmpInTotalReqVars': '1.3.6.1.2.1.11.13',
		'snmpInTotalSetVars': '1.3.6.1.2.1.11.14',
		'snmpInGetRequests': '1.3.6.1.2.1.11.15',
		'snmpInGetNexts': '1.3.6.1.2.1.11.16',
		'snmpInSetRequests': '1.3.6.1.2.1.11.17',
		'snmpInGetResponses': '1.3.6.1.2.1.11.18',
		'snmpInTraps': '1.3.6.1.2.1.11.19',
		'snmpOutPkts': '1.3.6.1.2.1.11.2',
		'snmpOutTooBigs': '1.3.6.1.2.1.11.20',
		'snmpOutNoSuchNames': '1.3.6.1.2.1.11.21',
		'snmpOutBadValues': '1.3.6.1.2.1.11.22',
		'snmpOutGenErrs': '1.3.6.1.2.1.11.24',
		'snmpOutGetRequests': '1.3.6.1.2.1.11.25',
		'snmpOutGetNexts': '1.3.6.1.2.1.11.26',
		'snmpOutSetRequests': '1.3.6.1.2.1.11.27',
		'snmpOutGetResponses': '1.3.6.1.2.1.11.28',
		'snmpOutTraps': '1.3.6.1.2.1.11.29',
		'snmpInBadVersions': '1.3.6.1.2.1.11.3',
		'snmpEnableAuthenTraps': '1.3.6.1.2.1.11.30',
		'snmpInBadCommunityNames': '1.3.6.1.2.1.11.4',
		'snmpInBadCommunityUses': '1.3.6.1.2.1.11.5',
		'snmpInASNParseErrs': '1.3.6.1.2.1.11.6',
		'snmpInTooBigs': '1.3.6.1.2.1.11.8',
		'snmpInNoSuchNames': '1.3.6.1.2.1.11.9',
		'experimental': '1.3.6.1.3',
		'private': '1.3.6.1.4',
		'enterprises': '1.3.6.1.4.1',
}
