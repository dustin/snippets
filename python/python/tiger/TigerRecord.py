#!/usr/bin/env python

class TigerRecord:

	def  __init__(self, parts):
		self.parts=parts

	def __str__(self):
		return "Tiger Record, type " + self.recordType
