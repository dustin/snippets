#!/usr/bin/env python

import psycopg;

dbconn=psycopg.connect('dbname=photo host=db user=nobody password=nopassword')

c=dbconn.cursor()

c.execute('select id, name from cat order by name')

rows=c.fetchall()
print rows
