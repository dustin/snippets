#!/usr/bin/env python
"""
Roll gateway_log tables and views and all that.

Copyright (c) 2005  Dustin Sallings <dustin@spy.net>
"""
# arch-tag: 081E7F9F-D5EE-4738-8AE3-630F81852873

import sys
import time

ts=time.strftime("%Y%m%d")

print """create table gateway_log_%(ts)s (
    gateway_key integer not null,
    username varchar(255),
    txn_type_key integer not null,
    org_key integer not null,
    version varchar(20),
    txn_time integer,
    source_ip inet,
    hp_ip inet,
    keycodestring varchar(30),
    created_date timestamp not null default now()
);
create index idx_gateway_log_%(ts)s_created_date
    on gateway_log_%(ts)s (created_date);
create index idx_gateway_log_%(ts)s_gateway_key
    on gateway_log_%(ts)s (gateway_key)
;
""" % {'ts': ts}

print "drop view show_trans\n;"
print "drop view gateway_log\n;"

cols=['gateway_key', 'username', 'txn_type_key', 'org_key', 'version',
    'txn_time', 'source_ip', 'hp_ip', 'keycodestring', 'created_date']

queries=[]
for table in sys.argv[1:] + ['gateway_log_%(ts)s' % {'ts': ts}]:
    queries.append('select ' + ', '.join([table + "." + col for col in cols]) \
        + ' from ' + table)

print 'create view gateway_log as\n' + '\nUNION ALL\n'.join(queries) + "\n;"

print """create rule gateway_log_rule as on insert to gateway_log
    do instead insert into gateway_log_%(ts)s
        (gateway_key, username, txn_type_key, org_key, version, txn_time,
            source_ip, hp_ip, keycodestring)
    values
        (new.gateway_key, new.username, new.txn_type_key, new.org_key,
            new.version, new.txn_time, new.source_ip, new.hp_ip,
            new.keycodestring)
;
""" % {'ts': ts}

print "grant insert on gateway_log to cmslogger;"
print "grant select on gateway_log to group logview;"

print """create view show_trans as
    SELECT l.gateway_key, t.name AS txn_type, l.org_key, l.version,
        l.source_ip, l.hp_ip, l.keycodestring, l.txn_time, l.created_date
    FROM gateway_log l
        JOIN txn_type t USING (txn_type_key)
;
grant select on show_trans to group logview
;
"""
