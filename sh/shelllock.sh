#!/bin/sh
#
# arch-tag: D03E70D6-4162-11D8-85EE-000393CFE6B8
#
# Primitive shell script locking

# Lock with a pid file
# Returns 0 if lock succeeds
shellLock() {
	PIDFILE="$1"

	# Normal case, no pid file, we're not locked
	if [ ! -f "$PIDFILE" ]
	then
		# echo "$PIDFILE not found." 1>&2
		echo $$ > "$PIDFILE"
		return 0
	fi

	PID=`cat "$PIDFILE"`

	# echo "shellLock debug:  Checking PID $PID" 1>&2

	if [ ! -n "$PID" ]
	then
		echo "PID doesn't seem to contain a PID:  $PID" 1>&2
		# Return unlocked, because we can't figure out what's in there
		return 0
	fi

	kill -s 0 "$PID"
	RV=$?
	if [ $RV -eq 0 ]
	then
		echo "Process $PID is still alive." 1>&2
		return 1
	else
		echo "Process $PID is dead, cleaning up." 1>&2
		rm "$PIDFILE"
		# Locking
		echo $$ > "$PIDFILE"
		return 0
	fi

	echo "FELL THROUGH" 1>&2
	return 1
}

# Unlock with a pid file
shellUnlock() {
	PIDFILE="$1"

	rm "$PIDFILE"
}
