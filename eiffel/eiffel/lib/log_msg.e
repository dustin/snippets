indexing
	description: "A message to be logged.";
	version: "$Revision: 1.1 $";
	author: "Dustin Sallings <dustin@spy.net>";
	copyright: "2002";
	license: "See forum.txt.";

--
-- Copyright (c) 2002  Dustin Sallings
--
-- $Id: log_msg.e,v 1.1 2002/11/14 08:37:23 dustin Exp $
--
class LOG_MSG

inherit
	LOGGER_CONSTANTS

creation {ANY}
	make

feature {LOG_MSG}
	-- Fields.

	msg: STRING
		-- The log message.

	level: INTEGER
		-- The log level

	timestamp: TIME
		-- The time this log thingy occurred

feature {ANY}
	-- Creation

	make(l: INTEGER; m: STRING) is
		-- Instantiate LOG_MSG
		require
			valid_level: level_is_valid(l)
			valid_message: m /= Void
		local
			ts: TIME
		do
			!!timestamp
			timestamp.update
			level := l
			!!msg.copy(m)
		ensure
			timestamp /= Void
			level /= Void
			msg /= Void
			level = l
			msg.is_equal(m)
		end -- make

feature {LOGGER}
	-- Printing out

	get_string: STRING is
		local
			tie: TIME_IN_ENGLISH
		do
			!!tie
			tie.set_time(timestamp)
			Result := tie.to_string + " "
				+ level_name(level) + ":  "
				+ msg
				+ "%N"
		end -- get_string

end -- class LOG_MSG
