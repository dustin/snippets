indexing
	description: "A message to be logged.";
	version: "$Revision: 1.2 $";
	author: "Dustin Sallings <dustin@spy.net>";
	copyright: "2002";
	license: "See forum.txt.";

-- 
-- Copyright (c) 2002  Dustin Sallings
--
-- $Id: log_msg.e,v 1.2 2002/11/25 07:19:15 dustin Exp $
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
			msg /= Void
			level = l
			msg.is_equal(m)
		end -- make

feature {LOGGER}
	-- Printing out

	two_digit_number(n: INTEGER): STRING is
		require
			non_negative: n >= 0
			less_than_100: n < 100
		do
			if n < 10 then
				Result := "0" + n.to_string
			else
				Result := n.to_string
			end
		end

	get_string: STRING is
		do
			Result :=
				timestamp.year.to_string + "-"
				+ two_digit_number(timestamp.month) + "-"
				+ two_digit_number(timestamp.day) + "T"
				+ two_digit_number(timestamp.hour) + ":"
				+ two_digit_number(timestamp.minute) + ":"
				+ two_digit_number(timestamp.second)
				+ " "
				+ level_name(level) + ":  "
				+ msg
				+ "%N"
		end -- get_string

end -- class LOG_MSG
