indexing
	description: "Logger object.";
	version: "$Revision: 1.2 $";
	author: "Dustin Sallings <dustin@spy.net>";
	copyright: "2002";
	license: "See forum.txt.";

--
-- Copyright (c) 2002  Dustin Sallings
--
-- $Id: logger.e,v 1.2 2002/11/25 07:11:00 dustin Exp $
--
class LOGGER

inherit
	LOGGER_CONSTANTS

creation make

feature {NONE}
	-- test

	make is
	do
		debug_msg("This is a test debug.")
		info_msg("This is a test info.")
		warn_msg("This is a test warning.")
		error_msg("This is a test error.")
	end

feature {NONE}
	-- Logging implementation

	log_channel(level: INTEGER): OUTPUT_STREAM is
		-- Get an output stream for the given log level.
		require
			valid_level: level_is_valid(level)
		do
			inspect level
				when debug_level then
					Result := std_output
				when info_level then
					Result := std_output
				when warn_level then
					Result := std_error
				when error_level then
					Result := std_error
			end
		ensure
			got_channel: Result /= Void
		end

	log_msg(level: INTEGER; msg: STRING) is
		-- Log a message at the given level.
		require
			valid_level: level_is_valid(level)
			valid_message: msg /= Void
		local
			msg_ob: LOG_MSG
			stream: OUTPUT_STREAM
		do
			!!msg_ob.make(level, msg)
			stream := log_channel(level)
			stream.put_string(msg_ob.get_string)
		end -- log_msg

feature {ANY}
	-- Logging strings.

	debug_msg(msg: STRING) is
		-- Log a message at debug level.
		do
			log_msg(debug_level, msg)
		end

	info_msg(msg: STRING) is
		-- Log a message at info level.
		do
			log_msg(info_level, msg)
		end

	warn_msg(msg: STRING) is
		-- Log a message at warning level.
		do
			log_msg(warn_level, msg)
		end

	error_msg(msg: STRING) is
		-- Log a message at error level.
		do
			log_msg(error_level, msg)
		end

end -- class LOGGER
