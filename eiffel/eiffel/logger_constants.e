indexing
	description: "";
	version: "$Revision: 1.2 $";
	author: "Dustin Sallings <dustin@spy.net>";
	copyright: "2002";
	license: "See forum.txt.";

--
-- Copyright (c) 2002  Dustin Sallings
--
-- $Id: logger_constants.e,v 1.2 2002/11/14 08:36:31 dustin Exp $
--
expanded class LOGGER_CONSTANTS

feature {LOGGER}
	-- Constants for logging.

	debug_level: INTEGER is 7
	info_level: INTEGER is 9
	warn_level: INTEGER is 11
	error_level: INTEGER is 13

feature {ANY}
	-- Asking about the logger constants.

	level_name(level: INTEGER): STRING is
		-- Get the name for the integer level.
		require
			valid_level: level_is_valid(level)
		do
			inspect level
				when debug_level then
					Result := "DEBUG"
				when info_level then
					Result := "INFO"
				when warn_level then
					Result := "WARN"
				when error_level then
					Result := "ERROR"
			end
		ensure
			Result /= Void
		end

	level_is_valid(level: INTEGER): BOOLEAN is
		-- True if the level number represents a valid level number
		require
			non_void_level: level /= Void
		do
			inspect level
				when debug_level, info_level, warn_level, error_level then
					Result := true
				else
					Result := false
			end
		end

end -- class LOGGER_CONSTANTS
