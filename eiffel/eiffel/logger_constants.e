indexing
	description: "";
	version: "$Revision: 1.1 $";
	author: "Dustin Sallings <dustin@spy.net>";
	copyright: "2002";
	license: "See forum.txt.";

--
-- Copyright (c) 2002  Dustin Sallings
--
-- $Id: logger_constants.e,v 1.1 2002/11/12 00:52:00 dustin Exp $
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
			level_is_valid(level);
		do
			if level = debug_level then
				Result := "DEBUG";
			end
			if level = info_level then
				Result := "INFO";
			end
			if level = warn_level then
				Result := "WARN";
			end
			if level = error_level then
				Result := "ERROR";
			end
		ensure
			Result /= Void
		end

	level_is_valid(level: INTEGER): BOOLEAN is
		-- True if the level number represents a valid level number
		do
			Result :=
				   level = debug_level
				or level = info_level
				or level = warn_level
				or level = error_level
		end

end -- class LOGGER_CONSTANTS
