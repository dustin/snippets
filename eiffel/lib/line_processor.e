indexing
	description: "";
	version: "$Revision: 1.1 $";
	author: "Dustin Sallings <dustin@spy.net>";
	copyright: "2002";
	license: "See forum.txt.";

--
-- Copyright (c) 2002  Dustin Sallings
--
-- $Id: line_processor.e,v 1.1 2002/12/08 10:06:37 dustin Exp $
--
deferred class LINE_PROCESSOR
	-- Process input, line by line.

feature {NONE}

	line_input_stream: INPUT_STREAM
		-- The input stream

	line_number: INTEGER
		-- The current line number

feature {LINE_PROCESSOR}
	-- Features for subclasses

	has_input_stream: BOOLEAN is
		do
			Result := line_input_stream /= Void
		end

	set_input_stream(stream: INPUT_STREAM) is
		-- Set the input stream to process.
		require
			valid_stream: stream /= Void
		do
			line_input_stream := stream
			line_number := 1
		ensure
			has_input_stream
		end

	process_input is
		-- This is called to begin the processing of the input stream.
		require
			has_input_stream
		do
			from
				line_input_stream.read_line
			until
				line_input_stream.end_of_input
			loop
				-- Process the current line
				process_line(line_input_stream.last_string)
				-- Get the next line
				line_input_stream.read_line
				line_number := line_number + 1
			end
		end

	process_line(line: STRING) is
		-- Process this line of the file.
		require
			has_line: line /= Void
			has_line_number: line_number /= Void
		deferred
		end

end -- class LINE_PROCESSOR
