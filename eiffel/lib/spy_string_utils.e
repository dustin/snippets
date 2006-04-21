indexing
   description: "String routines."
   author: "Dustin Sallings <dustin@spy.net>"
   version: "$Revision: 1.3 $"
   copyright: "2002"
   license: "See forum.txt"

class SPY_STRING_UTILS
	-- Extra string utilities.

feature {NONE} -- Private stuff

	split_buffer: ARRAY[STRING] is
		-- Split buffer, for splitting and stuff
		once
			!!Result.with_capacity(4,1)
		end

feature {ANY} -- Splitting

   split_on(s: STRING; on: CHARACTER): ARRAY[STRING] is
      -- split a string on a given character.
	  require
		has_string: s /= Void
      do
         if s.count > 0 then
            split_buffer.clear_count
            split_on_into(s,on,split_buffer)
            if not split_buffer.is_empty then
               Result := split_buffer.twin
            end
         end
      end -- split_on

   split_on_into(s: STRING; on: CHARACTER; words: COLLECTION[STRING]) is
      -- A version of split_on that adds to an existing collection.
      require
         have_string: s /= Void;
         have_array: words /= Void;
      local
         state, i: INTEGER
         c: CHARACTER
		 tmp_string: STRING
      do
         if s.count > 0 then
            from
               i := 1
            until
               i > s.count
            loop
               c := s.item(i)
               if state = 0 then
                  if not (c = on) then
                     !!tmp_string.make(s.count)
                     tmp_string.append_character(c)
                     state := 1
                  end
               else
                  -- state is not 0, looking for the end
                  if c = on then
                     words.add_last(tmp_string)
                     state := 0
                  else
                     -- this is the one for which we are searching
                     tmp_string.append_character(c)
                  end
               end
               i := i + 1
            end
            if state = 1 then
               words.add_last(tmp_string)
            end
         end
      end -- split_on_in

   split_onlist(s: STRING; chars: SET[CHARACTER]): ARRAY[STRING] is
      -- Split a string on a set of characters
	  require
		has_string: s /= Void
		has_char: chars /= Void
      do
         if s.count > 0 then
            split_buffer.clear_count
            split_onlist_into(s, chars, split_buffer)
            if not split_buffer.is_empty then
               Result := split_buffer.twin
            end
         end
      end -- split_onlist

	split_onlist_into(s: STRING; chars: SET[CHARACTER];
		words: COLLECTION[STRING]) is
      -- A version of split_onlist that adds to an existing collection.
      require
         have_string: s /= Void;
         have_characters: chars /= Void;
         have_array: words /= Void;
      local
         state, i: INTEGER
         c: CHARACTER
		 tmp_string: STRING
      do
         if s.count > 0 then
            from
               i := 1
            until
               i > s.count
            loop
               c := s.item(i)
               if state = 0 then
                  if not chars.has(c) then
                     !!tmp_string.make(s.count)
                     tmp_string.append_character(c)
                     state := 1
                  end
               else
                  -- state is not 0, looking for the end
                  if chars.has(c) then
                     words.add_last(tmp_string)
                     state := 0
                  else
                     -- this is the one for which we are searching
                     tmp_string.append_character(c)
                  end
               end
               i := i + 1
            end
            if state = 1 then
               words.add_last(tmp_string)
            end
         end
      end -- split_onlist_in

end -- class SPY_STRING_UTILS
