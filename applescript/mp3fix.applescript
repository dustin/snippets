-- the list of file types which will be processed
-- arch-tag: 7F5FC98F-4736-11D8-938B-000A957659CC

-- eg: {"PICT", "JPEG", "TIFF", "GIFf"}
property type_list : {}
-- since file types are optional in Mac OS X,
-- check the name extension if there is no file type
-- NOTE: do not use periods (.) with the items in the name extensions list
-- eg: {"txt", "text", "jpg", "jpeg"}, NOT: {".txt", ".text", ".jpg", ".jpeg"}
property extension_list : {"mp3"}

-- This droplet processes both files or folders of files dropped onto the applet
on open these_items
	repeat with i from 1 to the count of these_items
		set this_item to (item i of these_items)
		set the item_info to info for this_item
		if folder of the item_info is true then
			process_folder(this_item)
		else if (alias of the item_info is false) and Â
			((the file type of the item_info is in the type_list) or Â
				the name extension of the item_info is in the extension_list) then
			process_item(this_item)
		end if
	end repeat
end open

-- this sub-routine processes folders
on process_folder(this_folder)
	set these_items to list folder this_folder without invisibles
	repeat with i from 1 to the count of these_items
		set this_item to alias ((this_folder as text) & (item i of these_items))
		set the item_info to info for this_item
		if folder of the item_info is true then
			process_folder(this_item)
		else if (alias of the item_info is false) and Â
			((the file type of the item_info is in the type_list) or Â
				the name extension of the item_info is in the extension_list) then
			process_item(this_item)
		end if
	end repeat
end process_folder

-- this sub-routine processes files
on process_item(this_item)
	-- NOTE that the variable this_item is a file reference in alias format
	-- FILE PROCESSING STATEMENTS GOES HERE
	-- type: "MPG3"
	-- creator: "hook"
	display dialog "Working on " & this_item

	tell application "Finder"
		set the creator type of this_item to "hook"
		set the file type of this_item to "MPG3"
	end tell
end process_item
