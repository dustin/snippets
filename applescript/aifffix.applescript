-- arch-tag: D133FFA2-4F8F-11D8-8F1F-00039359E8C6
-- This droplet processes files dropped onto the applet

set f to choose file with prompt "Choose a file to operate on"
if f is not false then
	open {f}
end if


on open these_items
	repeat with i from 1 to the count of these_items
		set this_item to item i of these_items
		set the item_info to info for this_item
		if folder of the item_info is true then
			process_folder(this_item)
		else if (alias of the item_info is false) then
			process_item(this_item)
		end if

	end repeat
end open

on process_folder(this_folder)
	set these_items to list folder this_folder without invisibles
	repeat with i from 1 to the count of these_items
		set this_item to alias ((this_folder as text) & (item i of these_items))
		set the item_info to info for this_item
		if folder of the item_info is true then
			process_folder(this_item)
		else
			process_item(this_item)
		end if
	end repeat
end process_folder

-- this sub-routine processes files
on process_item(this_item)
	tell application "Finder"
		set processed_dir to the folder "Processed" of the container of this_item
		set orig_dir to the folder "Originals" of the container of this_item
		set this_name to the displayed name of this_item
		set that_item to ((processed_dir as text) & this_name)

	end tell
	tell application "QuickTime Player"

		activate
		close every movie saving no
		open this_item
		stop movie 1
		select window 1
		-- display dialog "Processing " & this_item & " to " & (that_item as text)
		export movie 1 to that_item as AIFF
	end tell
	-- Save the original
	tell application "Finder" to move this_item to orig_dir
end process_item

