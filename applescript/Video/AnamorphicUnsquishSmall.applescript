-- arch-tag: C64956E2-4737-11D8-95EF-000A957659CC
-- This droplet processes files dropped onto the applet
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
	tell application "QuickTime Player"
		
		activate
		close every movie saving no
		open this_item
		stop movie 1
		select window 1
		set the dimensions of movie 1 to {427, 240}
	end tell
end process_item

