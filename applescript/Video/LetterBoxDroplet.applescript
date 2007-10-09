property image4x3 : alias "dustinti:Users:dustin:Documents:4x3.png"
property property_list : {image4x3}

-- This makes debugging a bit easier
-- process_item(alias "Macintosh HD:Users:dustin:Desktop:dustin.mov")
-- error "done"

-- If the user just clicks on the script, have them choose a 4x3 file
set f to choose file with prompt "Choose a 4:3 background file (640x480)"
if f is not false then
	set tmp_list to {f}
	copy tmp_list to property_list
end if

-- This droplet processes files dropped onto the applet
on open these_items
	-- Grab the image from the property list
	set image4x3 to the first item of property_list
	
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
		open image4x3
		stop movie 1
		select window 1
		set dims to the dimensions of movie 1
		if dims is not equal to {640, 480} then
			error "The letterboxing image must be 640x480"
		end if
		select all movie 1
		copy movie 1
		close movie 1
		
		open this_item
		stop movie 1
		select window 1
		set dims to the dimensions of movie 1
		add movie 1 with scaled
		set t to track 1 of movie 1
		log result
		repeat with tt in every track of movie 1
			set track_type to kind of tt
			set frame_count to the count of frames in tt
			if frame_count = 1 and track_type = "Video" then
				-- this is the still, stretch it to fit the video
				set dimensions of tt to dims
				set layer of tt to 2
			else if track_type = "Video" then
				-- This is the video
				set layer of tt to 1
				set width to item 1 of dims
				set height to item 2 of dims
				set new_width to width
				set new_height to height
				if new_width is greater than 640 then
					set ratio to 640 / width
					set new_width to width
					set new_height to (height * ratio)
					set trans to {0, ((height - new_height) / 2)}
				else
					-- Don't bother playing with stuff that's not over 640
					-- error "This movie isn't wide enough to be letter boxed."
					-- set ratio to 480 / height
					-- set new_height to height
					-- set new_width to (width * ratio)
					-- set trans to {((width - new_width) / 2), 0}
					
					set ratio to width / 640
					set new_width to width
					set new_height to (height * ratio)
					set trans to {0, ((height - new_height) / 2)}
					
				end if
				set dimensions of tt to {new_width, new_height}
				translate tt by trans
			end if
		end repeat
		select window 1
		select all movie 1
		copy movie 1
		close movie 1 saving no
		make new movie
		paste movie 1
		set dimensions of movie 1 to {640, 480}
	end tell
end process_item

