property annotations_list : {"Album", "Artist", "Author", "Comment", "Copyright", "Creation Date", "Description", "Director", "Disclaimer", "Full Name", "Host Computer", "Information", "Make", "Model", "Original Format", "Original Source", "Performers", "Producer", "Product", "Software", "Special Playback Requirements", "Warning", "Writer"}
property annot_album : "«none»"
property annot_artist : "«none»"
property annot_author : "«none»"
property annot_comment : "«none»"
property annot_copyright : "«none»"
property annot_creation : "«none»"
property annot_description : "«none»"
property annot_director : "«none»"
property annot_disclaimer : "«none»"
property annot_fullname : "«none»"
property annot_host : "«none»"
property annot_information : "«none»"
property annot_make : "«none»"
property annot_model : "«none»"
property annot_orgformat : "«none»"
property annot_orgsource : "«none»"
property annot_performers : "«none»"
property annot_producer : "«none»"
property annot_product : "«none»"
property annot_software : "«none»"
property annot_specialplay : "«none»"
property annot_warning : "«none»"
property annot_writer : "«none»"

property property_list : {annot_album, annot_artist, annot_author, annot_comment, annot_copyright, annot_creation, annot_description, annot_director, annot_disclaimer, annot_fullname, annot_host, annot_information, annot_make, annot_model, annot_orgformat, annot_orgsource, annot_performers, annot_producer, annot_product, annot_software, annot_specialplay, annot_warning, annot_writer}

-- the list of file types which will be processed
-- eg: {"PICT", "JPEG", "GIFf", "TIFF"}
property type_list : {"MooV"}
-- since file types are optional in Mac OS X, 
-- check the name extension if there is no file type
-- NOTE: do not use periods (.) with the items in the name extensions list
-- eg: {"txt", "text", "jpg", "jpeg"}, NOT: {".txt", ".text", ".jpg", ".jpeg"}
property extension_list : {"mov"}
property protect_name : true


repeat
	display dialog "All Annotations Droplet

A droplet for setting the annotations of QuickTime movie files." buttons {"Set Prefs", "Done"} default button 2
	if the button returned of the result is "Done" then exit repeat
	display dialog "Retain the existing full name of the movie?" buttons {"Cancel", "No", "Yes"} default button 3
	if the button returned of the result is "Yes" then
		set protect_name to true
	else
		set protect_name to false
	end if
	set the values_list to {}
	repeat with i from 1 to the count of the annotations_list
		set this_annotation to item i of the the annotations_list
		set this_property to item i of property_list
		set the display_value to item i of property_list
		repeat
			display dialog "Enter a value for “" & this_annotation & "”:" default answer display_value buttons {"Cancel", "Reset", "Enter"} default button 3
			if the button returned of the result is "Enter" then
				set the end of the values_list to the text returned of the result
				exit repeat
			else
				set the display_value to "«none»"
			end if
		end repeat
	end repeat
	copy the values_list to the property_list
end repeat

-- This droplet processes both files or folders of files dropped onto the applet
on open these_items
	-- this routine uses the gestaltVersion_info() sub-routine
	copy my gestaltVersion_info("qtim", 8) to {QT_version, QT_string}
	if the QT_version is less than "0502" then
		display dialog "This script requires QuickTime 5.0.2 or higher." & ¬
			return & return & "The currently installed version is: " & ¬
			QT_string buttons {"Cancel"} default button 1
	end if
	repeat with i from 1 to the count of these_items
		set this_item to (item i of these_items)
		set the item_info to info for this_item
		if folder of the item_info is true then
			process_folder(this_item)
		else if (alias of the item_info is false) and ¬
			((the file type of the item_info is in the type_list) or ¬
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
		else if (alias of the item_info is false) and ¬
			((the file type of the item_info is in the type_list) or ¬
				the name extension of the item_info is in the extension_list) then
			process_item(this_item)
		end if
	end repeat
end process_folder

-- this sub-routine processes files
on process_item(this_item)
	-- NOTE that the variable this_item is a file reference in alias format
	-- FILE PROCESSING STATEMENTS GOES HERE
	tell application "QuickTime Player"
		launch -- bypasses promo movie
		activate
		my toggle_suppress(true)
		
		stop every movie
		close every movie saving no
		
		try
			open this_item
			tell movie 1
				if saveable is false then
					error "This movie has previously been set so that it cannot be copied, edited, or saved."
				end if
				if protect_name is true then
					try
						set existing_name to the full text of annotation "Full Name"
					on error
						set existing_name to ""
					end try
				end if
				-- remove any existing annotations
				try
					if protect_name is true then
						set the full text of (every annotation whose name is not "Full Name") to ""
					else
						set the full text of every annotation to ""
					end if
				end try
				-- apply the new annotations
				repeat with i from 1 to the count of the annotations_list
					set this_annotation to item i of the annotations_list
					set this_value to item i of the property_list
					if (protect_name is true) and (this_annotation is "Full Name") then
						-- do nothing
					else if this_value contains "«none»" then
						-- do nothing
					else
						set the full text of annotation this_annotation to this_value
					end if
				end repeat
			end tell
			close movie 1 saving yes
		on error error_message
			try
				beep
				display dialog error_message buttons {"Cancel", "Continue"} default button 2 with icon 1
				close movie 1 saving no
			on error
				my toggle_suppress(false)
				error number -128
			end try
		end try
		my toggle_suppress(false)
	end tell
end process_item

on toggle_suppress(status_flag)
	tell application "QuickTime Player"
		set ignore auto play to the status_flag
		set ignore auto present to the status_flag
	end tell
end toggle_suppress

on gestaltVersion_info(gestalt_code, string_length)
	try
		tell application "Finder" to ¬
			copy my NumToHex((system attribute gestalt_code), ¬
				string_length) to {a, b, c, d}
		set the numeric_version to {a, b, c, d} as string
		if a is "0" then set a to ""
		set the version_string to (a & b & "." & c & "." & d) as string
		return {numeric_version, version_string}
	on error
		return {"", "unknown"}
	end try
end gestaltVersion_info

on NumToHex(hexData, stringLength)
	set hexString to {}
	repeat with i from stringLength to 1 by -1
		set hexString to ((hexData mod 16) as string) & hexString
		set hexData to hexData div 16
	end repeat
	return (hexString as string)
end NumToHex
