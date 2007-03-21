-- arch-tag: CC63B0E4-4737-11D8-B42A-000A957659CC
tell application "iCal" to set cal_names to the title of every calendar whose writable is true

set chosen_calendar to choose from list cal_names with prompt "Select a calendar to clear"

if the chosen_calendar is not false then
	process_calendar_name(chosen_calendar)
end if

-- Process a calendar by name
on process_calendar_name(this_calendar_name)
	tell application "iCal" to set this_calendar to the first calendar whose title is this_calendar_name as string
	
	if this_calendar is not false then
		process_calendar(this_calendar)
	end if
end process_calendar_name

-- Process a calendar
on process_calendar(this_calendar)
	tell application "iCal" to delete every event of this_calendar
end process_calendar
