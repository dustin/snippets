set calFileName to (POSIX file "/Users/dustin/stuff/calendar.org") as text

try
	close access file calFileName -- Make sure the file wasn't accidentally left open
end try

set seenEvents to {}

set calFile to open for access file calFileName with write permission
set eof file calFileName to 0 -- clear the file contents

on zeroPrefix(n)
	if n < 10 then
		return "0" & n
	else
		return n
	end if
end zeroPrefix

on timestamp(t)
	set h to t div hours
	set m to t mod hours div minutes
	return zeroPrefix(h) & ":" & zeroPrefix(m)
end timestamp

on formatDate(startDate, endDate)
	set startTime to time of startDate
	set endTime to time of endDate
	
	set {year:y, month:m, day:d} to startDate
	set m to m as integer
	return "<" & y & "-" & zeroPrefix(m) & "-" & zeroPrefix(d) & " " & timestamp(startTime) & " - " & timestamp(endTime) & ">"
end formatDate

on formatAllDay(theDate)
	set {year:y, month:m, day:d} to theDate
	set m to m as integer
	return "<" & y & "-" & zeroPrefix(m) & "-" & zeroPrefix(d) & ">"
end formatAllDay

on isAllDay(startDate, endDate)
	set startTime to time of startDate
	set endTime to time of endDate
	return startDate ­ endDate and startTime = endTime
end isAllDay

on recordAnEvent(startDate, endDate, theSummary, calFile)
	set my seenEvents to my seenEvents & {theSummary, startDate}
	set s to "* " & theSummary & " "
	if isAllDay(startDate, endDate) of me then
		set s to s & formatAllDay(startDate) of me
	else
		set s to s & formatDate(startDate, endDate) of me
	end if
	set s to s & "
"
	-- set t to (get anEvent's description)
	write s to calFile starting at eof
end recordAnEvent

on doCalendar(aCalendar, calFile)
	set oldest to (current date) - (2 * weeks)
	set newest to (current date) + (6 * weeks)
	
	tell application "iCal"
		tell aCalendar
			repeat with anEvent in (every event whose start date > oldest and start date < newest)
				set {start date:startDate, end date:endDate, summary:theSummary} to anEvent
				if {theSummary, startDate} is not in my seenEvents then
					recordAnEvent(startDate, endDate, theSummary, calFile) of me
				end if
			end repeat
			summary of every event
		end tell
	end tell
end doCalendar

tell application "iCal"
	reload calendars
	doCalendar(calendar "Calendar", calFile) of me
	doCalendar(calendar "Google", calFile) of me
end tell

close access file calFileName