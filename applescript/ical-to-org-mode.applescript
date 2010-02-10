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

on shortDay(d)
	items 1 through 3 of (weekday of d as text)
end shortDay

on plainDate(dateOb)
	set {year:y, month:m, day:d} to dateOb
	set m to m as integer
	return y & "-" & zeroPrefix(m) & "-" & zeroPrefix(d) & " " & shortDay(dateOb)
end plainDate

on formatDate(startDate, endDate)
	set startTime to time of startDate
	set endTime to time of endDate
	
	return "<" & plainDate(startDate) & " " & timestamp(startTime) & " - " & timestamp(endTime) & ">"
end formatDate

on formatMultiDay(startDate, endDate)
	set wd1 to " " & shortDay(startDate)
	set wd2 to " " & shortDay(endDate)

	return "<" & plainDate(startDate) & ">--<" & plainDate(endDate) & ">"
end formatMultiDay

on isMultiDay(startDate, endDate)
	set {year:y1, month:m1, day:d1} to startDate
	set {year:y2, month:m2, day:d2} to endDate
	return not (y1 = y2 and m1 = m2 and d1 = d2)
end isMultiDay

on formatAllDay(theDate)
	set {year:y, month:m, day:d} to theDate
	set m to m as integer
	return "<" & plainDate(theDate) & ">"
end formatAllDay

on isAllDay(startDate, endDate)
	set {year:y1, month:m1, day:d1} to startDate
	set {year:y2, month:m2, day:d2} to endDate
	return isMultiDay(startDate, endDate) and (endDate - startDate) = 86400
end isAllDay

on recordAnEvent(startDate, endDate, theSummary, calFile)
	set my seenEvents to my seenEvents & {theSummary, startDate}
	set s to "* " & theSummary & " "

	if isAllDay(startDate, endDate) of me then
		set s to s & formatAllDay(startDate) of me
	else if isMultiDay(startDate, endDate) of me then
		set s to s & formatMultiDay(startDate, endDate)
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
			set releventEvents to every event whose start date > oldest and start date < newest
			repeat with anEvent in releventEvents
				set {start date:startDate, end date:endDate, summary:theSummary} to (properties of anEvent)
				if {theSummary, startDate} is not in my seenEvents then
					recordAnEvent(startDate, endDate, theSummary, calFile) of me
				end if
			end repeat
		end tell
	end tell
end doCalendar

tell application "iCal"
	reload calendars
	doCalendar(calendar "Calendar", calFile) of me
	doCalendar(calendar "Google", calFile) of me
end tell

close access file calFileName