-- MacGPSBabel: MacGPSBabel.applescript

--  File created by Jeremy Atherton on Sunday, September 28, 2003.
--  Last modified by Jeremy Atherton on Tuesday, September 7, 2004.

--  MacGPSBabel is part of the gpsbabel project and is Copyright (c) 2004 Robert Lipe.
-- see http://gpsbabel.sourceforge.net/ for more details

-- PROPERTIES AND GLOBALS --
property fileList : {}
property startIndex : 0
property startState : false
global theFiles, typeList, extList, aFile

-- EVENT HANDLERS --

-- Start up scripts

-- make empty entries in user defaults
on will finish launching theObject
	make new default entry at end of default entries of user defaults with properties {name:"theInputType", contents:startIndex}
	make new default entry at end of default entries of user defaults with properties {name:"theOutputType", contents:startIndex}
	make new default entry at end of default entries of user defaults with properties {name:"gpsIN", contents:startState}
	make new default entry at end of default entries of user defaults with properties {name:"gpsOUT", contents:startState}
	make new default entry at end of default entries of user defaults with properties {name:"gpsReceiver", contents:startIndex}
end will finish launching

on awake from nib theObject
	if theObject is window "MacGPSBabel" then
		
		-- get supported file types from gpsbabel and use these to populate the file types popup lists
		tell window "MacGPSBabel"
			set popList to my getFormats()
			repeat with i in popList
				make new menu item at the end of menu items of menu of popup button "inPop" with properties {title:i, enabled:true}
				make new menu item at the end of menu items of menu of popup button "outPop" with properties {title:i, enabled:true}
			end repeat
		end tell
		
		-- read current user defaults and set window controls as needed
		my readSettings()
		
		-- deal with changes to MacGPSBabel window needed if any of the GPS check boxes are checked by default
		if state of button "GPSswitchIN" of window "MacGPSBabel" is equal to 1 then
			my gpsIN()
		end if
		if state of button "GPSswitchOUT" of window "MacGPSBabel" is equal to 1 then
			my gpsOUT()
		end if
	end if
end awake from nib

-- Deal with the opening and closing of windows

on will open theObject
	
	-- Main Window
	if theObject is window "MacGPSBabel" then
		-- set the progress indicator style
		set p to progress indicator 1 of theObject
		call method "setStyle:" of p with parameter 1
		call method "setDisplayedWhenStopped:" of p with parameters {false}
	end if
	
	-- Select GPS Window
	if theObject is window "SelectGPS" then
		-- get the list of available serial ports
		set popList to my getSerial()
		-- use popList to populate the drop-down menu
		delete every menu item of menu of popup button "serialPop" of window "SelectGPS"
		repeat with i in popList
			make new menu item at the end of menu items of menu of popup button "serialPop" of window "SelectGPS" with properties {title:i, enabled:true}
		end repeat
		
		-- read user defaults for this window
		tell user defaults
			set defaultgpsReceiver to contents of default entry "gpsReceiver"
		end tell
		set state of popup button "gpsPop" of window "SelectGPS" to defaultgpsReceiver
		
		-- hide MacGPSBabel window
		set visible of window "MacGPSBabel" to false
	end if
	
end will open

on will close theObject
	if theObject is window "SelectGPS" then
		-- store user defaults for this window
		set newReceiverIndex to contents of popup button "gpsPop" of window "SelectGPS" as integer
		tell user defaults
			set contents of default entry "gpsReceiver" to newReceiverIndex
		end tell
		
		-- unhide MacGPSBabel window
		set visible of window "MacGPSBabel" to true
	end if
end will close


-- handler for the File>Open menu item
on choose menu item theObject
	if name of theObject is "open" then
		if visible of window "MacGPSBabel" is true then
			if contents of text field "inputFile" of window "MacGPSBabel" is equal to "" then
				my selectFile()
				return 0
			else if the title of current menu item of popup button "inPop" of window "MacGPSBabel" = "Select Input File Type" then
				display dialog "Please select an input file type for the previous file before adding another file" buttons {"OK"} default button 1
				return 0
			else if item 1 of (the last item in fileList) is not equal to (contents of text field "inputFile" of window "MacGPSBabel") then
				set the end of fileList to {contents of text field "inputFile" of window "MacGPSBabel", contents of popup button "inPop" of window "MacGPSBabel"}
			end if
			my addFile()
		else
			set visible of window "MacGPSBabel" to true
			if contents of text field "inputFile" of window "MacGPSBabel" is equal to "" then
				my selectFile()
				return 0
			else if the title of current menu item of popup button "inPop" of window "MacGPSBabel" = "Select Input File Type" then
				display dialog "Please select an input file type for the previous file before adding another file" buttons {"OK"} default button 1
				return 0
			else if item 1 of (the last item in fileList) is not equal to (contents of text field "inputFile" of window "MacGPSBabel") then
				set the end of fileList to {contents of text field "inputFile" of window "MacGPSBabel", contents of popup button "inPop" of window "MacGPSBabel"}
			end if
			my addFile()
		end if
	end if
end choose menu item

-- HANDLERS FOR BUTTON CLICKS

on clicked theObject
	-- MAIN WINDOW - Select File button
	if theObject is the button "selectButton" of window "MacGPSBabel" then
		if contents of text field "inputFile" of window "MacGPSBabel" is equal to "" then
			my selectFile()
			return 0
		else if the title of current menu item of popup button "inPop" of window "MacGPSBabel" = "Select Input File Type" then
			display dialog "Please select an input file type for the previous file before adding another file" buttons {"OK"} default button 1
			return 0
		else if fileList is equal to {} or item 1 of (the last item in fileList) is not equal to (contents of text field "inputFile" of window "MacGPSBabel") then
			set the end of fileList to {contents of text field "inputFile" of window "MacGPSBabel", contents of popup button "inPop" of window "MacGPSBabel"}
		end if
		my addFile()
	end if
	
	-- MAIN WINDOW - Clear button
	if theObject is the button "clearButton" of window "MacGPSBabel" then
		my clearFiles()
	end if
	
	-- MAIN WINDOW - Send button
	if theObject is the button "sendButton" of window "MacGPSBabel" then
		if state of button "GPSswitchIN" of window "MacGPSBabel" = 1 then
			set fileList to {}
		else if fileList is equal to {} or item 1 of (the last item in fileList) is not equal to (contents of text field "inputFile" of window "MacGPSBabel") then
			set the end of fileList to {contents of text field "inputFile" of window "MacGPSBabel", contents of popup button "inPop" of window "MacGPSBabel"}
		end if
		my sendFile(fileList)
	end if
	
	-- MAIN WINDOW - Use GPS radio buttons
	if theObject is the button "GPSswitchIN" of window "MacGPSBabel" then
		my GPSSwitchIN()
		if (state of button "GPSswitchIN" of window "MacGPSBabel" = 1) and (state of button "GPSswitchOUT" of window "MacGPSBabel" = 1) then
			set state of button "GPSswitchOUT" of window "MacGPSBabel" to 0
			my GPSswitchOUT()
		end if
	end if
	if theObject is the button "GPSswitchOUT" of window "MacGPSBabel" then
		my GPSswitchOUT()
		if (state of button "GPSswitchOUT" of window "MacGPSBabel" = 1) and (state of button "GPSswitchIN" of window "MacGPSBabel" = 1) then
			set state of button "GPSswitchIN" of window "MacGPSBabel" to 0
			my GPSSwitchIN()
		end if
	end if
	
	-- MAIN WINDOW - Filters button
	if theObject is the button "filterButton" of window "MacGPSBabel" then
		my showFilters()
	end if
	
	-- MAIN WINDOW - Set As Defaults Button
	if theObject is button "defaultsButton" of window "MacGPSBabel" then
		set newInputIndex to contents of popup button "inPop" of window "MacGPSBabel"
		set newOutputIndex to contents of popup button "outPop" of window "MacGPSBabel"
		set newINstate to state of button "GPSswitchIN" of window "MacGPSBabel" as boolean
		set newOUTstate to state of button "GPSswitchOUT" of window "MacGPSBabel" as boolean
		tell user defaults
			set contents of default entry "theInputType" to newInputIndex
			set contents of default entry "theOutputType" to newOutputIndex
			set contents of default entry "gpsIN" to newINstate
			set contents of default entry "gpsOUT" to newOUTstate
		end tell
	end if
	
	-- GPS Receiver Window - Continue button
	if theObject is the button "contButton" of window "SelectGPS" then
		if the state of button "GPSswitchIN" of window "MacGPSBabel" = 1 then
			my downloadFile()
		else
			my uploadFile(fileList)
		end if
	end if
	
	-- GPS Receiver Window - Cancel button
	if theObject is the button "cancelButton" of window "SelectGPS" then
		close window "SelectGPS"
	end if
	
	-- Filter Window - Distance filter check box
	if theObject is the button "distanceFilter" of window "filterWindow" then
		if state of button "distanceFilter" of window "filterWindow" is equal to 1 then
			set enabled of text field "dist1" of window "filterWindow" to true
			set editable of text field "dist1" of window "filterWindow" to true
			set enabled of popup button "dist1Select" of window "filterWindow" to true
			tell window "filterWindow"
				set first responder to text field "dist1"
			end tell
		else
			set enabled of text field "dist1" of window "filterWindow" to false
			set editable of text field "dist1" of window "filterWindow" to false
			set enabled of popup button "dist1Select" of window "filterWindow" to false
		end if
	end if
	
	-- Filter Window - Radius Filter check box
	if theObject is the button "radiusFilter" of window "filterWindow" then
		if state of button "radiusFilter" of window "filterWindow" is equal to 1 then
			set enabled of text field "dist2" of window "filterWindow" to true
			set editable of text field "dist2" of window "filterWindow" to true
			set enabled of popup button "dist2Select" of window "filterWindow" to true
			set enabled of popup button "nsSelect" of window "filterWindow" to true
			set enabled of popup button "ewSelect" of window "filterWindow" to true
			set enabled of text field "nDeg" of window "filterWindow" to true
			set editable of text field "nDeg" of window "filterWindow" to true
			set enabled of text field "nMin" of window "filterWindow" to true
			set editable of text field "nMin" of window "filterWindow" to true
			set enabled of text field "wDeg" of window "filterWindow" to true
			set editable of text field "wDeg" of window "filterWindow" to true
			set enabled of text field "wMin" of window "filterWindow" to true
			set editable of text field "wMin" of window "filterWindow" to true
			tell window "filterWindow"
				set first responder to text field "dist2"
			end tell
		else
			set enabled of text field "dist2" of window "filterWindow" to false
			set editable of text field "dist2" of window "filterWindow" to false
			set enabled of popup button "dist2Select" of window "filterWindow" to false
			set enabled of popup button "nsSelect" of window "filterWindow" to false
			set enabled of popup button "ewSelect" of window "filterWindow" to false
			set enabled of text field "nDeg" of window "filterWindow" to false
			set editable of text field "nDeg" of window "filterWindow" to false
			set enabled of text field "nMin" of window "filterWindow" to false
			set editable of text field "nMin" of window "filterWindow" to false
			set enabled of text field "wDeg" of window "filterWindow" to false
			set editable of text field "wDeg" of window "filterWindow" to false
			set enabled of text field "wMin" of window "filterWindow" to false
			set editable of text field "wMin" of window "filterWindow" to false
		end if
	end if
	
	-- Filter Window - Arc filter check box
	if theObject is the button "arcSwitch" of window "filterWindow" then
		if state of button "arcSwitch" of window "filterWindow" is equal to 1 then
			try
				set fFile to choose file with prompt "Select an arc filter file"
			on error
				set state of button "arcSwitch" of window "filterWindow" to 0
				return 0
			end try
			set contents of text field "arcFile" of window "filterWindow" to POSIX path of fFile as string
			set enabled of text field "arcDist" of window "filterWindow" to true
			set editable of text field "arcDist" of window "filterWindow" to true
			set enabled of popup button "arcUnits" of window "filterWindow" to true
		else
			set contents of text field "arcFile" of window "filterWindow" to ""
			set contents of text field "arcDist" of window "filterWindow" to ""
			set enabled of text field "arcDist" of window "filterWindow" to false
			set editable of text field "arcDist" of window "filterWindow" to false
			set enabled of popup button "arcUnits" of window "filterWindow" to false
		end if
	end if
	
	-- Filter Window - polygon filter check box
	if theObject is the button "polySwitch" of window "filterWindow" then
		if state of button "polySwitch" of window "filterWindow" is equal to 1 then
			try
				set pFile to choose file with prompt "Select a polygon filter file"
			on error
				set state of button "polySwitch" of window "filterWindow" to 0
				return 0
			end try
			set contents of text field "polyFile" of window "filterWindow" to POSIX path of pFile as string
		else
			set contents of text field "polyFile" of window "filterWindow" to ""
		end if
	end if
	
	-- Filter Window - Smart names check box
	if theObject is the button "smartSwitch" of window "filterWindow" then
		if state of button "smartSwitch" of window "filterWindow" is equal to 1 then
			set enabled of text field "smartLen" of window "filterWindow" to true
			set editable of text field "smartLen" of window "filterWindow" to true
			set contents of text field "smartLen" of window "filterWindow" to ""
		else
			set enabled of text field "smartLen" of window "filterWindow" to false
			set editable of text field "smartLen" of window "filterWindow" to false
		end if
	end if
	
	--debug mode
	if theObject is the button "debugButton" of window "MacGPSBabel" then
		if (visible of window "debugWindow") is false then
			set visible of window "debugWindow" to true
		else
			set visible of window "debugWindow" to false
		end if
	end if
	if theObject is the button "executeButton" of window "debugWindow" then
		set theScript to contents of text field "debugInput" of window "debugWindow"
		if theScript starts with "gpsbabel" then
			set thePath to quoted form of (POSIX path of (path to me) as string) & "Contents/Resources/"
			set theScript to thePath & theScript
		end if
		set theOutput to do shell script theScript as string
		set the contents of text view 1 of scroll view 1 of window "debugWindow" to ""
		set the contents of text view 1 of scroll view 1 of window "debugWindow" to theOutput
	end if
	
end clicked


-- MY HANDLERS --


-- SCRIPTS FOR CHOOSING THE INPUT FILE
-- select the first file
on selectFile()
	-- Choose a file (using the open file dialog box)
	set aFile to choose file with prompt "Select an input file"
	set contents of text field "inputFile" of window "MacGPSBabel" to aFile
	if contents of text field "inputFile" of window "MacGPSBabel" is not equal to "" then
		set key equivalent of button "selectButton" of window "MacGPSBabel" to ""
		set enabled of button "sendButton" of window "MacGPSBabel" to true
		set enabled of button "clearButton" of window "MacGPSBabel" to true
		set the title of button "selectButton" of window "MacGPSBabel" to "Add File"
	end if
end selectFile
-- add another file
on addFile()
	-- Choose a file (using the open file dialog box)
	set aFile to choose file with prompt "Select another input file"
	set contents of text field "inputFile" of window "MacGPSBabel" to aFile
	set the contents of popup button "inPop" of window "MacGPSBabel" to 0
end addFile

-- SCRIPTS FOR CONTROLLING THE CONVERSION
-- work out which kind of conversion to do
on sendFile(fileList)
	-- check for options selected
	if state of button "GPSswitchIN" of window "MacGPSBabel" = 1 then
		my GPSSend()
		return 0
	else if state of button "GPSswitchOUT" of window "MacGPSBabel" = 1 then
		set visible of window "SelectGPS" to true
		set state of button "trackSwitch" of window "SelectGPS" to 0
		set enabled of button "trackSwitch" of window "SelectGPS" to false
		return 0
	else if the title of current menu item of popup button "inPop" of window "MacGPSBabel" = "Select Input File Type" then
		display dialog "Please select an input file type" buttons {"OK"} default button 1
		return 0
	else if the title of current menu item of popup button "outPop" of window "MacGPSBabel" = "Select Output File Type" then
		display dialog "Please select an output file type" buttons {"OK"} default button 1
		return 0
	end if
	
	-- select where to save the file
	my convertFile(fileList)
end sendFile

-- this script handles conversions between file types
on convertFile(fileList)
	-- create string for filters
	if visible of window "filterWindow" is true then
		set filterText to my applyFilters()
	else
		set filterText to ""
	end if
	
	-- create string for input files
	set fileText to ""
	repeat with theItem in fileList
		set currentInIndex to item 2 of theItem
		set inType to item (currentInIndex) of typeList
		set inputFile to quoted form of item 1 of theItem
		set fileText to fileText & " -i " & inType & " -f " & inputFile
	end repeat
	
	-- create strings for output file
	set currentOutIndex to contents of popup button "outPop" of window "MacGPSBabel"
	set outType to item (currentOutIndex) of typeList
	if visible of window "filterWindow" is true then
		if state of button "smartSwitch" of window "filterWindow" is equal to 1 then
			set smartSwitch to " -s"
			if contents of text field "smartLen" of window "filterWindow" is not equal to "" then
				set outType to outType & ",snlen=" & ((contents of text field "smartLen" of window "filterWindow") as integer) & " "
			end if
		else
			set smartSwitch to ""
		end if
	else
		set smartSwitch to ""
	end if
	set OutExt to item (currentOutIndex) of extList
	-- set outPath to directory of aFile
	tell save panel
		set title to "Save Output As"
		set prompt to "Save"
		set treat packages as directories to 0
	end tell
	set oldDelimiters to AppleScript's text item delimiters
	set AppleScript's text item delimiters to "/"
	set TempFileName to last text item of inputFile
	set AppleScript's text item delimiters to "."
	set TempFileName to the first text item of TempFileName
	set AppleScript's text item delimiters to oldDelimiters
	if OutExt is not equal to "" then
		set TempFileName to TempFileName & "." & OutExt
	end if
	set theResult to display save panel in directory aFile with file name TempFileName
	if theResult is 1 then
		set outputFile to (path name of save panel) as string
	else
		set outputFile to ""
		display dialog "conversion cancelled" buttons {"OK"} default button 1
		return 0
	end if
	feedbackBusy(true)
	-- do the script
	set thePath to POSIX path of (path to me) as string
	set theConvertScript to (quoted form of thePath & "Contents/Resources/gpsbabel" & smartSwitch & fileText & " " & filterText & "-o " & outType & " -F " & quoted form of outputFile) as string
	try
		set scriptOut to do shell script theConvertScript as string
		set convertYN to "Conversion Completed Successfully"
	on error
		set scriptOut to "gpsbabel encountered an error"
		set convertYN to "Conversion Failed!"
	end try
	feedbackBusy(false)
	display dialog convertYN buttons {"OK"} default button 1
	if visible of window "debugWindow" is true then
		set the contents of text view 1 of scroll view 1 of window "debugWindow" to ""
		set the contents of text view 1 of scroll view 1 of window "debugWindow" to "MacGPSBabel Report" & return & return & "The Shell Script:" & return & theConvertScript & return & return & convertYN & return & return & "Output From gpsbabel:" & return & scriptOut
	end if
	my clearFiles()
end convertFile

-- GPS RECEIVER HANDLERS
-- open the GPS receiver window
on GPSSend()
	if the title of current menu item of popup button "outPop" of window "MacGPSBabel" = "Select Output File Type" then
		display dialog "Please select an output file type" buttons {"OK"} default button 1
	else
		set visible of window "SelectGPS" to true
		set enabled of button "trackSwitch" of window "selectGPS" to true
	end if
end GPSSend
-- deal with uploading files to GPS receiver
on uploadFile(fileList)
	-- create string for filters
	if visible of window "filterWindow" is true then
		set filterText to my applyFilters()
	else
		set filterText to ""
	end if
	
	-- create string for input files
	set fileText to ""
	repeat with theItem in fileList
		set currentInIndex to item 2 of theItem
		set inType to item (currentInIndex) of typeList
		set inputFile to quoted form of item 1 of theItem
		set fileText to fileText & " -i " & inType & " -f " & inputFile
	end repeat
	
	-- create string for GPS unit
	if the title of popup button "gpsPop" of window "selectGPS" is equal to "Garmin" then
		set gpsText to " garmin "
	else
		set gpsText to " magellan "
	end if
	if visible of window "filterWindow" is true then
		if state of button "smartSwitch" of window "filterWindow" is equal to 1 then
			set smartSwitch to " -s"
			if contents of text field "smartLen" of window "filterWindow" is not equal to "" then
				set gpsText to gpsText & ",snlen=" & ((contents of text field "smartLen" of window "filterWindow") as integer) & " "
			end if
		else
			set smartSwitch to ""
		end if
	else
		set smartSwitch to ""
	end if
	
	-- run the script
	set thePath to POSIX path of (path to me) as string
	set visible of window "SelectGPS" to false
	set visible of window "MacGPSBabel" to true
	feedbackBusy(true)
	set serialText to "-F /dev/cu." & (the title of popup button "serialPop" of window "selectGPS")
	do shell script (quoted form of thePath & "Contents/Resources/gpsbabel" & smartSwitch & fileText & " " & filterText & "-o " & gpsText & serialText)
	feedbackBusy(false)
	display dialog "Upload Complete" buttons {"OK"} default button 1
end uploadFile
-- deal with downloading files from GPS receiver
on downloadFile()
	set outName to "Waypoints."
	if visible of window "filterWindow" is true then
		set filterText to my applyFilters()
	else
		set filterText to ""
	end if
	if state of button "trackSwitch" of window "selectGPS" is equal to 1 then
		set trackText to " -t"
		set outName to "Tracks."
	else
		set trackText to ""
	end if
	set thePath to POSIX path of (path to me) as string
	
	tell save panel
		set title to "Save Output As"
		set prompt to "Save"
		set treat packages as directories to 0
	end tell
	
	set currentOutIndex to contents of popup button "outPop" of window "MacGPSBabel"
	set outType to item (currentOutIndex) of typeList
	if visible of window "filterWindow" is true then
		if state of button "smartSwitch" of window "filterWindow" is equal to 1 then
			set smartSwitch to " -s"
			if contents of text field "smartLen" of window "filterWindow" is not equal to "" then
				set outType to outType & ",snlen=" & ((contents of text field "smartLen" of window "filterWindow") as integer) & " "
			end if
		else
			set smartSwitch to ""
		end if
	else
		set smartSwitch to ""
	end if
	set OutExt to item (currentOutIndex) of extList
	set TempFileName to outName & OutExt
	set theResult to display save panel in directory "~/Desktop" with file name TempFileName
	if theResult is 1 then
		set outputFile to (path name of save panel) as string
		if the title of popup button "gpsPop" of window "selectGPS" is equal to "Garmin" then
			set gpsText to " garmin "
		else
			set gpsText to " magellan "
		end if
		set serialText to "/dev/cu." & (the title of popup button "serialPop" of window "selectGPS")
		set visible of window "SelectGPS" to false
		set visible of window "MacGPSBabel" to true
		feedbackBusy(true)
		do shell script (quoted form of thePath & "Contents/Resources/gpsbabel" & smartSwitch & trackText & " -i" & gpsText & "-f " & serialText & " " & filterText & " -o " & outType & " -F " & quoted form of outputFile)
		feedbackBusy(false)
		display dialog "Download from GPS is complete" buttons {"OK"} default button 1
	else
		set outputFile to ""
	end if
end downloadFile

-- FILTERING HANDLERS
-- show filters window
on showFilters()
	if visible of window "filterWindow" is false then
		set visible of window "filterWindow" to true
		set the title of button "filterButton" of window "MacGPSBabel" to "Remove Filters"
	else
		set visible of window "filterWindow" to false
		set the title of button "filterButton" of window "MacGPSBabel" to "Setup Filters"
	end if
end showFilters
-- create the filter code
on applyFilters()
	set filterText to ""
	if state of button "distanceFilter" of window "filterWindow" is equal to 1 then
		set distanceText to "-x position"
		if contents of text field "dist1" of window "filterWindow" is not equal to "" then
			set distanceText to distanceText & ",distance=" & (contents of text field "dist1" of window "filterWindow")
			if title of popup button "dist1Select" of window "filterWindow" is equal to "Feet" then
				set distanceText to distanceText & "f "
			else
				set distanceText to distanceText & "m "
			end if
		end if
	else
		set distanceText to ""
	end if
	if state of button "radiusFilter" of window "filterWindow" is equal to 1 then
		set radiusText to "-x radius"
		if contents of text field "dist2" of window "filterWindow" is not equal to "" then
			set radiusText to radiusText & ",distance=" & (contents of text field "dist2" of window "filterWindow")
			if title of popup button "dist2Select" of window "filterWindow" is equal to "Miles" then
				set radiusText to radiusText & "M"
			else
				set radiusText to radiusText & "K"
			end if
			if the title of current menu item of popup button "nsSelect" of window "filterWindow" = "N" then
				set lat to 1
			else
				set lat to -1
			end if
			if the title of current menu item of popup button "ewSelect" of window "filterWindow" = "W" then
				set lon to -1
			else
				set lon to 1
			end if
			set latDeg to lat * (((the contents of text field "nDeg" of window "filterWindow") as number) + (((the contents of text field "nMin" of window "filterWindow") as number) / 60)) as string
			set lonDeg to lon * ((the contents of text field "wDeg" of window "filterWindow") + ((the contents of text field "wMin" of window "filterWindow") / 60)) as string
			set radiusText to radiusText & ",lat=" & latDeg & ",lon=" & lonDeg & " "
		end if
	else
		set radiusText to ""
	end if
	if state of button "locationFilter" of window "filterWindow" is equal to 1 then
		set duplicateText to "-x duplicate,location"
		if state of button "shortFilter" of window "filterWindow" is not equal to 1 then
			set duplicateText to duplicateText & " "
		end if
	else
		set duplicateText to ""
	end if
	if state of button "shortFilter" of window "filterWindow" is equal to 1 then
		if duplicateText is not equal to "" then
			set duplicateText to duplicateText & ",shortname "
		else
			set duplicateText to "-x duplicate,shortname "
		end if
	end if
	if state of button "arcSwitch" of window "filterWindow" is equal to 1 then
		if (contents of text field "arcDist" of window "filterWindow" is not equal to "") then
			if title of popup button "arcUnits" of window "filterWindow" is equal to "Miles" then
				set aUnit to "M"
			else
				set aUnit to "K"
			end if
			set arcDistance to (contents of text field "arcDist" of window "filterWindow") & aUnit
			set arcText to "-x arc,file=\"" & (contents of text field "arcFile" of window "filterWindow") & "\",distance=" & arcDistance & " "
		else
			display dialog "Please input a distance for the arc filter" buttons ["OK"] default button 1
			set arcText to ""
			break
		end if
	else
		set arcText to ""
	end if
	if state of button "polySwitch" of window "filterWindow" is equal to 1 then
		set polyText to "-x polygon,file=\"" & (contents of text field "arcFile" of window "filterWindow") & "\" "
	else
		set polyText to ""
	end if
	set filterText to distanceText & radiusText & duplicateText & arcText & polyText
	return filterText
end applyFilters

-- handlers to deal with the GPS receiver checkboxes
on GPSSwitchIN()
	if state of button "GPSswitchIN" of window "MacGPSBabel" = 1 then
		set enabled of button "selectButton" of window "MacGPSBabel" to false
		set enabled of button "clearButton" of window "MacGPSBabel" to false
		set enabled of button "sendButton" of window "MacGPSBabel" to true
		set contents of text field "inputFile" of window "MacGPSBabel" to ""
		set enabled of text field "inputFile" of window "MacGPSBabel" to false
		set enabled of popup button "inPop" of window "MacGPSBabel" to false
		set title of button "sendButton" of window "MacGPSBabel" to "Download..."
	else
		set enabled of button "selectButton" of window "MacGPSBabel" to true
		set enabled of button "sendButton" of window "MacGPSBabel" to false
		set enabled of text field "inputFile" of window "MacGPSBabel" to true
		set enabled of popup button "inPop" of window "MacGPSBabel" to true
	end if
	if state of button "GPSswitchIN" of window "MacGPSBabel" = 0 and state of button "GPSswitchOUT" of window "MacGPSBabel" = 0 then
		set title of button "sendButton" of window "MacGPSBabel" to "Convert"
	end if
end GPSSwitchIN
on GPSswitchOUT()
	if state of button "GPSswitchOUT" of window "MacGPSBabel" = 1 then
		set enabled of popup button "outPop" of window "MacGPSBabel" to false
		set title of button "sendButton" of window "MacGPSBabel" to "Upload..."
	else
		set enabled of popup button "outPop" of window "MacGPSBabel" to true
	end if
	if state of button "GPSswitchIN" of window "MacGPSBabel" = 0 and state of button "GPSswitchOUT" of window "MacGPSBabel" = 0 then
		set title of button "sendButton" of window "MacGPSBabel" to "Convert"
	end if
end GPSswitchOUT

-- start/stop the Main window's progress indicator
on feedbackBusy(yn)
	tell window "MacGPSBabel"
		if yn then
			start progress indicator 1
		else
			stop progress indicator 1
		end if
	end tell
end feedbackBusy

on clearFiles()
	set contents of text field "inputFile" of window "MacGPSBabel" to ""
	set fileList to {}
	set title of button "selectButton" of window "MacGPSBabel" to "Select File"
	set enabled of button "sendButton" of window "MacGPSBabel" to false
	set key equivalent of button "selectButton" of window "MacGPSBabel" to return
	set enabled of button "clearButton" of window "MacGPSBabel" to false
	
	-- reset controls to user defaults
	-- read current user defaults and set window controls as needed
	my readSettings()
	
	-- deal with changes to MacGPSBabel window needed if any of the GPS check boxes are checked by default
	if state of button "GPSswitchIN" of window "MacGPSBabel" is equal to 1 then
		my gpsIN()
	end if
	if state of button "GPSswitchOUT" of window "MacGPSBabel" is equal to 1 then
		my gpsOUT()
	end if
end clearFiles


-- read user defaults
on readSettings()
	tell user defaults
		set defaultInputIndex to contents of default entry "theInputType" as integer
		set defaultOutputIndex to contents of default entry "theOutputType" as integer
		set defaultgpsIN to contents of default entry "gpsIN" as boolean
		set defaultgpsOUT to contents of default entry "gpsOUT" as boolean
	end tell
	-- call method "setObjectValue:" of object (popup button "inPop" of window "MacGPSBabel") with parameter defaultInputIndex
	-- call method "synchronizeTitleAndSelectedItem" of object (popup button "inPop" of window "MacGPSBabel")
	set contents of popup button "inPop" of window "MacGPSBabel" to defaultInputIndex
	set contents of popup button "outPop" of window "MacGPSBabel" to defaultOutputIndex
	set state of button "GPSswitchIN" of window "MacGPSBabel" to defaultgpsIN
	set state of button "GPSswitchOUT" of window "MacGPSBabel" to defaultgpsOUT
end readSettings

-- scripts for dealing with GPS checkboxes on MacGPSBabel window
on gpsIN()
	if state of button "GPSswitchIN" of window "MacGPSBabel" = 1 then
		set enabled of button "selectButton" of window "MacGPSBabel" to false
		set enabled of button "clearButton" of window "MacGPSBabel" to false
		set enabled of button "sendButton" of window "MacGPSBabel" to true
		set contents of text field "inputFile" of window "MacGPSBabel" to ""
		set enabled of text field "inputFile" of window "MacGPSBabel" to false
		set enabled of popup button "inPop" of window "MacGPSBabel" to false
	else
		set enabled of button "selectButton" of window "MacGPSBabel" to true
		set enabled of button "sendButton" of window "MacGPSBabel" to false
		set enabled of text field "inputFile" of window "MacGPSBabel" to true
		set enabled of popup button "inPop" of window "MacGPSBabel" to true
	end if
end gpsIN
on gpsOUT()
	if state of button "GPSswitchOUT" of window "MacGPSBabel" = 1 then
		set enabled of popup button "outPop" of window "MacGPSBabel" to false
	else
		set enabled of popup button "outPop" of window "MacGPSBabel" to true
	end if
end gpsOUT

-- List Populating Handlers

-- find the serial ports
on getSerial()
	set myList to {}
	set theScript to "cd /dev; ls | grep cu\\."
	set scriptOut to (do shell script theScript) as string
	set theCount to count of paragraphs in scriptOut
	set i to 0
	set defaultDelimiters to AppleScript's text item delimiters
	set AppleScript's text item delimiters to {"."}
	repeat until i = theCount
		set i to i + 1
		set theWords to the count of text items in paragraph i of scriptOut
		set z to 2
		set the end of myList to (text items z thru theWords of paragraph i of scriptOut) as string
	end repeat
	set AppleScript's text item delimiters to defaultDelimiters
	return myList
end getSerial

-- handler (called at startup) to check with GPS Babel which file formats it can handle. Return the result as a list
on getFormats()
	set myList to {}
	set typeList to {}
	set extList to {}
	set thePath to POSIX path of (path to me) as string
	set scriptOut to (do shell script quoted form of thePath & "Contents/Resources/gpsbabel -^1") as string
	set theCount to count of paragraphs in scriptOut
	set defaultDelimiters to AppleScript's text item delimiters
	set AppleScript's text item delimiters to tab
	repeat with i from 1 to theCount
		set theLine to paragraph i of scriptOut
		if (first text item of theLine) is equal to "file" then
			set the end of typeList to the second text item of theLine
			set the end of extList to the third text item of theLine
			set the end of myList to the last text item of theLine
		end if
	end repeat
	set AppleScript's text item delimiters to defaultDelimiters
	return myList
end getFormats