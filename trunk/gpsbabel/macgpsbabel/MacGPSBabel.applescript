-- MacGPSBabel: MacGPSBabel.applescript

--  File created by Jeremy Atherton on Sunday, September 28, 2003.
--  Last modified by Jeremy Atherton on Sunday, January 16, 2005.

--  MacGPSBabel is part of the gpsbabel project:

--		Copyright (C) 2003 - 2005 Robert Lipe

--		This program is free software; you can redistribute it and/or modify
--		it under the terms of the GNU General Public License as published by
--		the Free Software Foundation; either version 2 of the License, or
--		(at your option) any later version.

--		This program is distributed in the hope that it will be useful,
--		but WITHOUT ANY WARRANTY; without even the implied warranty of
--		MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
--		GNU General Public License for more details.

--		You should have received a copy of the GNU General Public License
--		along with this program; if not, write to the Free Software
--		Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111 USA

-- see http://gpsbabel.sourceforge.net/ for more details

-- PROPERTIES AND GLOBALS --
property fileList : {}
property startIndex : 0
property startState : false
global theFiles, inFormatList, outFormatList, inRWList, outRWList, inTypeList, outTypeList, extList, aFile

-- EVENT HANDLERS --

-- Start up scripts

-- make empty entries in user defaults
on will finish launching theObject
	make new default entry at end of default entries of user defaults with properties {name:"theInputType", contents:startIndex}
	make new default entry at end of default entries of user defaults with properties {name:"theOutputType", contents:startIndex}
	make new default entry at end of default entries of user defaults with properties {name:"gpsIN", contents:startState}
	make new default entry at end of default entries of user defaults with properties {name:"gpsOUT", contents:startState}
	make new default entry at end of default entries of user defaults with properties {name:"gpsReceiver", contents:startIndex}
	make new default entry at end of default entries of user defaults with properties {name:"filterNSSelect", contents:startIndex}
	make new default entry at end of default entries of user defaults with properties {name:"filterEWSelect", contents:startIndex}
	make new default entry at end of default entries of user defaults with properties {name:"filterNDeg", contents:0}
	make new default entry at end of default entries of user defaults with properties {name:"filterNMin", contents:0.0}
	make new default entry at end of default entries of user defaults with properties {name:"filterWDeg", contents:0}
	make new default entry at end of default entries of user defaults with properties {name:"filterWMin", contents:0.0}
end will finish launching

on awake from nib theObject
	log "awake from nib - " & name of theObject
	if theObject is window "MacGPSBabel" then
		-- get supported file types from gpsbabel and use these to populate the file types popup list
		my getFormats()
		
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
	
	log "will open - " & name of theObject
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
		make new menu item at the end of menu items of menu of popup button "serialPop" of window "SelectGPS" with properties {title:"Garmin USB", enabled:true}
		
		-- read user defaults for this window
		tell user defaults
			set defaultgpsReceiver to contents of default entry "gpsReceiver"
		end tell
		set state of popup button "gpsPop" of window "SelectGPS" to defaultgpsReceiver
		
		-- hide MacGPSBabel window
		set visible of window "MacGPSBabel" to false
	end if
	
end will open

-- to work around the NSReceiverEvaluationScriptError we need to use should close instead of will close
on should close theObject
	log "will close - " & name of theObject
	if theObject is window "SelectGPS" then
		-- store user defaults for this window
		set newReceiverIndex to contents of popup button "gpsPop" of window "SelectGPS" as integer
		tell user defaults
			set contents of default entry "gpsReceiver" to newReceiverIndex
		end tell
		
		-- unhide MacGPSBabel window
		set visible of window "MacGPSBabel" to true
	end if
	
	if theObject is window "filterWindow" then
		set the title of button "filterButton" of window "MacGPSBabel" to "Setup Filters"
	end if
	
	-- workarund NSReceiverEvaluationScriptError bug
	set visible of theObject to false
	return false
	
end should close


-- handler for the File>Open menu item
on choose menu item theObject
	log "choose menu item - " & name of theObject
	
	-- mainMenu File>Open
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
	
	if name of theObject is "modePop" then
		my getFormats()
	end if
end choose menu item

-- HANDLERS FOR BUTTON CLICKS

on clicked theObject
	log "clicked - " & name of theObject
	
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
			set enabled of button "makeHomeButton" of window "filterWindow" to true
			set enabled of button "useHomeButton" of window "filterWindow" to true
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
			set enabled of button "makeHomeButton" of window "filterWindow" to false
			set enabled of button "useHomeButton" of window "filterWindow" to false
		end if
	end if
	
	-- Filter Window - Make Home Button
	if theObject is button "makeHomeButton" of window "filterWindow" then
		set newNSIndex to contents of popup button "nsSelect" of window "filterWindow"
		set newEWIndex to contents of popup button "ewSelect" of window "filterWindow"
		set newNDeg to contents of text field "nDeg" of window "filterWindow"
		set newNMin to contents of text field "nMin" of window "filterWindow"
		set newWDeg to contents of text field "wDeg" of window "filterWindow"
		set newWMin to contents of text field "wMin" of window "filterWindow"
		tell user defaults
			set contents of default entry "filterNSSelect" to newNSIndex as integer
			set contents of default entry "filterEWSelect" to newEWIndex as integer
			set contents of default entry "filterNDeg" to newNDeg as integer
			set contents of default entry "filterNMin" to newNMin as number
			set contents of default entry "filterWDeg" to newWDeg as integer
			set contents of default entry "filterWMin" to newWMin as number
		end tell
	end if
	
	-- Filter Window - Use Home Button
	if theObject is button "useHomeButton" of window "filterWindow" then
		tell user defaults
			set homeNSSelectIndex to contents of default entry "filterNSSelect" as integer
			set homeEWSelectIndex to contents of default entry "filterEWSelect" as integer
			set homeNDeg to contents of default entry "filterNDeg" as integer
			set homeWDeg to contents of default entry "filterWDeg" as integer
			set homeNMin to contents of default entry "filterNMin" as number
			set homeWMin to contents of default entry "filterWMin" as number
		end tell
		set contents of popup button "nsSelect" of window "filterWindow" to homeNSSelectIndex
		set contents of popup button "ewSelect" of window "filterWindow" to homeEWSelectIndex
		set contents of text field "nDeg" of window "filterWindow" to homeNDeg
		set contents of text field "wDeg" of window "filterWindow" to homeWDeg
		set contents of text field "nMin" of window "filterWindow" to homeNMin
		set contents of text field "wMin" of window "filterWindow" to homeWMin
	end if
	
	-- Filter Window - Duplicate filter check boxes
	if theObject is the button "locationFilter" of window "filterWindow" then
		if state of button "locationFilter" of window "filterWindow" is equal to 1 then
			set enabled of button "allSwitch" of window "filterWindow" to 1
		else if state of button "shortFilter" of window "filterWindow" is equal to 0 then
			set enabled of button "allSwitch" of window "filterWindow" to 0
		end if
	end if
	
	if theObject is the button "shortFilter" of window "filterWindow" then
		if state of button "shortFilter" of window "filterWindow" is equal to 1 then
			set enabled of button "allSwitch" of window "filterWindow" to 1
		else if state of button "locationFilter" of window "filterWindow" is equal to 0 then
			set enabled of button "allSwitch" of window "filterWindow" to 0
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
			set enabled of button "arcPointsSwitch" of window "filterWindow" to true
		else
			set contents of text field "arcFile" of window "filterWindow" to ""
			set contents of text field "arcDist" of window "filterWindow" to ""
			set enabled of text field "arcDist" of window "filterWindow" to false
			set editable of text field "arcDist" of window "filterWindow" to false
			set enabled of popup button "arcUnits" of window "filterWindow" to false
			set enabled of button "arcPointsSwitch" of window "filterWindow" to false
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
	
	-- waypoint, routes or tracks
	if the title of popup button "modePop" of window "MacGPSBabel" is equal to "Tracks" then
		set trackText to " -t"
	else if the title of popup button "modePop" of window "MacGPSBabel" is equal to "Routes" then
		set trackText to " -r"
	else
		set trackText to ""
	end if
	
	-- create string for input files
	set fileText to ""
	repeat with theItem in fileList
		set currentInIndex to item 2 of theItem
		set inType to item (currentInIndex) of inTypeList
		set inputFile to quoted form of item 1 of theItem
		set fileText to fileText & " -i " & inType & " -f " & inputFile
	end repeat
	
	-- create strings for output file
	set currentOutIndex to contents of popup button "outPop" of window "MacGPSBabel"
	set outType to item (currentOutIndex) of outTypeList
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
	-- do the script
	set thePath to POSIX path of (path to me) as string
	set theConvertScript to (quoted form of thePath & "Contents/Resources/gpsbabel" & smartSwitch & trackText & fileText & " " & filterText & "-o " & outType & " -F " & quoted form of outputFile) as string
	if (my runBabel(theConvertScript)) then
		display dialog "File conversion is complete" buttons {"OK"} default button 1
	else
		display dialog "Sorry, the file conversion failed" buttons {"OK"} default button 1
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
	
	-- waypoint, routes or tracks
	if the title of popup button "modePop" of window "MacGPSBabel" is equal to "Tracks" then
		set trackText to " -t"
	else if the title of popup button "modePop" of window "MacGPSBabel" is equal to "Routes" then
		set trackText to " -r"
	else
		set trackText to ""
	end if
	
	-- create string for input files
	set fileText to ""
	repeat with theItem in fileList
		set currentInIndex to item 2 of theItem
		set inType to item (currentInIndex) of inTypeList
		set inputFile to quoted form of item 1 of theItem
		set fileText to fileText & " -i " & inType & " -f " & inputFile
	end repeat
	
	-- create string for GPS unit
	if the title of popup button "gpsPop" of window "selectGPS" is equal to "Garmin" then
		set gpsRText to "garmin"
	else
		set gpsRText to "magellan"
	end if
	if visible of window "filterWindow" is true then
		if state of button "smartSwitch" of window "filterWindow" is equal to 1 then
			set smartSwitch to " -s"
			if contents of text field "smartLen" of window "filterWindow" is not equal to "" then
				set gpsText to gpsRText & ",snlen=" & ((contents of text field "smartLen" of window "filterWindow") as integer)
			else
				set gpsText to gpsRText
			end if
		else
			set smartSwitch to ""
			set gpsText to gpsRText
		end if
	else
		set smartSwitch to ""
		set gpsText to gpsRText
	end if
	
	-- get the port
	if the the title of popup button "serialPop" of window "selectGPS" is equal to "Garmin USB" then
		set serialText to "usb:"
	else
		set serialText to quoted form of ("/dev/cu." & (the title of popup button "serialPop" of window "selectGPS"))
	end if
	
	-- run the script
	set thePath to POSIX path of (path to me) as string
	set visible of window "SelectGPS" to false
	set visible of window "MacGPSBabel" to true
	
	set theConvertScript to (quoted form of thePath & "Contents/Resources/gpsbabel" & smartSwitch & trackText & fileText & " " & filterText & "-o " & gpsText & " -F " & serialText)
	if (my runBabel(theConvertScript)) then
		display dialog "Upload to " & gpsRText & " GPS receiver is complete" buttons {"OK"} default button 1
	else
		display dialog "Sorry, upload to " & gpsRText & " GPS receiver failed" buttons {"OK"} default button 1
	end if
	my clearFiles()
end uploadFile
-- deal with downloading files from GPS receiver
on downloadFile()
	set outName to "Waypoints."
	if visible of window "filterWindow" is true then
		set filterText to my applyFilters()
	else
		set filterText to ""
	end if
	
	-- waypoint, routes or tracks
	if the title of popup button "modePop" of window "MacGPSBabel" is equal to "Tracks" then
		set trackText to " -t"
		set outName to "Tracks."
	else if the title of popup button "modePop" of window "MacGPSBabel" is equal to "Routes" then
		set trackText to " -r"
		set outName to "Routes."
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
	set outType to item (currentOutIndex) of outTypeList
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
		-- get the port
		if the the title of popup button "serialPop" of window "selectGPS" is equal to "Garmin USB" then
			set serialText to "usb:"
		else
			set serialText to quoted form of ("/dev/cu." & (the title of popup button "serialPop" of window "selectGPS"))
		end if
		set visible of window "SelectGPS" to false
		set visible of window "MacGPSBabel" to true
		set theConvertScript to (quoted form of thePath & "Contents/Resources/gpsbabel" & smartSwitch & trackText & " -i" & gpsText & "-f " & serialText & filterText & " -o " & outType & " -F " & quoted form of outputFile)
		if (my runBabel(theConvertScript)) then
			display dialog "Download from" & gpsText & "GPS receiver is complete" buttons {"OK"} default button 1
		else
			display dialog "Sorry, download from" & gpsText & "GPS receiver failed" buttons {"OK"} default button 1
		end if
	else
		set outputFile to ""
	end if
	my clearFiles()
end downloadFile

-- Send the call to gpsbabel
on runBabel(theConvertScript)
	log "Tried to execute: " & theConvertScript
	set theConvertScript to theConvertScript & " 2>&1"
	feedbackBusy(true)
	try
		set scriptOut to do shell script theConvertScript as string
		set babelHappy to true
		set convertYN to "ran successfully"
		log "Success! gpsbabel returned: " & scriptOut
	on error
		set scriptOut to "gpsbabel encountered an error"
		set babelHappy to false
		set convertYN to "failed"
		log "Error! gpsbabel returned: " & scriptOut
	end try
	feedbackBusy(false)
	if visible of window "debugWindow" is true then
		set the contents of text view 1 of scroll view 1 of window "debugWindow" to ""
		set the contents of text view 1 of scroll view 1 of window "debugWindow" to "MacGPSBabel Report" & return & return & "The Shell Script:" & return & theConvertScript & return & return & convertYN & return & return & "Output From gpsbabel:" & return & scriptOut
	end if
	return babelHappy
end runBabel




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
			set arcText to "-x arc,file='" & (contents of text field "arcFile" of window "filterWindow") & "',distance=" & arcDistance
			if the state of button "arcPointsSwitch" of window "filterWindow" is equal to 1 then
				set arcText to arcText & ",points"
			end if
			set arcText to arcText & " "
		else
			display dialog "Please input a distance for the arc filter" buttons ["OK"] default button 1
			set arcText to ""
			break
		end if
	else
		set arcText to ""
	end if
	if state of button "polySwitch" of window "filterWindow" is equal to 1 then
		set polyText to "-x polygon,file='" & (contents of text field "polyFile" of window "filterWindow") & "'"
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
	if the title of popup button "modePop" of window "MacGPSBabel" is equal to "Waypoints" then
		my readSettings()
	end if
	
	-- deal with changes to MacGPSBabel window needed if any of the GPS check boxes are checked by default
	my gpsIN()
	my gpsOUT()
end clearFiles


-- read user defaults
on readSettings()
	tell user defaults
		set defaultInputIndex to contents of default entry "theInputType" as integer
		set defaultOutputIndex to contents of default entry "theOutputType" as integer
		set defaultgpsIN to contents of default entry "gpsIN" as boolean
		set defaultgpsOUT to contents of default entry "gpsOUT" as boolean
	end tell
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

-- handler (called at startup) to check with GPS Babel which file formats it can handle.
-- Populates global lists with file types and capabilities
on getFormats()
	set inFormatList to {}
	set outFormatList to {}
	set inTypeList to {}
	set outTypeList to {}
	set extList to {}
	set thePath to POSIX path of (path to me) as string
	set scriptOut to (do shell script quoted form of thePath & "Contents/Resources/gpsbabel -^2") as string
	set theCount to count of paragraphs in scriptOut
	set defaultDelimiters to AppleScript's text item delimiters
	set AppleScript's text item delimiters to tab
	repeat with i from 1 to theCount
		set theLine to paragraph i of scriptOut
		if (first text item of theLine) is equal to "file" then
			if the title of popup button "modePop" of window "MacGPSBabel" is equal to "Waypoints" then
				if the first character of the second text item of theLine is equal to "r" then
					set the end of inTypeList to the third text item of theLine
					set the end of inFormatList to the last text item of theLine
				end if
				if the second character of the second text item of theLine is equal to "w" then
					set the end of extList to the 4th text item of theLine
					set the end of outTypeList to the third text item of theLine
					set the end of outFormatList to the last text item of theLine
				end if
			end if
			if the title of popup button "modePop" of window "MacGPSBabel" is equal to "Tracks" then
				if the third character of the second text item of theLine is equal to "r" then
					set the end of inTypeList to the third text item of theLine
					set the end of inFormatList to the last text item of theLine
				end if
				if the 4th character of the second text item of theLine is equal to "w" then
					set the end of extList to the 4th text item of theLine
					set the end of outTypeList to the third text item of theLine
					set the end of outFormatList to the last text item of theLine
				end if
			end if
			if the title of popup button "modePop" of window "MacGPSBabel" is equal to "Routes" then
				if the 5th character of the second text item of theLine is equal to "r" then
					set the end of inTypeList to the third text item of theLine
					set the end of inFormatList to the last text item of theLine
				end if
				if the 6th character of the second text item of theLine is equal to "w" then
					set the end of extList to the 4th text item of theLine
					set the end of outTypeList to the third text item of theLine
					set the end of outFormatList to the last text item of theLine
				end if
			end if
		end if
	end repeat
	set AppleScript's text item delimiters to defaultDelimiters
	
	-- set current menu item of popup button "inPop" of window "MacGPSBabel" to menu item 1 of menu of popup button "inPop" of window "MacGPSBabel"
	-- set current menu item of popup button "outPop" of window "MacGPSBabel" to menu item 1 of menu of popup button "outPop" of window "MacGPSBabel"
	
	tell window "MacGPSBabel"
		if (count of menu items of popup button "inPop") is greater than 0 then
			delete every menu item of menu of popup button "inPop"
			make new menu item at the end of menu items of menu of popup button "inPop" with properties {title:"Select Input File Type", enabled:true}
			delete every menu item of menu of popup button "outPop"
			make new menu item at the end of menu items of menu of popup button "outPop" with properties {title:"Select Output File Type", enabled:true}
		end if
		repeat with i in inFormatList
			make new menu item at the end of menu items of menu of popup button "inPop" with properties {title:i, enabled:true}
		end repeat
		repeat with i in outFormatList
			make new menu item at the end of menu items of menu of popup button "outPop" with properties {title:i, enabled:true}
		end repeat
	end tell
	
	if the title of popup button "modePop" of window "MacGPSBabel" is equal to "Waypoints" then
		my readSettings()
	end if
	
end getFormats