-- preferences.applescript
-- MacGPSBabel

-- This script deals mostly with reading and saving user defaults. Along the way, it also deals with getting the list of available serial ports.

--  Created by Jeremy Atherton on Fri Jan 30 2004.
--  Copyright (c) 2004 Jeremy Atherton.

-- PROPERTIES --
property startIndex : 0
property startState : false

-- EVENT HANDLERS --

on will finish launching theObject
	
	-- make empty entries in user defaults
	make new default entry at end of default entries of user defaults with properties {name:"theInputType", contents:startIndex}
	make new default entry at end of default entries of user defaults with properties {name:"theOutputType", contents:startIndex}
	make new default entry at end of default entries of user defaults with properties {name:"gpsIN", contents:startState}
	make new default entry at end of default entries of user defaults with properties {name:"gpsOUT", contents:startState}
	make new default entry at end of default entries of user defaults with properties {name:"gpsReceiver", contents:startIndex}
	
	-- read current user defaults
	my readSettings()
	
	-- deal with changes to MacGPSBabel window needed if any of the GPS check boxes are checked by default
	if state of button "GPSswitchIN" of window "MacGPSBabel" is equal to 1 then
		my gpsIN()
	end if
	if state of button "GPSswitchOUT" of window "MacGPSBabel" is equal to 1 then
		my gpsOUT()
	end if
end will finish launching

on will open theObject
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
		set newReceiverIndex to contents of popup button "gpsPop" of window "SelectGPS"
		tell user defaults
			set contents of default entry "gpsReceiver" to newReceiverIndex
		end tell
		
		-- unhide MacGPSBabel window
		set visible of window "MacGPSBabel" to true
	end if
end will close

-- store user defaults for MacGPSBabel window
on clicked theObject
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
end clicked

-- HANDLERS --

-- read user defaults
on readSettings()
	tell user defaults
		set defaultInputIndex to contents of default entry "theInputType"
		set defaultOutputIndex to contents of default entry "theOutputType"
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

-- find the serial ports
on getSerial()
	set myList to {}
	set theScript to "cd /dev; ls | grep cu..."
	set scriptOut to (do shell script theScript) as string
	set theCount to count of words in scriptOut
	set i to 0
	repeat until i = theCount
		set i to i + 1
		set defaultDelimiters to AppleScript's text item delimiters
		set AppleScript's text item delimiters to {"."}
		set theWords to the count of text items in word i of scriptOut
		set z to 2
		set the end of myList to (text items z thru theWords of word i of scriptOut) as string
		set AppleScript's text item delimiters to defaultDelimiters
	end repeat
	set AppleScript's text item delimiters to {" "}
	return myList
end getSerial