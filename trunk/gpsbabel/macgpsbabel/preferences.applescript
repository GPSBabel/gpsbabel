-- preferences.applescript
-- MacGPSBabel

--  Created by Jeremy Atherton on Fri Jan 30 2004.
--  Copyright (c) 2004 Jeremy Atherton.

property startIndex : 0
property startState : false

on will finish launching theObject
	make new default entry at end of default entries of user defaults with properties {name:"theInputType", contents:startIndex}
	make new default entry at end of default entries of user defaults with properties {name:"theOutputType", contents:startIndex}
	make new default entry at end of default entries of user defaults with properties {name:"gpsIN", contents:startState}
	make new default entry at end of default entries of user defaults with properties {name:"gpsOUT", contents:startState}
	my readSettings()
	if state of button "GPSswitchIN" of window "MacGPSBabel" is equal to 1 then
		my gpsIN()
	end if
	if state of button "GPSswitchOUT" of window "MacGPSBabel" is equal to 1 then
		my gpsOUT()
	end if
end will finish launching

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