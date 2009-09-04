; $Id: setup.iss,v 1.3 2009-09-04 16:55:59 robertl Exp $
;
; Script for generating installation setup program for GPSBabel
; Uses the Inno setup compiler.  Typically used from the command
; line "makesetup.bat" which copies QT system files which
; the Innosetup compiler cannot handle.  
;
; So it is not a good idea to run this file from the Inno Setup GUI.

[Setup]
; NOTE: The value of AppId uniquely identifies this application.
; Do not use the same AppId value in installers for other applications.
; (To generate a new GUID, click Tools | Generate GUID inside the IDE.)
AppId={{1B8FE958-A304-4902-BF7A-4E2F0F5B7017}
AppName=GPSBabelFE
AppVerName=GPSBabelFE 0.1
AppPublisher=GPSBabel
AppPublisherURL=http://www.gpsbabel.org
AppSupportURL=http://www.gpsbabel.org
AppUpdatesURL=http://www.gpsbabel.org
DefaultDirName={pf}\GPSBabelFE
DefaultGroupName=GPSBabelFE
OutputDir=release
OutputBaseFilename=GPSBabelSetup
SetupIconFile=images\babel2.ico
Compression=lzma
SolidCompression=yes

[Languages]
Name: "english"; MessagesFile: "compiler:Default.isl"

[Tasks]
Name: "desktopicon"; Description: "{cm:CreateDesktopIcon}"; GroupDescription: "{cm:AdditionalIcons}"; Flags: unchecked

[Files]
Source: qtdir\bin\QtCore4.dll;		DestDir: "{app}"; Flags: ignoreversion
Source: qtdir\bin\QtGui4.dll; 		DestDir: "{app}"; Flags: ignoreversion
Source: qtdir\bin\QtWebkit4.dll; 	DestDir: "{app}"; Flags: ignoreversion
Source: qtdir\bin\QtXml4.dll; 		DestDir: "{app}"; Flags: ignoreversion
Source: qtdir\bin\QtNetwork4.dll; 	DestDir: "{app}"; Flags: ignoreversion
Source: qtdir\mingw\mingwm10.dll;	DestDir: "{app}"; Flags: ignoreversion 
Source: qtdir\plugins\*; 		DestDir: "{app}\plugins"; Flags: ignoreversion recursesubdirs createallsubdirs

Source: gmapbase.html; 			DestDir: "{app}"; Flags: ignoreversion
Source: qt.conf;       			DestDir: "{app}"; Flags: ignoreversion

Source: release\gpsbabelfe.exe; 	DestDir: "{app}"; Flags: ignoreversion
Source: release\libexpat.dll;   	DestDir: "{app}"; Flags: ignoreversion
Source: release\gpsbabel.exe;   	DestDir: "{app}"; Flags: ignoreversion
Source: release\help\*;           	DestDir: "{app}\help"; Flags: ignoreversion recursesubdirs createallsubdirs

; Translation strings extracted from source code.  Include it in the dist
; so that users can translate if they want to. 
Source: gpsbabel_de.ts;           	DestDir: "{app}"; Flags: ignoreversion 
Source: gpsbabel_es.ts;           	DestDir: "{app}\translations"; Flags: ignoreversion 
Source: gpsbabel_fr.ts;           	DestDir: "{app}\translations"; Flags: ignoreversion 
Source: gpsbabel_hu.ts;           	DestDir: "{app}\translations"; Flags: ignoreversion 
Source: gpsbabel_it.ts;           	DestDir: "{app}\translations"; Flags: ignoreversion 
Source: gpsbabelfe_de.ts;           	DestDir: "{app}\translations"; Flags: ignoreversion 
Source: gpsbabelfe_es.ts;           	DestDir: "{app}\translations"; Flags: ignoreversion 
Source: gpsbabelfe_fr.ts;           	DestDir: "{app}\translations"; Flags: ignoreversion 
Source: gpsbabelfe_hu.ts;           	DestDir: "{app}\translations"; Flags: ignoreversion 
Source: gpsbabelfe_it.ts;           	DestDir: "{app}\translations"; Flags: ignoreversion 

; Compiled translation strings that are used at runtime.
; Only Spanish is adequately translated for now.
Source: gpsbabel_es.qm;           	DestDir: "{app}\translations"; Flags: ignoreversion 
Source: gpsbabelfe_es.qm;           	DestDir: "{app}\translations"; Flags: ignoreversion 

; German is usable
Source: gpsbabel_de.qm;           	DestDir: "{app}\translations"; Flags: ignoreversion 
Source: gpsbabelfe_de.qm;           	DestDir: "{app}\translations"; Flags: ignoreversion 

; Now translations from Qt's own UI stuff.
Source: qtdir\translations\*;  		DestDir: "{app}\translations"; Flags: ignoreversion recursesubdirs createallsubdirs

; Miscellaneous
Source: COPYING;			DestDir: {app}; Flags: ignoreversion
Source: AUTHORS;			DestDir: {app}; Flags: ignoreversion
Source: README.contrib;			DestDir: {app}; Flags: ignoreversion
Source: README.gui;			DestDir: {app}; Flags: ignoreversion


; NOTE: Don't use "Flags: ignoreversion" on any shared system files

[Icons]
Name: "{group}\GPSBabelFE"; Filename: "{app}\gpsbabelfe.exe"
Name: "{commondesktop}\GPSBabelFE"; Filename: "{app}\gpsbabelfe.exe"; Tasks: desktopicon

[Run]
Filename: "{app}\gpsbabelfe.exe"; Description: "{cm:LaunchProgram,GPSBabelFE}"; Flags: nowait postinstall skipifsilent

