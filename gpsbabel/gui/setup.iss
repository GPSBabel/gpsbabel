;
; NOTE: setup.iss is generated from setup.iss.in via autoconf.
; The generated setup.iss is checked in to help keep the version numbers
; synchronized.
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
AppName=GPSBabel
AppVerName=GPSBabel 1.4.3-beta20120114
AppPublisher=GPSBabel
AppPublisherURL=http://www.gpsbabel.org
AppSupportURL=http://www.gpsbabel.org
AppUpdatesURL=http://www.gpsbabel.org
DefaultDirName={pf}\GPSBabel
DefaultGroupName=GPSBabel
OutputDir=release
OutputBaseFilename=GPSBabel-1.4.3-beta20120114-Setup
SetupIconFile=images\babel2.ico
Compression=lzma
SolidCompression=yes
LicenseFile=COPYING.txt

[Languages]
Name: "english"; MessagesFile: "compiler:Default.isl"

[Tasks]
Name: "desktopicon"; Description: "{cm:CreateDesktopIcon}"; GroupDescription: "{cm:AdditionalIcons}"; Flags: unchecked

[Files]
; This isn't as wreckless as it seems; these directories are populated on a
; controlled way by the batch file.
Source: qtdir\bin\*.dll;		DestDir: "{app}"; Flags: ignoreversion
Source: qtdir\plugins\*; 		DestDir: "{app}\plugins"; Flags: ignoreversion recursesubdirs createallsubdirs

Source: gmapbase.html; 			DestDir: "{app}"; Flags: ignoreversion
Source: qt.conf;       			DestDir: "{app}"; Flags: ignoreversion

Source: release\gpsbabelfe.exe; 	DestDir: "{app}"; Flags: ignoreversion
Source: release\libexpat.dll;   	DestDir: "{app}"; Flags: ignoreversion
Source: release\gpsbabel.exe;   	DestDir: "{app}"; Flags: ignoreversion
; Source: release\help\*;           	DestDir: "{app}\help"; Flags: ignoreversion recursesubdirs createallsubdirs

; Translation strings extracted from source code.  Include it in the dist
; so that users can translate if they want to. 
; Source: gpsbabel_*.ts;           	DestDir: "{app}\translations"; Flags: ignoreversion 
; Source: gpsbabelfe_*.ts;           	DestDir: "{app}\translations"; Flags: ignoreversion 

; Compiled translation strings that are used at runtime.
; Source: gpsbabel_*.qm;           	DestDir: "{app}\translations"; Flags: ignoreversion 
Source: gpsbabelfe*.qm;           	DestDir: "{app}\translations"; Flags: ignoreversion 

; Now translations from Qt's own UI stuff.
Source: qtdir\translations\*;  		DestDir: "{app}\translations"; Flags: ignoreversion recursesubdirs createallsubdirs

; Miscellaneous
Source: COPYING.txt;			DestDir: {app}; Flags: ignoreversion
; Source: AUTHORS;			DestDir: {app}; Flags: ignoreversion
; Source: README.contrib;			DestDir: {app}; Flags: ignoreversion
; Source: README.gui;			DestDir: {app}; Flags: ignoreversion


; NOTE: Don't use "Flags: ignoreversion" on any shared system files

[Icons]
Name: "{group}\GPSBabel"; Filename: "{app}\gpsbabelfe.exe"
Name: "{commondesktop}\GPSBabel"; Filename: "{app}\gpsbabelfe.exe"; Tasks: desktopicon

[Run]
Filename: "{app}\gpsbabelfe.exe"; Description: "{cm:LaunchProgram,GPSBabelFE}"; Flags: nowait postinstall skipifsilent

[Registry]
Root: HKCU; Subkey: "Software\GPSBabel"; Flags: uninsdeletekeyifempty
Root: HKCU; Subkey: "Software\GPSBabel\GPSBabel"; Flags: uninsdeletekey
Root: HKCU; Subkey: "Software\GPSBabel\GPSBabelFE"; Flags: uninsdeletekey

