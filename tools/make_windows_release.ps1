# Script to build and create windows installer.
#
# Run this from a Qt Desktop command window that has the Qt and mingw compiler paths set up,
# such as the one Qt Creator will put on the start menu.
# For example, to run overriding the default locations of windeployqt and ISSC:
# powershell.exe -ExecutionPolicy Unrestricted -File tools\make_windows_release.ps1 -windeployqt "C:\Qt\5.6.3\mingw49_32\bin\windeployqt.exe" -iscc "C:\Program Files (x86)\Inno Setup 5\ISCC.exe"
#
# Be aware this script is used by appveyor.yml
# 
# The defaults should be set for appveyor builds.
Param(
  $windeployqt =  "C:\Qt\5.6\mingw49_32\bin\windeployqt.exe",
  $iscc = "C:\Program Files (x86)\Inno Setup 5\ISCC.exe",
  $gpsbabel_build_dir = "build-GPSBabel-Desktop_Qt_5_5_1_Mingw_32bit-Release",
  $gui_build_dir = "build-app-Desktop_Qt_5_5_1_MinGW_32bit-Release"
)
# verify we are in the top of the gpsbabel clone
Get-Item tools/make_windows_release.ps1 -ErrorAction Stop
# mimic creator shadow build to match Inno setup file
# make sure we are staring with a clean build directory
Remove-Item "..\$($gpsbabel_build_dir)" -Recurse -ErrorAction Ignore
New-Item "..\$($gpsbabel_build_dir)" -type directory -force
cd "..\$($gpsbabel_build_dir)"
qmake ..\gpsbabel\GPSBabel.pro -spec win32-g++
mingw32-make qmake_all
mingw32-make
if ($LastExitCode -ne 0) { $host.SetShouldExit($LastExitCode)  }
# copy GPSBabel.exe for use by test_script
Remove-Item ..\gpsbabel\release -Recurse -ErrorAction Ignore
New-Item ..\gpsbabel\release -type directory -force
Copy-Item release\GPSBabel.exe ..\gpsbabel\release\GPSBabel.exe
cd ..\gpsbabel
# make sure we are staring with a clean build directory
Remove-Item "$($gui_build_dir)" -Recurse -ErrorAction Ignore
New-Item "$($gui_build_dir)" -type directory -force
cd "$($gui_build_dir)"
qmake ..\gui\app.pro -spec win32-g++
mingw32-make qmake_all
mingw32-make
if ($LastExitCode -ne 0) { $host.SetShouldExit($LastExitCode)  }
lupdate ..\gui\app.pro
lrelease ..\gui\app.pro
# windeployqt in 5.5.1 suffers from bug
# https://bugreports.qt.io/browser/QTBUG-48946
# which trashes the translations.  Work around
# by using the next version of windeployqt which had this
# bug fixed.
# use --plugindir option to locate the plugins.
& "$($windeployqt)" --verbose 10 --plugindir release\plugins release\GPSBabelFE.exe
& "$($iscc)" ..\gui\setup.iss
cd ..
