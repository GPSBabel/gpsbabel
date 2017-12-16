# Script to build and create windows installer.
#
# Run this from a Qt Desktop command window that has the Qt and mingw compiler paths set up,
# such as the one Qt Creator will put on the start menu.
# For example, to run overriding the default locations of windeployqt and ISSC:
# powershell.exe -ExecutionPolicy Unrestricted -File tools\make_windows_release.ps1 -windeployqt "C:\Qt\5.6.3\mingw49_32\bin\windeployqt.exe" -iscc "C:\Program Files (x86)\Inno Setup 5\ISCC.exe"
# Or to do a 64 bit MSVC build with a newer Qt:
# powershell.exe -ExecutionPolicy Unrestricted -File tools\make_windows_release.ps1 -iscc "C:\Program Files (x86)\Inno Setup 5\ISCC.exe" -gpsbabel_build_dir_name "build-GPSBabel-Desktop_Qt_5_9_3_MSVC2015_64bit-Release" -gui_build_dir_name build-app-Desktop_Qt_5_9_3_MSVC2015_64bit-Release -mkspec win32-msvc -mkcmd nmake.exe
#
# Be aware this script is used by appveyor.yml
# 
# The defaults should be compatible with appveyor builds.
Param(
  $windeployqt = "windeployqt.exe",
  $iscc = "C:\Program Files (x86)\Inno Setup 5\ISCC.exe",
  $gpsbabel_build_dir_name = "build-GPSBabel-Desktop_Qt_5_5_1_MinGW_32bit-Release",
  $gui_build_dir_name = "build-app-Desktop_Qt_5_5_1_MinGW_32bit-Release",
  $mkspec = "win32-g++",
  $mkcmd = "mingw32-make"
)
# verify we are in the top of the gpsbabel clone
Get-Item tools/make_windows_release.ps1 -ErrorAction Stop
$gpsbabel_src_dir = "$Pwd"
$gpsbabel_build_dir = "$($gpsbabel_src_dir)\..\$($gpsbabel_build_dir_name)"
$gui_build_dir = "$($gpsbabel_src_dir)\$($gui_build_dir_name)"
# mimic creator shadow build to match Inno setup file
# make sure we are staring with a clean build directory
Remove-Item "$($gpsbabel_build_dir)" -Recurse -ErrorAction Ignore
New-Item "$($gpsbabel_build_dir)" -type directory -force
Set-Location "$($gpsbabel_build_dir)"
qmake "$($gpsbabel_src_dir)\GPSBabel.pro" -spec "$($mkspec)"
if ($LastExitCode -ne 0) { $host.SetShouldExit($LastExitCode)  }
& "$($mkcmd)" qmake_all
& "$($mkcmd)"
if ($LastExitCode -ne 0) { $host.SetShouldExit($LastExitCode)  }
# copy GPSBabel.exe for use by test_script
Remove-Item "$($gpsbabel_src_dir)\release" -Recurse -ErrorAction Ignore
New-Item "$($gpsbabel_src_dir)\release" -type directory -force
Copy-Item release\GPSBabel.exe "$($gpsbabel_src_dir)\release\GPSBabel.exe"
Set-Location "$($gpsbabel_src_dir)"
# make sure we are staring with a clean build directory
Remove-Item "$($gui_build_dir)" -Recurse -ErrorAction Ignore
New-Item "$($gui_build_dir)" -type directory -force
Set-Location "$($gui_build_dir)"
qmake "$($gpsbabel_src_dir)\gui\app.pro" -spec "$($mkspec)"
if ($LastExitCode -ne 0) { $host.SetShouldExit($LastExitCode)  }
& "$($mkcmd)" qmake_all
& "$($mkcmd)"
if ($LastExitCode -ne 0) { $host.SetShouldExit($LastExitCode)  }
lupdate "$($gpsbabel_src_dir)\gui\app.pro"
lrelease "$($gpsbabel_src_dir)\gui\app.pro"
# use --plugindir option to locate the plugins.
& "$($windeployqt)" --verbose 10 --plugindir release\plugins release\GPSBabelFE.exe
if ($LastExitCode -ne 0) { $host.SetShouldExit($LastExitCode)  }
Set-Location "$($gpsbabel_src_dir)\gui"
& "$($iscc)" /Dgpsbabel_build_dir_name="$($gpsbabel_build_dir_name)" /Dgui_build_dir_name="$($gui_build_dir_name)" setup.iss
if ($LastExitCode -ne 0) { $host.SetShouldExit($LastExitCode)  }
Set-Location "$($gpsbabel_src_dir)"
