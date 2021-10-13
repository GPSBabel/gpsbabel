# Script to build and create windows installer.
#
# Run this from a Qt Desktop command window that has the Qt and mingw compiler paths set up,
# such as the one Qt Creator will put on the start menu.
# For example, to run overriding the default locations of windeployqt and ISSC:
# powershell.exe -ExecutionPolicy Unrestricted -File tools\make_windows_release.ps1 -windeployqt "C:\Qt\5.6.3\mingw49_32\bin\windeployqt.exe" -iscc "C:\Program Files (x86)\Inno Setup 5\ISCC.exe" -flow mingw
# Or to do a 64 bit MSVC build with a newer Qt:
# powershell.exe -ExecutionPolicy Unrestricted -File tools\make_windows_release.ps1 -iscc "C:\Program Files (x86)\Inno Setup 5\ISCC.exe" -gpsbabel_build_dir_name "build-GPSBabel-Desktop_Qt_5_9_3_MSVC2015_64bit-Release" -gui_build_dir_name "build-app-Desktop_Qt_5_9_3_MSVC2015_64bit-Release"
#
# Be aware this script is used by appveyor.yml
#
# The defaults should be compatible with appveyor builds.
Param(
    $windeployqt = "windeployqt.exe",
    $iscc = "C:\Program Files (x86)\Inno Setup 5\ISCC.exe",
    $gpsbabel_build_dir_name = "build-GPSBabel-Desktop-Release",
    $gui_build_dir_name = "build-app-Desktop-Release",
    [ValidateSet("mingw", "msbuild", "nmake", "cmake")] $flow = "nmake",
    $buildinstaller = "false",
    [ValidateSet("x86", "amd64", "amd64_x86", "x86_amd64")] $arch = "amd64"
)
# the arch parameter values correspond to:
# vcvarsall arch parameter x86 => host x86, target x86.
# vcvarsall arch paramter amd64 => host amd64, target amd64.
# vcvarsall arch parameter amd64_x86 => host amd64, target x86
# vcvarsall arch parameter x86_amd64 => host x86, target amd64
# vsdevcmd arch parameter x86 => target x86.
# vsdevcmd arch parameter amd64 => target amd64.
$ErrorActionPreference = "Stop"
# verify we are in the top of the gpsbabel clone
Get-Item tools/make_windows_release.ps1 -ErrorAction Stop | Out-Null
$gpsbabel_src_dir = "$Pwd"
$gpsbabel_build_dir = "$($gpsbabel_src_dir)\..\$($gpsbabel_build_dir_name)"
$gui_build_dir = "$($gpsbabel_src_dir)\$($gui_build_dir_name)"
if ( "$flow" -eq "msbuild" ) {
    # translate target architecture to Platform property value.
    switch ($arch) {
        "x86" { $platform = "Win32" }
        "amd64" { $platform = "x64" }
        "amd64_x86" { $platform = "Win32" }
        "x86_amd64" { $platform = "x64" }
    }
}
if ( "$flow" -eq "cmake" ) {
    $Qt5_DIR = "$(Join-Path "$((Get-Command qmake) | Split-Path)"  '..\lib\cmake\Qt5' -Resolve)"
}
# mimic creator shadow build to match Inno setup file
# make sure we are staring with a clean build directory
Remove-Item "$($gpsbabel_build_dir)" -Recurse -ErrorAction Ignore
New-Item "$($gpsbabel_build_dir)" -type directory -Force | Out-Null
Set-Location "$($gpsbabel_build_dir)"
switch ($flow) {
    "mingw" { qmake "$($gpsbabel_src_dir)\GPSBabel.pro" -spec "win32-g++" }
    # work around Qt 5.12.1, 5.12.2 qmake bug.
    #C:\Qt\5.12.1\msvc2017\bin\qmake.exe -tp vc GPSBabel.pro
    #WARNING: Could not parse Compiler option '-std:c++14'; added to AdditionalOptions.
    #WARNING: You can suppress these warnings with CONFIG+=suppress_vcproj_warnings.
    #WARNING: Could not parse Compiler option '-std:c++14'; added to AdditionalOptions.
    "msbuild" { $ErrorActionPreference = "Continue"; qmake -tp vc "$($gpsbabel_src_dir)\GPSBabel.pro"; $ErrorActionPreference = "Stop" }
    "nmake" { qmake "$($gpsbabel_src_dir)\GPSBabel.pro" -spec "win32-msvc" }
    "cmake" { cmake -G "Ninja" -DCMAKE_BUILD_TYPE:STRING="Release" -DQt5_DIR:PATH="$($Qt5_DIR)" -DCMAKE_RUNTIME_OUTPUT_DIRECTORY:PATH="$($gpsbabel_build_dir)\release" "$($gpsbabel_src_dir)" }
}
if ($LastExitCode -ne 0) { $host.SetShouldExit($LastExitCode) }
switch ($flow) {
    "mingw" { ming32-make }
    "msbuild" { msbuild GPSBabel.vcxproj -property:Configuration=Release -property:Platform=$platform }
    "nmake" { nmake /NOLOGO }
    "cmake" { cmake --build . }
}
if ($LastExitCode -ne 0) { $host.SetShouldExit($LastExitCode) }
# copy GPSBabel.exe for use by test_script
Remove-Item "$($gpsbabel_src_dir)\release" -Recurse -ErrorAction Ignore
New-Item "$($gpsbabel_src_dir)\release" -type directory -Force | Out-Null
Copy-Item release\GPSBabel.exe "$($gpsbabel_src_dir)\release\GPSBabel.exe"
Set-Location "$($gpsbabel_src_dir)"
# make sure we are staring with a clean build directory
Remove-Item "$($gui_build_dir)" -Recurse -ErrorAction Ignore
New-Item "$($gui_build_dir)" -type directory -Force | Out-Null
Set-Location "$($gui_build_dir)"
switch ($flow) {
    "mingw" { qmake "$($gpsbabel_src_dir)\gui\app.pro" -spec "win32-g++" }
    "msbuild" { qmake -tp vc "$($gpsbabel_src_dir)\gui\app.pro" }
    "nmake" { qmake "$($gpsbabel_src_dir)\gui\app.pro" -spec "win32-msvc" }
    "cmake" { cmake -G "Ninja" -DCMAKE_BUILD_TYPE:STRING:="Release" -DQt5_DIR:PATH="$($Qt5_DIR)" -DCMAKE_RUNTIME_OUTPUT_DIRECTORY:PATH="$($gui_build_dir)\release" "$($gpsbabel_src_dir)\gui" }
}
if ($LastExitCode -ne 0) { $host.SetShouldExit($LastExitCode) }
switch ($flow) {
    "mingw" { ming32-make }
    "msbuild" { msbuild GPSBabelFE.vcxproj -property:Configuration=Release -property:Platform=$platform }
    "nmake" { nmake /NOLOGO }
    "cmake" { cmake --build . }
}
if ($LastExitCode -ne 0) { $host.SetShouldExit($LastExitCode) }
# work around errors with lupdate, lrelease misprocessing qtHaveModule(webenginewidgets)
# and generating a message to stderr WARNING: Project ERROR: Unknown module(s) in QT: webkit webkitwidgets
# and, on Windows, setting $? to false.
# (assuming they are not installed).
$ErrorActionPreference = "Continue"
lupdate "$($gpsbabel_src_dir)\gui\app.pro"
lrelease "$($gpsbabel_src_dir)\gui\app.pro"
$ErrorActionPreference = "Stop"
# deploy to a clean directory as different build systems create differently named debris in release.
Remove-Item "$($gui_build_dir)\package" -Recurse -ErrorAction Ignore
New-Item "$($gui_build_dir)\package" -type directory -Force | Out-Null
Copy-Item "$($gpsbabel_build_dir)\release\GPSBabel.exe" "$($gui_build_dir)\package\GPSBabel.exe"
Copy-Item "$($gui_build_dir)\release\GPSBabelFE.exe" "$($gui_build_dir)\package\GPSBabelFE.exe"
# use --plugindir option to locate the plugins.
& "$($windeployqt)" --verbose 1 --plugindir package\plugins package\GPSBabelFE.exe
if ($LastExitCode -ne 0) { $host.SetShouldExit($LastExitCode) }
if ($buildinstaller -eq "true") {
    Set-Location "$($gpsbabel_src_dir)\gui"
    & "$($iscc)" /Dpackage_dir="$($gui_build_dir)\package" setup.iss
    if ($LastExitCode -ne 0) { $host.SetShouldExit($LastExitCode) }
}
Set-Location "$($gpsbabel_src_dir)"
