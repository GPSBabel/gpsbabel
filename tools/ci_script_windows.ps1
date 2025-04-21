# Script to build and create windows installer.
#
# Run this from a Qt Desktop command window that has the Qt and mingw compiler paths set up,
# such as the one Qt Creator will put on the start menu, or
# from a power shell prompt after settings up the paths with tools\ci_setup_windows.ps1.
# For example
# powershell.exe -ExecutionPolicy Unrestricted -File tools\make_windows_release.ps1
#
# The defaults should be compatible with github action builds.
param(
    $build_dir_name = "bld",
    $generator = "Ninja",
    $toolset = "",
    [ValidateSet("x86", "amd64", "amd64_x86", "x86_amd64", "arm64")] $arch = "amd64"
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
Get-Item tools/ci_script_windows.ps1 -ErrorAction Stop | Out-Null
$src_dir = $Pwd
$build_dir = Join-Path $src_dir $build_dir_name
$CMAKE_PREFIX_PATH = Split-Path $((Get-Command qmake) | Split-Path) -Parent
# translate target architecture to Platform property value.
switch ($arch) {
    "x86" { $platform = "Win32" }
    "amd64" { $platform = "x64" }
    "amd64_x86" { $platform = "Win32" }
    "x86_amd64" { $platform = "x64" }
    "arm64" { $platform = "ARM64" }
}
# make sure we are staring with a clean build directory
Remove-Item $build_dir -Recurse -ErrorAction Ignore
New-Item $build_dir -type directory -Force | Out-Null
Set-Location $build_dir
$hashargs = "-G", $generator
if ( $toolset ) {
    $hashargs += "-T", $toolset
}
if ( $generator -like "Visual Studio*") {
    $hashargs += "-A", $platform
}
else {
    $hashargs += "-DCMAKE_BUILD_TYPE:STRING=Release"
}
$hashargs += "-DCMAKE_PREFIX_PATH:PATH=$CMAKE_PREFIX_PATH"
Write-Output "cmake $hashargs $src_dir"
cmake $hashargs $src_dir
if ($LastExitCode -ne 0) { $host.SetShouldExit($LastExitCode) }
switch -wildcard ($generator) {
    "Visual Studio*" { cmake --build $build_dir --config Release }
    default { cmake --build $build_dir }
}
if ($LastExitCode -ne 0) { $host.SetShouldExit($LastExitCode) }
switch -wildcard ($generator) {
    "Visual Studio*" { cmake --build $build_dir --config Release --target package_app }
    default { cmake --build $build_dir --target package_app }
}
if ($LastExitCode -ne 0) { $host.SetShouldExit($LastExitCode) }
