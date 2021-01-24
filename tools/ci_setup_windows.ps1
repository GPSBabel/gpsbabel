# set up environmental variables for Qt and MSVC

# Some tools only provide batch script files for setting up the environment.  We
# run these batch scripts and then (re)set all environmental variables from the
# command shell enviornment in the power shell environment.
# see https://github.com/actions/virtual-environments/issues/294
# and https://github.com/microsoft/vswhere/wiki/Start-Developer-Command-Prompt

Param(
    [string] $qtdir = "C:\Qt\Qt5.12.10\5.12.10\msvc2017_64",
    [ValidateSet("x86", "amd64")][string] $arch = "amd64",
    [ValidateSet("x86", "amd64")][string] $host_arch = "amd64",
    [string] $vcversion
)

# This is a bit of overkill.  All we really need from qtenv2.bat is the path to
# the Qt bin directory, which we know in order to find qtenv2.bat!
# But running qtenv2.bat is what the Qt command shell shortcuts placed on
# the start menu by Qt do.
# TODO: check for an error when the bat file is run.
function Invoke-QtEnvironment($installationPath) {
    $Command = Join-Path $installationPath "bin\qtenv2.bat"
    & "${env:COMSPEC}" /s /c "`"$Command`" && set" | ForEach-Object {
        if ($_ -match '^([^=]+)=(.*)') {
            [System.Environment]::SetEnvironmentVariable($matches[1], $matches[2])
        }
    }
}

# TODO: check for an error when the bat file is run.
# One way to generate an error is to request a vcvars_ver version that isn't
# available.
function Invoke-VSDevEnvironment($arch, $host_arch, $vcversion) {
    $vswhere = "${env:ProgramFiles(x86)}\Microsoft Visual Studio\Installer\vswhere.exe"
    $installationPath = & $vswhere -legacy -latest -property installationPath
    if ( $vcversion ) {
        $vcvars_ver = "-vcvars_ver=$vcversion"
    }
    $Command = Join-Path $installationPath "Common7\Tools\vsdevcmd.bat"
    & "${env:COMSPEC}" /s /c "`"$Command`" -no_logo -arch=$arch -host_arch=$host_arch $vcvars_ver && set" | ForEach-Object {
        if ($_ -match '^([^=]+)=(.*)') {
            [System.Environment]::SetEnvironmentVariable($matches[1], $matches[2])
        }
    }
}

$ErrorActionPreference = "Stop"

Invoke-QtEnvironment $qtdir
# verify qmake can be found.
Get-Command qmake.exe | Format-Table -AutoSize -Wrap

Invoke-VSDevEnvironment -arch $arch -host_arch $host_arch -vcversion $vcversion
# verify the c compiler can be found.
Get-Command cl.exe | Format-Table -AutoSize -Wrap
