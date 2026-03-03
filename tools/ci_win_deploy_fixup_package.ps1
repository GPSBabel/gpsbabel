# windeployqt is known to copy icuuc.dll from the system32 folder into the package, which causes issues on some machines. This script checks if the icuuc.dll in the package matches the one in system32, and if so, removes it from the package to avoid conflicts.
# https://qt-project.atlassian.net/browse/QTBUG-142131
param(
    [string] $packagedir = "package"
)

$packageicu = "$packagedir\icuuc.dll"
$systemicu = "$Env:SystemRoot\System32\icuuc.dll"
if ((Test-Path -Path "$packageicu") -and (Test-Path "$systemicu")) {
    $systemHash = Get-FileHash "$systemicu" | Select-Object -ExpandProperty Hash
    $packageHash = Get-FileHash "$packageicu" | Select-Object -ExpandProperty Hash
    if ($systemHash -eq $packageHash) {
        Write-Warning "Removing icuuc.dll that matches system32 copy from the package."
        Remove-Item -Path "$packageicu"
    }
}