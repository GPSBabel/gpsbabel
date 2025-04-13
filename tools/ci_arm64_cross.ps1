Param(
    $build_dir_name = "bld",
    $qt_root_dir = "C:/Qt/6.8.3",
    $host_arch = "amd64",
    $target_arch = "arm64",
    $compiler = "msvc2022_64",
    $cross_compiled = "msvc2022_arm64"
)

$ErrorActionPreference = "Stop"

# setup visual studio development envirnonment
$vswhere = "${env:ProgramFiles(x86)}\Microsoft Visual Studio\Installer\vswhere.exe"
$installationPath = & "$vswhere" -latest -property installationPath
& "$installationPath\Common7\Tools\Launch-VsDevShell.ps1" -Arch $target_arch -HostArch $host_arch -SkipAutomaticLocation

$src_dir = $Pwd
$build_dir = Join-Path $src_dir $build_dir_name

# make sure we are staring with a clean build directory
Remove-Item $build_dir -Recurse -ErrorAction Ignore
New-Item $build_dir -type directory -Force | Out-Null
Set-Location $build_dir

$toolchain_file = "$qt_root_dir/$cross_compiled/lib/cmake/Qt6/qt.toolchain.cmake"
$qt_host_path = "$qt_root_dir/$compiler"
cmake -G Ninja -DCMAKE_BUILD_TYPE:STRING=Release -DCMAKE_TOOLCHAIN_FILE:FILEPATH="$toolchain_file" -DQT_HOST_PATH:PATH="$qt_host_path" "$src_dir"
if ($LastExitCode -ne 0) { $host.SetShouldExit($LastExitCode) }
cmake --build "$build_dir" --target package_app
if ($LastExitCode -ne 0) { $host.SetShouldExit($LastExitCode) }
