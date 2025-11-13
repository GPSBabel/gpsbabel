#!/bin/bash -e
#
# this script is run on ci for the script stage of mac builds
#
 
function version_ge() { test "$(printf "%s\n%s" "$1" "$2" | sort -rV | head -n 1)" == "$1"; }

while getopts g:i: name
do
  case $name in
    g) CMAKEOPTIONS+=(-G "$OPTARG"); GENERATOR="$OPTARG";;
    i) CMAKEOPTIONS+=(-DGPSBABEL_CODESIGN_IDENTITY="$OPTARG");;
    ?) printf "Usage: %s: [-g generator] [-i identity] source_directory qt_version\n" "$0"
       exit 2;;
  esac
done
shift "$((OPTIND - 1))"
if [ $# -lt 2 ]; then
  printf "Usage: %s: [-g generator] [-i identity] source_directory qt_version\n" "$0"
  exit 1
fi
CMAKEOPTIONS+=("$1") # path-to-source
QTVER=$2
if version_ge "${QTVER}" 6.10.0; then
  DEPLOY_TARGET="13.0"
  ARCHS="x86_64;arm64"
elif version_ge "${QTVER}" 6.8.0; then
  DEPLOY_TARGET="12.0"
  ARCHS="x86_64;arm64"
elif version_ge "${QTVER}" 6.5.0; then
  DEPLOY_TARGET="11.0"
  ARCHS="x86_64;arm64"
elif version_ge "${QTVER}" 6.0.0; then
  DEPLOY_TARGET="10.14"
  ARCHS="x86_64;arm64"
elif version_ge "${QTVER}" 5.14.0; then
  DEPLOY_TARGET="10.13"
  ARCHS="x86_64"
else
  DEPLOY_TARGET="10.12"
  ARCHS="x86_64"
fi
  
# we assume we are on macOS, so date is not gnu date.
VERSIONID=${VERSIONID:-$(date -ju -f %Y-%m-%dT%H:%M:%S%z "$(git show -s --format="%aI" HEAD | sed 's/:\(..\)$/\1/')" +%Y%m%dT%H%MZ)-$(git rev-parse --short=7 HEAD)}

set -x
case "${GENERATOR}" in
Xcode | "Ninja Multi-Config")
  cmake -DCMAKE_OSX_ARCHITECTURES=${ARCHS} -DCMAKE_OSX_DEPLOYMENT_TARGET=${DEPLOY_TARGET} "${CMAKEOPTIONS[@]}"
  cmake --build . --config Release
  ctest -C Release --output-on-failure
  cmake --build . --config Release --target package_app
  ;;
*)
  cmake -DCMAKE_OSX_ARCHITECTURES=${ARCHS} -DCMAKE_OSX_DEPLOYMENT_TARGET=${DEPLOY_TARGET} -DCMAKE_BUILD_TYPE=Release "${CMAKEOPTIONS[@]}"
  cmake --build .
  ctest --output-on-failure
  cmake --build . --target package_app
  cmake --build . --target gpsbabel.html
  cmake --build . --target gpsbabel.pdf
  cmake --build . --target gpsbabel.org
  ;;
esac

# what is in there?
hdiutil attach -noverify gui/GPSBabelFE.dmg
find /Volumes/GPSBabelFE -ls
hdiutil detach /Volumes/GPSBabelFE

mv gui/GPSBabelFE.dmg "gui/GPSBabel-${VERSIONID}.dmg"
