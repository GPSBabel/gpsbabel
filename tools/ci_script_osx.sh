#!/bin/bash -ex
#
# this script is run on ci for the script stage of mac builds
#
 
function version_ge() { test "$(printf "%s\n%s" "$1" "$2" | sort -rV | head -n 1)" == "$1"; }

if [ $# -lt 2 ]; then
  echo "Usage: $0 source_directory qt_version [Generator]"
  exit 1
fi
SOURCE_DIR=$1
QTVER=$2
if [ $# -ge 3 ]; then
  if [ -n "$3" ]; then
    GENERATOR[0]=-G
    GENERATOR[1]=$3
  fi
fi
if version_ge "${QTVER}" 6.5.0; then
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

# debug tokens
"$(cd "$(dirname "${BASH_SOURCE[0]}" )" && pwd)"/ci_tokens

case "${GENERATOR[1]}" in
Xcode | "Ninja Multi-Config")
  cmake "${SOURCE_DIR}" -DCMAKE_OSX_ARCHITECTURES=${ARCHS} -DCMAKE_OSX_DEPLOYMENT_TARGET=${DEPLOY_TARGET} "${GENERATOR[@]}"
  cmake --build . --config Release
  cmake --build . --config Release --target gpsbabelfe_test
  ctest -C Release
  cmake --build . --config Release --target package_app
  ;;
*)
  cmake "${SOURCE_DIR}" -DCMAKE_OSX_ARCHITECTURES=${ARCHS} -DCMAKE_OSX_DEPLOYMENT_TARGET=${DEPLOY_TARGET} -DCMAKE_BUILD_TYPE=Release "${GENERATOR[@]}"
  cmake --build .
  cmake --build . --target gpsbabelfe_test
  ctest
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
