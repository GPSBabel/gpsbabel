#!/bin/bash -e

# install Qt
#
# Examples:
# ci_install_qt.sh mac 6.2.0 clang_64 /tmp/Qt
# ci_install_qt.sh windows 6.2.0 win64_msvc2019_64 /tmp/Qt
# ci_install_qt.sh linux 6.2.0 gcc_64 /tmp/Qt

host=$1
version=$2
arch=$3
outdir=$4

available=( $(aqt list-qt "$host" desktop --modules "$version" "$arch") )

# remove commercial/GPLv3 modules, see https://doc-snapshots.qt.io/qt6-dev/qtmodules.html
remove=( \
debug_info \
qtcharts \
qtdatavis3d \
qtlottie \
qtnetworkauth \
qtquick3d \
qtquick3dphysics \
qtquicktimeline \
qtwebglplugin \
qtshadertools \
qtvirtualkeyboard \
qtwaylandcompositor \
)

mods=()
for a in "${available[@]}"
do
  skip=false
  for r in "${remove[@]}"
  do
    if [ "$a" == "$r" ]; then
      skip=true
    fi
  done
  if [ $skip == false ]; then
    mods+=( "$a" )
  fi
done
echo Installing "${mods[@]}"
aqt install-qt "$host" desktop "$version" "$arch" -O "$outdir" -m "${mods[@]}"

