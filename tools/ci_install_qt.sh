#!/bin/bash -e

# install Qt
#
# Examples:
# ci_install_qt.sh mac 6.8.1 clang_64 /tmp/Qt
# ci_install_qt.sh windows 6.8.1 win64_msvc2022_64 /tmp/Qt
# ci_install_qt.sh linux 6.8.1 linux_gcc_64 /tmp/Qt

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
qtgraphs \
qtgrpc \
qthttpserver \
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
  if [[ "$a" == *".debug_information" ]]; then
    skip=true
  fi
  if [ $skip == false ]; then
    mods+=( "$a" )
  fi
done
echo Installing "${mods[@]}"
aqt install-qt "$host" desktop "$version" "$arch" -O "$outdir" -m "${mods[@]}"

