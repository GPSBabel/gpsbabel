#!/bin/bash -ex
#
# this script is triggered by SCM changes and is run on the build server.
# output is conditionally mailed to gpsbabel-code.
#

SOURCE_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}" )/.." && pwd)"

# echo some system info to log
uname -a
if [ -e /etc/system-release ]; then
	cat /etc/system-release
fi
if [ -e /etc/os-release ]; then
	cat /etc/os-release
fi
git -C "${SOURCE_DIR}" --no-pager log -n 1

# build and test keeping output within the pwd.
GBTEMP=$(mktemp -d -p "$(pwd)" GBTEMPXXXX)
export GBTEMP

rm -rf bld-sanitizeaddress
mkdir bld-sanitizeaddress
pushd bld-sanitizeaddress
#note that debug will also enable assertions.
cmake -DCMAKE_BUILD_TYPE=Debug -DGPSBABEL_EXTRA_COMPILE_OPTIONS="-fsanitize=address -fno-omit-frame-pointer" -DGPSBABEL_EXTRA_LINK_OPTIONS="-fsanitize=address" -G Ninja "${SOURCE_DIR}"
cmake --build . --target gpsbabel --verbose
cmake --build . --target check
popd

rm -rf bld-sanitizeundefined
mkdir bld-sanitizeundefined
pushd bld-sanitizeundefined
cmake -DCMAKE_BUILD_TYPE=Debug -DGPSBABEL_EXTRA_COMPILE_OPTIONS="-fsanitize=undefined -fsanitize=float-divide-by-zero -fno-sanitize-recover=undefined,float-divide-by-zero -fno-omit-frame-pointer" -DGPSBABEL_EXTRA_LINK_OPTIONS="-fsanitize=undefined -fsanitize=float-divide-by-zero -fno-sanitize-recover=undefined,float-divide-by-zero" -G Ninja "${SOURCE_DIR}"
cmake --build . --target gpsbabel --verbose
cmake --build . --target check
popd

rm -rf "${GBTEMP}"

# run clazy on both gpsbabel and gpsbabelfe.
# unlike qmake, cmake uses system includes for Qt which quiets warnings
# from the Qt headers.
rm -rf bld-clazy
mkdir bld-clazy
pushd bld-clazy
export CLAZY_CHECKS=level0,level1,no-non-pod-global-static,no-qstring-ref
cmake -DCMAKE_CXX_COMPILER=clazy -DCMAKE_BUILD_TYPE=Debug -DGPSBABEL_ENABLE_PCH=OFF -G Ninja "${SOURCE_DIR}"
cmake --build . 2>&1 | tee clazy.log
if grep -- '-Wclazy' clazy.log; then
  exit 1
else
  exit 0
fi
