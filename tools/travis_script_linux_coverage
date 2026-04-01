#!/bin/bash -ex
#
# generate coverage report
# lcov.info can be uploaded to codacy
# coverage_report/index.html is for humans

rm -rf bld-coverage
mkdir bld-coverage

cmake -DCMAKE_BUILD_TYPE=Release -DGPSBABEL_EXTRA_COMPILE_OPTIONS="--coverage" -DGPSBABEL_EXTRA_LINK_OPTIONS="--coverage" -G Ninja -S . -B bld-coverage
cmake --build bld-coverage --target check
lcov --capture --directory bld-coverage --base-directory "$(pwd)" --no-external --output-file lcov.info
genhtml lcov.info --demangle-cpp --output-directory coverage_report
