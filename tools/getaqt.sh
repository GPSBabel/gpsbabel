#!/bin/bash -e
# We have to build aqt from an unmerged PR that has fixes we need.

builddir=$(mktemp -d)
git clone https://github.com/miurahr/aqtinstall.git "${builddir}"
cd "${builddir}"
git fetch origin pull/1048/head
git checkout 6f0f842
pip3 install .
rm -fr "${builddir}"
