#!/bin/sh
set -ex

web=$1
docversion=$2

sourcedir=$(dirname "$(dirname "$0")")

mkdir -p "${web}/htmldoc-${docversion}"
xsltproc \
  --stringparam base.dir "${web}/htmldoc-${docversion}/" \
  --stringparam root.filename "index" \
  "$sourcedir/xmldoc/babelmain.xsl" \
  xmldoc/readme.xml
"$sourcedir/tools/fixdoc" "${web}/htmldoc-${docversion}" "GPSBabel ${docversion}:"
"$sourcedir/tools/mkcapabilities" "${web}" "${web}/htmldoc-${docversion}"
cp gpsbabel.pdf "${web}/htmldoc-${docversion}/gpsbabel-${docversion}.pdf"
