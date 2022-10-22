#!/bin/sh
set -ex

web=$1
docversion=$2

mkdir -p "${web}/htmldoc-${docversion}"
perl xmldoc/makedoc
xmllint --noout --valid xmldoc/readme.xml #valid and well-formed
xsltproc \
  --stringparam base.dir "${web}/htmldoc-${docversion}/" \
  --stringparam root.filename "index" \
  xmldoc/babelmain.xsl \
  xmldoc/readme.xml
tools/fixdoc "${web}/htmldoc-${docversion}" "GPSBabel ${docversion}:"
tools/mkcapabilities "${web}" "${web}/htmldoc-${docversion}"
cp gpsbabel.pdf "${web}/htmldoc-${docversion}/gpsbabel-${docversion}.pdf"
