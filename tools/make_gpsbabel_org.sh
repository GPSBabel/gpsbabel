#!/bin/sh
set -ex

web=$1
docversion=$2

mkdir -p "${web}/htmldoc-${docversion}"
perl xmldoc/makedoc
if [ "$(uname -s)" = "Darwin" ]; then
  # assume jing and docbook rng provided by homebrew
  /usr/local/bin/jing /usr/local/opt/docbook/docbook/xml/5.0/rng/docbook.rng xmldoc/readme.xml
else
  jing /usr/share/xml/docbook/schema/rng/5.0/docbook.rng xmldoc/readme.xml
fi
xsltproc \
  --stringparam base.dir "${web}/htmldoc-${docversion}/" \
  --stringparam root.filename "index" \
  xmldoc/babelmain.xsl \
  xmldoc/readme.xml
tools/fixdoc "${web}/htmldoc-${docversion}" "GPSBabel ${docversion}:"
tools/mkcapabilities "${web}" "${web}/htmldoc-${docversion}"
cp gpsbabel.pdf "${web}/htmldoc-${docversion}/gpsbabel-${docversion}.pdf"
