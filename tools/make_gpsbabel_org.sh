#!/bin/sh
set -ex

web=${1:-gpsbabel.org}
docversion=${2:-x.y.z}
tooldir=$(cd "$(dirname "$0")" && pwd)

mkdir -p "${web}/htmldoc-${docversion}"
"$tooldir"/make_gpsbabel_doc.sh
xsltproc \
  --stringparam base.dir "${web}/htmldoc-${docversion}/" \
  --stringparam root.filename "index" \
  xmldoc/babelmain.xsl \
  xmldoc/readme.xml
"${tooldir}"/fixdoc "${web}/htmldoc-${docversion}" "GPSBabel ${docversion}:"
"${tooldir}"/mkcapabilities "${web}" "${web}/htmldoc-${docversion}"
cp gpsbabel.pdf "${web}/htmldoc-${docversion}/gpsbabel-${docversion}.pdf"
