#!/bin/sh
set -ex

tooldir=$(cd "$(dirname "$0")" && pwd)

"$tooldir"/make_gpsbabel_doc.sh
xsltproc -o gpsbabel.fo xmldoc/babelpdf.xsl xmldoc/readme.xml
HOME=. fop -q -fo gpsbabel.fo -pdf gpsbabel.pdf
