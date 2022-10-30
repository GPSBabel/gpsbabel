#!/bin/sh
set -ex

perl xmldoc/makedoc
jing http://docs.oasis-open.org/docbook/rng/5.0/docbook.rng xmldoc/readme.xml
xsltproc -o gpsbabel.fo xmldoc/babelpdf.xsl xmldoc/readme.xml
HOME=. fop -q -fo gpsbabel.fo -pdf gpsbabel.pdf
