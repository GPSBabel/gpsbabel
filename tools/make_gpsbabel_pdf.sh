#!/bin/sh
set -ex

perl xmldoc/makedoc
jing /usr/share/xml/docbook/schema/rng/5.0/docbook.rng xmldoc/readme.xml
xsltproc -o gpsbabel.fo xmldoc/babelpdf.xsl xmldoc/readme.xml
HOME=. fop -q -fo gpsbabel.fo -pdf gpsbabel.pdf
