#!/bin/sh
set -ex

perl xmldoc/makedoc
jing https://docbook.org/xml/5.0/rng/docbook.rng xmldoc/readme.xml
xsltproc -o gpsbabel.fo xmldoc/babelpdf.xsl xmldoc/readme.xml
HOME=. fop -q -fo gpsbabel.fo -pdf gpsbabel.pdf
