#!/bin/sh
set -ex

perl xmldoc/makedoc
xmllint --noout --valid xmldoc/readme.xml #valid and well-formed
xsltproc -o gpsbabel.fo xmldoc/babelpdf.xsl xmldoc/readme.xml
HOME=. fop -q -fo gpsbabel.fo -pdf gpsbabel.pdf
