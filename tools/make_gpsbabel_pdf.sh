#!/bin/sh
set -ex

perl xmldoc/makedoc
xmlwf xmldoc/readme.xml #check for well-formedness
xmllint --noout --valid xmldoc/readme.xml #validate
xsltproc -o gpsbabel.fo xmldoc/babelpdf.xsl xmldoc/readme.xml
HOME=. fop -q -fo gpsbabel.fo -pdf gpsbabel.pdf
