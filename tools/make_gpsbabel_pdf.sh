#!/bin/sh
set -ex

sourcedir=$(dirname "$(dirname "$0")")

xsltproc -o gpsbabel.fo "$sourcedir/xmldoc/babelpdf.xsl" xmldoc/readme.xml
HOME=. fop -q -fo gpsbabel.fo -pdf gpsbabel.pdf
