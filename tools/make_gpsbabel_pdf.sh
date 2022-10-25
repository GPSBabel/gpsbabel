#!/bin/sh
set -ex

perl xmldoc/makedoc
if [ "$(uname -s)" = "Darwin" ]; then
  # assume jing and docbook rng provided by homebrew
  /usr/local/bin/jing /usr/local/opt/docbook/docbook/xml/5.0/rng/docbook.rng xmldoc/readme.xml
else
  jing /usr/share/xml/docbook/schema/rng/5.0/docbook.rng xmldoc/readme.xml
fi
xsltproc -o gpsbabel.fo xmldoc/babelpdf.xsl xmldoc/readme.xml
HOME=. fop -q -fo gpsbabel.fo -pdf gpsbabel.pdf
