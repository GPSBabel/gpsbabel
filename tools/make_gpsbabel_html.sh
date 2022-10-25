#!/bin/sh
set -ex

perl xmldoc/makedoc
if [ "$(uname -s)" = "Darwin" ]; then
  # assume jing and docbook rng provided by homebrew
  /usr/local/bin/jing /usr/local/opt/docbook/docbook/xml/5.0/rng/docbook.rng xmldoc/readme.xml
else
  jing /usr/share/xml/docbook/schema/rng/5.0/docbook.rng xmldoc/readme.xml
fi
xsltproc \
  --output gpsbabel.html \
  --stringparam toc.section.depth "1" \
  --stringparam html.cleanup "1" \
  --stringparam make.clean.html "1" \
  --stringparam html.valid.html "1" \
  --stringparam html.stylesheet "https://www.gpsbabel.org/style3.css" \
  http://docbook.sourceforge.net/release/xsl-ns/current/xhtml/docbook.xsl \
  xmldoc/readme.xml
