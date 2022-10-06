#!/bin/sh
set -ex

xsltproc \
  --output gpsbabel.html \
  --stringparam toc.section.depth "1" \
  --stringparam html.cleanup "1" \
  --stringparam make.clean.html "1" \
  --stringparam html.valid.html "1" \
  --stringparam html.stylesheet "https://www.gpsbabel.org/style3.css" \
  http://docbook.sourceforge.net/release/xsl/current/xhtml/docbook.xsl \
  xmldoc/readme.xml
