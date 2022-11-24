#!/bin/sh
set -ex

perl xmldoc/makedoc
xmllint --noout --relaxng http://www.oasis-open.org/docbook/xml/5.0/rng/docbook.rng xmldoc/readme.xml
# the following doesn't seem to work.
#xmllint --noout --schematron http://www.oasis-open.org/docbook/xml/5.0/sch/docbook.sch xmldoc/readme.xml
# jing and many depedencies removed from fedora
if command -v jing >/dev/null 2>&1; then
  jing http://docs.oasis-open.org/docbook/xml/5.0/rng/docbook.rng xmldoc/readme.xml
  # can seed a failure by removing version="5.0" from xmldoc/readme.xml
  jing http://docs.oasis-open.org/docbook/xml/5.0/sch/docbook.sch xmldoc/readme.xml
fi
