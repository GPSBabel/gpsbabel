#!/bin/sh

echo "/* This file is machine-generated from the contents of style/ */"
echo "/* by mkstyle.sh.   Editing it by hand is an exceedingly bad idea. */"
echo 

# set the locale for sorting so that the collate order doesn't depend
# on the users environment.
LC_COLLATE=C
export LC_COLLATE

# require gnu sed even though we aren't using gnu extensions.
# this avoids portability issues with other seds.
if gsed v /dev/null 1>/dev/null 2>&1; then
  SED=gsed
elif sed v /dev/null 1>/dev/null 2>&1; then
  # sed is gnu sed
  SED=sed
elif [ "$(uname -s)" = "Darwin" ]; then
  # BSD sed is usually fine, but if a style file is encoded in ISO-8859 then it
  # can echo a warning "sed: RE error: illegal byte sequence" to stderr and the
  # output can be corrupted, while return exit status 0.  Since we no longer use
  # this encoding for style files allow Darwin's sed.
  SED=/usr/bin/sed
elif [ "$(uname -s)" = "FreeBSD" ]; then
  # BSD sed is fine
  SED=/usr/bin/sed
else
  echo "Error: can't find gnu sed" 1>&2
  exit 1
fi

echo "#include <QtCore/QVector>"
echo "#include \"defs.h\""
echo "#if CSVFMTS_ENABLED"
for i in "$(dirname "$0")"/style/*.style
do
  A=$(basename "$i" .style)
  if [ "x${ALIST}" = "x" ]; then
    ALIST="{ \"$A\", $A }"
  else
    ALIST="{ \"$A\", $A }, $ALIST"
  fi
  echo "static char $A[] ="
  $SED 's/\\/\\\\/;s/"/\\"/g;s/^\(.\)/"\1/g;s/\(.\)$/\1\\n"/g;s/^\(.\)/  \1/' "$i"
  echo "  ;"
done
echo "const QVector<style_vecs_t> style_list = {$ALIST};"
echo "#else /* CSVFMTS_ENABLED */"
echo "const QVector<style_vecs_t> style_list;"
echo "#endif /* CSVFMTS_ENABLED */"

