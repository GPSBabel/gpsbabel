#!/bin/sh

echo "/* This file is machine-generated from the contents of style/ */"
echo "/* by mkstyle.sh.   Editing it by hand is an exeedingly bad idea. */"
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
elif [ `uname -s` = "FreeBSD" ]; then
       # BSD sed is fine
    SED=/usr/bin/sed
else
	echo "Error: can't find gnu sed" 1>&2
	exit 1
fi

echo "#include \"defs.h\""
echo "#if CSVFMTS_ENABLED"
nstyles="0"
for i in `dirname $0`/style/*.style
do
	A=`basename $i | sed "s/.style$//"`
	[ $A = "README" ] && continue
	[ $A = "custom.style" ] && continue
	ALIST="{ \"$A\", $A } , $ALIST"
	echo "static char $A[] ="
	$SED 's/\\/\\\\/;s/"/\\"/g;s/^\(.\)/"\1/g;s/\(.\)$/\1\\n"/g;s/^\(.\)/  \1/' $i
	echo "  ;"
	nstyles=`expr $nstyles + 1`;
done
echo "style_vecs_t style_list[] = {$ALIST {0,0}};"
echo "size_t nstyles = $nstyles;"
echo "#else /* CSVFMTS_ENABLED */"
echo "style_vecs_t style_list[] = {{0,0}};"
echo "size_t nstyles = 0;"
echo "#endif /* CSVFMTS_ENABLED */"

