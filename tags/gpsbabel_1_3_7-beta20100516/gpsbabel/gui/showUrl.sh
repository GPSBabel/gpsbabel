#!/bin/sh
# $Id: showUrl.sh,v 1.1 2009-07-05 21:14:56 robertl Exp $

if [ "$BROWSER" = "" ]; then
    BROWSER="firefox"
fi
$BROWSER $*
exit $?

