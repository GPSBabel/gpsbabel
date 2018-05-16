#!/bin/sh
set -e -x
DISTDIR=dist
QMAKE=${QMAKE:-qmake}
LINUXDEPLOYQT=${LINUXDEPLOYQT:-linuxdeployqt}
rm -fr $DISTDIR
mkdir $DISTDIR
cp gpsbabelfe.desktop $DISTDIR
cp images/appicon.png $DISTDIR/gpsbabelfe.png
cp objects/gpsbabelfe-bin $DISTDIR/gpsbabelfe
if [ -x ../gpsbabel ]; then
  cp ../gpsbabel $DISTDIR/gpsbabel
elif [ -x ../GPSBabel ]; then
  cp ../GPSBabel $DISTDIR/gpsbabel
else
  echo "Couldn't find command line executable gpsbabel or GPSBabel." >&2
  exit 1;
fi
cp COPYING.txt $DISTDIR
cp gmapbase.html $DISTDIR
mkdir $DISTDIR/translations
cp *.qm $DISTDIR/translations
# https://github.com/probonopd/linuxdeployqt
$LINUXDEPLOYQT $DISTDIR/gpsbabelfe -qmake=$QMAKE -appimage -verbose=2 -executable=$DISTDIR/gpsbabel -exclude-libs="libnss3.so,libnssutil3.so" 2>&1 | tee makeappimage.log
