#!/bin/sh
QTLIBDIR=/usr/lib
QTSHDIR=/usr/share/qt4

DISTDIR=gpsbabel-1.3.7Beta
rm -rf $DISTDIR
mkdir $DISTDIR
mkdir $DISTDIR/plugins
mkdir $DISTDIR/translations

cp `ldd objects/gpsbabelfe-bin  | grep libQtCore |awk '{print $3}'` $DISTDIR
cp `ldd objects/gpsbabelfe-bin  | grep libQtGui |awk '{print $3}'` $DISTDIR
cp `ldd objects/gpsbabelfe-bin  | grep libQtWebKit |awk '{print $3}'` $DISTDIR
cp `ldd objects/gpsbabelfe-bin  | grep libQtXml |awk '{print $3}'` $DISTDIR
cp `ldd objects/gpsbabelfe-bin  | grep libQtNetwork |awk '{print $3}'` $DISTDIR
cp `ldd objects/gpsbabelfe-bin  | grep libQtDBus |awk '{print $3}'` $DISTDIR
cp `ldd objects/gpsbabelfe-bin  | grep libphonon |awk '{print $3}'` $DISTDIR

cp -r $QTSHDIR/plugins/imageformats $DISTDIR/plugins
cp $QTSHDIR/translations/qt_*.qm $DISTDIR/translations/

# Generate the compiled translations
TSFILES="gpsbabel_de.ts 
gpsbabel_es.ts 
gpsbabel_fr.ts 
gpsbabel_hu.ts 
gpsbabel_it.ts 
gpsbabelfe_de.ts 
gpsbabelfe_es.ts 
gpsbabelfe_fr.ts 
gpsbabelfe_hu.ts 
gpsbabelfe_it.ts"

#
lrelease $TSFILES
QMFILES=`echo $TSFILES | sed -e 's/\.ts/.qm/g'`
cp $TSFILES $DISTDIR/translations
#
# Only Spanish and German are moderately OK.
cp gpsbabel_es.ts gpsbabelfe_es.ts gpsbabel_de.ts gpsbabelfe_de.ts $DISTDIR/translations

# Now our gui
cp gmapbase.html $DISTDIR/
cp gpsbabelfe $DISTDIR/
chmod +x $DISTDIR/gpsbabelfe
cp objects/gpsbabelfe-bin $DISTDIR
cp qt.conf $DISTDIR/
cp ../gpsbabel $DISTDIR/
#
# Help needs to be donea
cp COPYING $DISTDIR/
cp AUTHORS $DISTDIR/
cp README.contrib $DISTDIR/
cp README.gui $DISTDIR/


rm -f $DISTDIR.tar $DISTDIR.tar.bz2
tar cvf $DISTDIR.tar $DISTDIR
bzip2 $DISTDIR.tar

# cleanup needed
echo $QMFILES
rm -f $QMFILES
