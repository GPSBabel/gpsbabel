
export QTDIR=~/qt4/qt-x11-opensource-src-4.4.3

PATH=$QTDIR/bin:$PATH
LD_LIBRARY_PATH=$QTDIR/lib:$LD_LIBRARY_PATH
export QT_INSTALL_HEADERS=$QTDIR/include
export QMAKESPEC=$QTDIR/mkspecs/linux-g++ 



