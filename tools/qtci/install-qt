#!/bin/sh
#QT_VERSION=5.10.0

QT_VERSION=$1
echo Downloading Qt
wget  -c  https://download.qt.io/archive/qt/$(echo $1 |cut -d "." -f-2)/${QT_VERSION}/qt-opensource-linux-x64-${QT_VERSION}.run
INSTALLER=qt-opensource-linux-x64-${QT_VERSION}.run
ENVFILE=qt-${QT_VERSION}.env
echo Installing Qt


extract-qt-installer $PWD/$INSTALLER $PWD/Qt

echo Create $ENVFILE
cat << EOF > $ENVFILE
export PATH=$PWD/Qt/${QT_VERSION}/gcc_64/bin:$PATH
EOF
