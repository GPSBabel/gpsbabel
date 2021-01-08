#!/bin/bash
set -ex

# validate install
function validate() {
  (
    set +e
    # shellcheck source=/dev/null
    source "${CACHEDIR}/qt.env"
    if [ "$(cygpath -u "$(qmake -query QT_INSTALL_BINS)")" != "${QTDIR}/bin" ]; then
      echo "ERROR: unexpected Qt location." >&2
      exit 1
    fi
    if [ "$(qmake -query QT_VERSION)" != "${QT_VERSION}" ]; then
      echo "ERROR: wrong Qt version." >&2
      exit 1
    fi
  )
}

QT_VERSION=${1:-5.12.10}
COMPILER=${2:-msvc2017_64}
if [ "${COMPILER}" = "msvc2017_64" ]; then
  PACKAGE_SUFFIX=win64_msvc2017_64
elif [ "${COMPILER}" = "msvc2017" ]; then
  PACKAGE_SUFFIX=win32_msvc2017
else
  echo "ERROR: unrecognized Qt compiler ${COMPILER}." >&2
  exit 1
fi

CACHEDIR=${HOME}/Cache
QTDIR=${CACHEDIR}/Qt/${QT_VERSION}/${COMPILER}

if [ -d "${QTDIR}/bin" ]; then
  echo "Using cached Qt."
else
  rm -fr ${CACHEDIR}
  QT_VERSION_SHORT=${QT_VERSION//./}
  curl -s -L -o qt-opensource-windows-x86-${QT_VERSION}.exe "https://download.qt.io/official_releases/qt/5.12/${QT_VERSION}/qt-opensource-windows-x86-${QT_VERSION}.exe"
  ls -l *.exe
  netsh advfirewall firewall add rule name=dummyupqt dir=out action=block program="$(cygpath -w "${PWD}/qt-opensource-windows-x86-${QT_VERSION}.exe")"
  "${PWD}/qt-opensource-windows-x86-${QT_VERSION}.exe" --verbose --script "${CI_BUILD_DIR}/tools/qtci/qt-install.qs" QTCI_OUTPUT="${CACHEDIR}/Qt" QTCI_PACKAGES=qt.qt5.${QT_VERSION_SHORT}.${PACKAGE_SUFFIX},qt.qt5.${QT_VERSION_SHORT}.qtwebengine
  netsh advfirewall firewall delete rule name=dummyupqt
  rm qt-opensource-windows-x86-${QT_VERSION}.exe
  ls "${CACHEDIR}/Qt"
  rm -fr "${CACHEDIR}/Qt/Docs"
  rm -fr "${CACHEDIR}/Qt/Examples"
  rm -fr "${CACHEDIR}/Qt/Tools"
  rm -f "${CACHEDIR}/Qt/MaintenanceTool.*"
  echo "export PATH=${QTDIR}/bin:\$PATH" > ${CACHEDIR}/qt.env
fi
validate
