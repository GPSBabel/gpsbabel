#!/bin/bash
set -ex

# validate install
function validate() {
  (
    set +e
    PATH=${QTDIR}/bin:$PATH
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

QT_VERSION=${1:-6.5.3}
COMPILER=${2:-msvc2022_64}
METHOD=${3:-default}
CROSS_COMPILER=${4}

HOST=windows
if [ "${COMPILER}" = "msvc2017_64" ]; then
  PACKAGE_SUFFIX=win64_msvc2017_64
elif [ "${COMPILER}" = "msvc2017" ]; then
  PACKAGE_SUFFIX=win32_msvc2017
elif [ "${COMPILER}" = "msvc2019_64" ]; then
  PACKAGE_SUFFIX=win64_msvc2019_64
elif [ "${COMPILER}" = "msvc2019" ]; then
  PACKAGE_SUFFIX=win32_msvc2019
elif [ "${COMPILER}" = "msvc2022_64" ]; then
  PACKAGE_SUFFIX=win64_msvc2022_64
elif [ "${COMPILER}" = "msvc2022_arm64" ]; then
    PACKAGE_SUFFIX=win64_msvc2022_arm64
    HOST=windows_arm64
else
  echo "ERROR: unrecognized Qt compiler ${COMPILER}." >&2
  exit 1
fi

if [ -n "${CROSS_COMPILER}" ]; then
  if [ "${CROSS_COMPILER}" = "msvc2022_arm64" ]; then
    PACKAGE_SUFFIX_CROSS=win64_msvc2022_arm64_cross_compiled
  else
    echo "ERROR: unrecognized Qt cross compiler ${CROSS_COMPILER}." >&2
    exit 1
  fi
fi

CACHEDIR=${HOME}/Cache
QTDIR=${CACHEDIR}/Qt/${QT_VERSION}/${COMPILER}

if [ -d "${QTDIR}/bin" ]; then
  echo "Using cached Qt."
else
  rm -fr "${CACHEDIR}"
  mkdir -p "${CACHEDIR}"

  if [ "${METHOD}" = "aqt" ]; then
    pip3 install 'aqtinstall>=3.3.0'
    "${CI_BUILD_DIR}/tools/ci_install_qt.sh" "${HOST}" "${QT_VERSION}" "${PACKAGE_SUFFIX}" "${CACHEDIR}/Qt"
    if [ -n "${PACKAGE_SUFFIX_CROSS}" ]; then
      "${CI_BUILD_DIR}/tools/ci_install_qt.sh" "${HOST}" "${QT_VERSION}" "${PACKAGE_SUFFIX_CROSS}" "${CACHEDIR}/Qt"
    fi
  else
    echo "ERROR: unknown installation method ${METHOD}." >&2
    exit 1
  fi
fi
validate
