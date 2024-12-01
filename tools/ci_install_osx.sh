#!/bin/bash -ex
#
# This script is run on travis for the install stage of mac builds.
#

function version_ge() { test "$(printf "%s\n%s" "$1" "$2" | sort -rV | head -n 1)" == "$1"; }

#debug failed install
function debug() {
  cat "${CACHEDIR}/qt-${QT_VERSION}.env"
  find "${CACHEDIR}" -maxdepth 3 -ls
  echo "$1" >&2
  exit 1
}

# validate install
function validate() {
  (
    set +e
    # shellcheck source=/dev/null
    source "${CACHEDIR}/qt-${QT_VERSION}.env"
    if [ "$(qmake -query QT_INSTALL_BINS)" != "${QTDIR}/bin" ]; then
      debug "ERROR: unexpected Qt location."
    fi
    if [ "$(qmake -query QT_VERSION)" != "${QT_VERSION}" ]; then
      debug "ERROR: wrong Qt version."
    fi
  )
}

QT_VERSION=${1:-6.2.4}
METHOD=${2:-aqt}

# our expectation is that install-qt creates $QTDIR, $QTDIR/bin.
CACHEDIR=${HOME}/Cache
if [ "$METHOD" = "aqt" ]; then
  if version_ge "${QT_VERSION}" 6.1.2; then
    QTDIR=${CACHEDIR}/Qt/${QT_VERSION}/macos
  else
    QTDIR=${CACHEDIR}/Qt/${QT_VERSION}/clang_64
  fi
else
  QTDIR=${CACHEDIR}/Qt/${QT_VERSION}/clang_64
fi

if [ -d "${QTDIR}/bin" ]; then
  echo "Using cached Qt."
else
  rm -fr "${CACHEDIR}"
  mkdir -p "${CACHEDIR}"
  pushd "${CACHEDIR}"

  if [ "$METHOD" = "artifactory" ]; then
    (
      # Do not leak keys
      set +x
      if [ -z "${ARTIFACTORY_API_KEY}" ]; then
        echo "An untrusted build cannot load cache from artifactory."
        echo "A PR from a forked repo will be an untrusted build."
        echo "The cache can be loaded from a trusted build of the default branch."
        echo "A PR from the original repo will be trusted, but has it's own cache."
        echo "However, when that PR is merged it will build cache for the default branch."
        echo "Also, the cron job should rebuild the cache for the default branch, if necessary,"
        echo "once that flavor of build in .travis.yml makes it into the default branch."
        exit 1
      else
        archive=qt-${QT_VERSION}-release-macos.tar.xz
        curl -u "${ARTIFACTORY_USER}:${ARTIFACTORY_API_KEY}" "${ARTIFACTORY_BASE_URL}/${archive}" -o "/tmp/${archive}"
        tar -x -J -f "/tmp/${archive}"
        echo "export PATH=${QTDIR}/bin:\$PATH" > "${CACHEDIR}/qt-${QT_VERSION}.env"
        rm -f "/tmp/${archive}"
      fi
     )
  elif [ "$METHOD" = "aqt" ]; then
    pip3 install aqtinstall>=3.1.19
    "${CI_BUILD_DIR}/tools/ci_install_qt.sh" mac "${QT_VERSION}" clang_64 "${CACHEDIR}/Qt"
    echo "export PATH=${QTDIR}/bin:\$PATH" > "${CACHEDIR}/qt-${QT_VERSION}.env"
  else
    echo "ERROR: unknown installation method ${METHOD}." >&2
    exit 1
  fi
  popd
  validate
fi
