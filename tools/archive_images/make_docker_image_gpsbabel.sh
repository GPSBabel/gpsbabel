#!/bin/bash -ex
# you must be logged into docker for the push to succeed.

args=()
while getopts "s:mpl" opt; do
  case $opt in
    s) versuffix="$OPTARG";;
    m) args+=("--platform"); args+=("linux/amd64,linux/arm64");;
    p) args+=("--push");;
    l) args+=("--load");;
    *) echo "Usage: $0 [-s suffix] [-m] [-p]"; exit 1;;
  esac
done
shift $((OPTIND -1))

if [ "${versuffix}" == "dev" ]; then
  tag="master"
else
  tag="gpsbabel_$(echo "${versuffix}" | tr . _)"
fi

sourcedir=$(git rev-parse --show-toplevel)

origin=$(git remote get-url origin | sed 's|.*github.com/\(.*\)|\1|;s|\.git$||')
if [ "${origin,,}" == "GPSBabel/gpsbabel" ]; then
  dockerrepo=tsteven4/gpsbabel
else
  dockerrepo=tsteven4/testing
fi

TMPDIR=$(mktemp -d)
trap "rm -fr ${TMPDIR}" 0 1 2 3 15
cd "${TMPDIR}"

# get the branch we want to build
git clone --depth 1 --branch "${tag}" https://github.com/tsteven4/gpsbabel.git
cd gpsbabel

# override the docker build instructions with the current version.
# note the docker build instructions may not exist in the branch,
# or may exist and be outdated.
mkdir -p tools
rm -fr tools/archive_images
cp -pr "${sourcedir}/tools/archive_images" tools

# patch if necessary
patch="tools/archive_images/gpsbabel_$(echo "${versuffix}" | tr . _).patch"
if [ -e "${patch}" ]; then
  git apply -v "${patch}"
fi

docker buildx build \
             --pull \
             --file "tools/archive_images/Dockerfile_gpsbabel_${versuffix}" \
             "${args[@]}" \
             --tag "${dockerrepo}:${versuffix}" \
             --progress=plain \
             .
