#!/bin/bash -ex
# you must be logged into docker for the push to succeed.

args=()
while getopts "s:mp" opt; do
  case $opt in
    s) versuffix="_$OPTARG";;
    m) args+=("--platform"); args+=("linux/amd64,linux/arm64");;
    p) args+=("--push");;
    *) echo "Usage: $0 [-s suffix] [-m] [-p]"; exit 1;;
  esac
done
shift $(($OPTIND -1))

tag=$(date -u +%Y%m%dT%H%M%SZ)
TMPDIR=$(mktemp -d)

cp "Dockerfile${versuffix}" "$TMPDIR"

docker buildx build \
             --pull --file "Dockerfile${versuffix}" \
             "${args[@]}" \
             --tag "tsteven4/gpsbabel_build_environment${versuffix}:latest" \
             --tag "tsteven4/gpsbabel_build_environment${versuffix}:$tag" \
             --progress=plain \
             "$TMPDIR"
/bin/rm -fr "$TMPDIR"
