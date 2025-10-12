#!/bin/bash -ex
# you must be logged into docker for the push to succeed.

versuffix=${1:+$1} # tag name must be lower case
TMPDIR=$(mktemp -d)

cp "Dockerfile_gpsbabel_${versuffix}" "$TMPDIR"
cp setup_user.sh "$TMPDIR"
cp ./*.patch "$TMPDIR"

docker build --pull --file "Dockerfile_gpsbabel_${versuffix}" \
             --tag "tsteven4/gpsbabel:${versuffix}" \
             --progress=plain \
             "$TMPDIR"

/bin/rm -fr "$TMPDIR"
#docker push tsteven4/gpsbabel:latest
docker image ls
