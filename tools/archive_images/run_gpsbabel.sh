#!/bin/bash -e
VERSION=latest
while getopts "v:" opt; do
  case $opt in
    v) VERSION=$OPTARG;;
  esac
done
shift $(($OPTIND -1))

container=$(docker create -q -i -t -w /app -v "$(pwd):/app" "tsteven4/gpsbabel:${VERSION}")
docker start "${container}" >/dev/null
docker exec -i -t "${container}" setup_user.sh "$(id -u)" "$(id -g)"
docker exec -i -t -u "$(id -u):$(id -g)" "${container}" gpsbabel "$@"
docker rm -f "${container}" >/dev/null


