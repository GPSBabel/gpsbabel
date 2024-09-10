#!/bin/bash -e
VERSION=latest
while getopts "v:" opt; do
  case $opt in
    v) VERSION=$OPTARG;;
    *) exit 1;;
  esac
done
shift $((OPTIND -1))

container=$(docker create -q -i -t -w /app -v "$(pwd):/app" --net=host -v $HOME/.Xauthority:/root/.Xauthority -v /tmp/.X11-unix:/tmp/.X11-unix --env DISPLAY=$DISPLAY "tsteven4/gpsbabel:${VERSION}")
trap 'docker rm -f "${container}" >/dev/null' 0 1 2 3 15
docker start "${container}" >/dev/null
docker exec -i -t "${container}" setup_user.sh "$(id -u)" "$(id -g)"
docker exec -i -t -u "$(id -u):$(id -g)" "${container}" gpsbabelfe "$@"


