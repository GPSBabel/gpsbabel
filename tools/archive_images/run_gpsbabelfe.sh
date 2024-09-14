#!/bin/bash -e
VERSION=latest
while getopts "v:" opt; do
  case $opt in
    v) VERSION=$OPTARG;;
    *) echo "Usage: $0 [-v tag]"; exit 1;;
  esac
done
shift $((OPTIND -1))

# we use a persistent container so the gpsbabel GUI state is saved and restored (in the container).
container_name=tsteven4_gpsbabel_${VERSION}

# if the container doesn't exist create it.
if [ -z "$(docker ps -a -q -f "name=^${container_name}$")" ]; then
  if [ -n "$LANG" ]; then
    OPTIONS+=(-e "LANG=$LANG")
  fi

  docker container create -q -i -t \
                          -e "DISPLAY=$DISPLAY" \
                          "${OPTIONS[@]}"  \
                          --name "${container_name}" \
                          --network=host \
                          -v "$(pwd):/app" \
                          -v "$HOME/.Xauthority:/root/.Xauthority" \
                          -v /tmp/.X11-unix:/tmp/.X11-unix \
                          -w /app \
                          "tsteven4/gpsbabel:${VERSION}" >/dev/null

  docker start "${container_name}" >/dev/null

# make sure the users uid/gid exist in the container.
  docker exec -i -t "${container_name}" setup_user.sh "$(id -u)" "$(id -g)"
fi

# if necessary start the container.
if [ -z "$(docker ps -a -q -f "status=running" -f "name=^${container_name}$")" ]; then
  docker start "${container_name}" >/dev/null
fi

# run the gpsbabel GUI
docker exec -i -t -u "$(id -u):$(id -g)" "${container_name}" gpsbabelfe "$@"
