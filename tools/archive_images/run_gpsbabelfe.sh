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

uid=$(id -u)
gid=$(id -g)

# if the container doesn't exist create it.
if [ -z "$(docker ps -a -q -f "name=^${container_name}$")" ]; then

  OPTIONS=()

  # 1. Pass the users LANG along
  if [ -n "$LANG" ]; then
    OPTIONS+=(-e "LANG=$LANG")
  fi

  # 2. Dynamically build the optional Wayland mount arguments if the host uses Wayland
  if [ -n "$WAYLAND_DISPLAY" ] && [ -d "/run/user/$(id -u)" ]; then
    OPTIONS+=(
      -e "WAYLAND_DISPLAY=$WAYLAND_DISPLAY"
      -e "XDG_RUNTIME_DIR=/run/user/${uid}"
      -v "/run/user/${uid}:/run/user/${uid}"
    )
  fi

  # 3. Check if hardware graphics acceleration is available on the host
  if [ -d "/dev/dri" ]; then
    OPTIONS+=(--device /dev/dri)
  fi

  docker container create -q -i -t \
                          -e "DISPLAY=$DISPLAY" \
                          "${OPTIONS[@]}" \
                          --name "${container_name}" \
                          --network=host \
                          -v "$(pwd):/app" \
                          -v "$HOME/.Xauthority:/root/.Xauthority" \
                          -v /tmp/.X11-unix:/tmp/.X11-unix \
                          -w /app \
                          "tsteven4/gpsbabel:${VERSION}" >/dev/null

  docker start "${container_name}" >/dev/null

# make sure the users uid/gid exist in the container.
  docker exec -i -t -u 0:0 "${container_name}" setup_user.sh "${uid}" "${gid}" "${LANG}"
fi

# if necessary start the container.
if [ -z "$(docker ps -a -q -f "status=running" -f "name=^${container_name}$")" ]; then
  docker start "${container_name}" >/dev/null
fi

# run the gpsbabel GUI

# XWayland Authorization Bridge (Crucial for Ubuntu 26.04 host)
WAS_XHOST_GRANTED=true

if [ -n "$DISPLAY" ] && command -v xhost >/dev/null 2>&1; then
  if ! xhost | grep -qEi "LOCAL:|SI:localuser:$(id -un)"; then
    WAS_XHOST_GRANTED=false
    xhost +local:$(id -un) >/dev/null 2>&1 || true
  fi
fi

cleanup_xhost() {
  if [ "$WAS_XHOST_GRANTED" = false ] && command -v xhost >/dev/null 2>&1; then
    xhost -local:$(id -un) >/dev/null 2>&1 || true
  fi
}

trap cleanup_xhost EXIT

docker exec -i -t -u "${uid}:${gid}" "${container_name}" gpsbabelfe "$@"
