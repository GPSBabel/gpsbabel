#!/bin/bash

# avoid warning message:
# Qt: Session management error: Could not open network socket
# This occurs with an xrdp session, e.g. Xorg.

export -n SESSION_MANAGER

# Avoid issues with WSL2 https://github.com/microsoft/WSL/issues/11261, https://github.com/microsoft/WSL/issues/12616
# This can be fixed more generally with viruscamp's wslg-runtime-dir.service.
# Supposedly fixed in WSL prerelease 2.5.1, release 2.5.7 has been tested.
if [ -n "$XDG_RUNTIME_DIR" ]; then
  if [ -S "$XDG_RUNTIME_DIR/../${WAYLAND_DISPLAY:-wayland-0}" ]; then
    if [ ! -e "$XDG_RUNTIME_DIR/${WAYLAND_DISPLAY:-wayland-0}" ]; then
      ln -s "$XDG_RUNTIME_DIR/../${WAYLAND_DISPLAY:-wayland-0}" "$XDG_RUNTIME_DIR/${WAYLAND_DISPLAY:-wayland-0}"
    fi
  fi
fi

exec "$@"
