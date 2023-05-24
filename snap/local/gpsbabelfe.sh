#!/bin/bash

if [ -n "$XDG_RUNTIME_DIR" ]; then
  if [ -S "$XDG_RUNTIME_DIR/../${WAYLAND_DISPLAY:-wayland-0}" ]; then
    if [ ! -e "$XDG_RUNTIME_DIR/${WAYLAND_DISPLAY:-wayland-0}" ]; then
      ln -s "$XDG_RUNTIME_DIR/../${WAYLAND_DISPLAY:-wayland-0}" "$XDG_RUNTIME_DIR/${WAYLAND_DISPLAY:-wayland-0}"
    fi
  fi
fi

exec "$@"
