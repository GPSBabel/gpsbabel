#!/bin/bash
if ! getent group "$2" >/dev/null 2>&1; then
  groupadd -g "$2" gpsbabel_user
fi
if ! getent passwd "$1" >/dev/null 2>&1; then
  useradd -g "$2" -u "$1" -m gpsbabel_user
fi
