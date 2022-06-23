# Were using qmake to build a library that has nothing to do with Qt.
# Dummy out qt defaults.
QT -= gui
QT -= core

CONFIG += staticlib
TEMPLATE = lib

TARGET = usb-1.0

SOURCES = core.c \
  descriptor.c \
  hotplug.c \
  io.c \
  strerror.c \
  sync.c \
  os/darwin_usb.c \
  os/events_posix.c \
  os/threads_posix.c

HEADERS = libusb.h \
  libusbi.h \
  version.h \
  version_nano.h \
  os/darwin_usb.h \
  os/events_posix.h \
  os/threads_posix.h

# We use libusb-1.0.0's hardcoded config.h for Xcode
# Note that we want don't want the path containing the Xcode config.h
# to be found when building gpsbabel, i.e. it can't be next to libusb.h.
INCLUDEPATH = Xcode

