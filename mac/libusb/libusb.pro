# Were using qmake to build a library that has nothing to do with Qt.
# Dummy out qt defaults.
QT -= gui
QT -= core

CONFIG += staticlib
TEMPLATE = lib

TARGET = usb-1.0

# Set static library extension to o instead of the default a.
# This makes it more likely ld will find our library instead of
# another, e.g. one in /usr/local/lib from Homebrew.
QMAKE_EXTENSION_STATICLIB = o
QMAKE_CLEAN += libusb-1.0.o

SOURCES = core.c \
  descriptor.c \
  hotplug.c \
  io.c \
  strerror.c \
  sync.c \
  os/darwin_usb.c \
  os/poll_posix.c \
  os/threads_posix.c

HEADERS = hotplug.h \
  libusb.h \
  libusbi.h \
  version.h \
  version_nano.h \
  os/darwin_usb.h \
  os/poll_posix.h \
  os/threads_posix.h

# We use libusb-1.0.0's hardcoded config.h for Xcode
# Note that we want don't want the path containing the Xcode config.h
# to be found when building gpsbabel, i.e. it can't be next to libusb.h.
INCLUDEPATH = Xcode

