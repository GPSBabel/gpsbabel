macx|linux|openbsd {
  !defined(WITH_LIBUSB, var) {
    macx: WITH_LIBUSB = included
    linux|openbsd: WITH_LIBUSB = system
  }
  equals(WITH_LIBUSB, no) {
    JEEPS += jeeps/gpsusbstub.cc
  } else {
    DEFINES += HAVE_LIBUSB_1_0
    JEEPS += jeeps/gpslibusb.cc
    equals(WITH_LIBUSB, pkgconfig) {
      message("Using libusb-1.0 found by pkg-config")
      PKGCONFIG += libusb-1.0
      DEFINES += LIBUSB_H_INCLUDE=$$shell_quote(<libusb.h>)
    } else {
      linux|openbsd {
        equals(WITH_LIBUSB, system) {
          LIBS += "-lusb-1.0"
          DEFINES += LIBUSB_H_INCLUDE=$$shell_quote(<libusb-1.0/libusb.h>)
        } else:equals(WITH_LIBUSB, custom) {
          message("libusb-1.0 is enabled but but must be manually configured")
          message("  e.g. qmake WITH_LIBUSB=custom LIBS+=... INCLUDEPATH+=...")
          DEFINES += LIBUSB_H_INCLUDE=$$shell_quote(<libusb.h>)
        } else {
          error("WITH_LIBUSB=no|pkgconfig|system*|custom");
        }
      }

      macx {
        equals(WITH_LIBUSB, included) {
          # TODO: It would be better to create an archive and link to it
          #       to separate library build requirements from gpsbabel requirements.
          DEFINES += LIBUSB_H_INCLUDE=$$shell_quote(\"mac/libusb/libusb.h\")
          LIBS += -lobjc -framework IOKit -framework CoreFoundation
          INCLUDEPATH += mac/libusb \
                         mac/libusb/Xcode
          SOURCES += mac/libusb/core.c \
                     mac/libusb/descriptor.c \
                     mac/libusb/hotplug.c \
                     mac/libusb/io.c \
                     mac/libusb/strerror.c \
                     mac/libusb/sync.c \
                     mac/libusb/os/darwin_usb.c \
                     mac/libusb/os/poll_posix.c \
                     mac/libusb/os/threads_posix.c
          HEADERS += mac/libusb/hotplug.h \
                     mac/libusb/libusb.h \
                     mac/libusb/libusbi.h \
                     mac/libusb/version.h \
                     mac/libusb/version_nano.h \
                     mac/libusb/os/darwin_usb.h \
                     mac/libusb/os/poll_posix.h \
                     mac/libusb/os/threads_posix.h
        } else:equals(WITH_LIBUSB, custom) {
          message("libusb-1.0 is enabled but but must be manually configured")
          message("  e.g. qmake WITH_LIBUSB=custom LIBS+=... INCLUDEPATH+=...")
          DEFINES += LIBUSB_H_INCLUDE=$$shell_quote(<libusb.h>)
        } else {
          error("WITH_LIBUSB=no|pkgconfig|included*|custom");
        }
      }
    }
  }
}
