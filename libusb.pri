unix {
  !defined(WITH_LIBUSB, var) {
    mac: WITH_LIBUSB = included
    !mac: WITH_LIBUSB = system
  }
  equals(WITH_LIBUSB, no) {
    message("libusb-1.0 disabled")
    JEEPS += jeeps/gpsusbstub.cc
  } else {
    DEFINES += HAVE_LIBUSB_1_0
    JEEPS += jeeps/gpslibusb.cc
    equals(WITH_LIBUSB, pkgconfig) {
      message("Using libusb-1.0 found by pkg-config")
      PKGCONFIG += libusb-1.0
      DEFINES += LIBUSB_H_INCLUDE=$$shell_quote(<libusb.h>)
    } else {
      !mac {
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

      mac {
        equals(WITH_LIBUSB, included) {
          # TODO: It would be better to create an archive and link to it
          #       to separate library build requirements from gpsbabel requirements.
          DEFINES += LIBUSB_H_INCLUDE=$$shell_quote(\"mac/libusb/libusb.h\")
          LIBS += -lobjc -framework IOKit -framework CoreFoundation -framework Security
          INCLUDEPATH += mac/libusb \
                         mac/libusb/Xcode
          SOURCES += \
            mac/libusb/core.c \
            mac/libusb/descriptor.c \
            mac/libusb/hotplug.c \
            mac/libusb/io.c \
            mac/libusb/strerror.c \
            mac/libusb/sync.c \
            mac/libusb/os/darwin_usb.c \
            mac/libusb/os/events_posix.c \
            mac/libusb/os/threads_posix.c
          HEADERS += \
            mac/libusb/libusb.h \
            mac/libusb/libusbi.h \
            mac/libusb/version.h \
            mac/libusb/version_nano.h \
            mac/libusb/os/darwin_usb.h \
            mac/libusb/os/events_posix.h \
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
