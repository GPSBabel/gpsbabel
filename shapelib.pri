!defined(WITH_SHAPELIB, var):WITH_SHAPELIB = included
equals(WITH_SHAPELIB, no) {
  message("shapelib disabled")
} else {
  DEFINES += SHAPELIB_ENABLED
  equals(WITH_SHAPELIB, pkgconfig) {
    message("Using shapelib found by pkg-config")
    PKGCONFIG += shapelib
    DEFINES += HAVE_LIBSHAPE
  } else:equals(WITH_SHAPELIB, included) {
    # TODO: It would be better to create an archive and link to it
    #       to separate library build requirements from gpsbabel requirements.
    SOURCES += \
      shapelib/dbfopen.c \
      shapelib/safileio.c \
      shapelib/shpopen.c
    HEADERS += \
      shapelib/shapefil.h
  } else:equals(WITH_SHAPELIB, custom) {
    message("shapelib is enabled but but must be manually configured")
    message("  e.g. qmake WITH_SHAPELIB=custom LIBS+=... INCLUDEPATH+=...")
    DEFINES += HAVE_LIBSHAPE
  } else {
    error("WITH_SHAPELIB=no|pkgconfig|included*|custom");
  }
}
