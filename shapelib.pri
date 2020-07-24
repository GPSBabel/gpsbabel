!defined(WITH_SHAPELIB, var):WITH_SHAPELIB = included
equals(WITH_SHAPELIB, no) {
  message("shapelib disabled")
} else {
  DEFINES += SHAPELIB_ENABLED
  equals(WITH_SHAPELIB, pkgconfig) {
    message("Using shapelib found by pkg-config")
    PKGCONFIG += shapelib
    # This gets trashed on windows with qmake -tp vc:
    #DEFINES += SHAPEFIL_H_INCLUDE=$$shell_quote(<shapefil.h>)
    DEFINES += SHAPEFIL_H_SYSTEM_INCLUDE
  } else:equals(WITH_SHAPELIB, included) {
    # TODO: It would be better to create an archive and link to it
    #       to separate library build requirements from gpsbabel requirements.
    SOURCES += shapelib/shpopen.c shapelib/dbfopen.c shapelib/safileio.c
    #DEFINES += SHAPEFIL_H_INCLUDE=$$shell_quote(\"shapelib/shapefil.h\")
    DEFINES += SHAPEFIL_H_LOCAL_INCLUDE
    HEADERS += shapelib/shapefil.h
  } else:equals(WITH_SHAPELIB, custom) {
    message("shapelib is enabled but but must be manually configured")
    message("  e.g. qmake WITH_SHAPELIB=custom LIBS+=... INCLUDEPATH+=...")
    #DEFINES += SHAPEFIL_H_INCLUDE=$$shell_quote(<shapefil.h>)
    DEFINES += SHAPEFIL_H_SYSTEM_INCLUDE
  } else {
    error("WITH_SHAPELIB=no|pkgconfig|included*|custom");
  }
}
