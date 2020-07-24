!defined(WITH_ZLIB, var):WITH_ZLIB = included
equals(WITH_ZLIB, no) {
  message("zlib disabled")
  DEFINES += ZLIB_INHIBITED
} else {
  equals(WITH_ZLIB, pkgconfig) {
    message("Using zlib found by pkg-config")
    PKGCONFIG += $$shell_quote(zlib > 1.2.8)
    # This gets trashed on windows with qmake -tp vc:
    #DEFINES += ZLIB_H_INCLUDE=$$shell_quote(<zlib.h>)
    DEFINES += ZLIB_H_SYSTEM_INCLUDE
  } else:equals(WITH_ZLIB, included) {
    # TODO: It would be better to create an archive and link to it
    #       to separate library build requirements from gpsbabel requirements.
    SOURCES += zlib/adler32.c \
               zlib/compress.c \
               zlib/crc32.c \
               zlib/deflate.c \
               zlib/inffast.c \
               zlib/inflate.c \
               zlib/infback.c \
               zlib/inftrees.c \
               zlib/trees.c \
               zlib/uncompr.c \
               zlib/gzlib.c \
               zlib/gzclose.c \
               zlib/gzread.c \
               zlib/gzwrite.c \
               zlib/zutil.c
    #DEFINES += ZLIB_H_INCLUDE=$$shell_quote(\"zlib/zlib.h\")
    DEFINES += ZLIB_H_LOCAL_INCLUDE
    INCLUDEPATH += zlib
    HEADERS += zlib/crc32.h \
               zlib/deflate.h \
               zlib/gzguts.h \
               zlib/inffast.h \
               zlib/inffixed.h \
               zlib/inflate.h \
               zlib/inftrees.h \
               zlib/trees.h \
               zlib/zconf.h \
               zlib/zlib.h \
               zlib/zutil.h
  } else:equals(WITH_ZLIB, custom) {
    message("zlib is enabled but but must be manually configured")
    message("  e.g. qmake WITH_ZLIB=custom LIBS+=... INCLUDEPATH+=...")
    #DEFINES += ZLIB_H_INCLUDE=$$shell_quote(<zlib.h>)
    DEFINES += ZLIB_H_SYSTEM_INCLUDE
  } else {
    error("WITH_ZLIB=no|pkgconfig|included*|custom");
  }
}
