set(GPSBABEL_WITH_ZLIB "included" CACHE STRING "no|findpackage|pkgconfig|included*|custom.")
if(GPSBABEL_WITH_ZLIB STREQUAL "no")
  message(STATUS "zlib disabled")
  target_compile_definitions(gpsbabel PRIVATE ZLIB_INHIBITED)
else()
  if(GPSBABEL_WITH_ZLIB STREQUAL "findpackage")
    find_package(ZLIB 1.2.9 REQUIRED)
    list(APPEND LIBS ZLIB::ZLIB)
    target_compile_definitions(gpsbabel PRIVATE HAVE_LIBZ)
  elseif(GPSBABEL_WITH_ZLIB STREQUAL "pkgconfig")
    message("Using zlib found by pkg-config")
    find_package(PkgConfig REQUIRED)
    pkg_search_module(ZLIB REQUIRED zlib>=1.2.9 IMPORTED_TARGET)
    list(APPEND LIBS PkgConfig::ZLIB)
    target_compile_definitions(gpsbabel PRIVATE HAVE_LIBZ)
  elseif(GPSBABEL_WITH_ZLIB STREQUAL "included")
    add_library(z STATIC
      zlib/adler32.c
      zlib/compress.c
      zlib/crc32.c
      zlib/deflate.c
      zlib/inffast.c
      zlib/inflate.c
      zlib/infback.c
      zlib/inftrees.c
      zlib/trees.c
      zlib/uncompr.c
      zlib/gzlib.c
      zlib/gzclose.c
      zlib/gzread.c
      zlib/gzwrite.c
      zlib/zutil.c
      zlib/crc32.h
      zlib/deflate.h
      zlib/gzguts.h
      zlib/inffast.h
      zlib/inffixed.h
      zlib/inflate.h
      zlib/inftrees.h
      zlib/trees.h
      zlib/zconf.h
      zlib/zlib.h
      zlib/zutil.h
    )
    if(UNIX)
      # this is used by zlib
      check_include_file(unistd.h HAVE_UNISTD_H)
      if(HAVE_UNISTD_H)
        target_compile_definitions(z PRIVATE HAVE_UNISTD_H)
      endif()
      # this is used by zlib
      check_include_file(stdarg.h HAVE_STDARG_H)
      if(HAVE_STDARG_H)
        target_compile_definitions(z PRIVATE HAVE_STDARG_H)
      endif()
    endif()
    target_include_directories(z PUBLIC zlib)
    list(APPEND LIBS z)
  elseif(GPSBABEL_WITH_ZLIB STREQUAL "custom")
    message("zlib is enabled but but must be manually configured.")
    message("  e.g. GPSBABEL_WITH_ZLIB=custom GPSBABEL_EXTRA_LINK_LIBRARIES:STRING==... GPSBABEL_EXTRA_INCLUDE_DIRECTORIES:STRING=...")
    target_compile_definitions(gpsbabel PRIVATE HAVE_LIBZ)
  else()
    message(FATAL_ERROR "GPSBABEL_WITH_ZLIB=no|findpackage|pkgconfig|included*|custom")
  endif()
endif()
