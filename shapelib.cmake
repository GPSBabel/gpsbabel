set(GPSBABEL_WITH_SHAPELIB "included" CACHE STRING "no|pkgconfig|included*|custom.")
if(GPSBABEL_WITH_SHAPELIB STREQUAL "no")
  message(STATUS "shapelib disabled.")
else()
  target_compile_definitions(gpsbabel PRIVATE SHAPELIB_ENABLED)
  if(GPSBABEL_WITH_SHAPELIB STREQUAL "pkgconfig")
    message(STATUS "Using shapelib found by pkg-config.")
    find_package(PkgConfig REQUIRED)
    pkg_search_module(SHAPELIB REQUIRED shapelib IMPORTED_TARGET)
    list(APPEND LIBS PkgConfig::SHAPELIB)
    target_compile_definitions(gpsbabel PRIVATE HAVE_LIBSHAPE)
  elseif(GPSBABEL_WITH_SHAPELIB STREQUAL "included")
    add_library(shp STATIC
      shapelib/dbfopen.c
      shapelib/safileio.c
      shapelib/shpopen.c
      shapelib/shapefil.h
    )
    # target_include_directories(shp PUBLIC shape)
    list(APPEND LIBS shp)
  elseif(GPSBABEL_WITH_SHAPELIB STREQUAL "custom")
    message(STATUS "shapelib is enabled but but must be manually configured.")
    message(STATUS "  e.g. -DGPSBABEL_WITH_SHAPELIB:STRING=custom -DGPSBABEL_EXTRA_LINK_DIRECTORIES:STRING=... -DGPSBABEL_EXTRA_INCLUDE_DIRECTORIES:STRING=...")
    target_compile_definitions(gpsbabel PRIVATE HAVE_LIBSHAPE)
  else()
    message(FATAL_ERROR "GPSBABEL_WITH_SHAPELIB=no|pkgconfig|included*|custom")
  endif()
endif()
