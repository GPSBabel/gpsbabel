cmake_minimum_required(VERSION 3.11)

project(gpsbabel LANGUAGES C CXX)

set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED True)

# Find includes in corresponding build directories
set(CMAKE_INCLUDE_CURRENT_DIR ON)
# Handle the Qt rcc code generator automatically
set(CMAKE_AUTORCC ON)

add_executable(gpsbabel)

# Find the QtCore library
find_package(QT NAMES Qt6 Qt5 COMPONENTS Core REQUIRED)
find_package(Qt${QT_VERSION_MAJOR} COMPONENTS Core REQUIRED)
list(APPEND QT_LIBRARIES Qt${QT_VERSION_MAJOR}::Core)
if(${Qt${QT_VERSION_MAJOR}Core_VERSION} VERSION_LESS 5.12)
  message(FATAL_ERROR "Qt version ${Qt${QT_VERSION_MAJOR}Core_VERSION} found, but version 5.12 or newer is required.")
else()
  message(STATUS "Using Qt${QT_VERSION_MAJOR} version ${Qt${QT_VERSION_MAJOR}Core_VERSION}")
endif()
if(${QT_VERSION_MAJOR} EQUAL "6")
  find_package(Qt${QT_VERSION_MAJOR} COMPONENTS Core5Compat REQUIRED)
  list(APPEND QT_LIBRARIES Qt${QT_VERSION_MAJOR}::Core5Compat)
endif()

# RESOURCES
set(RESOURCES gpsbabel.qrc)

# MINIMAL_FMTS
set(MINIMAL_FMTS
  explorist_ini.cc
  garmin.cc
  garmin_device_xml.cc
  garmin_tables.cc
  geo.cc
  gpx.cc
  kml.cc
  magproto.cc
  nmea.cc
  wbt-200.cc
)

# ALL_FMTS
set(ALL_FMTS ${MINIMAL_FMTS}
  bcr.cc
  brauniger_iq.cc
  delgpl.cc
  destinator.cc
  dg-100.cc
  dmtlog.cc
  easygps.cc
  energympro.cc
  enigma.cc
  exif.cc
  f90g_track.cc
  garmin_fit.cc
  garmin_gpi.cc
  garmin_txt.cc
  garmin_xt.cc
  gdb.cc
  geojson.cc
  ggv_bin.cc
  ggv_log.cc
  ggv_ovl.cc
  globalsat_sport.cc
  glogbook.cc
  gnav_trl.cc
  googledir.cc
  gpssim.cc
  gtm.cc
  gtrnctr.cc
  hiketech.cc
  holux.cc
  html.cc
  humminbird.cc
  igc.cc
  ignrando.cc
  igo8.cc
  ik3d.cc
  itracku.cc
  lmx.cc
  lowranceusr.cc
  mapasia.cc
  mapbar_track.cc
  mapfactor.cc
  mmo.cc
  mtk_locus.cc
  mtk_logger.cc
  mynav.cc
  navilink.cc
  navitel.cc
  osm.cc
  ozi.cc
  qstarz_bl_1000.cc
  random.cc
  raymarine.cc
  saroute.cc
  sbn.cc
  sbp.cc
  shape.cc
  skytraq.cc
  subrip.cc
  tef_xml.cc
  teletype.cc
  text.cc
  tomtom.cc
  tpg.cc
  tpo.cc
  unicsv.cc
  v900.cc
  vcf.cc
  wintec_tes.cc
  xcsv.cc
  xol.cc
)

# ALL_FMTS = $$MINIMAL_FMTS

# FILTERS
set(FILTERS
  arcdist.cc
  bend.cc
  discard.cc
  duplicate.cc
  height.cc
  interpolate.cc
  nukedata.cc
  polygon.cc
  position.cc
  radius.cc
  resample.cc
  reverse_route.cc
  smplrout.cc
  sort.cc
  stackfilter.cc
  swapdata.cc
  trackfilter.cc
  transform.cc
  validate.cc
)

# SHAPE
set(SHAPE
  shapelib/dbfopen.c
  shapelib/safileio.c
  shapelib/shpopen.c
)

# ZLIB
set(ZLIB
  zlib/adler32.c
  zlib/compress.c
  zlib/crc32.c
  zlib/deflate.c
  zlib/gzclose.c
  zlib/gzlib.c
  zlib/gzread.c
  zlib/gzwrite.c
  zlib/infback.c
  zlib/inffast.c
  zlib/inflate.c
  zlib/inftrees.c
  zlib/trees.c
  zlib/uncompr.c
  zlib/zutil.c
)

# JEEPS
set(JEEPS
  jeeps/gpsapp.cc
  jeeps/gpscom.cc
  jeeps/gpsdevice.cc
  jeeps/gpsdevice_ser.cc
  jeeps/gpsdevice_usb.cc
  jeeps/gpsmath.cc
  jeeps/gpsmem.cc
  jeeps/gpsprot.cc
  jeeps/gpsread.cc
  jeeps/gpsrqst.cc
  jeeps/gpssend.cc
  jeeps/gpsserial.cc
  jeeps/gpsusbcommon.cc
  jeeps/gpsusbread.cc
  jeeps/gpsusbsend.cc
  jeeps/jgpsutil.cc
)

# SUPPORT
set(SUPPORT
  cet_util.cc
  csv_util.cc
  fatal.cc
  filter_vecs.cc
  formspec.cc
  garmin_fs.cc
  gbfile.cc
  gbser.cc
  globals.cc
  grtcirc.cc
  inifile.cc
  main.cc
  mkshort.cc
  parse.cc
  rgbcolors.cc
  route.cc
  session.cc
  src/core/logging.cc
  src/core/nvector.cc
  src/core/textstream.cc
  src/core/usasciicodec.cc
  src/core/vector3d.cc
  src/core/xmlstreamwriter.cc
  strptime.c
  units.cc
  util.cc
  util_crc.cc
  vecs.cc
  waypt.cc
  xmlgeneric.cc
  xmltag.cc
)
if(${QT_VERSION_MAJOR} EQUAL "6")
  set(SUPPORT ${SUPPORT} src/core/codecdevice.cc)
endif()

# HEADERS
set(HEADERS
  cet_util.h
  csv_util.h
  defs.h
  dg-100.h
  energympro.h
  explorist_ini.h
  filter.h
  filter_vecs.h
  format.h
  formspec.h
  garmin_device_xml.h
  garmin_fit.h
  garmin_fs.h
  garmin_gpi.h
  garmin_icon_tables.h
  garmin_tables.h
  gbfile.h
  gbser.h
  gbser_private.h
  gbversion.h
  geojson.h
  ggv_bin.h
  globalsat_sport.h
  gpx.h
  grtcirc.h
  heightgrid.h
  holux.h
  inifile.h
  kml.h
  legacyformat.h
  lowranceusr.h
  magellan.h
  mynav.h
  navilink.h
  nmea.h
  osm.h
  random.h
  session.h
  shape.h
  strptime.h
  subrip.h
  unicsv.h
  units.h
  vecs.h
  xcsv.h
  xmlgeneric.h
  jeeps/garminusb.h
  jeeps/gps.h
  jeeps/gpsapp.h
  jeeps/gpscom.h
  jeeps/gpsdatum.h
  jeeps/gpsdevice.h
  jeeps/gpsfmt.h
  jeeps/gpsmath.h
  jeeps/gpsmem.h
  jeeps/gpsport.h
  jeeps/gpsprot.h
  jeeps/gpsread.h
  jeeps/gpsrqst.h
  jeeps/gpssend.h
  jeeps/gpsserial.h
  jeeps/gpsusbcommon.h
  jeeps/gpsusbint.h
  jeeps/gpsutil.h
  src/core/datetime.h
  src/core/file.h
  src/core/logging.h
  src/core/nvector.h
  src/core/textstream.h
  src/core/usasciicodec.h
  src/core/vector3d.h
  src/core/xmlstreamwriter.h
  src/core/xmltag.h

  shapelib/shapefil.h

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
if(${QT_VERSION_MAJOR} EQUAL "6")
  set(HEADERS ${HEADERS} src/core/codecdevice.h)
endif()

string(REPLACE .cc .h FILTER_HEADERS "${FILTERS}")
set(HEADERS ${HEADERS} ${FILTER_HEADERS})

target_include_directories(gpsbabel PRIVATE zlib)

include(CheckIncludeFile)
if(UNIX)
  # this is used by zlib
  check_include_file("unistd.h" HAVE_UNISTD_H)
  if(${HAVE_UNISTD_H})
    target_compile_definitions(gpsbabel PRIVATE HAVE_UNISTD_H)
  endif()
  # this is used by zlib
  check_include_file("stdarg.h" HAVE_STDARG_H)
  if(${HAVE_STDARG_H})
    target_compile_definitions(gpsbabel PRIVATE HAVE_STDARG_H)
  endif()
  target_compile_definitions(gpsbabel PRIVATE HAVE_LIBUSB_1_0)
  set(SOURCES ${SOURCES} gbser_posix.cc)
  set(HEADERS ${HEADERS} gbser_posix.h)
  set(JEEPS ${JEEPS} jeeps/gpslibusb.cc)
  target_compile_options(gpsbabel PRIVATE -Wall)
endif()

if(WIN32)
  target_compile_definitions(gpsbabel PRIVATE __WIN32__)
  if(${QT_VERSION_MAJOR} EQUAL "6")
    qt_disable_unicode_defines(gpsbabel)
  endif()
  if(CMAKE_BUILD_TYPE STREQUAL Debug)
    target_compile_definitions(gpsbabel PRIVATE _DEBUG)
  endif()
  set(SOURCES ${SOURCES} gbser_win.cc)
  set(HEADERS ${HEADERS} gbser_win.h)
  set(JEEPS ${JEEPS} jeeps/gpsusbwin.cc)
  set(LIBS ${LIBS} setupapi)
  set(RESOURCES ${RESOURCES} win32/gpsbabel.rc)
endif()

if(MSVC)
  target_compile_definitions(gpsbabel PRIVATE _CRT_SECURE_NO_DEPRECATE)
  target_compile_options(gpsbabel PRIVATE /MP -wd4100 -wd4267)
endif()

if(UNIX AND NOT APPLE)
  set(LIBS ${LIBS} usb-1.0)
endif()

if(APPLE)
  set(CMAKE_EXE_LINKER_FLAGS "${CMAKE_EXE_LINKER_FLAGS} -lobjc -framework IOKit -framework CoreFoundation")
  target_include_directories(gpsbabel PRIVATE mac/libusb mac/libusb/Xcode)
  set(SOURCES ${SOURCES}
    mac/libusb/core.c
    mac/libusb/descriptor.c
    mac/libusb/hotplug.c
    mac/libusb/io.c
    mac/libusb/strerror.c
    mac/libusb/sync.c
    mac/libusb/os/darwin_usb.c
    mac/libusb/os/events_posix.c
    mac/libusb/os/threads_posix.c
  )
  set(HEADERS ${HEADERS}
    mac/libusb/hotplug.h
    mac/libusb/libusb.h
    mac/libusb/libusbi.h
    mac/libusb/version.h
    mac/libusb/version_nano.h
    mac/libusb/os/darwin_usb.h
    mac/libusb/os/events_posix.h
    mac/libusb/os/threads_posix.h
  )
  target_compile_options(gpsbabel PRIVATE -Wall -Wsign-compare)
endif()

set(SOURCES
  ${SOURCES} ${ALL_FMTS} ${FILTERS} ${SUPPORT} ${SHAPE} ${ZLIB} ${JEEPS} ${RESOURCES}
)

list(SORT SOURCES)
list(SORT HEADERS)

target_sources(gpsbabel PRIVATE ${SOURCES} ${HEADERS})

# We don't care about stripping things out of the build.  Full monty, baby.
target_compile_definitions(gpsbabel PRIVATE MAXIMAL_ENABLED)
target_compile_definitions(gpsbabel PRIVATE FILTERS_ENABLED)
target_compile_definitions(gpsbabel PRIVATE SHAPELIB_ENABLED)
target_compile_definitions(gpsbabel PRIVATE CSVFMTS_ENABLED)

target_link_libraries(gpsbabel ${QT_LIBRARIES} ${LIBS})

get_target_property(Srcs gpsbabel SOURCES)
message(STATUS "Sources are: \"${Srcs}\"")
get_target_property(DirDefs gpsbabel COMPILE_DEFINITIONS)
message(STATUS "Defines are: \"${DirDefs}\"")
get_target_property(LnkLibs gpsbabel LINK_LIBRARIES)
message(STATUS "Libs are: \"${LnkLibs}\"")
get_target_property(IncDirs gpsbabel INCLUDE_DIRECTORIES)
message(STATUS "Include Directores are: \"${IncDirs}\"")

if(UNIX)
  # the tests only work if the pwd is top level source dir due to the file name getting embedded in the file nonexistent.err.
  add_custom_target(check cd ${CMAKE_SOURCE_DIR}\; PNAME=${CMAKE_BINARY_DIR}/gpsbabel ./testo DEPENDS gpsbabel)
endif()
