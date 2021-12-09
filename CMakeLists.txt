
# It isn't clear what the minimum required version is.
# tested against 3.10.2 on ubuntu bionic 2018/07/05
# tested against 3.5.1 on ubuntu xenial 2018/07/05
# tested against MSVC 2017 which included 3.11.* 2018/07/05
cmake_minimum_required(VERSION 3.5.1)

project(gpsbabel LANGUAGES C CXX)

set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED True)

# Find includes in corresponding build directories
set(CMAKE_INCLUDE_CURRENT_DIR ON)

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

set(MINIMAL_FMTS
  magproto.cc explorist_ini.cc gpx.cc geo.cc garmin.cc
  garmin_device_xml.cc garmin_tables.cc internal_styles.cc nmea.cc
  kml.cc wbt-200.cc
)

set(ALL_FMTS
  ${MINIMAL_FMTS} gtm.cc
  skytraq.cc holux.cc tpg.cc tpo.cc
  xcsv.cc easygps.cc
  saroute.cc navicache.cc delgpl.cc
  ozi.cc text.cc html.cc
  igc.cc brauniger_iq.cc shape.cc hiketech.cc glogbook.cc
  vcf.cc lowranceusr.cc tomtom.cc
  tef_xml.cc gdb.cc bcr.cc
  ignrando.cc
  unicsv.cc garmin_txt.cc gpssim.cc
  gtrnctr.cc dmtlog.cc raymarine.cc 
  ggv_log.cc garmin_gpi.cc lmx.cc random.cc xol.cc dg-100.cc
  navilink.cc mtk_logger.cc ik3d.cc osm.cc destinator.cc exif.cc
  igo8.cc humminbird.cc mapasia.cc gnav_trl.cc navitel.cc ggv_ovl.cc
  sbp.cc sbn.cc mmo.cc itracku.cc v900.cc
  enigma.cc
  teletype.cc wintec_tes.cc
  subrip.cc garmin_xt.cc garmin_fit.cc
  mtk_locus.cc googledir.cc mapbar_track.cc mapfactor.cc f90g_track.cc
  energympro.cc mynav.cc ggv_bin.cc globalsat_sport.cc geojson.cc qstarz_bl_1000.cc
)

# ALL_FMTS=$$MINIMAL_FMTS
set(FILTERS
  position.cc radius.cc duplicate.cc arcdist.cc polygon.cc smplrout.cc
  reverse_route.cc sort.cc stackfilter.cc trackfilter.cc discard.cc
  nukedata.cc interpolate.cc transform.cc height.cc swapdata.cc bend.cc
  validate.cc resample.cc
)

set(SHAPE
  shapelib/shpopen.c shapelib/dbfopen.c shapelib/safileio.c
)

set(ZLIB
  zlib/adler32.c zlib/compress.c zlib/crc32.c zlib/deflate.c zlib/inffast.c
  zlib/inflate.c zlib/infback.c zlib/inftrees.c zlib/trees.c
  zlib/uncompr.c zlib/gzlib.c zlib/gzclose.c zlib/gzread.c
  zlib/gzwrite.c zlib/zutil.c
)

set(JEEPS
  jeeps/gpsapp.cc jeeps/gpscom.cc
  jeeps/gpsmath.cc jeeps/gpsmem.cc
  jeeps/gpsprot.cc jeeps/gpsread.cc
  jeeps/gpsdevice.cc jeeps/gpsdevice_ser.cc jeeps/gpsdevice_usb.cc
  jeeps/gpsrqst.cc jeeps/gpssend.cc jeeps/gpsserial.cc jeeps/jgpsutil.cc
  jeeps/gpsusbread.cc jeeps/gpsusbsend.cc
  jeeps/gpsusbcommon.cc
)

set(SUPPORT
  route.cc waypt.cc filter_vecs.cc util.cc vecs.cc mkshort.cc
  csv_util.cc strptime.c grtcirc.cc util_crc.cc xmlgeneric.cc
  formspec.cc xmltag.cc cet_util.cc fatal.cc rgbcolors.cc
  inifile.cc garmin_fs.cc units.cc gbser.cc
  gbfile.cc parse.cc session.cc main.cc globals.cc
  src/core/nvector.cc
  src/core/textstream.cc
  src/core/usasciicodec.cc
  src/core/vector3d.cc
  src/core/xmlstreamwriter.cc
)
if(${QT_VERSION_MAJOR} EQUAL "6")
  set(SUPPORT ${SUPPORT} src/core/codecdevice.cc)
endif()

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
  shapelib/shapefil.h
  strptime.h
  subrip.h
  unicsv.h
  units.h
  vecs.h
  xcsv.h
  xmlgeneric.h
  
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
  src/core/datetime.h
  src/core/file.h
  src/core/logging.h
  src/core/nvector.h
  src/core/textstream.h
  src/core/usasciicodec.h
  src/core/vector3d.h
  src/core/xmlstreamwriter.h
  src/core/xmltag.h
)
if(${QT_VERSION_MAJOR} EQUAL "6")
  set(HEADERS ${HEADERS} src/core/codecdevice.h)
endif()

string(REPLACE .cc .h FILTER_HEADERS "${FILTERS}")
set(HEADERS ${HEADERS} ${FILTER_HEADERS})

include_directories(AFTER zlib)

include(CheckIncludeFile)
if(UNIX)
  # this is used by zlib
  check_include_file("unistd.h" HAVE_UNISTD_H)
  if(${HAVE_UNISTD_H})
    add_definitions(-DHAVE_UNISTD_H)
  endif()
  # this is used by zlib
  check_include_file("stdarg.h" HAVE_STDARG_H)
  if(${HAVE_STDARG_H})
    add_definitions(-DHAVE_STDARG_H)
  endif()
  add_definitions(-DHAVE_LIBUSB_1_0)
  set(SOURCES ${SOURCES} gbser_posix.cc)
  set(HEADERS ${HEADERS} gbser_posix.h)
  set(JEEPS ${JEEPS} jeeps/gpslibusb.cc)
  add_compile_options(-O2 -Wall)
endif()

if(WIN32)
  add_definitions(-D__WIN32__ -D_CONSOLE)
  remove_definitions(-DUNICODE -DZLIB_INHIBITED)
  if(CMAKE_BUILD_TYPE STREQUAL Debug)
    add_definitions(-D_DEBUG)
  endif()
  set(SOURCES ${SOURCES} gbser_win.cc)
  set(HEADERS ${HEADERS} gbser_win.h)
  set(JEEPS ${JEEPS} jeeps/gpsusbwin.cc)
  set(LIBS ${LIBS} setupapi)
  set(RESOURCES ${RESOURCES} win32/gpsbabel.rc)
endif()

if(MSVC)
  add_definitions(-D_CRT_SECURE_NO_DEPRECATE)
  add_compile_options(/MP -wd4100 -wd4267)
endif()

if(UNIX AND NOT APPLE)
  set(LIBS ${LIBS} usb-1.0)
endif()

if(APPLE)
  set(CMAKE_EXE_LINKER_FLAGS "${CMAKE_EXE_LINKER_FLAGS} -lobjc -framework IOKit -framework CoreFoundation")
  include_directories(AFTER mac/libusb mac/libusb/Xcode)
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
  add_compile_options(-Wall -Wsign-compare)
endif()

set(SOURCES
  ${SOURCES} ${ALL_FMTS} ${FILTERS} ${SUPPORT} ${SHAPE} ${ZLIB} ${JEEPS} ${RESOURCES}
)

list(SORT SOURCES)
list(SORT HEADERS)

# We don't care about stripping things out of the build.  Full monty, baby.
add_definitions(-DMAXIMAL_ENABLED)
add_definitions(-DFILTERS_ENABLED)
add_definitions(-DSHAPELIB_ENABLED)
add_definitions(-DCSVFMTS_ENABLED)

add_executable(gpsbabel ${SOURCES} ${HEADERS})
target_link_libraries(gpsbabel ${QT_LIBRARIES} ${LIBS})

message(STATUS "Sources are: \"${SOURCES}\"")
message(STATUS "Headers are: \"${HEADERS}\"")
get_directory_property(DirDefs COMPILE_DEFINITIONS)
message(STATUS "Defines are: \"${DirDefs}\"")
get_target_property(LnkLibs gpsbabel LINK_LIBRARIES)
message(STATUS "Libs are: \"${LnkLibs}\"")
get_directory_property(IncDirs INCLUDE_DIRECTORIES)
message(STATUS "Include Directores are: \"${IncDirs}\"")

if(UNIX)
  # the tests only work if the pwd is top level source dir due to the file name getting embedded in the file nonexistent.err.
  add_custom_target(check cd ${CMAKE_SOURCE_DIR}\; PNAME=${CMAKE_BINARY_DIR}/gpsbabel ./testo DEPENDS gpsbabel)
endif()
