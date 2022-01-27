cmake_minimum_required(VERSION 3.11)

include(CMakeDependentOption)
include(CheckIncludeFile)

include(gbversion.cmake)
project(gpsbabel LANGUAGES C CXX VERSION ${GB.VERSION})

set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED True)

# Do this after we set up common variables but before creating other
# variables that will be inherited.
add_subdirectory(gui)

# FIXME: When we rearrange the project directory structure everything
# below here should be in it's own CMakeList.txt

configure_file(gbversion.h.in gbversion.h @ONLY NEWLINE_STYLE LF)

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
  mapfactor.h
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
)
if(${QT_VERSION_MAJOR} EQUAL "6")
  set(HEADERS ${HEADERS} src/core/codecdevice.h)
endif()

string(REPLACE .cc .h FILTER_HEADERS "${FILTERS}")
set(HEADERS ${HEADERS} ${FILTER_HEADERS})

if(UNIX)
  set(SOURCES ${SOURCES} gbser_posix.cc)
  set(HEADERS ${HEADERS} gbser_posix.h)
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

if(APPLE)
  target_compile_options(gpsbabel PRIVATE -Wall -Wsign-compare)
endif()

include(shapelib.cmake)
include(zlib.cmake)
include(libusb.cmake)

set(GPSBABEL_EXTRA_LINK_LIBRARIES "" CACHE STRING "extra libraries to link with.")
list(APPEND LIBS ${GPSBABEL_EXTRA_LINK_LIBRARIES})
set(GPSBABEL_EXTRA_INCLUDE_DIRECTORIES "" CACHE STRING "extra directories to include.")
target_include_directories(gpsbabel PRIVATE ${GPSBABEL_EXTRA_INCLUDE_DIRECTORIES})

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

target_link_libraries(gpsbabel PRIVATE ${QT_LIBRARIES} ${LIBS})

get_target_property(Srcs gpsbabel SOURCES)
message(STATUS "Sources are: \"${Srcs}\"")
get_target_property(DirDefs gpsbabel COMPILE_DEFINITIONS)
message(STATUS "Defines are: \"${DirDefs}\"")
get_target_property(LnkLibs gpsbabel LINK_LIBRARIES)
message(STATUS "Libs are: \"${LnkLibs}\"")
get_target_property(IncDirs gpsbabel INCLUDE_DIRECTORIES)
message(STATUS "Include Directores are: \"${IncDirs}\"")

set(TESTS
  arc-project
  arc
  batch
  bcr
  bend
  classic-1
  classic-2
  classic-3
  delgpl
  destinator
  dg100
  dmtlog
  dna
  dop_filter
  duplicate
  easygps
  energympro
  enigma
  exif
  f90g
  fugawi
  garmin_fit
  garmin_g1000
  garmin_gpi
  garmin_txt
  garmin_xt
  gbfile
  gdb
  geojson
  geo
  ggv_bin
  ggv_log
  ggv_ovl
  globalsat_sport
  glogbook
  gnav_trl
  googledir
  gpsdrive
  gpssim
  gpx
  grapheme
  gtm
  gtrnctr
  height
  hiketech
  holux
  humminbird
  iblue747
  igc
  ignrando
  igo2008_poi
  igo8
  igoprimo_poi
  ik3d
  interpolate
  itracku
  kml-read
  kml
  lmx
  lowranceusr
  magellan_sd
  magellan
  mainnav
  mapasia
  mapbar
  mapfactor
  mmo
  motoactv
  mtk
  multiurlgpx
  mxf
  mynav
  navilink
  navitel
  nmea
  osm
  ozi
  polygon
  position
  qstarz_bl_1000
  radius
  raymarine
  realtime
  resample
  route_reverse
  saroute
  sbn
  sbp
  serialization
  shape
  simplify-relative
  simplify
  skytraq
  sort
  stackfilter
  subrip
  swap
  tef
  teletype
  text
  tomtom_asc
  tomtom_itn
  tomtom
  tpg
  tpo
  track-discard
  track
  transform
  unicsv_grids
  unicsv
  unitconversion
  v900
  validate_formats
  validate
  vcard
  wintec_tes
  xcsv
  xol
)

list(SORT TESTS)

if(UNIX)
  # This test only works if the pwd is top level source dir due to the
  # file name getting embedded in the file nonexistent.err.
  enable_testing()
  foreach(TESTNAME IN LISTS TESTS)
    add_test(NAME test-${TESTNAME}
             COMMAND ${CMAKE_SOURCE_DIR}/testo -p $<TARGET_FILE:gpsbabel> ${TESTNAME}
             WORKING_DIRECTORY ${CMAKE_SOURCE_DIR}
            )
  endforeach()
endif()

if(UNIX)
  # This test only works if the pwd is top level source dir due to the
  # file name getting embedded in the file nonexistent.err.
  add_custom_target(check
                    ${CMAKE_SOURCE_DIR}/testo -p $<TARGET_FILE:gpsbabel>
                    DEPENDS gpsbabel
                    WORKING_DIRECTORY ${CMAKE_SOURCE_DIR}
                    VERBATIM)
endif()
if(UNIX AND NOT APPLE)
  # This test only works if the pwd is top level source dir due to the
  # file name getting embedded in the file nonexistent.err.
  file(MAKE_DIRECTORY ${CMAKE_BINARY_DIR}/testo.d)
  list(JOIN TESTS "\\n" VTESTS)
  add_custom_target(check-vtesto
                    printf "${VTESTS}" |
                    xargs -P 3 -I TESTNAME ${CMAKE_SOURCE_DIR}/vtesto
                    -l -j ${CMAKE_BINARY_DIR}/testo.d/TESTNAME.vglog -p $<TARGET_FILE:gpsbabel> TESTNAME
                    DEPENDS gpsbabel
                    WORKING_DIRECTORY ${CMAKE_SOURCE_DIR}
                    VERBATIM)
endif()

if(CMAKE_SOURCE_DIR STREQUAL CMAKE_BINARY_DIR)
  set(WEB "../babelweb" CACHE PATH "Path where the documentation will be stored for www.gpsbabel.org.")
  add_custom_target(gpsbabel.org
                    ${CMAKE_SOURCE_DIR}/tools/make_gpsbabel_org.sh ${WEB} ${DOCVERSION}
                    DEPENDS gpsbabel gpsbabel.pdf
                    VERBATIM)
endif()

if(CMAKE_SOURCE_DIR STREQUAL CMAKE_BINARY_DIR)
  add_custom_target(gpsbabel.html
                    ${CMAKE_SOURCE_DIR}/tools/make_gpsbabel_html.sh
                    DEPENDS gpsbabel)
endif()

if(CMAKE_SOURCE_DIR STREQUAL CMAKE_BINARY_DIR)
  add_custom_target(gpsbabel.pdf
                    ${CMAKE_SOURCE_DIR}/tools/make_gpsbabel_pdf.sh
                    DEPENDS gpsbabel)
endif()
