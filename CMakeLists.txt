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

option(GPSBABEL_ENABLE_PCH "enable precompiled headers." ON)
if (GPSBABEL_ENABLE_PCH)
  target_precompile_headers(gpsbabel PRIVATE
    "$<$<COMPILE_LANGUAGE:CXX>:<algorithm$<ANGLE-R>>"
    "$<$<COMPILE_LANGUAGE:CXX>:<cmath$<ANGLE-R>>"
    "$<$<COMPILE_LANGUAGE:CXX>:<cstdarg$<ANGLE-R>>"
    "$<$<COMPILE_LANGUAGE:CXX>:<cstddef$<ANGLE-R>>"
    "$<$<COMPILE_LANGUAGE:CXX>:<cstdint$<ANGLE-R>>"
    "$<$<COMPILE_LANGUAGE:CXX>:<cstdio$<ANGLE-R>>"
    "$<$<COMPILE_LANGUAGE:CXX>:<ctime$<ANGLE-R>>"
    "$<$<COMPILE_LANGUAGE:CXX>:<optional$<ANGLE-R>>"
    "$<$<COMPILE_LANGUAGE:CXX>:<utility$<ANGLE-R>>"
    "$<$<COMPILE_LANGUAGE:CXX>:<QDebug$<ANGLE-R>>"
    "$<$<COMPILE_LANGUAGE:CXX>:<QList$<ANGLE-R>>"
    "$<$<COMPILE_LANGUAGE:CXX>:<QScopedPointer$<ANGLE-R>>"
    "$<$<COMPILE_LANGUAGE:CXX>:<QString$<ANGLE-R>>"
    "$<$<COMPILE_LANGUAGE:CXX>:<QTextCodec$<ANGLE-R>>"
    "$<$<COMPILE_LANGUAGE:CXX>:<QVector$<ANGLE-R>>"
    "$<$<COMPILE_LANGUAGE:CXX>:<Qt$<ANGLE-R>>"
    "$<$<COMPILE_LANGUAGE:CXX>:<QtGlobal$<ANGLE-R>>"
  )
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
  brauniger_iq.cc
  dg-100.cc
  exif.cc
  garmin_fit.cc
  garmin_gpi.cc
  garmin_txt.cc
  garmin_xt.cc
  gdb.cc
  geojson.cc
  globalsat_sport.cc
  gpssim.cc
  gtm.cc
  gtrnctr.cc
  holux.cc
  html.cc
  humminbird.cc
  igc.cc
  lowranceusr.cc
  mtk_logger.cc
  navilink.cc
  osm.cc
  ozi.cc
  qstarz_bl_1000.cc
  random.cc
  sbn.cc
  sbp.cc
  shape.cc
  skytraq.cc
  subrip.cc
  text.cc
  tpg.cc
  tpo.cc
  unicsv.cc
  v900.cc
  vcf.cc
  xcsv.cc
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
  exif.h
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
  gdb.h
  geojson.h
  globalsat_sport.h
  gpx.h
  grtcirc.h
  gtrnctr.h
  heightgrid.h
  holux.h
  humminbird.h
  html.h
  inifile.h
  kml.h
  legacyformat.h
  lowranceusr.h
  magellan.h
  navilink.h
  nmea.h
  osm.h
  qstarz_bl_1000.h
  random.h
  session.h
  shape.h
  skytraq.h
  strptime.h
  subrip.h
  text.h
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
  # disable warning from strptime.c, which we don't want to own or modify
  set_source_files_properties(strptime.c PROPERTIES COMPILE_OPTIONS -Wno-unused-but-set-variable)
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
  target_compile_definitions(gpsbabel PRIVATE _CRT_SECURE_NO_WARNINGS)
  target_compile_definitions(gpsbabel PRIVATE _CRT_NONSTDC_NO_WARNINGS)
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
set(GPSBABEL_EXTRA_COMPILE_OPTIONS "" CACHE STRING "extra compile options.")
# Qt sanitize.conf used:
#   -fsanitize=address -fno-omit-frame-pointer
#   -fsanitize=undefined -fsanitize=float-divide-by-zero -fno-omit-frame-pointer
# For coverage use --coverage
separate_arguments(GPSBABEL_EXTRA_COMPILE_OPTIONS)
target_compile_options(gpsbabel PRIVATE ${GPSBABEL_EXTRA_COMPILE_OPTIONS})
set(GPSBABEL_EXTRA_LINK_OPTIONS "" CACHE STRING "extra link options.")
# Qt sanitize.conf used:
#   -fsanitize=address
#   -fsanitize=undefined -fsanitize=float-divide-by-zero
# For coverage use --coverage
separate_arguments(GPSBABEL_EXTRA_LINK_OPTIONS)
target_link_options(gpsbabel PRIVATE ${GPSBABEL_EXTRA_LINK_OPTIONS})

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
  bend
  classic-1
  classic-2
  classic-3
  dg100
  dop_filter
  duplicate
  exif
  garmin_fit
  garmin_g1000
  garmin_gpi
  garmin_txt
  garmin_xt
  gbfile
  gdb
  geojson
  geo
  globalsat_sport
  gpsdrive
  gpssim
  gpx
  grapheme
  gtm
  gtrnctr
  height
  holux
  humminbird
  iblue747
  igc
  interpolate
  kml-read
  kml
  lowranceusr
  magellan_sd
  magellan
  mtk
  multiurlgpx
  navilink
  nmea
  osm
  ozi
  polygon
  position
  qstarz_bl_1000
  radius
  realtime
  resample
  route_reverse
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
  text
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
  xcsv
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
                    VERBATIM
                    USES_TERMINAL)
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
                    VERBATIM
                    USES_TERMINAL)
endif()

find_package(Perl)
if (NOT Perl_FOUND)
  message(WARNING "Perl was not found, documentation cannot be created.")
else()
  # We need to uriencode the xml entity system identifiers, e.g. if CMAKE_SOURCE_DIR contains a space character.
  execute_process(COMMAND "${PERL_EXECUTABLE}" ${CMAKE_SOURCE_DIR}/tools/uriencode ${CMAKE_SOURCE_DIR}
                  RESULT_VARIABLE GB.ENCODE_RESULT
                  OUTPUT_VARIABLE GB.ENCODED_SOURCE_DIR)
  execute_process(COMMAND "${PERL_EXECUTABLE}" ${CMAKE_SOURCE_DIR}/tools/uriencode ${CMAKE_BINARY_DIR}
                  RESULT_VARIABLE GB.ENCODE_RESULT
                  OUTPUT_VARIABLE GB.ENCODED_BINARY_DIR)
  if(GB.ENCODE_RESULT OR NOT GB.ENCODE_RESULT EQUAL 0)
    message(WARNING "uriencode failed, documentation cannot be created.")
  else()
    configure_file(xmldoc/readme.xml.in xmldoc/readme.xml @ONLY NEWLINE_STYLE LF)
    configure_file(xmldoc/makedoc.in xmldoc/makedoc @ONLY NEWLINE_STYLE LF)
    set(WEB "gpsbabel.org" CACHE PATH "Path where the documentation will be stored for www.gpsbabel.org.")
    add_custom_target(xmldoc
                      COMMAND "${PERL_EXECUTABLE}" xmldoc/makedoc
                      COMMAND xmlwf xmldoc/readme.xml #check for well-formedness
                      COMMAND xmllint --noout --valid xmldoc/readme.xml #validate
                      DEPENDS gpsbabel
                      VERBATIM)
    
    add_custom_target(gpsbabel.org
                      COMMAND ${CMAKE_COMMAND} -E make_directory "${WEB}/htmldoc-${DOCVERSION}"
                      COMMAND xsltproc
                                --stringparam base.dir "${WEB}/htmldoc-${DOCVERSION}/"
                                --stringparam root.filename "index"
                                ${CMAKE_SOURCE_DIR}/xmldoc/babelmain.xsl
                                xmldoc/readme.xml
                      COMMAND ${CMAKE_SOURCE_DIR}/tools/fixdoc "${WEB}/htmldoc-${DOCVERSION}" "GPSBabel ${DOCVERSION}:"
                      COMMAND ${CMAKE_SOURCE_DIR}/tools/mkcapabilities "${WEB}" "${WEB}/htmldoc-${DOCVERSION}"
                      COMMAND ${CMAKE_COMMAND} -E copy gpsbabel.pdf "${WEB}/htmldoc-${DOCVERSION}/gpsbabel-${DOCVERSION}.pdf"
                      DEPENDS xmldoc gpsbabel.pdf
                      VERBATIM)
    
    add_custom_target(gpsbabel.html
                      COMMAND xsltproc
                                --output gpsbabel.html
                                --stringparam toc.section.depth "1"
                                --stringparam html.cleanup "1"
                                --stringparam make.clean.html "1"
                                --stringparam html.valid.html "1"
                                --stringparam html.stylesheet "https://www.gpsbabel.org/style3.css"
                                http://docbook.sourceforge.net/release/xsl/current/xhtml/docbook.xsl
                                xmldoc/readme.xml
                      DEPENDS xmldoc
                      VERBATIM)
    
    # the purpose of setting HOME with fop is to read .foprc to avoid INFO messages.
    # fop has a bug that causes it to die if HOME contains and embedded space character.
    # https://bugs.launchpad.net/ubuntu/+source/fop/+bug/1993853
    # https://bugs.debian.org/cgi-bin/bugreport.cgi?bug=1022197
    add_custom_target(gpsbabel.pdf
                      COMMAND xsltproc -o gpsbabel.fo ${CMAKE_SOURCE_DIR}/xmldoc/babelpdf.xsl xmldoc/readme.xml
                      COMMAND ${CMAKE_COMMAND} -E env "HOME=${CMAKE_SOURCE_DIR}" fop -q -fo gpsbabel.fo -pdf gpsbabel.pdf
                      DEPENDS xmldoc
                      VERBATIM)
  endif()
endif()
