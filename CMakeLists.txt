
# It isn't clear what the minimum required version is.
# tested against 3.10.2 on ubuntu bionic 2018/07/05
# tested against 3.5.1 on ubuntu xenial 2018/07/05
# tested against MSVC 2017 which included 3.11.* 2018/07/05
cmake_minimum_required(VERSION 3.5.1)

project(gpsbabel)

set(CMAKE_CXX_STANDARD 14)

# Find includes in corresponding build directories
set(CMAKE_INCLUDE_CURRENT_DIR ON)

# Find the Qt5Core library
find_package(Qt5 COMPONENTS Core REQUIRED)
message("Qt5Core_VERSION String describing the version of the module: \"${Qt5Core_VERSION}\"")
message("Qt5Core_LIBRARIES List of libraries for use with the target_link_libraries command: \"${Qt5Core_LIBRARIES}\"")
message("Qt5Core_INCLUDE_DIRS List of directories for use with the include_directories command: \"${Qt5Core_INCLUDE_DIRS}\"")
message("Qt5Core_DEFINITIONS List of definitions for use with add_definitions: \"${Qt5Core_DEFINITIONS}\"")
message("Qt5Core_COMPILE_DEFINITIONS List of definitions for use with the COMPILE_DEFINITIONS target property: \"${Qt5Core_COMPILE_DEFINITIONS}\"")
message("Qt5Core_FOUND Boolean describing whether the module was found successfully: \"${Qt5Core_FOUND}\"")
message("Qt5Core_EXECUTABLE_COMPILE_FLAGS String of flags to be used when building executables: \"${Qt5Core_EXECUTABLE_COMPILE_FLAGS}\"")
if (${Qt5Core_VERSION} VERSION_LESS 5.9)
  message(FATAL_ERROR "Qt version ${Qt5Core_VERSION} found, but version 5.9 or newer is required.")
endif()
include_directories(AFTER SYSTEM ${Qt5Core_INCLUDE_DIRS})
add_definitions(${Qt5Core_DEFINITIONS})

set(MINIMAL_FMTS
  magproto.cc explorist_ini.cc gpx.cc geo.cc mapsend.cc mapsource.cc garmin.cc
  garmin_device_xml.cc garmin_tables.cc internal_styles.cc nmea.cc
  kml.cc wbt-200.cc
)

set(ALL_FMTS
  ${MINIMAL_FMTS} gtm.cc gpsutil.cc pcx.cc
  skytraq.cc holux.cc tmpro.cc tpg.cc tpo.cc
  xcsv.cc tiger.cc easygps.cc
  saroute.cc navicache.cc psitrex.cc delgpl.cc
  ozi.cc text.cc html.cc netstumbler.cc
  igc.cc brauniger_iq.cc shape.cc hiketech.cc glogbook.cc
  vcf.cc lowranceusr.cc an1.cc tomtom.cc
  tef_xml.cc maggeo.cc vitosmt.cc gdb.cc bcr.cc
  ignrando.cc stmwpp.cc cst.cc nmn4.cc compegps.cc
  yahoo.cc unicsv.cc wfff_xml.cc garmin_txt.cc gpssim.cc
  stmsdf.cc gtrnctr.cc dmtlog.cc raymarine.cc alan.cc vitovtt.cc
  ggv_log.cc g7towin.cc garmin_gpi.cc lmx.cc random.cc xol.cc dg-100.cc
  navilink.cc mtk_logger.cc ik3d.cc osm.cc destinator.cc exif.cc vidaone.cc
  igo8.cc gopal.cc humminbird.cc mapasia.cc gnav_trl.cc navitel.cc ggv_ovl.cc
  jtr.cc sbp.cc sbn.cc mmo.cc skyforce.cc itracku.cc v900.cc
  pocketfms_bc.cc pocketfms_fp.cc pocketfms_wp.cc naviguide.cc enigma.cc
  vpl.cc teletype.cc jogmap.cc bushnell.cc bushnell_trl.cc wintec_tes.cc
  subrip.cc garmin_xt.cc garmin_fit.cc
  mtk_locus.cc googledir.cc mapbar_track.cc mapfactor.cc f90g_track.cc
  energympro.cc mynav.cc ggv_bin.cc globalsat_sport.cc geojson.cc
)

#DEPRECATED_FMTS=cetus.cc copilot.cc gpspilot.cc magnav.cc psp.cc gcdb.cc quovadis.cc gpilots.cc geoniche.cc palmdoc.cc hsa_ndv.cc coastexp.cc pathaway.cc coto.cc msroute.cc mag_pdb.cc axim_gpb.cc delbin.cc google.cc

#DEPRECATED_HEADERS=geo.h quovadis.h
#DEPRECATED_SHAPE=pdbfile.cc

# ALL_FMTS=$$MINIMAL_FMTS
set(FILTERS
  position.cc radius.cc duplicate.cc arcdist.cc polygon.cc smplrout.cc
  reverse_route.cc sort.cc stackfilter.cc trackfilter.cc discard.cc
  nukedata.cc interpolate.cc transform.cc height.cc swapdata.cc bend.cc
  validate.cc
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
  formspec.cc xmltag.cc cet.cc cet_util.cc fatal.cc rgbcolors.cc
  inifile.cc garmin_fs.cc units.cc gbser.cc
  gbfile.cc parse.cc session.cc main.cc globals.cc
  src/core/textstream.cc
  src/core/usasciicodec.cc
  src/core/xmlstreamwriter.cc 
)

set(HEADERS
  an1sym.h
  cet.h
  cet/ansi_x3_4_1968.h
  cet/cp1252.h
  cet/iso_8859_8.h
  cet_util.h
  csv_util.h
  defs.h
  explorist_ini.h
  filter.h
  filterdefs.h
  garmin_device_xml.h
  garmin_fs.h
  garmin_gpi.h
  garmin_tables.h
  gbfile.h
  gbser.h
  gbser_private.h
  gbversion.h
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
  magellan.h
  mapsend.h
  navilink.h
  session.h
  shapelib/shapefil.h
  strptime.h
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
  src/core/textstream.h
  src/core/usasciicodec.h
  src/core/xmlstreamwriter.h
  src/core/xmltag.h
)

string(REPLACE .cc .h FILTER_HEADERS "${FILTERS}")
set(HEADERS ${HEADERS} ${FILTER_HEADERS})

include_directories(AFTER zlib)

INCLUDE (CheckIncludeFile)
if(UNIX)
  # this is used by zlib
  CHECK_INCLUDE_FILE("unistd.h" HAVE_UNISTD_H)
  if(${HAVE_UNISTD_H})
    add_definitions(-DHAVE_UNISTD_H)
  endif()
  # this is used by zlib
  CHECK_INCLUDE_FILE("stdarg.h" HAVE_STDARG_H)
  if(${HAVE_STDARG_H})
    add_definitions(-DHAVE_STDARG_H)
  endif()
  add_definitions(-DHAVE_LIBUSB -DHAVE_GLOB)
  set(SOURCES ${SOURCES} gbser_posix.cc)
  set(HEADERS ${HEADERS} gbser_posix.h)
  set(JEEPS ${JEEPS} jeeps/gpslibusb.cc)
  include_directories(AFTER jeeps)
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
endif()

if(MSVC)
  add_definitions(-D_CRT_SECURE_NO_DEPRECATE)
  add_compile_options(/MP -wd4100)
endif()

if (UNIX AND NOT APPLE)
  add_definitions(-DHAVE_LINUX_HID)
  set(LIBS ${LIBS} usb)
endif()

if (APPLE)
  set(CMAKE_EXE_LINKER_FLAGS "${CMAKE_EXE_LINKER_FLAGS} -framework IOKit -framework CoreFoundation")
  include_directories(AFTER mac/libusb)
  set(SOURCES ${SOURCES}
    mac/libusb/darwin.c
    mac/libusb/descriptors.c
    mac/libusb/error.c
    mac/libusb/usb.c
  )
  set(HEADERS ${HEADERS}
    mac/libusb/error.h
    mac/libusb/usb.h
    mac/libusb/usbi.h
  )
  add_compile_options(-Wall -Wsign-compare)
endif()

set(SOURCES
  ${SOURCES} ${ALL_FMTS} ${FILTERS} ${SUPPORT} ${SHAPE} ${ZLIB} ${JEEPS}
)
add_definitions(-DNEW_STRINGS)

# We don't care about stripping things out of the build.  Full monty, baby.
add_definitions(-DMAXIMAL_ENABLED)
add_definitions(-DFILTERS_ENABLED)
add_definitions(-DSHAPELIB_ENABLED)
add_definitions(-DCSVFMTS_ENABLED)

add_executable(GPSBabel ${SOURCES} ${HEADERS})
target_link_libraries(GPSBabel ${Qt5Core_LIBRARIES} ${LIBS})

message("Sources are:")
message("${SOURCES}")
message("Headers are:")
message("${HEADERS}")
message("Defines are:")
get_directory_property( DirDefs COMPILE_DEFINITIONS)
message("${DirDefs}")
message("Libs are:")
message("${LIBS}")

if(UNIX)
  # the tests only work if the pwd is top level source dir due to the file name getting embedded in the file nonexistent.err.
  add_custom_target(check cd ${CMAKE_SOURCE_DIR}\; PNAME=${CMAKE_BINARY_DIR}/GPSBabel ./testo DEPENDS GPSBabel)
endif()
