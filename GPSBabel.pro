# Enforce minimum Qt version.
# versionAtLeast() was introduced in Qt 5.10, so we can't count on it being available.
MIN_QT_VERSION = 5.9 # major[.minor[.patch]]
MIN_QT_VERSION_COMPONENTS = $$split(MIN_QT_VERSION, .)
MIN_QT_VERSION_MAJOR = $$member(MIN_QT_VERSION_COMPONENTS, 0)
MIN_QT_VERSION_MINOR = $$member(MIN_QT_VERSION_COMPONENTS, 1)
MIN_QT_VERSION_PATCH = $$member(MIN_QT_VERSION_COMPONENTS, 2)
count(MIN_QT_VERSION_MINOR, 0): MIN_QT_VERSION_MINOR = 0
count(MIN_QT_VERSION_PATCH, 0): MIN_QT_VERSION_PATCH = 0
lessThan(QT_MAJOR_VERSION, $$MIN_QT_VERSION_MAJOR) | \
if(equals(QT_MAJOR_VERSION, $$MIN_QT_VERSION_MAJOR):lessThan(QT_MINOR_VERSION, $$MIN_QT_VERSION_MINOR)) | \
if(equals(QT_MAJOR_VERSION, $$MIN_QT_VERSION_MAJOR):equals(QT_MINOR_VERSION, $$MIN_QT_VERSION_MINOR):lessThan(QT_PATCH_VERSION, $$MIN_QT_VERSION_PATCH)) {
  error("$$QMAKE_QMAKE uses Qt version $$QT_VERSION but version $${MIN_QT_VERSION_MAJOR}.$${MIN_QT_VERSION_MINOR}.$${MIN_QT_VERSION_PATCH} or newer is required.")  
}

QT -= gui

linux: {
  TARGET = gpsbabel
} else {
  TARGET = GPSBabel
}
CONFIG += console
CONFIG -= app_bundle
CONFIG += c++14

TEMPLATE = app

MINIMAL_FMTS =  magproto.cc explorist_ini.cc gpx.cc geo.cc mapsend.cc mapsource.cc garmin.cc \
               garmin_device_xml.cc garmin_tables.cc internal_styles.cc nmea.cc \
               kml.cc wbt-200.cc

ALL_FMTS=$$MINIMAL_FMTS gtm.cc gpsutil.cc pcx.cc \
        skytraq.cc holux.cc tmpro.cc tpg.cc tpo.cc \
        xcsv.cc tiger.cc easygps.cc \
        saroute.cc navicache.cc psitrex.cc delgpl.cc \
        ozi.cc text.cc html.cc netstumbler.cc \
        igc.cc brauniger_iq.cc shape.cc hiketech.cc glogbook.cc \
        vcf.cc lowranceusr.cc an1.cc tomtom.cc \
        tef_xml.cc maggeo.cc vitosmt.cc gdb.cc bcr.cc \
        ignrando.cc stmwpp.cc cst.cc nmn4.cc compegps.cc \
        yahoo.cc unicsv.cc wfff_xml.cc garmin_txt.cc gpssim.cc \
        stmsdf.cc gtrnctr.cc dmtlog.cc raymarine.cc alan.cc vitovtt.cc \
        ggv_log.cc g7towin.cc garmin_gpi.cc lmx.cc random.cc xol.cc dg-100.cc \
        navilink.cc mtk_logger.cc ik3d.cc osm.cc destinator.cc exif.cc vidaone.cc \
        igo8.cc gopal.cc humminbird.cc mapasia.cc gnav_trl.cc navitel.cc ggv_ovl.cc \
        jtr.cc sbp.cc sbn.cc mmo.cc skyforce.cc itracku.cc v900.cc \
        pocketfms_bc.cc pocketfms_fp.cc pocketfms_wp.cc naviguide.cc enigma.cc \
        vpl.cc teletype.cc jogmap.cc bushnell.cc bushnell_trl.cc wintec_tes.cc \
        subrip.cc garmin_xt.cc garmin_fit.cc \
        mtk_locus.cc googledir.cc mapbar_track.cc mapfactor.cc f90g_track.cc \
        energympro.cc mynav.cc ggv_bin.cc globalsat_sport.cc geojson.cc

DEPRECATED_FMTS=cetus.cc copilot.cc gpspilot.cc magnav.cc psp.cc gcdb.cc quovadis.cc gpilots.cc geoniche.cc palmdoc.cc hsa_ndv.cc coastexp.cc pathaway.cc coto.cc msroute.cc mag_pdb.cc axim_gpb.cc delbin.cc google.cc

DEPRECATED_HEADERS=geo.h quovadis.h
DEPRECATED_SHAPE=pdbfile.cc

# ALL_FMTS=$$MINIMAL_FMTS
FILTERS=position.cc radius.cc duplicate.cc arcdist.cc polygon.cc smplrout.cc \
        reverse_route.cc sort.cc stackfilter.cc trackfilter.cc discard.cc \
        nukedata.cc interpolate.cc transform.cc height.cc swapdata.cc bend.cc \
        validate.cc
FILTER_HEADERS = $$FILTERS
FILTER_HEADERS ~= s/\.cc/.h/g

SHAPE=shapelib/shpopen.c shapelib/dbfopen.c shapelib/safileio.c

ZLIB=zlib/adler32.c zlib/compress.c zlib/crc32.c zlib/deflate.c zlib/inffast.c \
        zlib/inflate.c zlib/infback.c zlib/inftrees.c zlib/trees.c \
        zlib/uncompr.c zlib/gzlib.c zlib/gzclose.c zlib/gzread.c \
        zlib/gzwrite.c zlib/zutil.c

JEEPS += jeeps/gpsapp.cc jeeps/gpscom.cc \
         jeeps/gpsmath.cc jeeps/gpsmem.cc  \
         jeeps/gpsprot.cc jeeps/gpsread.cc \
         jeeps/gpsdevice.cc jeeps/gpsdevice_ser.cc jeeps/gpsdevice_usb.cc \
         jeeps/gpsrqst.cc jeeps/gpssend.cc jeeps/gpsserial.cc jeeps/jgpsutil.cc \
         jeeps/gpsusbread.cc jeeps/gpsusbsend.cc \
         jeeps/gpsusbcommon.cc


SUPPORT = route.cc waypt.cc filter_vecs.cc util.cc vecs.cc mkshort.cc \
          csv_util.cc strptime.c grtcirc.cc util_crc.cc xmlgeneric.cc \
          formspec.cc xmltag.cc cet.cc cet_util.cc fatal.cc rgbcolors.cc \
          inifile.cc garmin_fs.cc units.cc gbser.cc \
          gbfile.cc parse.cc session.cc main.cc globals.cc \
          src/core/textstream.cc \
          src/core/usasciicodec.cc \
          src/core/xmlstreamwriter.cc 

HEADERS =  \
	an1sym.h \
	cet.h \
	cet/ansi_x3_4_1968.h \
	cet/cp1252.h \
	cet/iso_8859_8.h \
	cet_util.h \
	csv_util.h \
	defs.h \
	explorist_ini.h \
	filter.h \
	filterdefs.h \
	garmin_device_xml.h \
	garmin_fs.h \
	garmin_gpi.h \
	garmin_tables.h \
	gbfile.h \
	gbser.h \
	gbser_private.h \
	gbversion.h \
	grtcirc.h \
	heightgrid.h \
	holux.h \
	inifile.h \
	jeeps/garminusb.h \
	jeeps/gps.h \
	jeeps/gpsapp.h \
	jeeps/gpscom.h \
	jeeps/gpsdatum.h \
	jeeps/gpsdevice.h \
	jeeps/gpsfmt.h \
	jeeps/gpsmath.h \
	jeeps/gpsmem.h \
	jeeps/gpsport.h \
	jeeps/gpsprot.h \
	jeeps/gpsread.h \
	jeeps/gpsrqst.h \
	jeeps/gpssend.h \
	jeeps/gpsserial.h \
	jeeps/gpsusbcommon.h \
	jeeps/gpsusbint.h \
	jeeps/gpsutil.h \
	magellan.h \
	mapsend.h \
	navilink.h \
	session.h \
	shapelib/shapefil.h \
	strptime.h \
	xcsv.h \
	xmlgeneric.h \
	zlib/crc32.h \
	zlib/deflate.h \
	zlib/gzguts.h \
	zlib/inffast.h \
	zlib/inffixed.h \
	zlib/inflate.h \
	zlib/inftrees.h \
	zlib/trees.h \
	zlib/zconf.h \
	zlib/zlib.h \
	zlib/zutil.h \
	src/core/datetime.h \
	src/core/file.h \
	src/core/logging.h \
	src/core/textstream.h \
	src/core/usasciicodec.h \
	src/core/xmlstreamwriter.h \
	src/core/xmltag.h

HEADERS += $$FILTER_HEADERS

INCLUDEPATH += zlib

load(configure)

CONFIG(release, debug|release): DEFINES *= NDEBUG

macx|linux {
  qtCompileTest(unistd) {
    # this is used by zlib
    DEFINES += HAVE_UNISTD_H
  }
  qtCompileTest(stdarg) {
    # this is used by zlib
    DEFINES += HAVE_STDARG_H
  }
  DEFINES += HAVE_LIBUSB HAVE_GLOB
  SOURCES += gbser_posix.cc
  HEADERS += gbser_posix.h
  JEEPS += jeeps/gpslibusb.cc
  INCLUDEPATH += jeeps
}

win32 {
  DEFINES += __WIN32__ _CONSOLE
  DEFINES -= UNICODE ZLIB_INHIBITED
  CONFIG(debug, debug|release) {
    DEFINES += _DEBUG
  }
  SOURCES += gbser_win.cc
  HEADERS += gbser_win.h
  JEEPS += jeeps/gpsusbwin.cc
  LIBS += "-lsetupapi" 
}

win32-msvc* {
  DEFINES += _CRT_SECURE_NO_DEPRECATE
  QMAKE_CXXFLAGS += /MP -wd4100
}

linux {
  DEFINES += HAVE_LINUX_HID
  LIBS += "-lusb"
}

macx {
  LIBS += -framework IOKit -framework CoreFoundation
  INCLUDEPATH += mac/libusb
  SOURCES += mac/libusb/darwin.c \
             mac/libusb/descriptors.c \
             mac/libusb/error.c \
             mac/libusb/usb.c
  HEADERS += mac/libusb/error.h \
             mac/libusb/usb.h \
             mac/libusb/usbi.h
}

SOURCES += $$ALL_FMTS $$FILTERS $$SUPPORT $$SHAPE $$ZLIB $$JEEPS
DEFINES += NEW_STRINGS

# We don't care about stripping things out of the build.  Full monty, baby.
DEFINES += MAXIMAL_ENABLED
DEFINES += FILTERS_ENABLED
DEFINES += SHAPELIB_ENABLED
DEFINES += CSVFMTS_ENABLED

# Creator insists on adding -W to -Wall which results in a completely
# absurd amount of jibber-jabber on perfectly legally formed code.
# Rather than wade through a thousand lines of yammer, let's just nuke -W
# but leave -Wall, which actually has useful stuff.
# Citation: http://stackoverflow.com/questions/18667291/disable-wall-compiler-warnings-in-a-qt-project
QMAKE_CFLAGS_WARN_ON -= -W
QMAKE_CXXFLAGS_WARN_ON -= -W

macx|linux{
  check.commands = PNAME=./$(TARGET) ./testo
  check.depends = $(TARGET)
  QMAKE_EXTRA_TARGETS += check
}

# build the compilation data base used by clang tools including clang-tidy.
macx|linux{
  compile_command_database.target = compile_commands.json
  compile_command_database.commands = make clean; bear make
  QMAKE_EXTRA_TARGETS += compile_command_database
}

# run clang-tidy
# example usage:
# make clang-tidy RUN_CLANG_TIDY_FLAGS="-header-filter=.*\\\.h -checks=-*,modernize-use-nullptr -fix"
# It seems to be better to use run-clang-tidy with the compilation database as opposed to
# running clang-tidy directly and listing the 
# compilation options on the clang-tidy line after --.
# An example is modernize-use-override which inserts repeadted overrides when run directly,
# but works as expected when run with run-clang-tidy.
clang-tidy.commands = run-clang-tidy.py $(RUN_CLANG_TIDY_FLAGS)
clang-tidy.depends = compile_commands.json
QMAKE_EXTRA_TARGETS += clang-tidy

# generate coverage report for codacy
# must use gcc, g++
# dependencies:
# extra ubuntu bionic packages: gcovr lcov
linux{
  coverage.commands = make clean;
  coverage.commands += rm -f gpsbabel_coverage.xml;
  coverage.commands += ln -sf GPSBabel gpsbabel;
  coverage.commands += $(MAKE) CFLAGS=\"$(CFLAGS) -fprofile-arcs -ftest-coverage\" CXXFLAGS=\"$(CXXFLAGS) -fprofile-arcs -ftest-coverage\" LFLAGS=\"$(LFLAGS) --coverage\" &&
  coverage.commands += ./testo &&
  coverage.commands += gcov -r -o . $(SOURCES) &&
  coverage.commands += gcovr -k -r . --xml --exclude='zlib/*' --exclude='shapelib/*' -o gpsbabel_coverage.xml;
  coverage.commands += lcov --capture --directory . --no-external --output-file coverage.info;
  coverage.commands += genhtml coverage.info --output-directory coverage_report;
  QMAKE_EXTRA_TARGETS += coverage
}

cppcheck.commands = cppcheck --enable=all --force --config-exclude=zlib --config-exclude=shapelib $(INCPATH) $$ALL_FMTS $$FILTERS $$SUPPORT $$JEEPS
QMAKE_EXTRA_TARGETS += cppcheck
