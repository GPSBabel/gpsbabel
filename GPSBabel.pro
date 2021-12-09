# Enforce minimum Qt version.
# versionAtLeast() was introduced in Qt 5.10, so we can't count on it being available.
MIN_QT_VERSION = 5.12 # major[.minor[.patch]]
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
versionAtLeast(QT_VERSION, 6.0): QT += core5compat

TARGET = gpsbabel
VERSION = 1.7.0

CONFIG += console
CONFIG -= app_bundle
CONFIG += c++17
CONFIG += link_pkgconfig

TEMPLATE = app

# use GB variable to express ownership intention and
# avoid conflict with documented and undocumented qmake variables
GB.VERSION_COMPONENTS = $$split(VERSION, .)
GB.MAJOR = $$member(GB.VERSION_COMPONENTS, 0)
GB.MINOR = $$member(GB.VERSION_COMPONENTS, 1)
GB.MICRO = $$member(GB.VERSION_COMPONENTS, 2)
# Increase GB.BUILD for a new release (why? Where is this ever used?)
# A: it's used by win32/gpsbabel.rc which includes gbversion.h
GB.BUILD = 31
# GB.PACKAGE_RELEASE = "-beta20190413"

# may be overwridden on qmake command line
!defined(DOCVERSION, var) {
  DOCVERSION=$${VERSION}
}

# may be overwridden on qmake command line
!defined(WEB, var) {
  WEB = ../babelweb
}

# use undocumented QMAKE_SUBSTITUTES variable to emulate AC_CONFIG_FILES
GB.versionfile.input = gbversion.h.qmake.in
GB.versionfile.output = gbversion.h
QMAKE_SUBSTITUTES += GB.versionfile
GB.setupfile.input = gui/setup.iss.qmake.in
GB.setupfile.output = gui/setup.iss
QMAKE_SUBSTITUTES += GB.setupfile

MINIMAL_FMTS =  magproto.cc explorist_ini.cc gpx.cc geo.cc garmin.cc \
               garmin_device_xml.cc garmin_tables.cc nmea.cc \
               kml.cc wbt-200.cc

ALL_FMTS=$$MINIMAL_FMTS gtm.cc \
        skytraq.cc holux.cc tpg.cc tpo.cc \
        xcsv.cc easygps.cc \
        saroute.cc navicache.cc delgpl.cc \
        ozi.cc text.cc html.cc \
        igc.cc brauniger_iq.cc shape.cc hiketech.cc glogbook.cc \
        vcf.cc lowranceusr.cc tomtom.cc \
        tef_xml.cc gdb.cc bcr.cc \
        ignrando.cc \
        unicsv.cc garmin_txt.cc gpssim.cc \
        gtrnctr.cc dmtlog.cc raymarine.cc \
        ggv_log.cc garmin_gpi.cc lmx.cc random.cc xol.cc dg-100.cc \
        navilink.cc mtk_logger.cc ik3d.cc osm.cc destinator.cc exif.cc \
        igo8.cc humminbird.cc mapasia.cc gnav_trl.cc navitel.cc ggv_ovl.cc \
        sbp.cc sbn.cc mmo.cc itracku.cc v900.cc \
        enigma.cc \
        teletype.cc wintec_tes.cc \
        subrip.cc garmin_xt.cc garmin_fit.cc \
        mtk_locus.cc googledir.cc mapbar_track.cc mapfactor.cc f90g_track.cc \
        energympro.cc mynav.cc ggv_bin.cc globalsat_sport.cc geojson.cc qstarz_bl_1000.cc

# ALL_FMTS=$$MINIMAL_FMTS
FILTERS=position.cc radius.cc duplicate.cc arcdist.cc polygon.cc smplrout.cc \
        reverse_route.cc sort.cc stackfilter.cc trackfilter.cc discard.cc \
        nukedata.cc interpolate.cc transform.cc height.cc swapdata.cc bend.cc \
        validate.cc resample.cc
FILTER_HEADERS = $$FILTERS
FILTER_HEADERS ~= s/\\.cc/.h/g

JEEPS += jeeps/gpsapp.cc jeeps/gpscom.cc \
         jeeps/gpsmath.cc jeeps/gpsmem.cc  \
         jeeps/gpsprot.cc jeeps/gpsread.cc \
         jeeps/gpsdevice.cc jeeps/gpsdevice_ser.cc jeeps/gpsdevice_usb.cc \
         jeeps/gpsrqst.cc jeeps/gpssend.cc jeeps/gpsserial.cc jeeps/jgpsutil.cc \
         jeeps/gpsusbread.cc jeeps/gpsusbsend.cc \
         jeeps/gpsusbcommon.cc


SUPPORT = route.cc waypt.cc filter_vecs.cc util.cc vecs.cc mkshort.cc \
          csv_util.cc strptime.c grtcirc.cc util_crc.cc xmlgeneric.cc \
          formspec.cc xmltag.cc cet_util.cc fatal.cc rgbcolors.cc \
          inifile.cc garmin_fs.cc units.cc gbser.cc \
          gbfile.cc parse.cc session.cc main.cc globals.cc \
          src/core/nvector.cc \
          src/core/textstream.cc \
          src/core/usasciicodec.cc \
          src/core/vector3d.cc \
          src/core/xmlstreamwriter.cc

versionAtLeast(QT_VERSION, 6.0): SUPPORT += src/core/codecdevice.cc

HEADERS =  \
	cet_util.h \
	csv_util.h \
	defs.h \
	dg-100.h \
	energympro.h \
	explorist_ini.h \
	filter.h \
	filter_vecs.h \
	format.h \
	formspec.h \
	garmin_device_xml.h \
	garmin_fit.h \
	garmin_fs.h \
	garmin_gpi.h \
	garmin_icon_tables.h \
	garmin_tables.h \
	gbfile.h \
	gbser.h \
	gbser_private.h \
	gbversion.h \
	geojson.h \
	ggv_bin.h \
	globalsat_sport.h \
	gpx.h \
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
	kml.h \
	legacyformat.h \
	lowranceusr.h \
	magellan.h \
	mynav.h \
	navilink.h \
	nmea.h \
	osm.h \
	random.h \
	session.h \
	shape.h \
	strptime.h \
	subrip.h \
	unicsv.h \
	units.h \
	vecs.h \
	xcsv.h \
	xmlgeneric.h \
	src/core/datetime.h \
	src/core/file.h \
	src/core/logging.h \
	src/core/nvector.h \
	src/core/textstream.h \
	src/core/usasciicodec.h \
	src/core/vector3d.h \
	src/core/xmlstreamwriter.h \
	src/core/xmltag.h

versionAtLeast(QT_VERSION, 6.0): HEADERS += src/core/codecdevice.h

HEADERS += $$FILTER_HEADERS

win32-msvc* {
  # avoid attempts by cmd.exe to execute mkstyle.sh
  SOURCES += internal_styles.cc
} else {
  # It would be nice to do this when make runs instead of qmake, but we will
  # monitor the style directory to catch new or deleted .style files.
  STYLE_FILES = $$files($${PWD}/style/*.style)
  # It's a bit tacky, but this may modify source files when doing an out of source build.
  # The root of this is that internal_styles.cc is checked in as it can't be built on all platforms,
  # and we want to make sure it is up to date on commit.
  styles.commands += $${PWD}/mkstyle.sh > $${PWD}/internal_styles.cc || (rm -f $${PWD}/internal_styles.cc ; exit 1)
  styles.CONFIG += combine no_clean
  styles.depends += $${PWD}/mkstyle.sh
  styles.depends += $${PWD}/style # this catches the creation/deletion of a style file.
  styles.input = STYLE_FILES
  styles.output = $${PWD}/internal_styles.cc
  styles.variable_out = SOURCES
  QMAKE_EXTRA_COMPILERS += styles
}

CONFIG(release, debug|release): DEFINES *= NDEBUG

macx|linux|openbsd {
  if (equals(MAKEFILE_GENERATOR, XCODE)) {
    # "Configure tests are not supported with the XCODE Makefile generator"
    # assume we have the following headers
    # these are used by zlib
    DEFINES += HAVE_UNISTD_H
    DEFINES += HAVE_STDARG_H
  } else {
    load(configure)
    qtCompileTest(unistd) {
      # this is used by zlib
      DEFINES += HAVE_UNISTD_H
    }
    qtCompileTest(stdarg) {
      # this is used by zlib
      DEFINES += HAVE_STDARG_H
    }
  }
  SOURCES += gbser_posix.cc
  HEADERS += gbser_posix.h
}

win32 {
  DEFINES += __WIN32__ _CONSOLE
  DEFINES -= UNICODE
  CONFIG(debug, debug|release) {
    DEFINES += _DEBUG
  }
  SOURCES += gbser_win.cc
  HEADERS += gbser_win.h
  JEEPS += jeeps/gpsusbwin.cc
  LIBS += "-lsetupapi"
  RC_FILE = win32/gpsbabel.rc
}

win32-msvc* {
  DEFINES += _CRT_SECURE_NO_DEPRECATE
  QMAKE_CFLAGS += /MP -wd4100 -wd4267
  QMAKE_CXXFLAGS += /MP -wd4100 -wd4267
}

include(shapelib.pri)
include(zlib.pri)
include(libusb.pri)

SOURCES += $$ALL_FMTS $$FILTERS $$SUPPORT $$JEEPS

SOURCES = $$sorted(SOURCES)
HEADERS = $$sorted(HEADERS)

# We don't care about stripping things out of the build.  Full monty, baby.
DEFINES += MAXIMAL_ENABLED
DEFINES += FILTERS_ENABLED
DEFINES += CSVFMTS_ENABLED

# Creator insists on adding -W to -Wall which results in a completely
# absurd amount of jibber-jabber on perfectly legally formed code.
# Rather than wade through a thousand lines of yammer, let's just nuke -W
# but leave -Wall, which actually has useful stuff.
# Citation: http://stackoverflow.com/questions/18667291/disable-wall-compiler-warnings-in-a-qt-project
QMAKE_CFLAGS_WARN_ON -= -W
QMAKE_CXXFLAGS_WARN_ON -= -W

check.depends = $(TARGET) FORCE
check.commands = @PNAME=./$(TARGET) $${PWD}/testo
QMAKE_EXTRA_TARGETS += check

check-vtesto.depends = $(TARGET) FORCE
check-vtesto.commands += @$(MAKE) -s -f $${PWD}/Makefile_vtesto srcdir=$${PWD} builddir=$${OUT_PWD} check-vtesto
QMAKE_EXTRA_TARGETS += check-vtesto
QMAKE_CLEAN += $${OUT_PWD}/testo.d/*.vglog

# build the compilation data base used by clang tools including clang-tidy.
macx|linux|openbsd{
  compile_command_database.target = compile_commands.json
  compile_command_database.commands = $(MAKE) clean; bear $(MAKE)
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
  coverage.commands = $(MAKE) clean;
  coverage.commands += rm -f gpsbabel_coverage.xml;
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

gpsbabel.org.depends = gpsbabel gpsbabel.pdf FORCE
equals(PWD, $${OUT_PWD}) {
  gpsbabel.org.commands += web=\$\${WEB:-$${WEB}};
  gpsbabel.org.commands += docversion=\$\${DOCVERSION:-$${DOCVERSION}};
  gpsbabel.org.commands += tools/make_gpsbabel_org.sh \"\$\${web}\" \"\$\${docversion}\";
} else {
  gpsbabel.org.commands += echo "target gpsbabel.org is not supported for out of source builds.";
  gpsbabel.org.commands += exit 1;
}
QMAKE_EXTRA_TARGETS += gpsbabel.org

#
# The gpsbabel.pdf target depends on additional tools.
# On macOS you can 'brew install fop' to get fop and the hyphenation package.
# On Debian/Ubuntu you can 'apt-get install fop' to get fop and the hyphenation package.
# 'fop' must be obtained from your distribution or http://xmlgraphics.apache.org/fop/
#   0.92beta seems to work OK, BUT.
#   * If you have a package called 'docbook-xml-website' it's reported
#     to prevent the build from working.   Remove it. (Suse)
#   * Sun Java seems to be required.   GCJ 1.4.2 doesn't work.   You can
#     force Sun Java to be used by creating ~/.foprc with 'rpm_mode=' (Fedora)
#     Get it from http://www.java.com
# The Hyphenation package must be obtained from your distribution or
#   the project site at http://offo.sourceforge.net/ - be sure to get the
#   version that corresponds to the version of FOP that you used above.
#
#
# The docbook XSL must be 1.71.1 or higher.
#   * Remember to update /etc/xml/catalogs if you manually update this.
#

gpsbabel.html.depends = gpsbabel FORCE
equals(PWD, $${OUT_PWD}) {
  gpsbabel.html.commands += tools/make_gpsbabel_html.sh
} else {
  gpsbabel.html.commands += echo "target gpsbabel.html is not supported for out of source builds.";
  gpsbabel.html.commands += exit 1;
}
QMAKE_EXTRA_TARGETS += gpsbabel.html

gpsbabel.pdf.depends = gpsbabel FORCE
equals(PWD, $${OUT_PWD}) {
  gpsbabel.pdf.commands += tools/make_gpsbabel_pdf.sh
} else {
  gpsbabel.pdf.commands += echo "target gpsbabel.pdf is not supported for out of source builds.";
  gpsbabel.pdf.commands += exit 1;
}
QMAKE_EXTRA_TARGETS += gpsbabel.pdf

gui.depends = $(TARGET) FORCE
disable-mappreview {
  guiconfig = "CONFIG+=disable-mappreview"
}
gui.commands += cd gui; $(QMAKE) $${guiconfig} app.pro && $(MAKE)
QMAKE_EXTRA_TARGETS += gui

unix-gui.depends = gui FORCE
unix-gui.commands += cd gui; $(MAKE) package
QMAKE_EXTRA_TARGETS += unix-gui

toolinfo.depends = FORCE
toolinfo.commands += $(CC) --version;
toolinfo.commands += $(CXX) --version;
toolinfo.commands += $(QMAKE) -v;
QMAKE_EXTRA_TARGETS += toolinfo

