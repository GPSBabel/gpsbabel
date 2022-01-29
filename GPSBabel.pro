# Enforce minimum Qt version.
# versionAtLeast() was introduced in Qt 5.10, so we cannot count on it being available.
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

# set VERSION related variables and generate gbversion.h
include(gbversion.pri)

TARGET = gpsbabel
VERSION = $$GB.VERSION

CONFIG += console
CONFIG -= app_bundle
CONFIG += c++17
CONFIG += link_pkgconfig

TEMPLATE = app

# RESOURCES
RESOURCES = gpsbabel.qrc

# MINIMAL_FMTS
MINIMAL_FMTS = \
  explorist_ini.cc \
  garmin.cc \
  garmin_device_xml.cc \
  garmin_tables.cc \
  geo.cc \
  gpx.cc \
  kml.cc \
  magproto.cc \
  nmea.cc \
  wbt-200.cc

# ALL_FMTS
ALL_FMTS = $$MINIMAL_FMTS \
  bcr.cc \
  brauniger_iq.cc \
  delgpl.cc \
  destinator.cc \
  dg-100.cc \
  dmtlog.cc \
  easygps.cc \
  energympro.cc \
  enigma.cc \
  exif.cc \
  f90g_track.cc \
  garmin_fit.cc \
  garmin_gpi.cc \
  garmin_txt.cc \
  garmin_xt.cc \
  gdb.cc \
  geojson.cc \
  ggv_bin.cc \
  ggv_log.cc \
  ggv_ovl.cc \
  globalsat_sport.cc \
  glogbook.cc \
  gnav_trl.cc \
  googledir.cc \
  gpssim.cc \
  gtm.cc \
  gtrnctr.cc \
  hiketech.cc \
  holux.cc \
  html.cc \
  humminbird.cc \
  igc.cc \
  ignrando.cc \
  igo8.cc \
  ik3d.cc \
  itracku.cc \
  lmx.cc \
  lowranceusr.cc \
  mapasia.cc \
  mapbar_track.cc \
  mapfactor.cc \
  mmo.cc \
  mtk_locus.cc \
  mtk_logger.cc \
  mynav.cc \
  navilink.cc \
  navitel.cc \
  osm.cc \
  ozi.cc \
  qstarz_bl_1000.cc \
  random.cc \
  raymarine.cc \
  saroute.cc \
  sbn.cc \
  sbp.cc \
  shape.cc \
  skytraq.cc \
  subrip.cc \
  tef_xml.cc \
  teletype.cc \
  text.cc \
  tomtom.cc \
  tpg.cc \
  tpo.cc \
  unicsv.cc \
  v900.cc \
  vcf.cc \
  wintec_tes.cc \
  xcsv.cc \
  xol.cc

# ALL_FMTS = $$MINIMAL_FMTS

# FILTERS
FILTERS = \
  arcdist.cc \
  bend.cc \
  discard.cc \
  duplicate.cc \
  height.cc \
  interpolate.cc \
  nukedata.cc \
  polygon.cc \
  position.cc \
  radius.cc \
  resample.cc \
  reverse_route.cc \
  smplrout.cc \
  sort.cc \
  stackfilter.cc \
  swapdata.cc \
  trackfilter.cc \
  transform.cc \
  validate.cc
FILTER_HEADERS = $$FILTERS
FILTER_HEADERS ~= s/\\.cc/.h/g

# JEEPS
JEEPS += \
  jeeps/gpsapp.cc \
  jeeps/gpscom.cc \
  jeeps/gpsdevice.cc \
  jeeps/gpsdevice_ser.cc \
  jeeps/gpsdevice_usb.cc \
  jeeps/gpsmath.cc \
  jeeps/gpsmem.cc \
  jeeps/gpsprot.cc \
  jeeps/gpsread.cc \
  jeeps/gpsrqst.cc \
  jeeps/gpssend.cc \
  jeeps/gpsserial.cc \
  jeeps/gpsusbcommon.cc \
  jeeps/gpsusbread.cc \
  jeeps/gpsusbsend.cc \
  jeeps/jgpsutil.cc


# SUPPORT
SUPPORT = \
  cet_util.cc \
  csv_util.cc \
  fatal.cc \
  filter_vecs.cc \
  formspec.cc \
  garmin_fs.cc \
  gbfile.cc \
  gbser.cc \
  globals.cc \
  grtcirc.cc \
  inifile.cc \
  main.cc \
  mkshort.cc \
  parse.cc \
  rgbcolors.cc \
  route.cc \
  session.cc \
  src/core/logging.cc \
  src/core/nvector.cc \
  src/core/textstream.cc \
  src/core/usasciicodec.cc \
  src/core/vector3d.cc \
  src/core/xmlstreamwriter.cc \
  strptime.c \
  units.cc \
  util.cc \
  util_crc.cc \
  vecs.cc \
  waypt.cc \
  xmlgeneric.cc \
  xmltag.cc

versionAtLeast(QT_VERSION, 6.0): SUPPORT += src/core/codecdevice.cc

# HEADERS
HEADERS =  \
  cet_util.h \
  csv_util.h \
  defs.h \
  dg-100.h \
  energympro.h \
  exif.h \
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
  geojson.h \
  ggv_bin.h \
  globalsat_sport.h \
  gpx.h \
  grtcirc.h \
  gtrnctr.h \
  heightgrid.h \
  holux.h \
  html.h \
  inifile.h \
  kml.h \
  legacyformat.h \
  lowranceusr.h \
  magellan.h \
  mapbar_track.h \
  mapfactor.h \
  mynav.h \
  navilink.h \
  nmea.h \
  osm.h \
  random.h \
  session.h \
  shape.h \
  skytraq.h \
  strptime.h \
  subrip.h \
  teletype.h \
  unicsv.h \
  units.h \
  vecs.h \
  wintec_tes.h \
  xcsv.h \
  xmlgeneric.h \
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

CONFIG(release, debug|release): DEFINES *= NDEBUG

unix {
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
  DEFINES += __WIN32__
  DEFINES -= UNICODE _UNICODE
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
unix {
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
  # may be overridden on qmake command line
  !defined(WEB, var) {
    WEB = ../babelweb
  }
  gpsbabel.org.commands += tools/make_gpsbabel_org.sh $$shell_quote($$WEB) $$shell_quote($$DOCVERSION);
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

