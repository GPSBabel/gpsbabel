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

TARGET = gpsbabel
VERSION = 1.7.0

CONFIG += console
CONFIG -= app_bundle
CONFIG += c++14
CONFIG += link_pkgconfig

TEMPLATE = app

MINIMAL_FMTS =  magproto.cc explorist_ini.cc gpx.cc geo.cc mapsend.cc garmin.cc \
               garmin_device_xml.cc garmin_tables.cc internal_styles.cc nmea.cc \
               kml.cc wbt-200.cc

ALL_FMTS=$$MINIMAL_FMTS gtm.cc gpsutil.cc pcx.cc \
        skytraq.cc holux.cc tmpro.cc tpg.cc tpo.cc \
        xcsv.cc tiger.cc easygps.cc \
        saroute.cc navicache.cc delgpl.cc \
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
        energympro.cc mynav.cc ggv_bin.cc globalsat_sport.cc geojson.cc qstarz_bl_1000.cc

DEPRECATED_FMTS=cetus.cc copilot.cc gpspilot.cc magnav.cc psp.cc gcdb.cc quovadis.cc gpilots.cc geoniche.cc palmdoc.cc hsa_ndv.cc coastexp.cc pathaway.cc coto.cc msroute.cc mag_pdb.cc axim_gpb.cc delbin.cc google.cc psitrex.cc

DEPRECATED_HEADERS=geo.h quovadis.h
DEPRECATED_SHAPE=pdbfile.cc

# ALL_FMTS=$$MINIMAL_FMTS
FILTERS=position.cc radius.cc duplicate.cc arcdist.cc polygon.cc smplrout.cc \
        reverse_route.cc sort.cc stackfilter.cc trackfilter.cc discard.cc \
        nukedata.cc interpolate.cc transform.cc height.cc swapdata.cc bend.cc \
        validate.cc
FILTER_HEADERS = $$FILTERS
FILTER_HEADERS ~= s/\.cc/.h/g

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
	mapsend.h \
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
	yahoo.h \
	src/core/datetime.h \
	src/core/file.h \
	src/core/logging.h \
	src/core/optional.h \
	src/core/textstream.h \
	src/core/usasciicodec.h \
	src/core/xmlstreamwriter.h \
	src/core/xmltag.h

HEADERS += $$FILTER_HEADERS

load(configure)

CONFIG(release, debug|release): DEFINES *= NDEBUG

macx|linux|openbsd {
  qtCompileTest(unistd) {
    # this is used by zlib
    DEFINES += HAVE_UNISTD_H
  }
  qtCompileTest(stdarg) {
    # this is used by zlib
    DEFINES += HAVE_STDARG_H
  }
  SOURCES += gbser_posix.cc
  HEADERS += gbser_posix.h
  INCLUDEPATH += jeeps
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
  QMAKE_CXXFLAGS += /MP -wd4100
}

include(shapelib.pri)
include(zlib.pri)
include(libusb.pri)

SOURCES += $$ALL_FMTS $$FILTERS $$SUPPORT $$JEEPS

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

!defined(WEB, var) {
  WEB = ../babelweb
}
!defined(DOCVERSION, var) {
  DOCVERSION=$${VERSION}
}

index.html.depends = gpsbabel FORCE
index.html.commands += web=\$\${WEB:-$${WEB}} &&
index.html.commands += docversion=\$\${DOCVERSION:-$${DOCVERSION}} &&
index.html.commands += mkdir -p \$\${web}/htmldoc-\$\${docversion} &&
index.html.commands += perl xmldoc/makedoc &&
index.html.commands += xmlwf xmldoc/readme.xml &&  #check for well-formedness
index.html.commands += xmllint --noout --valid xmldoc/readme.xml &&    #validate
index.html.commands += xsltproc \
  --stringparam base.dir "\$\${web}/htmldoc-\$\${docversion}/" \
  --stringparam root.filename "index" \
  xmldoc/babelmain.xsl \
  xmldoc/readme.xml &&
index.html.commands += tools/fixdoc \$\${web}/htmldoc-\$\${docversion} "GPSBabel \$\${docversion}:" &&
index.html.commands += tools/mkcapabilities \$\${web} \$\${web}/htmldoc-\$\${docversion}
QMAKE_EXTRA_TARGETS += index.html

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
gpsbabel.html.commands += perl xmldoc/makedoc &&
gpsbabel.html.commands += xsltproc \
   --output gpsbabel.html \
   --stringparam toc.section.depth "1" \
   --stringparam html.cleanup "1" \
   --stringparam make.clean.html "1" \
   --stringparam html.valid.html "1" \
   --stringparam html.stylesheet \
   "https://www.gpsbabel.org/style3.css" \
   http://docbook.sourceforge.net/release/xsl/current/xhtml/docbook.xsl \
 xmldoc/readme.xml
QMAKE_EXTRA_TARGETS += gpsbabel.html

gpsbabel.pdf.depends = gpsbabel FORCE
gpsbabel.pdf.commands += web=\$\${WEB:-$${WEB}} &&
gpsbabel.pdf.commands += docversion=\$\${DOCVERSION:-$${DOCVERSION}} &&
gpsbabel.pdf.commands += perl xmldoc/makedoc && 
gpsbabel.pdf.commands += xmlwf xmldoc/readme.xml && #check for well-formedness
gpsbabel.pdf.commands += xmllint --noout --valid xmldoc/readme.xml &&   #validate
gpsbabel.pdf.commands += xsltproc -o gpsbabel.fo xmldoc/babelpdf.xsl xmldoc/readme.xml &&
gpsbabel.pdf.commands += HOME=. fop -q -fo gpsbabel.fo -pdf gpsbabel.pdf &&
gpsbabel.pdf.commands += mkdir -p \$\${web}/htmldoc-\$\${docversion} &&
gpsbabel.pdf.commands += cp gpsbabel.pdf \$\${web}/htmldoc-\$\${docversion}/gpsbabel-\$\${docversion}.pdf
QMAKE_EXTRA_TARGETS += gpsbabel.pdf

