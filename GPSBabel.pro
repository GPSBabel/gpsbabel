QT -= gui

TARGET = GPSBabel
CONFIG += console
CONFIG -= app_bundle
CONFIG += c++11

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
        vcf.cc xhtmlent.cc lowranceusr.cc an1.cc tomtom.cc \
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
        subrip.cc garmin_xt.cc garmin_fit.cc lowranceusr4.cc \
        mtk_locus.cc googledir.cc mapbar_track.cc mapfactor.cc f90g_track.cc \
        energympro.cc mynav.cc ggv_bin.cc globalsat_sport.cc geojson.cc

DEPRECIATED_FMTS=cetus.cc copilot.cc gpspilot.cc magnav.cc psp.cc gcdb.cc quovadis.cc gpilots.cc geoniche.cc palmdoc.cc hsa_ndv.cc coastexp.cc pathaway.cc coto.cc msroute.cc mag_pdb.cc axim_gpb.cc delbin.cc google.cc

DEPRECIATED_HEADERS=geo.h quovadis.h
DEPRECIATED_SHAPE=pdbfile.cc

# ALL_FMTS=$$MINIMAL_FMTS
FILTERS=position.cc radius.cc duplicate.cc arcdist.cc polygon.cc smplrout.cc \
        reverse_route.cc sort.cc stackfilter.cc trackfilter.cc discard.cc \
        nukedata.cc interpolate.cc transform.cc height.cc swapdata.cc bend.cc \
        validate.cc

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


SUPPORT = queue.cc route.cc waypt.cc filter_vecs.cc util.cc vecs.cc mkshort.cc \
          csv_util.cc strptime.c grtcirc.cc util_crc.cc xmlgeneric.cc \
          formspec.cc xmltag.cc cet.cc cet_util.cc fatal.cc rgbcolors.cc \
          inifile.cc garmin_fs.cc gbsleep.cc units.cc gbser.cc \
          gbfile.cc parse.cc session.cc main.cc globals.cc \
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
	height.h \
	holux.h \
	inifile.h \
	jeeps/garminusb.h \
	jeeps/gps.h \
	jeeps/gpsapp.h \
	jeeps/gpscom.h \
	jeeps/gpsdatum.h \
	jeeps/gpsdevice.h \
	jeeps/gpsfmt.h \
	jeeps/gpsinput.h \
	jeeps/gpsmath.h \
	jeeps/gpsmem.h \
	jeeps/gpsport.h \
	jeeps/gpsproj.h \
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
	queue.h \
	session.h \
	shapelib/shapefil.h \
	strptime.h \
	xmlgeneric.h \
	zlib/crc32.h \
	zlib/deflate.h \
	zlib/inffast.h \
	zlib/inffixed.h \
	zlib/inflate.h \
	zlib/inftrees.h \
	zlib/trees.h \
	zlib/zconf.h \
	zlib/zconf.in.h \
	zlib/zlib.h \
	zlib/zutil.h \
	src/core/xmlstreamwriter.h \
	src/core/logging.h

SUBDIRS += jeeps

macx|linux {
  DEFINES += HAVE_NANOSLEEP HAVE_LIBUSB HAVE_GLOB
  SOURCES += gbser_posix.cc
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
  JEEPS += jeeps/gpsusbwin.cc
  LIBS += "C:/Program Files/Windows Kits/8.0/Lib/win8/um/x86/setupapi.lib" "C:/Program Files/Windows Kits/8.0/Lib/win8/um/x86/hid.lib"
}

win32-msvc*{
  DEFINES += _CRT_SECURE_NO_DEPRECATE
  INCLUDEPATH += ../../src/core src/core
  QMAKE_CXXFLAGS += /MP -wd4100
  TEMPLATE=vcapp
}

linux {
  DEFINES += HAVE_LINUX_HID
}

macx {
  LIBS += -framework IOKit -framework CoreFoundation
  INCLUDEPATH += mac/libusb
  SOURCES += mac/libusb/darwin.c \
             mac/libusb/descriptors.c \
             mac/libusb/error.c \
             mac/libusb/usb.c
}

SOURCES += $$ALL_FMTS $$FILTERS $$SUPPORT $$SHAPE $$ZLIB $$JEEPS
DEFINES += NEW_STRINGS

# We don't care about stripping things out of the build.  Full monty, baby.
DEFINES += MAXIMAL_ENABLED
DEFINES += FILTERS_ENABLED
DEFINES += PDBFMTS_ENABLED
DEFINES += SHAPELIB_ENABLED
DEFINES += CSVFMTS_ENABLED
DEFINES += CET_WANTED

# Creator insists on adding -W to -Wall which results in a completely
# absurd amount of jibber-jabber on perfectly legally formed code.
# Rather than wade through a thousand lines of yammer, let's just nuke -W
# but leave -Wall, which actually has useful stuff.
# Citation: http://stackoverflow.com/questions/18667291/disable-wall-compiler-warnings-in-a-qt-project
QMAKE_CFLAGS_WARN_ON -= -W
QMAKE_CXXFLAGS_WARN_ON -= -W
