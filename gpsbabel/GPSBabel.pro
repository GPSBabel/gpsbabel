QT -= gui

TARGET = GPSBabel
CONFIG += console
CONFIG -= app_bundle

TEMPLATE = app

MINIMAL_FMTS =  magproto.cc explorist_ini.cc gpx.cc geo.cc mapsend.cc mapsource.cc garmin.cc \
               garmin_device_xml.cc garmin_tables.cc internal_styles.cc nmea.cc \
               kml.cc wbt-200.cc

ALL_FMTS=$$MINIMAL_FMTS gtm.cc gpsutil.cc pcx.cc cetus.cc copilot.cc \
        gpspilot.cc magnav.cc skytraq.cc \
        psp.cc holux.cc tmpro.cc tpg.cc tpo.cc \
        xcsv.cc gcdb.cc tiger.cc easygps.cc quovadis.cc \
        gpilots.cc saroute.cc navicache.cc psitrex.cc geoniche.cc delgpl.cc \
        ozi.cc text.cc html.cc palmdoc.cc netstumbler.cc hsa_ndv.cc \
        igc.cc brauniger_iq.cc shape.cc hiketech.cc glogbook.cc coastexp.cc \
        vcf.cc overlay.cc google.cc xhtmlent.cc lowranceusr.cc an1.cc tomtom.cc \
        tef_xml.cc maggeo.cc pathaway.cc vitosmt.cc gdb.cc bcr.cc coto.cc \
        ignrando.cc stmwpp.cc msroute.cc cst.cc nmn4.cc mag_pdb.cc compegps.cc \
        yahoo.cc unicsv.cc wfff_xml.cc garmin_txt.cc axim_gpb.cc gpssim.cc \
        stmsdf.cc gtrnctr.cc dmtlog.cc raymarine.cc alan.cc vitovtt.cc \
        ggv_log.cc g7towin.cc garmin_gpi.cc lmx.cc random.cc xol.cc dg-100.cc \
        navilink.cc mtk_logger.cc ik3d.cc osm.cc destinator.cc exif.cc vidaone.cc \
        igo8.cc gopal.cc humminbird.cc mapasia.cc gnav_trl.cc navitel.cc ggv_ovl.cc \
        jtr.cc sbp.cc sbn.cc mmo.cc skyforce.cc itracku.cc v900.cc delbin.cc \
        pocketfms_bc.cc pocketfms_fp.cc pocketfms_wp.cc naviguide.cc enigma.cc \
        vpl.cc teletype.cc jogmap.cc bushnell.cc bushnell_trl.cc wintec_tes.cc \
        subrip.cc garmin_xt.cc explorist_ini.cc \

ALL_FMTS=$$MINIMAL_FMTS
FILTERS=position.cc radius.cc duplicate.cc arcdist.cc polygon.cc smplrout.cc \
        reverse_route.cc sort.cc stackfilter.cc trackfilter.cc discard.cc \
        nukedata.cc interpolate.cc transform.cc height.cc swapdata.cc

SHAPE=shapelib/shpopen.c shapelib/dbfopen.c pdbfile.cc

ZLIB=zlib/adler32.c zlib/compress.c zlib/crc32.c zlib/deflate.c zlib/inffast.c \
        zlib/inflate.c zlib/infback.c zlib/inftrees.c zlib/trees.c \
        zlib/uncompr.c zlib/gzio.c zlib/zutil.c

JEEPS += jeeps/gpsapp.cc jeeps/gpscom.cc \
         jeeps/gpsmath.cc jeeps/gpsmem.cc  \
         jeeps/gpsprot.cc jeeps/gpsread.cc \
         jeeps/gpsdevice.cc jeeps/gpsdevice_ser.cc jeeps/gpsdevice_usb.cc \
         jeeps/gpsrqst.cc jeeps/gpssend.cc jeeps/gpsserial.cc jeeps/jgpsutil.cc \
         jeeps/gpsusbread.cc jeeps/gpsusbsend.cc \
         jeeps/gpsusbcommon.cc


SUPPORT = queue.cc route.cc waypt.cc filter_vecs.cc util.cc vecs.cc mkshort.cc \
          csv_util.cc strptime.c grtcirc.cc vmem.cc util_crc.cc xmlgeneric.cc \
          uuid.cc formspec.cc xmltag.cc cet.cc cet_util.cc fatal.cc rgbcolors.cc \
          inifile.cc garmin_fs.cc gbsleep.cc units.cc gbser.cc \
          gbfile.cc parse.cc avltree.cc session.cc main.cc globals.cc

SUBDIRS += jeeps

macx|linux {
  DEFINES += HAVE_NANOSLEEP HAVE_LIBUSB HAVE_LIBEXPAT HAVE_GLOB
  DEFINES += HAVE_VA_COPY HAVE_VA_LIST_AS_ARRAY
  SOURCES += gbser_posix.cc jeeps/gpslibusb.cc
  INCLUDEPATH += jeeps
  LIBS += -lexpat
}

win32 {
  SOURCES += gbser_win32.cc jeeps/gpsusbwin.c
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

# We don't care about stripping things out of the build.  Full monty, baby.
#DEFINES += MAXIMAL_ENABLED
DEFINES += FILTERS_ENABLED
#DEFINES += PDBFMTS_ENABLED
#DEFINES += SHAPELIB_ENABLED
#DEFINES += CSVFMTS_ENABLED
#DEFINES += CET_WANTED
