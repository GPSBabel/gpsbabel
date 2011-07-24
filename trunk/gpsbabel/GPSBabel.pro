QT -= gui

TARGET = GPSBabel
CONFIG += console
CONFIG -= app_bundle

TEMPLATE = app

MINIMAL_FMTS = magproto.c gpx.c geo.c mapsend.c mapsource.c garmin.c \
               garmin_device_xml.c garmin_tables.c internal_styles.c nmea.c \
               kml.c wbt-200.c

ALL_FMTS=$$MINIMAL_FMTS gtm.c gpsutil.c pcx.c cetus.c copilot.c \
        gpspilot.c magnav.c skytraq.c \
        psp.c holux.c tmpro.c tpg.c tpo.c \
        xcsv.c gcdb.c tiger.c easygps.c quovadis.c \
        gpilots.c saroute.c navicache.c psitrex.c geoniche.c delgpl.c \
        ozi.c text.c html.c palmdoc.c netstumbler.c hsa_ndv.c \
        igc.c brauniger_iq.c shape.c hiketech.c glogbook.c coastexp.c \
        vcf.c overlay.c google.c xhtmlent.c lowranceusr.c an1.c tomtom.c \
        tef_xml.c maggeo.c pathaway.c vitosmt.c gdb.c bcr.c coto.c \
        ignrando.c stmwpp.c msroute.c cst.c nmn4.c mag_pdb.c compegps.c \
        yahoo.c unicsv.c wfff_xml.c garmin_txt.c axim_gpb.c gpssim.c \
        stmsdf.c gtrnctr.c dmtlog.c raymarine.c alan.c vitovtt.c \
        ggv_log.c g7towin.c garmin_gpi.c lmx.c random.c xol.c dg-100.c \
        navilink.c mtk_logger.c ik3d.c osm.c destinator.c exif.c vidaone.c \
        igo8.c gopal.c humminbird.c mapasia.c gnav_trl.c navitel.c ggv_ovl.c \
        jtr.c sbp.c sbn.c mmo.c skyforce.c itracku.c v900.c delbin.c \
        pocketfms_bc.c pocketfms_fp.c pocketfms_wp.c naviguide.c enigma.c \
        vpl.c teletype.c jogmap.c bushnell.c bushnell_trl.c wintec_tes.c \
        subrip.c garmin_xt.c explorist_ini.c \

FILTERS=position.c radius.c duplicate.c arcdist.c polygon.c smplrout.c \
        reverse_route.c sort.c stackfilter.c trackfilter.c discard.c \
        nukedata.c interpolate.c transform.c height.c swapdata.c

SHAPE=shapelib/shpopen.c shapelib/dbfopen.c pdbfile.c

ZLIB=zlib/adler32.c zlib/compress.c zlib/crc32.c zlib/deflate.c zlib/inffast.c \
        zlib/inflate.c zlib/infback.c zlib/inftrees.c zlib/trees.c \
        zlib/uncompr.c zlib/gzio.c zlib/zutil.c

JEEPS += jeeps/gpsapp.c jeeps/gpscom.c \
         jeeps/gpsmath.c jeeps/gpsmem.c  \
         jeeps/gpsprot.c jeeps/gpsread.c \
         jeeps/gpsdevice.c jeeps/gpsdevice_ser.c jeeps/gpsdevice_usb.c \
         jeeps/gpsrqst.c jeeps/gpssend.c jeeps/gpsserial.c jeeps/jgpsutil.c \
         jeeps/gpsusbread.c jeeps/gpsusbsend.c \
         jeeps/gpsusbcommon.c


SUPPORT = queue.c route.c waypt.c filter_vecs.c util.c vecs.c mkshort.c \
          csv_util.c strptime.c grtcirc.c vmem.c util_crc.c xmlgeneric.c \
          uuid.c formspec.c xmltag.c cet.c cet_util.c fatal.c rgbcolors.c \
          inifile.c garmin_fs.c gbsleep.c units.c gbser.c \
          gbfile.c parse.c avltree.c session.c main.c globals.c

SUBDIRS += jeeps

macx|linux {
  DEFINES += HAVE_NANOSLEEP HAVE_LIBUSB HAVE_LIBEXPAT HAVE_GLOB
  DEFINES += HAVE_VA_COPY HAVE_VA_LIST_AS_ARRAY
  SOURCES += gbser_posix.c jeeps/gpslibusb.c
  INCLUDEPATH += jeeps
  LIBS += -lexpat
}
win32 {
  SOURCES += gbser_win32.c jeeps/gpsusbwin.c
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
DEFINES += MAXIMAL_ENABLED
DEFINES += FILTERS_ENABLED
DEFINES += PDBFMTS_ENABLED
DEFINES += SHAPELIB_ENABLED
DEFINES += CSVFMTS_ENABLED
DEFINES += CET_WANTED
