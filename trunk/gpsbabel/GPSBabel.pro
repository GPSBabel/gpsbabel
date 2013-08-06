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
        subrip.cc garmin_xt.cc garmin_fit.cc lowranceusr4.cc \
        mtk_locus.cc googledir.cc mapbar_track.cc

# ALL_FMTS=$$MINIMAL_FMTS
FILTERS=position.cc radius.cc duplicate.cc arcdist.cc polygon.cc smplrout.cc \
        reverse_route.cc sort.cc stackfilter.cc trackfilter.cc discard.cc \
        nukedata.cc interpolate.cc transform.cc height.cc swapdata.cc bend.cc

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
          gbfile.cc parse.cc session.cc main.cc globals.cc

HEADERS =  \
	an1sym.h \
	cet.h \
	cet/ansi_x3_4_1968.h \
	cet/atarist.h \
	cet/baltic.h \
	cet/bs_4730.h \
	cet/bs_viewdata.h \
	cet/cp1250.h \
	cet/cp1251.h \
	cet/cp1252.h \
	cet/cp1253.h \
	cet/cp1254.h \
	cet/cp1255.h \
	cet/cp1256.h \
	cet/cp1257.h \
	cet/csa_z243_4_1985_1.h \
	cet/csa_z243_4_1985_2.h \
	cet/csa_z243_4_1985_gr.h \
	cet/csn_369103.h \
	cet/cwi.h \
	cet/dec_mcs.h \
	cet/din_66003.h \
	cet/ds_2089.h \
	cet/ecma_cyrillic.h \
	cet/es.h \
	cet/es2.h \
	cet/gb_1988_80.h \
	cet/gost_19768_87.h \
	cet/hp_roman8.h \
	cet/ibm037.h \
	cet/ibm1004.h \
	cet/ibm1026.h \
	cet/ibm1047.h \
	cet/ibm256.h \
	cet/ibm273.h \
	cet/ibm277.h \
	cet/ibm278.h \
	cet/ibm280.h \
	cet/ibm284.h \
	cet/ibm285.h \
	cet/ibm297.h \
	cet/ibm437.h \
	cet/ibm500.h \
	cet/ibm850.h \
	cet/ibm851.h \
	cet/ibm852.h \
	cet/ibm855.h \
	cet/ibm857.h \
	cet/ibm860.h \
	cet/ibm861.h \
	cet/ibm862.h \
	cet/ibm863.h \
	cet/ibm864.h \
	cet/ibm865.h \
	cet/ibm868.h \
	cet/ibm869.h \
	cet/ibm871.h \
	cet/ibm891.h \
	cet/ibm903.h \
	cet/ibm904.h \
	cet/iec_p27_1.h \
	cet/iso_10367_box.h \
	cet/iso_5427.h \
	cet/iso_646_irv.h \
	cet/iso_6937_2_25.h \
	cet/iso_8859_1.h \
	cet/iso_8859_10.h \
	cet/iso_8859_13.h \
	cet/iso_8859_14.h \
	cet/iso_8859_15.h \
	cet/iso_8859_2.h \
	cet/iso_8859_3.h \
	cet/iso_8859_4.h \
	cet/iso_8859_5.h \
	cet/iso_8859_6.h \
	cet/iso_8859_7.h \
	cet/iso_8859_8.h \
	cet/iso_8859_9.h \
	cet/iso_8859_supp.h \
	cet/it.h \
	cet/jis_c6220_1969_ro.h \
	cet/jis_x0201.h \
	cet/jus_i_b1_002.h \
	cet/jus_i_b1_003_mac.h \
	cet/jus_i_b1_003_serb.h \
	cet/keybcs2.h \
	cet/koi8_r.h \
	cet/koi8_ru.h \
	cet/koi8_u.h \
	cet/koi_7.h \
	cet/koi_8.h \
	cet/koi_8_cs2.h \
	cet/ksc5636.h \
	cet/latin_greek_1.h \
	cet/mac_is.h \
	cet/macintosh.h \
	cet/macintosh_ce.h \
	cet/msz_7795_3.h \
	cet/nats_dano.h \
	cet/nats_sefi.h \
	cet/nc_nc00_10.h \
	cet/nextstep.h \
	cet/nf_z_62_010.h \
	cet/nf_z_62_010__1973_.h \
	cet/ns_4551_1.h \
	cet/ns_4551_2.h \
	cet/pt.h \
	cet/pt2.h \
	cet/sami.h \
	cet/sen_850200_b.h \
	cet/sen_850200_c.h \
	cet/tcvn.h \
	cet/viscii.h \
	cet/vps.h \
	cet_util.h \
	config.h \
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
	geo.h \
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
	msvc/config.h \
	navilink.h \
	pdbfile.h \
	queue.h \
	quovadis.h \
	session.h \
	shapelib/shapefil.h \
	strptime.h \
	uuid.h \
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
	zlib/zutil.h

SUBDIRS += jeeps

macx|linux {
  DEFINES += HAVE_NANOSLEEP HAVE_LIBUSB HAVE_LIBEXPAT HAVE_GLOB
  DEFINES += HAVE_VA_COPY HAVE_VA_LIST_AS_ARRAY
  SOURCES += gbser_posix.cc
  JEEPS += jeeps/gpslibusb.cc
  INCLUDEPATH += jeeps
  LIBS += -lexpat
}

win32 {
  DEFINES += __WIN32__ _CONSOLE
  DEFINES -= UNICODE ZLIB_INHIBITED
  DEFINES += HAVE_LIBEXPAT
  CONFIG(debug, debug|release) {
    DEFINES += _DEBUG
  }
  SOURCES += gbser_win.cc
  JEEPS += jeeps/gpsusbwin.cc
  INCLUDEPATH += msvc/expat
  LIBS += ../gpsbabel/msvc/Expat/libexpat.lib setupapi.lib hid.lib
  TEMPLATE=vcapp
}

win32-msvc*{
  DEFINES += _CRT_SECURE_NO_DEPRECATE
  QMAKE_CXXFLAGS += /MP
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
