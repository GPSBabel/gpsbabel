
VERSU=1_2_8
VERSD=1.2.8
RELEASE=-beta11082005
VERSIONU=$(VERSU)$(RELEASE)
VERSIOND=$(VERSD)$(RELEASE)

# VERSIONU=1_2_7
# VERSIOND=1.2.7

# If you do not have libexpat and you have no use for reading any input
# type that is XML-ish (i.e. gpx or geocaching.com's/loc) you can uncomment
# INHIBIT_EXPAT and coment out LIBEXPAT on just to get a build working quickly.
# INHIBIT_EXPAT=-DNO_EXPAT
LIBEXPAT=-lexpat # -lefence

# USB may required non-standard libraries (like libusb) be installed
# and may not be available on all OSes.  Uncomment this to remove the key
# parts of USB from the build.
LIBUSB=-lusb

# Space is significant, because MSVC wants no space between switch and arg (-Fofoo.o)
# but cc/gcc does:
#  $(OUTPUT_SWITCH)main.o
#  becomes -o main.o (gcc)
#   or     -Fomain.o (cl.exe)
OUTPUT_SWITCH=-o #

#
# Enable either or both of these as you wish.
#
OPTIMIZATION=-O $(EXTRA_OPTIMIZATION)
DEBUGGING=-g $(EXTRA_DEBUGGING)
# add -DDEBUG_MEM to turn on memory allocation logging
CFLAGS=$(EXTRA_CFLAGS) $(DEBUGGING) -Icoldsync $(INHIBIT_EXPAT) $(INHIBIT_USB) $(OPTIMIZATION)
INSTALL_TARGETDIR=/usr/local/

FMTS=magproto.o gpx.o geo.o mapsend.o mapsource.o garmin_tables.o \
	gpsutil.o pcx.o cetus.o copilot.o gpspilot.o magnav.o \
	psp.o holux.o garmin.o tmpro.o tpg.o tpo.o \
	xcsv.o gcdb.o tiger.o internal_styles.o easygps.o quovadis.o \
	gpilots.o saroute.o navicache.o psitrex.o geoniche.o delgpl.o \
	ozi.o nmea.o text.o html.o palmdoc.o netstumbler.o hsa_ndv.o \
	igc.o brauniger_iq.o shape.o hiketech.o glogbook.o coastexp.o \
	vcf.o overlay.o kml.o google.o lowranceusr.o an1.o tomtom.o \
	tef_xml.o maggeo.o pathaway.o vitosmt.o gdb.o bcr.o coto.o \
	ignrando.o stmwpp.o msroute.o cst.o nmn4.o mag_pdb.o compegps.o

FILTERS=position.o duplicate.o arcdist.o polygon.o smplrout.o \
	reverse_route.o sort.o stackfilter.o trackfilter.o discard.o \
	nukedata.o

OSJEEPS=jeeps/gpslibusb.o
JEEPS=jeeps/gpsapp.o jeeps/gpscom.o \
	jeeps/gpsmath.o jeeps/gpsmem.o  \
	jeeps/gpsprot.o jeeps/gpsread.o \
	jeeps/gpsrqst.o jeeps/gpssend.o jeeps/gpsserial.o jeeps/gpsutil.o \
	jeeps/gpsusbread.o jeeps/gpsusbsend.o jeeps/gpsusbstub.o $(OSJEEPS)
# Extra modules in Jeeps that we don't use
# 	jeeps/gpsfmt.o jeeps/gpsinput.o jeeps/gpsproj.o


COLDSYNC=coldsync/util.o coldsync/pdb.o

SHAPE=shapelib/shpopen.o shapelib/dbfopen.o

LIBOBJS = queue.o route.o waypt.o filter_vecs.o util.o vecs.o mkshort.o \
          csv_util.o strptime.o grtcirc.o vmem.o util_crc.o xmlgeneric.o \
          uuid.o formspec.o xmltag.o cet.o cet_util.o fatal.o \
	$(COLDSYNC) $(GARMIN) $(JEEPS) $(SHAPE) $(FMTS) $(FILTERS)
OBJS = main.o globals.o $(LIBOBJS)

.c.o:
	$(CC) -c $(CFLAGS) $< $(OUTPUT_SWITCH)$@

# Directory of web doc 
WEB=../babelweb/


all: gpsbabel

#
# Alternate makefile target for the case when you have no libusb and no 
# need for Garmin/USB (60, 76C, VistaC, Quest, etc.) support.
#
usbfree:
	make LIBUSB= INHIBIT_USB=-DNO_USB

gpsbabel: $(OBJS)
	$(CC) $(CFLAGS) $(OBJS) $(LIBEXPAT) $(LIBUSB) -lm $(OUTPUT_SWITCH)$@

globals.o:
	$(CC) -c $(CFLAGS) -DVERSION=\"$(VERSIOND)\" $< $(OUTPUT_SWITCH)$@

clean:
	rm -f $(OBJS) gpsbabel gpsbabel.exe

check:
	./testo

torture:
	./testo
	./torture_test

#
# This will only work on UNIX-like substances.
#
install:
	install gpsbabel  $(INSTALL_TARGETDIR)/bin

# Nerdy release stuff that needs to work only on Linux.

leaktest:
	make EXTRA_CFLAGS=-DDEBUG_MEM
	tools/cleardebug
	./testo
	tools/memdebug | grep -v '^command line:'

dep:
	make clean && make CC="gcc -MMD"  && cat *.d */*.d > /tmp/dep && rm *.d */*.d
	(echo -n "internal_styles.c: mkstyle.sh " ; echo style/*.style ; /bin/echo -e '\t./mkstyle.sh > internal_styles.c || (rm -f internal_styles.c ; exit 1)' ) >> /tmp/dep
	echo Edit Makefile and bring in /tmp/dep

# Doc targets have large external requirements.
# Requires saxon6, not saxon8.  Saxon requires Java.
# Requires hardcoded pathnames to installed docbook both in the 
# source file and on this command like.  (Ick.)
# Requires CLASSPATH be exported to include full path to saxon.jar.
# (typically /usr/share/java on a Linux system.)
readme.html: readme.xml 
	java  com.icl.saxon.StyleSheet $< \
	/usr/share/sgml/docbook/xsl-stylesheets-1.68.1-1/xhtml/docbook.xsl \
	html.stylesheet="http://www.gpsbabel.org/style3.css" > $@
	cp readme.html $(WEB)/readme.xhtml
	tools/mkcapabilities

readme.txt: readme.html
	lynx -nolist -dump readme.html  > $@

doc: readme.html # readme.txt

release: 
	cvs commit
	./chkdoc
	make clean &&  cd mingw ; make clean 
	rm -fr gpsbabel-$(VERSIOND)
	cvs tag -F gpsbabel_$(VERSIONU)
	cvs export -r gpsbabel_$(VERSIONU) -d gpsbabel-$(VERSIOND) gpsbabel
	tar czf /tmp/gpsbabel-$(VERSIOND).tar.gz gpsbabel-$(VERSIOND)
	cd /tmp ; tar xzf gpsbabel-$(VERSIOND).tar.gz
	touch /tmp/gpsbabel-$(VERSIOND)/internal_styles.c
	cd /tmp/gpsbabel-$(VERSIOND)/mingw ; make
	curl -u anonymous:anonymous --upload-file /tmp/gpsbabel-$(VERSIOND).tar.gz ftp://upload.sf.net/incoming/
	curl -u anonymous:anonymous --upload-file /tmp/gpsbabel-$(VERSIOND).zip ftp://upload.sf.net/incoming/

rpm: clean
	tools/mkrpm $(VERSD) $(RELEASE)

rpmrelease:
	curl -u anonymous:anonymous --upload-file /usr/src/redhat/SRPMS/gpsbabel-$(VERSIOND).src.rpm  ftp://upload.sf.net/incoming/ 
	curl -u anonymous:anonymous --upload-file /usr/src/redhat/RPMS/i386/gpsbabel-$(VERSIOND).i386.rpm  ftp://upload.sf.net/incoming/ 

mac-usbfree:
	make LIBEXPAT=/sw/lib/libexpat.a EXTRA_CFLAGS="-I/sw/include" LIBUSB= INHIBIT_USB=-DNO_USB

mac-build:
	make LIBEXPAT=/sw/lib/libexpat.a EXTRA_CFLAGS="-I/sw/include" LIBUSB="/sw/lib/libusb.a -lIOKit  -lBSDPClient -framework CoreFoundation"

mac-release:
	mkdir -p usr/bin usr/share/gpsbabel/doc
	cp gpsbabel usr/bin/
	cp README* COPYING usr/share/gpsbabel/doc
	tar cvzf gpsbabel-osx.tgz usr/bin/gpsbabel
	curl -u anonymous:anonymous --upload-file gpsbabel-osx.tgz ftp://upload.sf.net/incoming/

msvc-build:
	make CC=@CL.EXE DEBUGGING="" EXTRA_CFLAGS="-nologo -W3 -WL -D__WIN32__ -I msvc/expat " OUTPUT_SWITCH="-Fo" $(OBJS)
	echo $(OBJS) > objs.lst
	LINK.EXE /NOLOGO @objs.lst ./msvc/expat/libexpat.lib /out:gpsbabel.exe 

# Machine generated from here down.  
an1.o: an1.c defs.h queue.h gbtypes.h an1sym.h
arcdist.o: arcdist.c defs.h queue.h gbtypes.h grtcirc.h
bcr.o: bcr.c defs.h queue.h gbtypes.h garmin_tables.h
brauniger_iq.o: brauniger_iq.c defs.h queue.h gbtypes.h jeeps/gpsserial.h \
  jeeps/gps.h jeeps/../defs.h jeeps/gpsport.h jeeps/gpsserial.h \
  jeeps/gpssend.h jeeps/gpsread.h jeeps/gpsutil.h jeeps/gpsapp.h \
  jeeps/gpsprot.h jeeps/gpscom.h jeeps/gpsfmt.h jeeps/gpsmath.h \
  jeeps/gpsnmea.h jeeps/gpsmem.h jeeps/gpsrqst.h jeeps/gpsinput.h \
  jeeps/gpsproj.h jeeps/gpsnmeafmt.h jeeps/gpsnmeaget.h
cetus.o: cetus.c defs.h queue.h gbtypes.h coldsync/palm.h coldsync/pdb.h
coastexp.o: coastexp.c defs.h queue.h gbtypes.h xmlgeneric.h uuid.h cet_util.h
compegps.o: compegps.c defs.h queue.h gbtypes.h csv_util.h
copilot.o: copilot.c defs.h queue.h gbtypes.h coldsync/palm.h \
  coldsync/pdb.h
coto.o: coto.c defs.h queue.h gbtypes.h csv_util.h coldsync/palm.h \
  coldsync/pdb.h
cst.o: cst.c defs.h queue.h gbtypes.h
csv_util.o: csv_util.c defs.h queue.h gbtypes.h csv_util.h grtcirc.h \
  strptime.h
delgpl.o: delgpl.c defs.h queue.h gbtypes.h
discard.o: discard.c defs.h queue.h gbtypes.h
duplicate.o: duplicate.c defs.h queue.h gbtypes.h
easygps.o: easygps.c defs.h queue.h gbtypes.h
filter_vecs.o: filter_vecs.c defs.h queue.h gbtypes.h
formspec.o: formspec.c defs.h queue.h gbtypes.h
garmin.o: garmin.c defs.h queue.h gbtypes.h jeeps/gps.h jeeps/../defs.h \
  jeeps/gpsport.h jeeps/gpsserial.h jeeps/gps.h jeeps/gpssend.h \
  jeeps/gpsread.h jeeps/gpsutil.h jeeps/gpsapp.h jeeps/gpsprot.h \
  jeeps/gpscom.h jeeps/gpsfmt.h jeeps/gpsmath.h jeeps/gpsnmea.h \
  jeeps/gpsmem.h jeeps/gpsrqst.h jeeps/gpsinput.h jeeps/gpsproj.h \
  jeeps/gpsnmeafmt.h jeeps/gpsnmeaget.h garmin_tables.h
garmin_tables.o: garmin_tables.c garmin_tables.h
gcdb.o: gcdb.c defs.h queue.h gbtypes.h coldsync/palm.h coldsync/pdb.h
gdb.o: gdb.c defs.h queue.h gbtypes.h garmin_tables.h jeeps/gpsmath.h \
  jeeps/gps.h jeeps/../defs.h jeeps/gpsport.h jeeps/gpsserial.h \
  jeeps/gpssend.h jeeps/gpsread.h jeeps/gpsutil.h jeeps/gpsapp.h \
  jeeps/gpsprot.h jeeps/gpscom.h jeeps/gpsfmt.h jeeps/gpsmath.h \
  jeeps/gpsnmea.h jeeps/gpsmem.h jeeps/gpsrqst.h jeeps/gpsinput.h \
  jeeps/gpsproj.h jeeps/gpsnmeafmt.h jeeps/gpsnmeaget.h
geo.o: geo.c defs.h queue.h gbtypes.h xmlgeneric.h
geoniche.o: geoniche.c defs.h queue.h gbtypes.h coldsync/palm.h \
  coldsync/pdb.h
globals.o: globals.c defs.h queue.h gbtypes.h
glogbook.o: glogbook.c defs.h queue.h gbtypes.h xmlgeneric.h
google.o: google.c defs.h queue.h gbtypes.h xmlgeneric.h
gpilots.o: gpilots.c defs.h queue.h gbtypes.h coldsync/palm.h \
  coldsync/pdb.h garmin_tables.h
gpspilot.o: gpspilot.c defs.h queue.h gbtypes.h coldsync/palm.h \
  coldsync/pdb.h
gpsutil.o: gpsutil.c defs.h queue.h gbtypes.h magellan.h
gpx.o: gpx.c defs.h queue.h gbtypes.h xmlgeneric.h cet_util.h
grtcirc.o: grtcirc.c defs.h queue.h gbtypes.h
hiketech.o: hiketech.c defs.h queue.h gbtypes.h xmlgeneric.h
holux.o: holux.c defs.h queue.h gbtypes.h holux.h
hsa_ndv.o: hsa_ndv.c defs.h queue.h gbtypes.h cet_util.h
html.o: html.c defs.h queue.h gbtypes.h jeeps/gpsmath.h jeeps/gps.h \
  jeeps/../defs.h jeeps/gpsport.h jeeps/gpsserial.h jeeps/gpssend.h \
  jeeps/gpsread.h jeeps/gpsutil.h jeeps/gpsapp.h jeeps/gpsprot.h \
  jeeps/gpscom.h jeeps/gpsfmt.h jeeps/gpsmath.h jeeps/gpsnmea.h \
  jeeps/gpsmem.h jeeps/gpsrqst.h jeeps/gpsinput.h jeeps/gpsproj.h \
  jeeps/gpsnmeafmt.h jeeps/gpsnmeaget.h
igc.o: igc.c defs.h queue.h gbtypes.h
ignrando.o: ignrando.c defs.h queue.h gbtypes.h xmlgeneric.h
internal_styles.o: internal_styles.c defs.h queue.h gbtypes.h
kml.o: kml.c defs.h queue.h gbtypes.h xmlgeneric.h
lowranceusr.o: lowranceusr.c defs.h queue.h gbtypes.h
maggeo.o: maggeo.c defs.h queue.h gbtypes.h xmlgeneric.h magellan.h
magnav.o: magnav.c defs.h queue.h gbtypes.h coldsync/palm.h \
  coldsync/pdb.h
magproto.o: magproto.c defs.h queue.h gbtypes.h magellan.h
main.o: main.c defs.h queue.h gbtypes.h
mapsend.o: mapsend.c defs.h queue.h gbtypes.h mapsend.h magellan.h
mapsource.o: mapsource.c defs.h queue.h gbtypes.h garmin_tables.h
mkshort.o: mkshort.c defs.h queue.h gbtypes.h
msroute.o: msroute.c defs.h queue.h gbtypes.h
navicache.o: navicache.c defs.h queue.h gbtypes.h cet_util.h
netstumbler.o: netstumbler.c defs.h queue.h gbtypes.h csv_util.h
nmea.o: nmea.c defs.h queue.h gbtypes.h
nmn4.o: nmn4.c defs.h queue.h gbtypes.h
mag_pdb.o: mag_pdb.c defs.h queue.h gbtypes.h coldsync/palm.h coldsync/pdb.h \
  jeeps/gpsmath.h
overlay.o: overlay.c defs.h queue.h gbtypes.h grtcirc.h
ozi.o: ozi.c defs.h queue.h gbtypes.h csv_util.h
palmdoc.o: palmdoc.c defs.h queue.h gbtypes.h jeeps/gpsmath.h jeeps/gps.h \
  jeeps/../defs.h jeeps/gpsport.h jeeps/gpsserial.h jeeps/gpssend.h \
  jeeps/gpsread.h jeeps/gpsutil.h jeeps/gpsapp.h jeeps/gpsprot.h \
  jeeps/gpscom.h jeeps/gpsfmt.h jeeps/gpsmath.h jeeps/gpsnmea.h \
  jeeps/gpsmem.h jeeps/gpsrqst.h jeeps/gpsinput.h jeeps/gpsproj.h \
  jeeps/gpsnmeafmt.h jeeps/gpsnmeaget.h coldsync/palm.h coldsync/pdb.h
pathaway.o: pathaway.c defs.h queue.h gbtypes.h coldsync/palm.h \
  coldsync/pdb.h csv_util.h
pcx.o: pcx.c defs.h queue.h gbtypes.h garmin_tables.h
polygon.o: polygon.c defs.h queue.h gbtypes.h
position.o: position.c defs.h queue.h gbtypes.h grtcirc.h
psitrex.o: psitrex.c defs.h queue.h gbtypes.h garmin_tables.h
psp.o: psp.c defs.h queue.h gbtypes.h cet_util.h
queue.o: queue.c queue.h
quovadis.o: quovadis.c quovadis.h defs.h queue.h gbtypes.h \
  coldsync/palm.h coldsync/pdb.h
reverse_route.o: reverse_route.c defs.h queue.h gbtypes.h
route.o: route.c defs.h queue.h gbtypes.h
saroute.o: saroute.c defs.h queue.h gbtypes.h
shape.o: shape.c defs.h queue.h gbtypes.h shapelib/shapefil.h
smplrout.o: smplrout.c defs.h queue.h gbtypes.h grtcirc.h
sort.o: sort.c defs.h queue.h gbtypes.h
stackfilter.o: stackfilter.c defs.h queue.h gbtypes.h
strptime.o: strptime.c strptime.h
stmwpp.o: stmwpp.c defs.h queue.h gbtypes.h csv_util.h
tef_xml.o: tef_xml.c defs.h queue.h gbtypes.h xmlgeneric.h
text.o: text.c defs.h queue.h gbtypes.h jeeps/gpsmath.h jeeps/gps.h \
  jeeps/../defs.h jeeps/gpsport.h jeeps/gpsserial.h jeeps/gpssend.h \
  jeeps/gpsread.h jeeps/gpsutil.h jeeps/gpsapp.h jeeps/gpsprot.h \
  jeeps/gpscom.h jeeps/gpsfmt.h jeeps/gpsmath.h jeeps/gpsnmea.h \
  jeeps/gpsmem.h jeeps/gpsrqst.h jeeps/gpsinput.h jeeps/gpsproj.h \
  jeeps/gpsnmeafmt.h jeeps/gpsnmeaget.h
tiger.o: tiger.c defs.h queue.h gbtypes.h csv_util.h
tmpro.o: tmpro.c defs.h queue.h gbtypes.h csv_util.h
tomtom.o: tomtom.c defs.h queue.h gbtypes.h
tpg.o: tpg.c defs.h queue.h gbtypes.h jeeps/gpsmath.h jeeps/gps.h \
  jeeps/../defs.h jeeps/gpsport.h jeeps/gpsserial.h jeeps/gpssend.h \
  jeeps/gpsread.h jeeps/gpsutil.h jeeps/gpsapp.h jeeps/gpsprot.h \
  jeeps/gpscom.h jeeps/gpsfmt.h jeeps/gpsmath.h jeeps/gpsnmea.h \
  jeeps/gpsmem.h jeeps/gpsrqst.h jeeps/gpsinput.h jeeps/gpsproj.h \
  jeeps/gpsnmeafmt.h jeeps/gpsnmeaget.h
trackfilter.o: trackfilter.c defs.h queue.h gbtypes.h strptime.h
util.o: util.c defs.h queue.h gbtypes.h
util_crc.o: util_crc.c
uuid.o: uuid.c uuid.h
vcf.o: vcf.c defs.h queue.h gbtypes.h jeeps/gpsmath.h jeeps/gps.h \
  jeeps/../defs.h jeeps/gpsport.h jeeps/gpsserial.h jeeps/gpssend.h \
  jeeps/gpsread.h jeeps/gpsutil.h jeeps/gpsapp.h jeeps/gpsprot.h \
  jeeps/gpscom.h jeeps/gpsfmt.h jeeps/gpsmath.h jeeps/gpsnmea.h \
  jeeps/gpsmem.h jeeps/gpsrqst.h jeeps/gpsinput.h jeeps/gpsproj.h \
  jeeps/gpsnmeafmt.h jeeps/gpsnmeaget.h
vecs.o: vecs.c defs.h queue.h gbtypes.h csv_util.h
vitosmt.o: vitosmt.c defs.h queue.h gbtypes.h
vmem.o: vmem.c defs.h queue.h gbtypes.h
waypt.o: waypt.c defs.h queue.h gbtypes.h
xcsv.o: xcsv.c defs.h queue.h gbtypes.h csv_util.h
xmlgeneric.o: xmlgeneric.c defs.h queue.h gbtypes.h cet_util.h xmlgeneric.h
cet.o: cet.h cet.c defs.h
cet_util.o: cet_util.h cet_util.c cet.h defs.h \
    cet/ansi_x3_4_1968.h cet/atarist.h cet/baltic.h cet/bs_4730.h \
    cet/bs_viewdata.h cet/cp1250.h cet/cp1251.h cet/cp1252.h \
    cet/cp1253.h cet/cp1254.h cet/cp1255.h cet/cp1256.h \
    cet/cp1257.h cet/csa_z243_4_1985_1.h cet/csa_z243_4_1985_2.h cet/csa_z243_4_1985_gr.h \
    cet/csn_369103.h cet/cwi.h cet/dec_mcs.h cet/din_66003.h \
    cet/ds_2089.h cet/ecma_cyrillic.h cet/es.h cet/es2.h \
    cet/gb_1988_80.h cet/gost_19768_87.h cet/hp_roman8.h cet/ibm037.h \
    cet/ibm1004.h cet/ibm1026.h cet/ibm1047.h cet/ibm256.h \
    cet/ibm273.h cet/ibm277.h cet/ibm278.h cet/ibm280.h \
    cet/ibm284.h cet/ibm285.h cet/ibm297.h cet/ibm437.h \
    cet/ibm500.h cet/ibm850.h cet/ibm851.h cet/ibm852.h \
    cet/ibm855.h cet/ibm857.h cet/ibm860.h cet/ibm861.h \
    cet/ibm862.h cet/ibm863.h cet/ibm864.h cet/ibm865.h \
    cet/ibm868.h cet/ibm869.h cet/ibm871.h cet/ibm891.h \
    cet/ibm903.h cet/ibm904.h cet/iec_p27_1.h cet/iso_10367_box.h \
    cet/iso_5427.h cet/iso_646_irv.h cet/iso_6937_2_25.h cet/iso_8859_1.h \
    cet/iso_8859_10.h cet/iso_8859_13.h cet/iso_8859_14.h cet/iso_8859_15.h \
    cet/iso_8859_2.h cet/iso_8859_3.h cet/iso_8859_4.h cet/iso_8859_5.h \
    cet/iso_8859_6.h cet/iso_8859_7.h cet/iso_8859_8.h cet/iso_8859_9.h \
    cet/iso_8859_supp.h cet/it.h cet/jis_c6220_1969_ro.h cet/jis_x0201.h \
    cet/jus_i_b1_002.h cet/jus_i_b1_003_mac.h cet/jus_i_b1_003_serb.h cet/keybcs2.h \
    cet/koi8_r.h cet/koi8_ru.h cet/koi8_u.h cet/koi_7.h \
    cet/koi_8.h cet/koi_8_cs2.h cet/ksc5636.h cet/latin_greek_1.h \
    cet/mac_is.h cet/macintosh.h cet/macintosh_ce.h cet/msz_7795_3.h \
    cet/nats_dano.h cet/nats_sefi.h cet/nc_nc00_10.h cet/nextstep.h \
    cet/nf_z_62_010.h cet/nf_z_62_010__1973_.h cet/ns_4551_1.h cet/ns_4551_2.h \
    cet/pt.h cet/pt2.h cet/sami.h cet/sen_850200_b.h \
    cet/sen_850200_c.h cet/tcvn.h cet/viscii.h cet/vps.h
coldsync/pdb.o: coldsync/pdb.c coldsync/config.h coldsync/palm.h \
  coldsync/pdb.h
coldsync/util.o: coldsync/util.c coldsync/config.h coldsync/pconn/util.h \
  coldsync/palm.h
jeeps/gpsapp.o: jeeps/gpsapp.c jeeps/gps.h jeeps/../defs.h \
  jeeps/../queue.h jeeps/../gbtypes.h jeeps/gpsport.h jeeps/gpsserial.h \
  jeeps/gpssend.h jeeps/gpsread.h jeeps/gpsutil.h jeeps/gpsapp.h \
  jeeps/gpsprot.h jeeps/gpscom.h jeeps/gpsfmt.h jeeps/gpsmath.h \
  jeeps/gpsnmea.h jeeps/gpsmem.h jeeps/gpsrqst.h jeeps/gpsinput.h \
  jeeps/gpsproj.h jeeps/gpsnmeafmt.h jeeps/gpsnmeaget.h
jeeps/gpscom.o: jeeps/gpscom.c jeeps/gps.h jeeps/../defs.h \
  jeeps/../queue.h jeeps/../gbtypes.h jeeps/gpsport.h jeeps/gpsserial.h \
  jeeps/gpssend.h jeeps/gpsread.h jeeps/gpsutil.h jeeps/gpsapp.h \
  jeeps/gpsprot.h jeeps/gpscom.h jeeps/gpsfmt.h jeeps/gpsmath.h \
  jeeps/gpsnmea.h jeeps/gpsmem.h jeeps/gpsrqst.h jeeps/gpsinput.h \
  jeeps/gpsproj.h jeeps/gpsnmeafmt.h jeeps/gpsnmeaget.h
jeeps/gpslibusb.o: jeeps/gpslibusb.c jeeps/gps.h jeeps/../defs.h \
  jeeps/../queue.h jeeps/../gbtypes.h jeeps/gpsport.h jeeps/gpsserial.h \
  jeeps/gpssend.h jeeps/gpsread.h jeeps/gpsutil.h jeeps/gpsapp.h \
  jeeps/gpsprot.h jeeps/gpscom.h jeeps/gpsfmt.h jeeps/gpsmath.h \
  jeeps/gpsnmea.h jeeps/gpsmem.h jeeps/gpsrqst.h jeeps/gpsinput.h \
  jeeps/gpsproj.h jeeps/gpsnmeafmt.h jeeps/gpsnmeaget.h jeeps/garminusb.h
jeeps/gpsmath.o: jeeps/gpsmath.c jeeps/gps.h jeeps/../defs.h \
  jeeps/../queue.h jeeps/../gbtypes.h jeeps/gpsport.h jeeps/gpsserial.h \
  jeeps/gpssend.h jeeps/gpsread.h jeeps/gpsutil.h jeeps/gpsapp.h \
  jeeps/gpsprot.h jeeps/gpscom.h jeeps/gpsfmt.h jeeps/gpsmath.h \
  jeeps/gpsnmea.h jeeps/gpsmem.h jeeps/gpsrqst.h jeeps/gpsinput.h \
  jeeps/gpsproj.h jeeps/gpsnmeafmt.h jeeps/gpsnmeaget.h jeeps/gpsdatum.h
jeeps/gpsmem.o: jeeps/gpsmem.c jeeps/gps.h jeeps/../defs.h \
  jeeps/../queue.h jeeps/../gbtypes.h jeeps/gpsport.h jeeps/gpsserial.h \
  jeeps/gpssend.h jeeps/gpsread.h jeeps/gpsutil.h jeeps/gpsapp.h \
  jeeps/gpsprot.h jeeps/gpscom.h jeeps/gpsfmt.h jeeps/gpsmath.h \
  jeeps/gpsnmea.h jeeps/gpsmem.h jeeps/gpsrqst.h jeeps/gpsinput.h \
  jeeps/gpsproj.h jeeps/gpsnmeafmt.h jeeps/gpsnmeaget.h jeeps/garminusb.h
jeeps/gpsprot.o: jeeps/gpsprot.c jeeps/gps.h jeeps/../defs.h \
  jeeps/../queue.h jeeps/../gbtypes.h jeeps/gpsport.h jeeps/gpsserial.h \
  jeeps/gpssend.h jeeps/gpsread.h jeeps/gpsutil.h jeeps/gpsapp.h \
  jeeps/gpsprot.h jeeps/gpscom.h jeeps/gpsfmt.h jeeps/gpsmath.h \
  jeeps/gpsnmea.h jeeps/gpsmem.h jeeps/gpsrqst.h jeeps/gpsinput.h \
  jeeps/gpsproj.h jeeps/gpsnmeafmt.h jeeps/gpsnmeaget.h
jeeps/gpsread.o: jeeps/gpsread.c jeeps/gps.h jeeps/../defs.h \
  jeeps/../queue.h jeeps/../gbtypes.h jeeps/gpsport.h jeeps/gpsserial.h \
  jeeps/gpssend.h jeeps/gpsread.h jeeps/gpsutil.h jeeps/gpsapp.h \
  jeeps/gpsprot.h jeeps/gpscom.h jeeps/gpsfmt.h jeeps/gpsmath.h \
  jeeps/gpsnmea.h jeeps/gpsmem.h jeeps/gpsrqst.h jeeps/gpsinput.h \
  jeeps/gpsproj.h jeeps/gpsnmeafmt.h jeeps/gpsnmeaget.h jeeps/gpsusbint.h
jeeps/gpsrqst.o: jeeps/gpsrqst.c jeeps/gps.h jeeps/../defs.h \
  jeeps/../queue.h jeeps/../gbtypes.h jeeps/gpsport.h jeeps/gpsserial.h \
  jeeps/gpssend.h jeeps/gpsread.h jeeps/gpsutil.h jeeps/gpsapp.h \
  jeeps/gpsprot.h jeeps/gpscom.h jeeps/gpsfmt.h jeeps/gpsmath.h \
  jeeps/gpsnmea.h jeeps/gpsmem.h jeeps/gpsrqst.h jeeps/gpsinput.h \
  jeeps/gpsproj.h jeeps/gpsnmeafmt.h jeeps/gpsnmeaget.h
jeeps/gpssend.o: jeeps/gpssend.c jeeps/gps.h jeeps/../defs.h \
  jeeps/../queue.h jeeps/../gbtypes.h jeeps/gpsport.h jeeps/gpsserial.h \
  jeeps/gpssend.h jeeps/gpsread.h jeeps/gpsutil.h jeeps/gpsapp.h \
  jeeps/gpsprot.h jeeps/gpscom.h jeeps/gpsfmt.h jeeps/gpsmath.h \
  jeeps/gpsnmea.h jeeps/gpsmem.h jeeps/gpsrqst.h jeeps/gpsinput.h \
  jeeps/gpsproj.h jeeps/gpsnmeafmt.h jeeps/gpsnmeaget.h jeeps/gpsusbint.h
jeeps/gpsserial.o: jeeps/gpsserial.c jeeps/gps.h jeeps/../defs.h \
  jeeps/../queue.h jeeps/../gbtypes.h jeeps/gpsport.h jeeps/gpsserial.h \
  jeeps/gpssend.h jeeps/gpsread.h jeeps/gpsutil.h jeeps/gpsapp.h \
  jeeps/gpsprot.h jeeps/gpscom.h jeeps/gpsfmt.h jeeps/gpsmath.h \
  jeeps/gpsnmea.h jeeps/gpsmem.h jeeps/gpsrqst.h jeeps/gpsinput.h \
  jeeps/gpsproj.h jeeps/gpsnmeafmt.h jeeps/gpsnmeaget.h jeeps/garminusb.h
jeeps/gpsusbread.o: jeeps/gpsusbread.c jeeps/gps.h jeeps/../defs.h \
  jeeps/../queue.h jeeps/../gbtypes.h jeeps/gpsport.h jeeps/gpsserial.h \
  jeeps/gpssend.h jeeps/gpsread.h jeeps/gpsutil.h jeeps/gpsapp.h \
  jeeps/gpsprot.h jeeps/gpscom.h jeeps/gpsfmt.h jeeps/gpsmath.h \
  jeeps/gpsnmea.h jeeps/gpsmem.h jeeps/gpsrqst.h jeeps/gpsinput.h \
  jeeps/gpsproj.h jeeps/gpsnmeafmt.h jeeps/gpsnmeaget.h jeeps/garminusb.h \
  jeeps/gpsusbint.h
jeeps/gpsusbsend.o: jeeps/gpsusbsend.c jeeps/gps.h jeeps/../defs.h \
  jeeps/../queue.h jeeps/../gbtypes.h jeeps/gpsport.h jeeps/gpsserial.h \
  jeeps/gpssend.h jeeps/gpsread.h jeeps/gpsutil.h jeeps/gpsapp.h \
  jeeps/gpsprot.h jeeps/gpscom.h jeeps/gpsfmt.h jeeps/gpsmath.h \
  jeeps/gpsnmea.h jeeps/gpsmem.h jeeps/gpsrqst.h jeeps/gpsinput.h \
  jeeps/gpsproj.h jeeps/gpsnmeafmt.h jeeps/gpsnmeaget.h jeeps/garminusb.h \
  jeeps/gpsusbint.h
jeeps/gpsusbstub.o: jeeps/gpsusbstub.c
jeeps/gpsutil.o: jeeps/gpsutil.c jeeps/gps.h jeeps/../defs.h \
  jeeps/../queue.h jeeps/../gbtypes.h jeeps/gpsport.h jeeps/gpsserial.h \
  jeeps/gpssend.h jeeps/gpsread.h jeeps/gpsutil.h jeeps/gpsapp.h \
  jeeps/gpsprot.h jeeps/gpscom.h jeeps/gpsfmt.h jeeps/gpsmath.h \
  jeeps/gpsnmea.h jeeps/gpsmem.h jeeps/gpsrqst.h jeeps/gpsinput.h \
  jeeps/gpsproj.h jeeps/gpsnmeafmt.h jeeps/gpsnmeaget.h
shapelib/dbfopen.o: shapelib/dbfopen.c shapelib/shapefil.h
shapelib/shpopen.o: shapelib/shpopen.c shapelib/shapefil.h
internal_styles.c: mkstyle.sh style/README.style style/arc.style style/csv.style style/custom.style style/dna.style style/fugawi.style style/geonet.style style/gpsdrive.style style/gpsdrivetrack.style style/gpsman.style style/mapconverter.style style/mxf.style style/nima.style style/openoffice.style style/s_and_t.style style/saplus.style style/tabsep.style style/xmap.style style/xmapwpt.style
	./mkstyle.sh > internal_styles.c || (rm -f internal_styles.c ; exit 1)
