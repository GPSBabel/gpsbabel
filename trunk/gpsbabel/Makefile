# add -DDEBUG_MEM to turn on memory allocation logging
CFLAGS=$(EXTRA_CFLAGS) -g -Icoldsync
INSTALL_TARGETDIR=/usr/local/

FMTS=magproto.o gpx.o geo.o mapsend.o mapsource.o \
	gpsutil.o pcx.o cetus.o copilot.o gpspilot.o magnav.o \
	psp.o holux.o garmin.o tmpro.o tpg.o \
	xcsv.o gcdb.o tiger.o internal_styles.o easygps.o

FILTERS=position.o duplicate.o

JEEPS=jeeps/gpsapp.o jeeps/gpscom.o \
	jeeps/gpsmath.o jeeps/gpsmem.o  \
	jeeps/gpsprot.o jeeps/gpsread.o \
	jeeps/gpsrqst.o jeeps/gpssend.o jeeps/gpsserial.o jeeps/gpsutil.o
# Extra modules in Jeeps that we don't use
# 	jeeps/gpsfmt.o jeeps/gpsinput.o jeeps/gpsproj.o


COLDSYNC=coldsync/util.o coldsync/pdb.o

OBJS=main.o queue.o route.o waypt.o filter_vecs.o util.o vecs.o mkshort.o csv_util.o \
	$(COLDSYNC) $(GARMIN) $(JEEPS) $(FMTS) $(FILTERS)

.c.o:
	$(CC) -c $(CFLAGS) $< -o $@

all: gpsbabel

gpsbabel: $(OBJS)
	$(CC) $(CFLAGS) $(OBJS) -o gpsbabel -lexpat -lm

main.o:
	$(CC) -c $(CFLAGS) -DVERSION=\"$(VERSIOND)\" $<

clean:
	rm -f $(OBJS) gpsbabel gpsbabel.exe

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
	(echo -n "internal_styles.c: mkstyle.sh " ; echo style/*.style ; /bin/echo -e "\t./mkstyle.sh > internal_styles.c" ) >> /tmp/dep
	echo Edit Makefile and bring in /tmp/dep

VERSIONU=1_1_1_beta00514003
VERSIOND=1.1.1_beta00514003
release:
	rm -fr gpsbabel-$(VERSIOND)
	cvs tag gpsbabel_$(VERSIONU)
	cvs export -r gpsbabel_$(VERSIONU) -d gpsbabel-$(VERSIOND) gpsbabel
	tar cvzf /tmp/gpsbabel-$(VERSIOND).tar.gz gpsbabel-$(VERSIOND)
	cd /tmp ; tar xvzf gpsbabel-$(VERSIOND).tar.gz
	touch /tmp/gpsbabel-$(VERSIOND)/internal_styles.c
	cd /tmp/gpsbabel-$(VERSIOND)/mingw ; make  && zip -j gpsbabel-$(VERSIOND).zip  gpsbabel.exe libexpat.dll ../win32/gpsbabelfront.exe && cp gpsbabel-$(VERSIOND).zip /tmp
	curl -u anonymous:anonymous --upload-file /tmp/gpsbabel-$(VERSIOND).tar.gz ftp://upload.sf.net/incoming/
	curl -u anonymous:anonymous --upload-file /tmp/gpsbabel-$(VERSIOND).zip ftp://upload.sf.net/incoming/

# Machine generated from here down.   

cetus.o: cetus.c defs.h queue.h coldsync/palm.h coldsync/pdb.h
copilot.o: copilot.c defs.h queue.h coldsync/palm.h coldsync/pdb.h
csv_util.o: csv_util.c defs.h queue.h csv_util.h
duplicate.o: duplicate.c defs.h queue.h
filter_vecs.o: filter_vecs.c defs.h queue.h
garmin.o: garmin.c defs.h queue.h jeeps/gps.h jeeps/gpsport.h \
  jeeps/gpsserial.h jeeps/gpssend.h jeeps/gpsread.h jeeps/gpsutil.h \
  jeeps/gpsapp.h jeeps/gpsprot.h jeeps/gpscom.h jeeps/gpsfmt.h \
  jeeps/gpsmath.h jeeps/gpsnmea.h jeeps/gpsmem.h jeeps/gpsrqst.h \
  jeeps/gpsinput.h jeeps/gpsproj.h jeeps/gpsnmeafmt.h jeeps/gpsnmeaget.h
gcdb.o: gcdb.c defs.h queue.h coldsync/palm.h coldsync/pdb.h
geo.o: geo.c defs.h queue.h
gpspilot.o: gpspilot.c defs.h queue.h coldsync/palm.h coldsync/pdb.h
gpsutil.o: gpsutil.c defs.h queue.h magellan.h
gpx.o: gpx.c defs.h queue.h
holux.o: holux.c defs.h queue.h holux.h
internal_styles.o: internal_styles.c defs.h queue.h
magnav.o: magnav.c defs.h queue.h coldsync/palm.h coldsync/pdb.h
magproto.o: magproto.c defs.h queue.h magellan.h
main.o: main.c defs.h queue.h
mapsend.o: mapsend.c defs.h queue.h mapsend.h magellan.h
mapsource.o: mapsource.c defs.h queue.h
mkshort.o: mkshort.c defs.h queue.h
pcx.o: pcx.c defs.h queue.h
position.o: position.c defs.h queue.h
psp.o: psp.c defs.h queue.h
queue.o: queue.c queue.h
route.o: route.c defs.h queue.h
tiger.o: tiger.c defs.h queue.h magellan.h
tmpro.o: tmpro.c defs.h queue.h csv_util.h
tpg.o: tpg.c defs.h queue.h jeeps/gpsmath.h jeeps/gps.h jeeps/gpsport.h \
  jeeps/gpsserial.h jeeps/gpssend.h jeeps/gpsread.h jeeps/gpsutil.h \
  jeeps/gpsapp.h jeeps/gpsprot.h jeeps/gpscom.h jeeps/gpsfmt.h \
  jeeps/gpsnmea.h jeeps/gpsmem.h jeeps/gpsrqst.h jeeps/gpsinput.h \
  jeeps/gpsproj.h jeeps/gpsnmeafmt.h jeeps/gpsnmeaget.h
util.o: util.c defs.h queue.h
vecs.o: vecs.c defs.h queue.h csv_util.h
waypt.o: waypt.c defs.h queue.h
xcsv.o: xcsv.c defs.h queue.h csv_util.h
coldsync/pdb.o: coldsync/pdb.c coldsync/config.h coldsync/palm.h \
  coldsync/pdb.h
coldsync/util.o: coldsync/util.c coldsync/config.h coldsync/pconn/util.h \
  coldsync/palm.h
jeeps/gpsapp.o: jeeps/gpsapp.c jeeps/gps.h jeeps/gpsport.h \
  jeeps/gpsserial.h jeeps/gpssend.h jeeps/gpsread.h jeeps/gpsutil.h \
  jeeps/gpsapp.h jeeps/gpsprot.h jeeps/gpscom.h jeeps/gpsfmt.h \
  jeeps/gpsmath.h jeeps/gpsnmea.h jeeps/gpsmem.h jeeps/gpsrqst.h \
  jeeps/gpsinput.h jeeps/gpsproj.h jeeps/gpsnmeafmt.h jeeps/gpsnmeaget.h
jeeps/gpscom.o: jeeps/gpscom.c jeeps/gps.h jeeps/gpsport.h \
  jeeps/gpsserial.h jeeps/gpssend.h jeeps/gpsread.h jeeps/gpsutil.h \
  jeeps/gpsapp.h jeeps/gpsprot.h jeeps/gpscom.h jeeps/gpsfmt.h \
  jeeps/gpsmath.h jeeps/gpsnmea.h jeeps/gpsmem.h jeeps/gpsrqst.h \
  jeeps/gpsinput.h jeeps/gpsproj.h jeeps/gpsnmeafmt.h jeeps/gpsnmeaget.h
jeeps/gpsmath.o: jeeps/gpsmath.c jeeps/gps.h jeeps/gpsport.h \
  jeeps/gpsserial.h jeeps/gpssend.h jeeps/gpsread.h jeeps/gpsutil.h \
  jeeps/gpsapp.h jeeps/gpsprot.h jeeps/gpscom.h jeeps/gpsfmt.h \
  jeeps/gpsmath.h jeeps/gpsnmea.h jeeps/gpsmem.h jeeps/gpsrqst.h \
  jeeps/gpsinput.h jeeps/gpsproj.h jeeps/gpsnmeafmt.h jeeps/gpsnmeaget.h \
  jeeps/gpsdatum.h
jeeps/gpsmem.o: jeeps/gpsmem.c jeeps/gps.h jeeps/gpsport.h \
  jeeps/gpsserial.h jeeps/gpssend.h jeeps/gpsread.h jeeps/gpsutil.h \
  jeeps/gpsapp.h jeeps/gpsprot.h jeeps/gpscom.h jeeps/gpsfmt.h \
  jeeps/gpsmath.h jeeps/gpsnmea.h jeeps/gpsmem.h jeeps/gpsrqst.h \
  jeeps/gpsinput.h jeeps/gpsproj.h jeeps/gpsnmeafmt.h jeeps/gpsnmeaget.h
jeeps/gpsprot.o: jeeps/gpsprot.c jeeps/gps.h jeeps/gpsport.h \
  jeeps/gpsserial.h jeeps/gpssend.h jeeps/gpsread.h jeeps/gpsutil.h \
  jeeps/gpsapp.h jeeps/gpsprot.h jeeps/gpscom.h jeeps/gpsfmt.h \
  jeeps/gpsmath.h jeeps/gpsnmea.h jeeps/gpsmem.h jeeps/gpsrqst.h \
  jeeps/gpsinput.h jeeps/gpsproj.h jeeps/gpsnmeafmt.h jeeps/gpsnmeaget.h
jeeps/gpsread.o: jeeps/gpsread.c jeeps/gps.h jeeps/gpsport.h \
  jeeps/gpsserial.h jeeps/gpssend.h jeeps/gpsread.h jeeps/gpsutil.h \
  jeeps/gpsapp.h jeeps/gpsprot.h jeeps/gpscom.h jeeps/gpsfmt.h \
  jeeps/gpsmath.h jeeps/gpsnmea.h jeeps/gpsmem.h jeeps/gpsrqst.h \
  jeeps/gpsinput.h jeeps/gpsproj.h jeeps/gpsnmeafmt.h jeeps/gpsnmeaget.h
jeeps/gpsrqst.o: jeeps/gpsrqst.c jeeps/gps.h jeeps/gpsport.h \
  jeeps/gpsserial.h jeeps/gpssend.h jeeps/gpsread.h jeeps/gpsutil.h \
  jeeps/gpsapp.h jeeps/gpsprot.h jeeps/gpscom.h jeeps/gpsfmt.h \
  jeeps/gpsmath.h jeeps/gpsnmea.h jeeps/gpsmem.h jeeps/gpsrqst.h \
  jeeps/gpsinput.h jeeps/gpsproj.h jeeps/gpsnmeafmt.h jeeps/gpsnmeaget.h
jeeps/gpssend.o: jeeps/gpssend.c jeeps/gps.h jeeps/gpsport.h \
  jeeps/gpsserial.h jeeps/gpssend.h jeeps/gpsread.h jeeps/gpsutil.h \
  jeeps/gpsapp.h jeeps/gpsprot.h jeeps/gpscom.h jeeps/gpsfmt.h \
  jeeps/gpsmath.h jeeps/gpsnmea.h jeeps/gpsmem.h jeeps/gpsrqst.h \
  jeeps/gpsinput.h jeeps/gpsproj.h jeeps/gpsnmeafmt.h jeeps/gpsnmeaget.h
jeeps/gpsserial.o: jeeps/gpsserial.c jeeps/gps.h jeeps/gpsport.h \
  jeeps/gpsserial.h jeeps/gpssend.h jeeps/gpsread.h jeeps/gpsutil.h \
  jeeps/gpsapp.h jeeps/gpsprot.h jeeps/gpscom.h jeeps/gpsfmt.h \
  jeeps/gpsmath.h jeeps/gpsnmea.h jeeps/gpsmem.h jeeps/gpsrqst.h \
  jeeps/gpsinput.h jeeps/gpsproj.h jeeps/gpsnmeafmt.h jeeps/gpsnmeaget.h
jeeps/gpsutil.o: jeeps/gpsutil.c jeeps/gps.h jeeps/gpsport.h \
  jeeps/gpsserial.h jeeps/gpssend.h jeeps/gpsread.h jeeps/gpsutil.h \
  jeeps/gpsapp.h jeeps/gpsprot.h jeeps/gpscom.h jeeps/gpsfmt.h \
  jeeps/gpsmath.h jeeps/gpsnmea.h jeeps/gpsmem.h jeeps/gpsrqst.h \
  jeeps/gpsinput.h jeeps/gpsproj.h jeeps/gpsnmeafmt.h jeeps/gpsnmeaget.h
internal_styles.c: mkstyle.sh style/README.style style/csv.style style/custom.style style/dna.style style/fugawi.style style/gpsdrive.style style/gpsman.style style/mxf.style style/nima.style style/ozi.style style/s_and_t.style style/xmap.style style/xmapwpt.style
	./mkstyle.sh > $@ || (rm -f $@ ; exit 1)
