CFLAGS=-g -Icoldsync

FMTS=magproto.o gpx.o geo.o gpsman.o mapsend.o mapsource.o \
	gpsutil.o tiger.o pcx.o csv.o cetus.o gpspilot.o magnav.o \
	psp.o mxf.o holux.o garmin.o ozi.o tmpro.o dna.o tpg.o gpsdrive.o \
	xcsv.o xmapwpt.o

JEEPS=jeeps/gpsapp.o jeeps/gpscom.o jeeps/gpsfmt.o jeeps/gpsinput.o \
	jeeps/gpsmath.o jeeps/gpsmem.o  \
	jeeps/gpsproj.o jeeps/gpsprot.o jeeps/gpsread.o \
	jeeps/gpsrqst.o jeeps/gpssend.o jeeps/gpsserial.o jeeps/gpsutil.o

COLDSYNC=coldsync/util.o coldsync/pdb.o

OBJS=main.o queue.o route.o waypt.o util.o vecs.o mkshort.o csv_util.o \
	$(COLDSYNC) $(GARMIN) $(JEEPS) $(FMTS)

all: gpsbabel

gpsbabel: $(OBJS)
	$(CC) $(CFLAGS) $(OBJS) -o gpsbabel -lexpat -lm

main.o:
	$(CC) -c $(CFLAGS) -DVERSION=\"$(VERSIOND)\" $<

clean:
	rm -f $(OBJS) gpsbabel gpsbabel.exe

# Nerdy release stuff that needs to work only on Linux.

dep:
	make clean && make CC="gcc -MMD"  && cat *.d */*.d > /tmp/dep
	echo Edit Makefile and bring in /tmp/dep

VERSIONU=1_1_0a
VERSIOND=1.1.0a
release:
	rm -fr gpsbabel-$(VERSIOND)
	cvs tag gpsbabel_$(VERSIONU)
	cvs export -r gpsbabel_$(VERSIONU) -d gpsbabel-$(VERSIOND) gpsbabel
	tar cvzf /tmp/gpsbabel-$(VERSIOND).tar.gz gpsbabel-$(VERSIOND)
	cd /tmp ; tar xvzf gpsbabel-$(VERSIOND).tar.gz
	cd /tmp/gpsbabel-$(VERSIOND)/mingw ; make  && zip -j gpsbabel-$(VERSIOND).zip  gpsbabel.exe libexpat.dll ../win32/gpsbabelfront.exe && cp gpsbabel-$(VERSIOND).zip /tmp
	ncftpput -u anonymous upload.sf.net  /incoming /tmp/gpsbabel-$(VERSIOND).tar.gz /tmp/gpsbabel-$(VERSIOND).zip

cetus.o: cetus.c defs.h queue.h coldsync/palm.h coldsync/pdb.h
csv.o: csv.c defs.h queue.h csv_util.h
csv_util.o: csv_util.c defs.h queue.h csv_util.h
dna.o: dna.c defs.h queue.h csv_util.h
garmin.o: garmin.c defs.h queue.h jeeps/gps.h jeeps/gpsport.h \
  jeeps/gpsserial.h jeeps/gpssend.h jeeps/gpsread.h jeeps/gpsutil.h \
  jeeps/gpsapp.h jeeps/gpsprot.h jeeps/gpscom.h jeeps/gpsfmt.h \
  jeeps/gpsmath.h jeeps/gpsnmea.h jeeps/gpsmem.h jeeps/gpsrqst.h \
  jeeps/gpsinput.h jeeps/gpsproj.h jeeps/gpsnmeafmt.h jeeps/gpsnmeaget.h
geo.o: geo.c defs.h queue.h
gpsdrive.o: gpsdrive.c defs.h queue.h csv_util.h
gpsman.o: gpsman.c defs.h queue.h
gpspilot.o: gpspilot.c defs.h queue.h coldsync/palm.h coldsync/pdb.h
gpsutil.o: gpsutil.c defs.h queue.h magellan.h
gpx.o: gpx.c defs.h queue.h
holux.o: holux.c defs.h queue.h holux.h
magnav.o: magnav.c defs.h queue.h coldsync/palm.h coldsync/pdb.h
magproto.o: magproto.c defs.h queue.h magellan.h
main.o: main.c defs.h queue.h
mapsend.o: mapsend.c defs.h queue.h mapsend.h
mapsource.o: mapsource.c defs.h queue.h
mkshort.o: mkshort.c defs.h queue.h
xmapwpt.o: mxf.c defs.h queue.h csv_util.h
xcsv.o: xcsv.c defs.h queue.h csv_util.h
mxf.o: mxf.c defs.h queue.h csv_util.h
ozi.o: ozi.c defs.h queue.h csv_util.h
pcx.o: pcx.c defs.h queue.h
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
vecs.o: vecs.c defs.h queue.h
waypt.o: waypt.c defs.h queue.h
coldsync/pdb.o: coldsync/pdb.c coldsync/config.h coldsync/pdb.h
coldsync/util.o: coldsync/util.c coldsync/config.h
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
jeeps/gpsfmt.o: jeeps/gpsfmt.c jeeps/gps.h jeeps/gpsport.h \
  jeeps/gpsserial.h jeeps/gpssend.h jeeps/gpsread.h jeeps/gpsutil.h \
  jeeps/gpsapp.h jeeps/gpsprot.h jeeps/gpscom.h jeeps/gpsfmt.h \
  jeeps/gpsmath.h jeeps/gpsnmea.h jeeps/gpsmem.h jeeps/gpsrqst.h \
  jeeps/gpsinput.h jeeps/gpsproj.h jeeps/gpsnmeafmt.h jeeps/gpsnmeaget.h
jeeps/gpsinput.o: jeeps/gpsinput.c jeeps/gps.h jeeps/gpsport.h \
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
jeeps/gpsproj.o: jeeps/gpsproj.c jeeps/gps.h jeeps/gpsport.h \
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
