CFLAGS=-g -Icoldsync

FMTS=magproto.o gpx.o geo.o gpsman.o mapsend.o mapsource.o \
	gpsutil.o tiger.o pcx.o csv.o cetus.o gpspilot.o magnav.o \
	psp.o mxf.o holux.o garmin.o ozi.o

JEEPS=jeeps/gpsapp.o jeeps/gpscom.o jeeps/gpsfmt.o jeeps/gpsinput.o \
	jeeps/gpsmath.o jeeps/gpsmem.o  \
	jeeps/gpsproj.o jeeps/gpsprot.o jeeps/gpsread.o \
	jeeps/gpsrqst.o jeeps/gpssend.o jeeps/gpsserial.o jeeps/gpsutil.o

COLDSYNC=coldsync/util.o coldsync/pdb.o

OBJS=main.o queue.o route.o waypt.o util.o vecs.o mkshort.o csv_util.o \
	$(COLDSYNC) $(GARMIN) $(JEEPS) $(FMTS)

all: gpsbabel

gpsbabel: $(OBJS)
	$(CC) $(CFLAGS) $(OBJS) -o gpsbabel -lexpat -lm #-lpdb

clean:
	rm -f $(OBJS) gpsbabel gpsbabel.exe

cetus.o: cetus.c defs.h queue.h coldsync/palm.h coldsync/pdb.h
csv.o: csv.c defs.h queue.h
geo.o: geo.c defs.h queue.h
gpsman.o: gpsman.c defs.h queue.h
gpspilot.o: gpspilot.c defs.h queue.h coldsync/palm.h coldsync/pdb.h
gpsutil.o: gpsutil.c defs.h queue.h magellan.h
gpx.o: gpx.c defs.h queue.h
magnav.o: magnav.c defs.h queue.h coldsync/palm.h coldsync/pdb.h
magproto.o: magproto.c defs.h queue.h magellan.h
main.o: main.c defs.h queue.h
mapsend.o: mapsend.c defs.h queue.h mapsend.h
mapsource.o: mapsource.c defs.h queue.h
mkshort.o: mkshort.c
pcx.o: pcx.c defs.h queue.h
queue.o: queue.c queue.h
route.o: route.c defs.h queue.h
tiger.o: tiger.c defs.h queue.h magellan.h
util.o: util.c defs.h queue.h
vecs.o: vecs.c defs.h queue.h
waypt.o: waypt.c defs.h queue.h
psp.o: psp.c defs.h queue.h
mxf.o: mxf.c csv_util.c defs.h queue.h csv_util.h
ozi.o: ozi.c csv_util.c defs.h queue.h csv_util.h
csv_util.o: csv_util.c csv_util.h defs.h 
