CFLAGS=-g -Icoldsync

FMTS=magproto.o gpx.o geo.o gpsman.o mapsend.o mapsource.o \
	gpsutil.o tiger.o pcx.o csv.o cetus.o gpspilot.o magnav.o

OBJS=main.o queue.o route.o waypt.o util.o vecs.o coldsync/util.o coldsync/pdb.o $(FMTS)

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
