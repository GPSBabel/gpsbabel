CFLAGS=-g -Icoldsync

FMTS=magproto.o gpx.o geo.o gpsman.o mapsend.o mapsource.o \
	gpsutil.o tiger.o pcx.o csv.o cetus.o gpspilot.o magnav.o

OBJS=main.o queue.o route.o waypt.o util.o vecs.o coldsync/util.o coldsync/pdb.o $(FMTS)

all: gpsbabel

gpsbabel: $(OBJS)
	$(CC) $(CFLAGS) $(OBJS) -o gpsbabel -lexpat -lm #-lpdb

clean:
	rm -f $(OBJS) gpsbabel gpsbabel.exe

mapsend.o: mapsend.h
magproto.o: magellan.h
