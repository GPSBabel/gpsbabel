CFLAGS=-g

FMTS=magproto.o gpx.o geo.o gpsman.o mapsend.o mapsource.o \
	gpsutil.o tiger.o pcx.o csv.o

OBJS=main.o queue.o route.o waypt.o util.o vecs.o $(FMTS)

gpsbabel: $(OBJS)
	$(CC) $(CFLAGS) $(OBJS) -o gpsbabel -lexpat -lm

clean:
	rm -f $(OBJS) gpsbabel

mapsend.o: mapsend.h
magproto.o: magellan.h
