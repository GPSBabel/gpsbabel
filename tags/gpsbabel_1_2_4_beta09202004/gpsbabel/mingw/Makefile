CC=/usr/local/cross-tools/bin/i386-mingw32msvc-gcc
VPATH=..:../shapelib

FILES=gpsbabel.exe libexpat.dll ../win32/gpsbabelfront.exe ../README* ../COPYING

gpsbabel.exe:  wintesto.cmd

include ../Makefile
CFLAGS=-Iinclude -I../coldsync -O $(INHIBIT_USB)
INHIBIT_USB=-DNO_USB
OSJEEPS=jeeps/gpsusbwin.o

gpsbabel.exe: $(OBJS)
	$(CC) -static $(CFLAGS) $(OBJS) lib/libexpat.a -o gpsbabel.exe  -lsetupapi
	zip -j /tmp/gpsbabel-$(VERSIOND).zip $(FILES)
	cp gpsbabel.exe /tmp

mkwintesto: mkwintesto.c
	/usr/bin/cc mkwintesto.c -o mkwintesto

wintesto.cmd: mkwintesto
	./mkwintesto ../testo
