CC=/home/robertl/cross-tools/bin/i386-mingw32msvc-gcc
VPATH=..

FILES=gpsbabel.exe libexpat.dll ../win32/gpsbabelfront.exe ../README* ../COPYING

gpsbabel.exe:  wintesto.cmd

include ../Makefile
CFLAGS=-Iinclude -I../coldsync -O

gpsbabel.exe: $(OBJS)
	$(CC) -static $(CFLAGS) $(OBJS) lib/libexpat.a -o gpsbabel.exe 
	zip -j /tmp/gpsbabel-$(VERSIOND).zip $(FILES)
	cp gpsbabel.exe /tmp

mkwintesto: mkwintesto.c
	/usr/bin/cc mkwintesto.c ../util.c -o mkwintesto

wintesto.cmd: mkwintesto
	./mkwintesto ../testo
