CC=/home/robertl/cross-tools/bin/i386-mingw32msvc-gcc
VPATH=..

gpsbabel.exe:

include ../Makefile
CFLAGS=-Iinclude -I../coldsync -O

gpsbabel.exe: $(OBJS)
	$(CC) -static $(CFLAGS) $(OBJS) lib/libexpat.a -o gpsbabel.exe 
	cp gpsbabel.exe /tmp
