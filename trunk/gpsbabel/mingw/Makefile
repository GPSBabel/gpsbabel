CC=/home/robertl/cross-tools/bin/i386-mingw32msvc-gcc -Iinclude -I../coldsync -DCETUS
VPATH=..

gpsbabel.exe:

include ../Makefile

gpsbabel.exe: $(OBJS)
	$(CC) -static $(CFLAGS) $(OBJS) lib/libexpat.a -o gpsbabel.exe 
	cp gpsbabel.exe /tmp
