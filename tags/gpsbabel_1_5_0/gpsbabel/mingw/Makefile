CC=/usr/local/cross-tools/bin/i386-mingw32msvc-gcc
VPATH=..:../shapelib

FILES=gpsbabel.exe ../win32/GPSBabelGUI.exe ../win32/gui-2/README.gui \
  ../README* ../style/README.style ../COPYING ../readme.html

include ../Makefile
CFLAGS=-Iinclude -I../coldsync -O $(INHIBIT_USB) $(EXTRA_CFLAGS)
#
# Must define empty (don't comment out the whole line) if you want to 
# override INHIBIT_USB from the parent Makefile.
#
INHIBIT_USB=#-DNO_USB
OSJEEPS=jeeps/gpsusbwin.o

gpsbabel.exe: $(OBJS)
	$(CC) -static $(CFLAGS) $(OBJS) -o gpsbabel.exe  -lsetupapi
	zip -j /tmp/gpsbabel-$(VERSIOND).zip $(FILES)
	cp gpsbabel.exe /tmp

# The "usbfree" target is useful for generating an executable that 
# works on NT 3.5 or 4.0 since it doesn't have USB support and tanks
# on our libsetupapi references.

usbfree:
	$(MAKE) INHIBIT_USB=-DNO_USB gpsbabel-usbfree.exe

gpsbabel-usbfree.exe: $(OBJS)
	$(CC) -static $(CFLAGS) $(OBJS) -o gpsbabel-usbfree.exe
	zip -j /tmp/gpsbabel-nousb-$(VERSIOND).zip gpsbabel-usbfree.exe



