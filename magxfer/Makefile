VERSIONU=1_1_0
VERSIOND=1.1.0

all: magxfer

magxfer: magxfer.c
	$(CC) -o $@ $(CFLAGS) -DVERSION=\"$(VERSIOND)\" $< 

clean:
	rm -f magxfer
release:
	cvs tag -F magxfer_$(VERSIONU)
	cvs export -r magxfer_$(VERSIONU) -d magxfer-$(VERSIOND) magxfer
	tar cvzf /tmp/magxfer-$(VERSIOND).tar.gz magxfer-$(VERSIOND)
	curl -u anonymous:anonymous --upload-file /tmp/magxfer-$(VERSIOND).tar.gz ftp://upload.sf.net/incoming/
