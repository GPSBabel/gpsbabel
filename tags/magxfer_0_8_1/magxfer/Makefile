all: magxfer

magxfer: magxfer.c

clean:
	rm -f magxfer

VERSIONU=0_8_1
VERSIOND=0.8.1
release:
	cvs tag magxfer_$(VERSIONU)
	cvs export -r magxfer_$(VERSIONU) -d magxfer-$(VERSIOND) magxfer
	tar cvzf /tmp/magxfer-$(VERSIOND).tar.gz magxfer-$(VERSIOND)
	curl -u anonymous:anonymous --upload-file /tmp/magxfer-$(VERSIOND).tar.gz ftp://upload.sf.net/incoming
