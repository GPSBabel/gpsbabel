all: magxfer

magxfer: magxfer.c

clean:
	rm -f magxfer

VERSIONU=1_0_0
VERSIOND=1.0.0
release:
	cvs tag magxfer_$(VERSIONU)
	cvs export -r magxfer_$(VERSIONU) -d magxfer-$(VERSIOND) magxfer
	tar cvzf /tmp/magxfer-$(VERSIOND).tar.gz magxfer-$(VERSIOND)
	curl -u anonymous:anonymous --upload-file /tmp/magxfer-$(VERSIOND).tar.gz ftp://upload.sf.net/incoming/
