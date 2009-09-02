# -*- Makefile -*-
# $Id: makeextras.mak,v 1.2 2009-09-02 19:05:27 robertl Exp $
#
# Some make targets and scripts that I find hard to do in the
# Qt qmake system.
#


COPY=cp

XLATE=\
objects/translations/gpsbabelfe_de.ts \
objects/translations/gpsbabelfe_fr.ts \
objects/translations/gpsbabelfe_es.ts \
objects/translations/gpsbabelfe_hu.ts \
objects/translations/gpsbabelfe_it.ts \
objects/translations/gpsbabelfe_de.qm \
objects/translations/gpsbabelfe_fr.qm \
objects/translations/gpsbabelfe_es.qm \
objects/translations/gpsbabelfe_hu.qm \
objects/translations/gpsbabelfe_it.qm \
objects/translations/gpsbabel_de.ts \
objects/translations/gpsbabel_fr.ts \
objects/translations/gpsbabel_es.ts \
objects/translations/gpsbabel_hu.ts \
objects/translations/gpsbabel_it.ts \
objects/translations/gpsbabel_de.qm \
objects/translations/gpsbabel_fr.qm \
objects/translations/gpsbabel_es.qm \
objects/translations/gpsbabel_hu.qm \
objects/translations/gpsbabel_it.qm 

EXTRAS=\
objects/showUrl.sh \
objects/qt.conf \
objects/gpsbabelfe \

EXTERNS=\
objects/gpsbabel \
objects/help/gpsbabel.html \
objects/libQtCore.so.4 \
objects/libQtGui.so.4 \
objects/libQtCore.so.4.4.3 \
objects/libQtGui.so.4.4.3 \


all: $(EXTRAS) $(XLATE) #$(EXTERNS)

objects/translations:
	mkdir -p objects/translations

$(XLATE): objects/translations

objects/showUrl.sh: showUrl.sh
	$(COPY) $< $@

objects/qt.conf: qt.conf
	$(COPY) $< $@
objects/gpsbabelfe: gpsbabelfe
	$(COPY) $< $@

babelts: 
	./babelstrings.pl >foo.h && \
	lupdate foo.h -ts gpsbabel_de.ts -ts gpsbabel_fr.ts -ts gpsbabel-es.ts -ts gpsbabel_hu.ts \
		-ts gpsbabel_it.ts

babelfets:
	lupdate app.pro

objects/translations/gpsbabel_de.ts: gpsbabel_de.ts
	$(COPY) $< $@
objects/translations/gpsbabel_fr.ts: gpsbabel_fr.ts
	$(COPY) $< $@
objects/translations/gpsbabel_es.ts: gpsbabel_es.ts
	$(COPY) $< $@
objects/translations/gpsbabel_hu.ts: gpsbabel_hu.ts
	$(COPY) $< $@
objects/translations/gpsbabel_it.ts: gpsbabel_it.ts
	$(COPY) $< $@
objects/translations/gpsbabelfe_de.ts: gpsbabelfe_de.ts
	$(COPY) $< $@
objects/translations/gpsbabelfe_fr.ts: gpsbabelfe_fr.ts
	$(COPY) $< $@
objects/translations/gpsbabelfe_es.ts: gpsbabelfe_es.ts
	$(COPY) $< $@
objects/translations/gpsbabelfe_hu.ts: gpsbabelfe_hu.ts
	$(COPY) $< $@
objects/translations/gpsbabelfe_it.ts: gpsbabelfe_it.ts
	$(COPY) $< $@

$(EXTERNS):
	(cd externs && tar cf - .) | (cd objects && tar xvf -)

%.qm: %.ts
	lrelease $< -qm $@

clean: 
	rm -f $(EXTRAS)

