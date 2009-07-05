# -*- Makefile -*-
# $Id: makeextras.mak,v 1.1 2009-07-05 21:14:56 robertl Exp $

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


all: $(EXTRAS) $(XLATE) $(EXTERNS) 

objects/translations:
	mkdir -p objects/translations

$(XLATE): objects/translations

objects/showUrl.sh: showUrl.sh
	$(COPY) $< $@

objects/qt.conf: qt.conf
	$(COPY) $< $@
objects/gpsbabelfe: gpsbabelfe
	$(COPY) $< $@

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
	(cd externs; tar cf - .) | (cd objects; tar xvf -)

%.qm: %.ts
	lrelease $< -qm $@

clean: 
	rm -f $(EXTRAS)

