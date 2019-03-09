#
# This project extracts the strings from the command line tool
# that the GUI translates and updates the translation files for
# these strings.
#
CONFIG += console

QT -= gui
QT += core \
      widgets

TEMPLATE = app

DESTDIR=objects
OBJECTS_DIR=objects
# trick qmake into making objects directory by listing it as MOC_DIR.
MOC_DIR=objects

DEFINES += GENERATE_CORE_STRINGS

INCLUDEPATH += ..
SOURCES += ../formatload.cc
SOURCES += coretool.cc

HEADERS += ../format.h
HEADERS += ../formatload.h

core_strings.target = core_strings.h
core_strings.depends = $(TARGET)
core_strings.depends += ../../gpsbabel
core_strings.commands = $(COPY_FILE) ../../gpsbabel $(DESTDIR)gpsbabel &&
core_strings.commands += ./$(TARGET) 2>core_strings.h;
QMAKE_EXTRA_TARGETS += core_strings
QMAKE_DISTCLEAN += $(DESTDIR)gpsbabel

qtPrepareTool(LUPDATE, lupdate)
update.depends = core_strings.h
update.commands = $$LUPDATE -locations absolute core.pro
QMAKE_EXTRA_TARGETS += update

qtPrepareTool(LRELEASE, lrelease)
release.depends = update
release.commands = $$LRELEASE core.pro
QMAKE_EXTRA_TARGETS += release

