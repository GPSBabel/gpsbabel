# $Id: app.pro,v 1.19 2010-11-01 03:30:42 robertl Exp $
#

# set VERSION related variables and generate gbversion.h
include(../gbversion.pri)

GB.setupfile.input = setup.iss.qmake.in
GB.setupfile.output = setup.iss
QMAKE_SUBSTITUTES += GB.setupfile

VERSION = $$GB.VERSION

#CONFIG += qt causes link failure on msvc.  Qt6EntryPoint.lib not added to libs.
CONFIG(debug, debug|release) {
  CONFIG += console
}
CONFIG += c++17

ICON = images/appicon.icns

QT += core \
      gui \
      network \
      serialport \
      widgets \
      xml

disable-mappreview {
  DEFINES += DISABLE_MAPPREVIEW
} else {
  QT += webenginewidgets webchannel
}

unix:DESTDIR = objects
unix:MOC_DIR = objects
unix:OBJECTS_DIR = objects
unix:RCC_DIR = objects
mac:DESTDIR = .

UI_DIR = tmp

RESOURCES = app.qrc
RC_FILE = app.rc

win32 {
  TARGET=GPSBabelFE
}
win32-g++ {
  QMAKE_LFLAGS_RELEASE += -static-libgcc
}
unix:TARGET=gpsbabelfe
mac:TARGET=GPSBabelFE

# Set QMAKE_TARGET_BUNDLE_PREFIX so we get the correct CFBundleIdentifier in Info.plist
darwin:QMAKE_TARGET_BUNDLE_PREFIX=org.gpsbabel

# FORMS
FORMS += aboutui.ui
FORMS += advui.ui
FORMS += donate.ui
FORMS += filterui.ui
!disable-mappreview {
  FORMS += gmapui.ui
}
FORMS += mainwinui.ui
FORMS += miscfltui.ui
FORMS += preferences.ui
FORMS += rttrkui.ui
FORMS += trackui.ui
FORMS += upgrade.ui
FORMS += version_mismatch.ui
FORMS += wayptsui.ui

# SOURCES
SOURCES += aboutdlg.cc
SOURCES += advdlg.cc
SOURCES += donate.cc
SOURCES += dpencode.cc
SOURCES += filterdata.cc
SOURCES += filterdlg.cc
SOURCES += filterwidgets.cc
SOURCES += format.cc
SOURCES += formatload.cc
!disable-mappreview{
  SOURCES += gmapdlg.cc
  SOURCES += gpx.cc
}
SOURCES += help.cc
SOURCES += latlng.cc
SOURCES += main.cc
SOURCES += mainwindow.cc
!disable-mappreview{
  SOURCES += map.cc
}
SOURCES += optionsdlg.cc
SOURCES += preferences.cc
SOURCES += processwait.cc
SOURCES += runmachine.cc
SOURCES += upgrade.cc
SOURCES += version_mismatch.cc
unix {
  SOURCES += serial_unix.cc
} else:windows {
  SOURCES += serial_win.cc
}

# HEADERS
HEADERS += aboutdlg.h
HEADERS += advdlg.h
HEADERS += appname.h
HEADERS += babeldata.h
HEADERS += donate.h
HEADERS += filterdata.h
HEADERS += filterdlg.h
HEADERS += filterwidgets.h
HEADERS += format.h
HEADERS += formatload.h
!disable-mappreview {
  HEADERS += gmapdlg.h
  HEADERS += gpx.h
}
HEADERS += help.h
HEADERS += mainwindow.h
!disable-mappreview {
  HEADERS += map.h
}
HEADERS += optionsdlg.h
HEADERS += preferences.h
HEADERS += processwait.h
HEADERS += runmachine.h
HEADERS += setting.h
HEADERS += upgrade.h
HEADERS += version_mismatch.h

TRANSLATIONS += gpsbabelfe_ru.ts
TRANSLATIONS += gpsbabelfe_de.ts
TRANSLATIONS += gpsbabelfe_es.ts
TRANSLATIONS += gpsbabelfe_fr.ts
TRANSLATIONS += gpsbabelfe_hu.ts
TRANSLATIONS += gpsbabelfe_it.ts

unix:!mac {
  !defined(EMBED_TRANSLATIONS, var):EMBED_TRANSLATIONS = on
  !defined(EMBED_MAP, var):EMBED_MAP = on
}
equals(EMBED_TRANSLATIONS, on) {
  RESOURCES += translations.qrc
}
equals(EMBED_MAP, on) {
  RESOURCES += map.qrc
}

macx|linux{
  package.commands = QMAKE=$(QMAKE) ./package_app
  package.depends = $(TARGET)
  QMAKE_EXTRA_TARGETS += package
}
linux: QMAKE_DISTCLEAN += -r GPSBabelFE

# build the compilation data base used by clang tools including clang-tidy,
# as well as CLion.
macx|linux{
  compile_command_database.target = compile_commands.json
  compile_command_database.commands = make clean; bear make
  QMAKE_EXTRA_TARGETS += compile_command_database
  QMAKE_DISTCLEAN += compile_commands.json
}
