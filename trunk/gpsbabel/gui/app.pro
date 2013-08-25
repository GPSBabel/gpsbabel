# $Id: app.pro,v 1.19 2010-11-01 03:30:42 robertl Exp $
#

CONFIG += qt release 
#CONFIG += qt debug console

# For Mac, x86 and x64, but not PPC binary.   Ignored on other OSes.
# macx:CONFIG -= x86_64 
# macx:CONFIG += x86
macx:DEFINES += HAVE_CONFIG_H

ICON = images/appicon.icns

QT += network \
    xml \
    webkit \

unix:DESTDIR = objects
unix:MOC_DIR = objects
unix:OBJECTS_DIR = objects
unix:RCC_DIR = objects

mac:LIBS += -framework IOKit -framework CoreFoundation

UI_DIR = tmp

RESOURCES = app.qrc 
RC_FILE = app.rc

win32 { 
  TARGET=GPSBabelFE
  QMAKE_LFLAGS_RELEASE += -static-libgcc
}
unix:TARGET=gpsbabelfe-bin
mac:TARGET=GPSBabelFE

FORMS += aboutui.ui
FORMS += advui.ui
FORMS += donate.ui
FORMS += filterui.ui
FORMS += gmapui.ui
FORMS += mainwinui.ui
FORMS += miscfltui.ui
FORMS += preferences.ui
FORMS += rttrkui.ui
FORMS += trackui.ui
FORMS += upgrade.ui
FORMS += version_mismatch.ui
FORMS += wayptsui.ui

SOURCES += aboutdlg.cc
SOURCES += advdlg.cc
SOURCES += donate.cc
SOURCES += dpencode.cc
SOURCES += filterdata.cc
SOURCES += filterdlg.cc
SOURCES += filterwidgets.cc
SOURCES += format.cc
SOURCES += formatload.cc
SOURCES += gmapdlg.cc
SOURCES += gpx.cc
SOURCES += help.cc
SOURCES += latlng.cc
SOURCES += main.cc
SOURCES += mainwindow.cc
SOURCES += map.cc
SOURCES += optionsdlg.cc
SOURCES += preferences.cc
SOURCES += processwait.cc
SOURCES += upgrade.cc
SOURCES += version_mismatch.cc
macx:SOURCES += serial_mac.cc
unix:SOURCES += serial_unix.cc
windows:SOURCES += serial_win.cc

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
HEADERS += gmapdlg.h
HEADERS += gpx.h
HEADERS += help.h
HEADERS += mainwindow.h
HEADERS += map.h
HEADERS += optionsdlg.h
HEADERS += preferences.h
HEADERS += processwait.h
HEADERS += setting.h
HEADERS += upgrade.h
HEADERS += version_mismatch.h

TRANSLATIONS += gpsbabelfe_ru.ts
TRANSLATIONS += gpsbabelfe_de.ts
TRANSLATIONS += gpsbabelfe_es.ts
TRANSLATIONS += gpsbabelfe_fr.ts
TRANSLATIONS += gpsbabelfe_hu.ts
TRANSLATIONS += gpsbabelfe_it.ts
TRANSLATIONS += gpsbabelfe.ts
TRANSLATIONS += gpsbabel.ts




