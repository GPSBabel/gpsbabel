# $Id: app.pro,v 1.9 2009-09-06 17:04:36 robertl Exp $
#

CONFIG += qt release 
#CONFIG += qt debug console

# For Mac, build Universal binary.   Ignored on other OSes.
CONFIG += x86 ppc

ICON = images/appicon.icns

QT += network \
    xml \
    webkit

unix:DESTDIR = objects
unix:MOC_DIR = objects
unix:OBJECTS_DIR = objects
unix:RCC_DIR = objects

mac:LIBS += -framework IOKit

UI_DIR = tmp

RESOURCES = app.qrc 
RC_FILE = app.rc

TARGET=GPSBabelFE

extras.commands = (make -f makeextras.mak)
extras.target = extras
QMAKE_EXTRA_TARGETS += extras
#POST_TARGETDEPS=extras

FORMS += mainwinui.ui
FORMS += advui.ui
FORMS += aboutui.ui
FORMS += trackui.ui
FORMS += filterui.ui
FORMS += wayptsui.ui
FORMS += rttrkui.ui
FORMS += miscfltui.ui
FORMS += gmapui.ui
FORMS += upgrade.ui

SOURCES += advdlg.cpp
SOURCES += dpencode.cpp
SOURCES += map.cpp
SOURCES += latlng.cpp
SOURCES += gpx.cpp
SOURCES += gmapdlg.cpp
SOURCES += aboutdlg.cpp
SOURCES += main.cpp
SOURCES += help.cpp
SOURCES += mainwindow.cpp
SOURCES += format.cpp
SOURCES += filterdata.cpp
SOURCES += formatload.cpp
SOURCES += optionsdlg.cpp
SOURCES += processwait.cpp
SOURCES += filterwidgets.cpp
SOURCES += filterdlg.cpp
SOURCES += upgrade.cpp
macx:SOURCES += serial_mac.cpp
unix:SOURCES += serial_unix.cpp
windows:SOURCES += serial_win.cpp

HEADERS += mainwindow.h
HEADERS += map.h
HEADERS += gmapdlg.h
HEADERS += gpx.h
HEADERS += babeldata.h
HEADERS += filterdlg.h
HEADERS += appname.h
HEADERS += advdlg.h
HEADERS += aboutdlg.h
HEADERS += help.h
HEADERS += format.h
HEADERS += formatload.h
HEADERS += optionsdlg.h
HEADERS += processwait.h
HEADERS += filterwidgets.h
HEADERS += filterdata.h
HEADERS += setting.h
HEADERS += upgrade.h

TRANSLATIONS += gpsbabelfe_de.ts
TRANSLATIONS += gpsbabelfe_es.ts
TRANSLATIONS += gpsbabelfe_fr.ts
TRANSLATIONS += gpsbabelfe_hu.ts
TRANSLATIONS += gpsbabelfe_it.ts




