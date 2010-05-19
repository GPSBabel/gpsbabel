# $Id: app.pro,v 1.16 2010-05-19 11:41:02 robertl Exp $
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

mac:LIBS += -framework IOKit -framework CoreFoundation

UI_DIR = tmp

RESOURCES = app.qrc 
RC_FILE = app.rc

mac:TARGET=GPSBabelFE
win32:TARGET=GPSBabelFE
unix:TARGET=gpsbabelfe-bin

FORMS += aboutui.ui
FORMS += advui.ui
FORMS += filterui.ui
FORMS += gmapui.ui
FORMS += mainwinui.ui
FORMS += miscfltui.ui
FORMS += preferences.ui
FORMS += rttrkui.ui
FORMS += trackui.ui
FORMS += upgrade.ui
FORMS += wayptsui.ui

SOURCES += aboutdlg.cpp
SOURCES += advdlg.cpp
SOURCES += dpencode.cpp
SOURCES += filterdata.cpp
SOURCES += filterdlg.cpp
SOURCES += filterwidgets.cpp
SOURCES += format.cpp
SOURCES += formatload.cpp
SOURCES += gmapdlg.cpp
SOURCES += gpx.cpp
SOURCES += help.cpp
SOURCES += latlng.cpp
SOURCES += main.cpp
SOURCES += mainwindow.cpp
SOURCES += map.cpp
SOURCES += optionsdlg.cpp
SOURCES += preferences.cpp
SOURCES += processwait.cpp
SOURCES += upgrade.cpp
macx:SOURCES += serial_mac.cpp
unix:SOURCES += serial_unix.cpp
windows:SOURCES += serial_win.cpp

HEADERS += aboutdlg.h
HEADERS += advdlg.h
HEADERS += appname.h
HEADERS += babeldata.h
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

TRANSLATIONS += gpsbabelfe_de.ts
TRANSLATIONS += gpsbabelfe_es.ts
TRANSLATIONS += gpsbabelfe_fr.ts
TRANSLATIONS += gpsbabelfe_hu.ts
TRANSLATIONS += gpsbabelfe_it.ts
TRANSLATIONS += gpsbabelfe.ts
TRANSLATIONS += gpsbabel.ts




