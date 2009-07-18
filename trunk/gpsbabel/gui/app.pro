# $Id: app.pro,v 1.2 2009-07-18 06:11:17 robertl Exp $
#

#CONFIG += qt debug console
CONFIG += qt release 

#QT += network \
#    xml \
#    webkit

unix:DESTDIR = objects
unix:MOC_DIR = objects
unix:OBJECTS_DIR = objects
unix:RCC_DIR = objects

win32:LIBS += SHELL32.LIB

UI_DIR = tmp

RESOURCES = app.qrc 
RC_FILE = app.rc

win32:TARGET=gpsbabelfe
unix:TARGET=gpsbabelfe-bin

extras.commands = (make -f makeextras.mak)
extras.target = extras
QMAKE_EXTRA_TARGETS += extras
#POST_TARGETDEPS=extras

FORMS += mainui.ui
FORMS += advui.ui
FORMS += aboutui.ui
FORMS += trackui.ui
FORMS += filterui.ui
FORMS += wayptsui.ui
FORMS += rttrkui.ui
FORMS += miscfltui.ui
FORMS += gmapui.ui

SOURCES += advdlg.cpp
SOURCES += dpencode.cpp
SOURCES += map.cpp
SOURCES += latlng.cpp
SOURCES += gpx.cpp
SOURCES += gmapdlg.cpp
SOURCES += aboutdlg.cpp
SOURCES += main.cpp
SOURCES += help.cpp
SOURCES += maindlg.cpp
SOURCES += persistdlg.cpp
SOURCES += format.cpp
SOURCES += filterdata.cpp
SOURCES += formatload.cpp
SOURCES += optionsdlg.cpp
SOURCES += processwait.cpp
SOURCES += filterwidgets.cpp
SOURCES += filterdlg.cpp


HEADERS += maindlg.h
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
HEADERS += persistdlg.h
HEADERS += processwait.h
HEADERS += filterwidgets.h
HEADERS += filterdata.h
HEADERS += setting.h

TRANSLATIONS += gpsbabelfe_de.ts
TRANSLATIONS += gpsbabelfe_es.ts
TRANSLATIONS += gpsbabelfe_fr.ts
TRANSLATIONS += gpsbabelfe_hu.ts
TRANSLATIONS += gpsbabelfe_it.ts




