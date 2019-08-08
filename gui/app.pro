# $Id: app.pro,v 1.19 2010-11-01 03:30:42 robertl Exp $
#

CONFIG += qt
CONFIG(debug, debug|release) {
  CONFIG += console
}

ICON = images/appicon.icns

QT += core \
      gui \
      network \
      xml

qtHaveModule(webenginewidgets) {
  QT += webenginewidgets webchannel
  DEFINES += HAVE_WEBENGINE
} else {
  QT += webkit webkitwidgets 
}

unix:DESTDIR = objects
unix:MOC_DIR = objects
unix:OBJECTS_DIR = objects
unix:RCC_DIR = objects
mac:DESTDIR = .

mac:LIBS += -framework IOKit -framework CoreFoundation
unix {
    CONFIG += link_pkgconfig
    packagesExist(libudev) {
        DEFINES += HAVE_UDEV
        PKGCONFIG += libudev
    }

    # Setting PKGDATADIR will:
    # 1. Look for gmapbase.html in PKGDATADIR instead
    # of in QApplication::applicationDirPath().
    # E.g. qmake PKGDATADIR=/usr/share/gpsbabel
    # will use /usr/share/gpsbabel/gmapbase.html instead of
    # QApplication::applicationDirPath() + "/gmapbase.html".
    # 2. Substitude PKGDATADIR/translations in the search path for
    # translation files instead of
    # QApplication::applicationDirPath() + "/translations".
    # E.g. qmake PKGDATADIR=/usr/share/gpsbabel
    # will look in /usr/share/gpsbabel/translations instead of
    # QApplication::applicationDirPath() + "/translations".

    # We search for translation files (*.qm) in this order:
    # 1a. Relative to the executable, specifically at
    # QApplication::applicationDirPath() + "/translations".
    # This works when we package the app on windows and macos.
    # It also works we create a bundled app on linux,
    # for example with the package target.  However, linux packagers typically
    # install the translation files in another location.
    #  OR
    # 1b. In PKGDATADIR/translations.  This works for linux packages that
    # place the translations in PKGDATADIR/translations, e.g.
    # qmake PKGDATADIR=/usr/share/gpsbabel with the translations in
    # /usr/share/gpsbabel/translations.
    # 2. In the Qt TranslationsPath.  If this is not overridden in qt.conf it
    # will point to the hard-coded paths that are compiled into the Qt library.
    # This hard-coded path can be found with "qmake -query QT_INSTALL_TRANSLATIONS".
    # This works for linux packages that have some or all translation files
    # installed in the original location they used when compiling Qt.

    # There are three sets of translation files that should be available
    # when running gpsbabelfe:
    # 1. gpsbabelfe_*.qm
    # 2. gpsbabel_*.qm
    # 3a. The Qt supplied meta catalogs (qt_*.qm) and the module files they
    #     refer to (qt*_*.qm).
    #     Note if you are counting on finding the Qt supplied translations,
    #     then the package that provides the Qt translations
    #     needs to be a prerequisite to the gpsbabel gui package.
    #  OR
    # 3b. The concatentated translation files for the modules that gpsbabelfe
    #     uses.  These files are created by building the package target.
    #     These are named identically to the Qt translation meta catalogs, i.e.
    #     qt_*.qm, but contain the necessary translation data for the modules
    #     gpsbabelfe uses.
    # A description of the meta catalogs and concatentation process is
    # available at
    # https://doc.qt.io/qt-5/linguist-programmers.html#deploying-translations

    !isEmpty(PKGDATADIR):DEFINES += PKGDATADIR=\\\"$$PKGDATADIR\\\"
}

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

