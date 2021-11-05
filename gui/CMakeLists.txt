
# It isn't clear what the minimum required version is.
# tested against 3.10.2 on ubuntu bionic 2018/07/05
# tested against 3.5.1 on ubuntu xenial 2018/07/05
# tested against MSVC 2017 which included 3.11.* 2018/07/05
cmake_minimum_required(VERSION 3.5.1)

project(gpsbabelfe LANGUAGES CXX)

set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED True)

# Find includes in corresponding build directories
set(CMAKE_INCLUDE_CURRENT_DIR ON)
# Instruct CMake to run moc automatically when needed
set(CMAKE_AUTOMOC ON)
# Create code from a list of Qt designer ui files
set(CMAKE_AUTOUIC ON)
# Handle the Qt rcc code generator automatically
set(CMAKE_AUTORCC ON)

# Find the QtCore library
find_package(QT NAMES Qt6 Qt5 COMPONENTS Core REQUIRED)
find_package(Qt${QT_VERSION_MAJOR} COMPONENTS Core Gui Network Widgets Xml REQUIRED)
list(APPEND QT_LIBRARIES Qt${QT_VERSION_MAJOR}::Core Qt${QT_VERSION_MAJOR}::Gui Qt${QT_VERSION_MAJOR}::Network Qt${QT_VERSION_MAJOR}::Widgets Qt${QT_VERSION_MAJOR}::Xml)
if(${Qt${QT_VERSION_MAJOR}Core_VERSION} VERSION_LESS 5.12)
  message(FATAL_ERROR "Qt version ${Qt${QT_VERSION_MAJOR}Core_VERSION} found, but version 5.12 or newer is required.")
else()
  message(STATUS "Using Qt${QT_VERSION_MAJOR} version ${Qt${QT_VERSION_MAJOR}Core_VERSION}")
endif()

find_package(Qt${QT_VERSION_MAJOR} COMPONENTS WebEngineWidgets WebChannel REQUIRED)
list(APPEND QT_LIBRARIES Qt${QT_VERSION_MAJOR}::WebEngineWidgets Qt${QT_VERSION_MAJOR}::WebChannel)

if(APPLE)
  find_library(IOKIT_LIBRARIES IOKit)
  find_library(COREFOUNDATION_LIBRARIES CoreFoundation)
endif()

if(UNIX AND NOT APPLE)
  find_package(PkgConfig REQUIRED)
  pkg_check_modules(LIBUDEV libudev)
  if(${LIBUDEV_FOUND})
    add_definitions(-DHAVE_UDEV)
  endif()
endif()

set(RESOURCES app.qrc)

if(WIN32)
  set(RESOURCES ${RESOURCES} app.rc)
endif()

if(UNIX AND NOT APPLE)
  set(TARGET gpsbabelfe)
  set(CMAKE_RUNTIME_OUTPUT_DIRECTORY GPSBabelFE)
else()
  set(TARGET GPSBabelFE)
endif()

set(FORMS
  aboutui.ui
  advui.ui
  donate.ui
  filterui.ui
  gmapui.ui
  mainwinui.ui
  miscfltui.ui
  preferences.ui
  rttrkui.ui
  trackui.ui
  upgrade.ui
  version_mismatch.ui
  wayptsui.ui
)

set(SOURCES
  aboutdlg.cc
  advdlg.cc
  donate.cc
  dpencode.cc
  filterdata.cc
  filterdlg.cc
  filterwidgets.cc
  format.cc
  formatload.cc
  gmapdlg.cc
  gpx.cc
  help.cc
  latlng.cc
  main.cc
  mainwindow.cc
  map.cc
  optionsdlg.cc
  preferences.cc
  processwait.cc
  runmachine.cc
  upgrade.cc
  version_mismatch.cc
)

if(UNIX AND NOT APPLE)
  set(SOURCES ${SOURCES} serial_unix.cc)
elseif(APPLE)
  set(SOURCES ${SOURCES} serial_mac.cc)
elseif(WIN32)
  set(SOURCES ${SOURCES} serial_win.cc)
endif()

set(HEADERS
  aboutdlg.h
  advdlg.h
  appname.h
  babeldata.h
  donate.h
  filterdata.h
  filterdlg.h
  filterwidgets.h
  format.h
  formatload.h
  gmapdlg.h
  gpx.h
  help.h
  mainwindow.h
  map.h
  optionsdlg.h
  preferences.h
  processwait.h
  runmachine.h
  setting.h
  upgrade.h
  version_mismatch.h
)

if(APPLE)
  set(MACOSX_BUNDLE_ICON_FILE appicon.icns)
  set(ICON_FILE images/${MACOSX_BUNDLE_ICON_FILE})
  set_source_files_properties(${ICON_FILE} PROPERTIES MACOSX_PACKAGE_LOCATION Resources)

  add_executable(${TARGET} MACOSX_BUNDLE ${SOURCES} ${HEADERS} ${ICON_FILE} ${RESOURCES})

  # Info.plist has not been debugged with the cmake flow, it's a bit different than with the qmake flow.
  set_target_properties(${TARGET} PROPERTIES
    MACOSX_BUNDLE_GUI_IDENTIFIER org.gpsbabel.${TARGET}
    MACOSX_BUNDLE_ICON_FILE ${MACOSX_BUNDLE_ICON_FILE}
  )
else()
  add_executable(${TARGET} ${SOURCES} ${HEADERS} ${RESOURCES})
endif()

set(LIBS ${QT_LIBRARIES} ${LIBUDEV_LIBRARIES} ${IOKIT_LIBRARIES} ${COREFOUNDATION_LIBRARIES})
list(REMOVE_DUPLICATES LIBS)
target_link_libraries(${TARGET} ${LIBS})

message(STATUS "Sources are: \"${SOURCES}\"")
message(STATUS "Headers are: \"${HEADERS}\"")
get_directory_property(DirDefs COMPILE_DEFINITIONS)
message(STATUS "Defines are: \"${DirDefs}\"")
get_target_property(LnkLibs ${TARGET} LINK_LIBRARIES)
message(STATUS "Libs are: \"${LnkLibs}\"")
get_directory_property(IncDirs INCLUDE_DIRECTORIES)
message(STATUS "Include Directores are: \"${IncDirs}\"")

add_custom_target(package_app COMMAND ./package_app DEPENDS ${TARGET})
