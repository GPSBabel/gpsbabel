
# It isn't clear what the minimum required version is.
# tested against 3.10.2 on ubuntu bionic 2018/07/05
# tested against 3.5.1 on ubuntu xenial 2018/07/05
# tested against MSVC 2017 which included 3.11.* 2018/07/05
cmake_minimum_required(VERSION 3.5.1)

project(gpsbabelfe)

set(CMAKE_CXX_STANDARD 14)

# Find includes in corresponding build directories
set(CMAKE_INCLUDE_CURRENT_DIR ON)
# Instruct CMake to run moc automatically when needed
set(CMAKE_AUTOMOC ON)
# Create code from a list of Qt designer ui files
set(CMAKE_AUTOUIC ON)
# Handle the Qt rcc code generator automatically
set(CMAKE_AUTORCC ON)

# Find the Qt5Core library
find_package(Qt5 COMPONENTS Core Gui Network Xml REQUIRED)
#message("Qt5Core_VERSION String describing the version of the module: \"${Qt5Core_VERSION}\"")
#message("Qt5Core_LIBRARIES List of libraries for use with the target_link_libraries command: \"${Qt5Core_LIBRARIES}\"")
#message("Qt5Core_INCLUDE_DIRS List of directories for use with the include_directories command: \"${Qt5Core_INCLUDE_DIRS}\"")
#message("Qt5Core_DEFINITIONS List of definitions for use with add_definitions: \"${Qt5Core_DEFINITIONS}\"")
#message("Qt5Core_COMPILE_DEFINITIONS List of definitions for use with the COMPILE_DEFINITIONS target property: \"${Qt5Core_COMPILE_DEFINITIONS}\"")
#message("Qt5Core_FOUND Boolean describing whether the module was found successfully: \"${Qt5Core_FOUND}\"")
#message("Qt5Core_EXECUTABLE_COMPILE_FLAGS String of flags to be used when building executables: \"${Qt5Core_EXECUTABLE_COMPILE_FLAGS}\"")
if (${Qt5Core_VERSION} VERSION_LESS 5.9)
  message(FATAL_ERROR "Qt version ${Qt5Core_VERSION} found, but version 5.9 or newer is required.")
endif()

# hard code webengine instead of webkit for now
find_package(Qt5 COMPONENTS WebEngineWidgets WebChannel REQUIRED)

set(QT_INCLUDE_DIRS ${Qt5Core_INCLUDE_DIRS} ${Qt5Gui_INCLUDE_DIRS} ${Qt5Network_INCLUDE_DIRS} ${Qt5Xml_INCLUDE_DIRS} ${Qt5WebEngineWidgets_INCLUDE_DIRS} ${Qt5WebChannel_INCLUDE_DIRS})
list(REMOVE_DUPLICATES QT_INCLUDE_DIRS)
message("QT INCLUDE DIRS: \"${QT_INCLUDE_DIRS}\"\n")

set(QT_DEFINITIONS ${Qt5Core_DEFINITIONS} ${QT5Gui_DEFINITIONS} ${Qt5Network_DEFINITIONS} ${Qt5Xml_DEFINITIONS} ${Qt5WebEngineWidgets_DEFINITIONS} ${Qt5WwebChannel_DEFINITIONS})
list(REMOVE_DUPLICATES QT_DEFINITIONS)
message("QT DEFS: \"${QT_DEFINITIONS}\"\n")

set(QT_LIBRARIES ${Qt5Core_LIBRARIES} ${Qt5Gui_LIBRARIES} ${Qt5Network_LIBRARIES} ${Qt5Xml_LIBRARIES} ${Qt5WebEngineWidgets_LIBRARIES} ${Qt5WebChannel_LIBRARIES} ${LIBUDEV_LIBRARIES} ${LIBS})
#list(REMOVE_DUPLICATES QT_LIBRARIES)
message("QT LIBS: \"${QT_LIBRARIES}\"\n")

include_directories(AFTER SYSTEM ${QT_INCLUDE_DIRS})
add_definitions(${QT_DEFINITIONS} -DHAVE_WEBENGINE)

# mac:LIBS += -framework IOKit -framework CoreFoundation

if (UNIX AND NOT APPLE)
  find_package(PkgConfig REQUIRED)
  pkg_check_modules(LIBUDEV libudev)
  if (${LIBUDEV_FOUND})
    add_definitions(-DHAVE_UDEV)
  endif()
endif()

set(RESOURCES app.qrc)

if (WIN32)
  set(RC_FILE app.rc)
endif()

if (UNIX AND NOT APPLE)
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
  upgrade.cc
  version_mismatch.cc
)

if (UNIX AND NOT APPLE)
  set(SOURCES ${SOURCES} serial_unix.cc)
elseif (APPLE)
  set(SOURCES ${SOURCES} serial_mac.cc)
elseif (WIN32)
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
  setting.h
  upgrade.h
  version_mismatch.h
)

add_executable(${TARGET} ${SOURCES} ${HEADERS} ${RESOURCES})
set(LIBS ${QT_LIBRARIES} ${LIBUDEV_LIBRARIES})
list(REMOVE_DUPLICATES LIBS)
target_link_libraries(${TARGET} ${LIBS})

message("Sources are: \"${SOURCES}\"\n")
message("Headers are: \"${HEADERS}\"\n")
get_directory_property(DirDefs COMPILE_DEFINITIONS)
message("Defines are: \"${DirDefs}\"\n")
get_target_property(LnkLibs ${TARGET} LINK_LIBRARIES)
message("Libs are: \"${LnkLibs}\"\n")
get_directory_property(IncDirs INCLUDE_DIRECTORIES)
message("Include Directores are: \"${IncDirs}\"\n")

