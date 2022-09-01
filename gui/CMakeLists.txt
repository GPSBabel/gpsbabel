if(${CMAKE_SOURCE_DIR} STREQUAL ${CMAKE_CURRENT_SOURCE_DIR})
  message(FATAL_ERROR "Please use CMakeLists.txt in the project root directory to generate a build system.")
endif()

configure_file(${CMAKE_SOURCE_DIR}/gbversion.h.in gbversion.h @ONLY NEWLINE_STYLE LF)
configure_file(setup.iss.in setup.iss @ONLY NEWLINE_STYLE CRLF)

# Find includes in corresponding build directories
set(CMAKE_INCLUDE_CURRENT_DIR ON)
# Instruct CMake to run moc automatically when needed
set(CMAKE_AUTOMOC ON)
# Create code from a list of Qt designer ui files
set(CMAKE_AUTOUIC ON)
# Handle the Qt rcc code generator automatically
set(CMAKE_AUTORCC ON)

if(UNIX AND NOT APPLE)
  set(TARGET gpsbabelfe)
else()
  set(TARGET GPSBabelFE)
endif()
set(LIBTARGET ${TARGET}_lib)
set(TESTTARGET ${TARGET}_test)

add_library(${LIBTARGET} STATIC)
set(CMAKE_AUTOMOC OFF)
set(CMAKE_AUTOUIC OFF)
add_executable(${TARGET} WIN32 MACOSX_BUNDLE main.cc)
add_executable(${TESTTARGET} WIN32 MACOSX_BUNDLE EXCLUDE_FROM_ALL test_main.cc test_main.h)

# Find the QtCore library
find_package(QT NAMES Qt6 Qt5 COMPONENTS Core REQUIRED)
find_package(Qt${QT_VERSION_MAJOR} COMPONENTS Core Gui Network SerialPort Widgets Xml Test REQUIRED)
list(APPEND QT_LIBRARIES Qt${QT_VERSION_MAJOR}::Core Qt${QT_VERSION_MAJOR}::Gui Qt${QT_VERSION_MAJOR}::Network Qt${QT_VERSION_MAJOR}::SerialPort Qt${QT_VERSION_MAJOR}::Widgets Qt${QT_VERSION_MAJOR}::Xml Qt${QT_VERSION_MAJOR}::Test)
if(${Qt${QT_VERSION_MAJOR}Core_VERSION} VERSION_LESS 5.12)
  message(FATAL_ERROR "Qt version ${Qt${QT_VERSION_MAJOR}Core_VERSION} found, but version 5.12 or newer is required.")
else()
  message(STATUS "Using Qt${QT_VERSION_MAJOR} version ${Qt${QT_VERSION_MAJOR}Core_VERSION}")
endif()

option(GPSBABEL_MAPPREVIEW "enable map preview." ON)
if (GPSBABEL_MAPPREVIEW)
  find_package(Qt${QT_VERSION_MAJOR} COMPONENTS WebEngineWidgets WebChannel REQUIRED)
  list(APPEND QT_LIBRARIES Qt${QT_VERSION_MAJOR}::WebEngineWidgets Qt${QT_VERSION_MAJOR}::WebChannel)
else()
  target_compile_definitions(${LIBTARGET} PRIVATE DISABLE_MAPPREVIEW)
endif()

if (${QT_VERSION_MAJOR} EQUAL "5")
  qt5_wrap_cpp(TESTTARGET_MOC test_main.h TARGET ${TESTTARGET})
else()
  qt_wrap_cpp(TESTTARGET_MOC test_main.h TARGET ${TESTTARGET})
endif()
target_sources(${TESTTARGET} PRIVATE ${TESTTARGET_MOC})

if(UNIX AND NOT APPLE)
  set_target_properties(${TARGET} PROPERTIES RUNTIME_OUTPUT_DIRECTORY GPSBabelFE)
  set_target_properties(${TESTTARGET} PROPERTIES RUNTIME_OUTPUT_DIRECTORY GPSBabelFE_test)
endif()

# RESOURCES
list(APPEND RESOURCES app.qrc)
if(WIN32)
  list(APPEND RESOURCES app.rc)
endif()

# FORMS
list(APPEND FORMS aboutui.ui)
list(APPEND FORMS advui.ui)
list(APPEND FORMS donate.ui)
list(APPEND FORMS filterui.ui)
if (GPSBABEL_MAPPREVIEW)
  list(APPEND FORMS gmapui.ui)
endif()
list(APPEND FORMS mainwinui.ui)
list(APPEND FORMS miscfltui.ui)
list(APPEND FORMS preferences.ui)
list(APPEND FORMS rttrkui.ui)
list(APPEND FORMS trackui.ui)
list(APPEND FORMS upgrade.ui)
list(APPEND FORMS version_mismatch.ui)
list(APPEND FORMS wayptsui.ui)

# SOURCES
list(APPEND SOURCES aboutdlg.cc)
list(APPEND SOURCES advdlg.cc)
list(APPEND SOURCES donate.cc)
list(APPEND SOURCES dpencode.cc)
list(APPEND SOURCES filterdata.cc)
list(APPEND SOURCES filterdlg.cc)
list(APPEND SOURCES filterwidgets.cc)
list(APPEND SOURCES format.cc)
list(APPEND SOURCES formatload.cc)
if (GPSBABEL_MAPPREVIEW)
  list(APPEND SOURCES gmapdlg.cc)
  list(APPEND SOURCES gpx.cc)
endif()
list(APPEND SOURCES help.cc)
list(APPEND SOURCES latlng.cc)
list(APPEND SOURCES mainwindow.cc)
if (GPSBABEL_MAPPREVIEW)
  list(APPEND SOURCES map.cc)
endif()
list(APPEND SOURCES optionsdlg.cc)
list(APPEND SOURCES preferences.cc)
list(APPEND SOURCES processwait.cc)
list(APPEND SOURCES runmachine.cc)
list(APPEND SOURCES upgrade.cc)
list(APPEND SOURCES version_mismatch.cc)

if(UNIX)
  list(APPEND SOURCES serial_unix.cc)
elseif(WIN32)
  list(APPEND SOURCES serial_win.cc)
endif()

# HEADERS
list(APPEND HEADERS aboutdlg.h)
list(APPEND HEADERS advdlg.h)
list(APPEND HEADERS appname.h)
list(APPEND HEADERS babeldata.h)
list(APPEND HEADERS donate.h)
list(APPEND HEADERS filterdata.h)
list(APPEND HEADERS filterdlg.h)
list(APPEND HEADERS filterwidgets.h)
list(APPEND HEADERS format.h)
list(APPEND HEADERS formatload.h)
if (GPSBABEL_MAPPREVIEW)
  list(APPEND HEADERS gmapdlg.h)
  list(APPEND HEADERS gpx.h)
endif()
list(APPEND HEADERS help.h)
list(APPEND HEADERS mainwindow.h)
if (GPSBABEL_MAPPREVIEW)
  list(APPEND HEADERS map.h)
endif()
list(APPEND HEADERS optionsdlg.h)
list(APPEND HEADERS preferences.h)
list(APPEND HEADERS processwait.h)
list(APPEND HEADERS runmachine.h)
list(APPEND HEADERS setting.h)
list(APPEND HEADERS upgrade.h)
list(APPEND HEADERS version_mismatch.h)

if(UNIX AND NOT APPLE)
  option(GPSBABEL_EMBED_TRANSLATIONS "embed translations." ON)
  cmake_dependent_option(GPSBABEL_EMBED_MAP "embed gmapbase.html for map preview." ON "GPSBABEL_MAPPREVIEW" OFF)
else()
  option(GPSBABEL_EMBED_TRANSLATIONS "embed translations." OFF)
  cmake_dependent_option(GPSBABEL_EMBED_MAP "embed gmapbase.html for map preview." OFF "GPSBABEL_MAPPREVIEW" OFF)
endif()
if (GPSBABEL_EMBED_TRANSLATIONS)
  list(APPEND RESOURCES translations.qrc)
  target_compile_definitions(${TARGET} PRIVATE HAVE_EMBEDDED_TRANSLATIONS)
  target_compile_definitions(${TESTTARGET} PRIVATE HAVE_EMBEDDED_TRANSLATIONS)
endif()
if (GPSBABEL_EMBED_MAP)
  list(APPEND RESOURCES map.qrc)
  target_compile_definitions(${TARGET} PRIVATE HAVE_EMBEDDED_MAP)
  target_compile_definitions(${TESTTARGET} PRIVATE HAVE_EMBEDDED_MAP)
endif()

target_sources(${LIBTARGET} PRIVATE ${SOURCES} ${HEADERS} ${RESOURCES})
if(APPLE)
  set(MACOSX_BUNDLE_ICON_FILE appicon.icns)
  set(ICON_FILE images/${MACOSX_BUNDLE_ICON_FILE})
  set_source_files_properties(${ICON_FILE} PROPERTIES MACOSX_PACKAGE_LOCATION Resources)

  target_sources(${TARGET} PRIVATE ${HEADERS} ${ICON_FILE})
  target_sources(${TESTTARGET} PRIVATE ${HEADERS} ${ICON_FILE})

  # Info.plist has not been debugged with the cmake flow, it's a bit different than with the qmake flow.
  set_target_properties(${TARGET} PROPERTIES
    MACOSX_BUNDLE_GUI_IDENTIFIER org.gpsbabel.${TARGET}
    MACOSX_BUNDLE_ICON_FILE ${MACOSX_BUNDLE_ICON_FILE}
  )
  set_target_properties(${TESTTARGET} PROPERTIES
    MACOSX_BUNDLE_GUI_IDENTIFIER org.gpsbabel.${TESTTARGET}
    MACOSX_BUNDLE_ICON_FILE ${MACOSX_BUNDLE_ICON_FILE}
  )
else()
  target_sources(${TARGET} PRIVATE ${HEADERS})
  target_sources(${TESTTARGET} PRIVATE ${HEADERS})
endif()

get_property(_isMultiConfig GLOBAL PROPERTY GENERATOR_IS_MULTI_CONFIG)
if(_isMultiConfig)
  set(AUTOGEN_INCLUDE_DIR ${CMAKE_CURRENT_BINARY_DIR}/${LIBTARGET}_autogen/include_$<CONFIG>)
else()
  set(AUTOGEN_INCLUDE_DIR ${CMAKE_CURRENT_BINARY_DIR}/${LIBTARGET}_autogen/include)
endif()
target_include_directories(${LIBTARGET} INTERFACE $<BUILD_INTERFACE:${AUTOGEN_INCLUDE_DIR}>)

target_link_libraries(${TARGET} ${LIBTARGET} ${QT_LIBRARIES})
target_link_libraries(${TESTTARGET} ${LIBTARGET} ${QT_LIBRARIES})
target_link_libraries(${LIBTARGET} ${QT_LIBRARIES})

get_target_property(idirs ${LIBTARGET} INTERFACE_INCLUDE_DIRECTORIES)
message(STATUS "lib incs \"${idirs}\"")

get_target_property(Srcs ${TARGET} SOURCES)
message(STATUS "Sources are: \"${Srcs}\"")
get_target_property(DirDefs ${TARGET} COMPILE_DEFINITIONS)
message(STATUS "Defines are: \"${DirDefs}\"")
get_target_property(LnkLibs ${TARGET} LINK_LIBRARIES)
message(STATUS "Libs are: \"${LnkLibs}\"")
get_target_property(IncDirs ${TARGET} INCLUDE_DIRECTORIES)
message(STATUS "Include Directores are: \"${IncDirs}\"")

add_custom_target(package_app COMMAND ./package_app DEPENDS ${TARGET})

add_custom_command(
  TARGET ${TESTTARGET} POST_BUILD
  COMMAND ${CMAKE_COMMAND} -E copy "$<TARGET_FILE:gpsbabel>" "$<TARGET_FILE_DIR:${TESTTARGET}>/gpsbabel"
  COMMAND ${CMAKE_COMMAND} -E copy "${CMAKE_CURRENT_SOURCE_DIR}/gmapbase.html" "$<TARGET_FILE_DIR:${TESTTARGET}>"
  VERBATIM
)

if(UNIX)
  enable_testing()
  add_test(NAME test-gui
           COMMAND ${TESTTARGET}
           WORKING_DIRECTORY ${CMAKE_SOURCE_DIR}
          )
endif()


