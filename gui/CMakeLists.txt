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

add_executable(${TARGET} WIN32 MACOSX_BUNDLE)

# Find the QtCore library
find_package(QT NAMES Qt6 Qt5 COMPONENTS Core REQUIRED)
find_package(Qt${QT_VERSION_MAJOR} COMPONENTS Core Gui Network SerialPort Widgets Xml REQUIRED)
list(APPEND QT_LIBRARIES Qt${QT_VERSION_MAJOR}::Core Qt${QT_VERSION_MAJOR}::Gui Qt${QT_VERSION_MAJOR}::Network Qt${QT_VERSION_MAJOR}::SerialPort Qt${QT_VERSION_MAJOR}::Widgets Qt${QT_VERSION_MAJOR}::Xml)
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
  target_compile_definitions(${TARGET} PRIVATE DISABLE_MAPPREVIEW)
endif()

if(UNIX AND NOT APPLE)
  set_target_properties(${TARGET} PROPERTIES RUNTIME_OUTPUT_DIRECTORY GPSBabelFE)
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
list(APPEND SOURCES main.cc)
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
endif()
if (GPSBABEL_EMBED_MAP)
  list(APPEND RESOURCES map.qrc)
endif()

if(APPLE)
  set(MACOSX_BUNDLE_ICON_FILE appicon.icns)
  set(ICON_FILE images/${MACOSX_BUNDLE_ICON_FILE})
  set_source_files_properties(${ICON_FILE} PROPERTIES MACOSX_PACKAGE_LOCATION Resources)

  target_sources(${TARGET} PRIVATE ${SOURCES} ${HEADERS} ${ICON_FILE} ${RESOURCES})

  # Info.plist has not been debugged with the cmake flow, it's a bit different than with the qmake flow.
  set_target_properties(${TARGET} PROPERTIES
    MACOSX_BUNDLE_GUI_IDENTIFIER org.gpsbabel.${TARGET}
    MACOSX_BUNDLE_ICON_FILE ${MACOSX_BUNDLE_ICON_FILE}
  )
else()
  target_sources(${TARGET} PRIVATE ${SOURCES} ${HEADERS} ${RESOURCES})
endif()

target_link_libraries(${TARGET} ${QT_LIBRARIES})

get_target_property(Srcs ${TARGET} SOURCES)
message(STATUS "Sources are: \"${Srcs}\"")
get_target_property(DirDefs ${TARGET} COMPILE_DEFINITIONS)
message(STATUS "Defines are: \"${DirDefs}\"")
get_target_property(LnkLibs ${TARGET} LINK_LIBRARIES)
message(STATUS "Libs are: \"${LnkLibs}\"")
get_target_property(IncDirs ${TARGET} INCLUDE_DIRECTORIES)
message(STATUS "Include Directores are: \"${IncDirs}\"")

if(APPLE)
  add_custom_target(package_app
                    COMMAND ${CMAKE_CURRENT_SOURCE_DIR}/package_app $<TARGET_BUNDLE_DIR:${TARGET}> $<TARGET_FILE:gpsbabel> ${CMAKE_CURRENT_SOURCE_DIR}
                    COMMAND ${CMAKE_COMMAND} -E copy $<TARGET_BUNDLE_DIR:${TARGET}>/../GPSBabelFE.dmg ${CMAKE_CURRENT_BINARY_DIR}
                    DEPENDS ${TARGET}
                    VERBATIM)
elseif(UNIX)
  add_custom_target(package_app
                    COMMAND ${CMAKE_CURRENT_SOURCE_DIR}/package_app $<TARGET_FILE_DIR:${TARGET}> $<TARGET_FILE:gpsbabel> ${CMAKE_CURRENT_SOURCE_DIR}
                    DEPENDS ${TARGET}
                    VERBATIM)
elseif(WIN32)
  find_program(INNO_COMPILER NAMES iscc ISCC
               PATHS "C:/Program Files (x86)/Inno Setup 6" "C:/Program Files/Inno Setup 6")
  file(TO_NATIVE_PATH "${CMAKE_CURRENT_BINARY_DIR}" win_binary_path)
  file(TO_NATIVE_PATH "${CMAKE_CURRENT_SOURCE_DIR}" win_source_path)
  add_custom_target(package_app
                    COMMAND lupdate ${CMAKE_CURRENT_SOURCE_DIR}/app.pro
                    COMMAND lrelease ${CMAKE_CURRENT_SOURCE_DIR}/app.pro
                    # deploy to a clean directory as different build systems create differently named debris in release.
                    COMMAND ${CMAKE_COMMAND} -E remove_directory ${CMAKE_CURRENT_BINARY_DIR}/package
                    COMMAND ${CMAKE_COMMAND} -E make_directory ${CMAKE_CURRENT_BINARY_DIR}/package
                    COMMAND ${CMAKE_COMMAND} -E copy $<TARGET_FILE:${TARGET}> ${CMAKE_CURRENT_BINARY_DIR}/package
                    COMMAND ${CMAKE_COMMAND} -E copy $<TARGET_FILE:gpsbabel> ${CMAKE_CURRENT_BINARY_DIR}/package
                    # use --plugindir option to locate the plugins.
                    COMMAND windeployqt --verbose 1 --plugindir ${CMAKE_CURRENT_BINARY_DIR}/package/plugins ${CMAKE_CURRENT_BINARY_DIR}/package/GPSBabelFE.exe ${CMAKE_CURRENT_BINARY_DIR}/package/GPSBabel.exe
                    # set location to location of generated setup.iss file.
                    COMMAND ${CMAKE_COMMAND} -E chdir ${CMAKE_CURRENT_BINARY_DIR} ${INNO_COMPILER} /Doutput_dir=${win_binary_path} /Dsource_dir=${win_source_path} setup.iss
                    DEPENDS ${TARGET}
                    VERBATIM
                    USES_TERMINAL)

endif()
