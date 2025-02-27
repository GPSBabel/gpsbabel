if(${CMAKE_SOURCE_DIR} STREQUAL ${CMAKE_CURRENT_SOURCE_DIR})
  message(FATAL_ERROR "Please use CMakeLists.txt in the project root directory to generate a build system.")
endif()

add_subdirectory(coretool)

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

add_executable(gpsbabelfe WIN32 MACOSX_BUNDLE)
if(NOT UNIX OR APPLE)
  set_target_properties(gpsbabelfe PROPERTIES OUTPUT_NAME GPSBabelFE)
endif()

# Find the QtCore library
find_package(Qt6 REQUIRED COMPONENTS Core Gui Network SerialPort Widgets Xml OPTIONAL_COMPONENTS WebEngineWidgets WebChannel)
list(APPEND QT_LIBRARIES Qt6::Core Qt6::Gui Qt6::Network Qt6::SerialPort Qt6::Widgets Qt6::Xml)
if(${Qt6Core_VERSION} VERSION_LESS 6.2)
  message(FATAL_ERROR "Qt version ${Qt6Core_VERSION} found, but version 6.2 or newer is required.")
else()
  message(STATUS "Using Qt6 version ${Qt6Core_VERSION}")
endif()

cmake_dependent_option(GPSBABEL_MAPPREVIEW "enable map preview." ON "Qt6WebEngineWidgets_FOUND;Qt6WebChannel_FOUND" OFF)
if (GPSBABEL_MAPPREVIEW)
  list(APPEND QT_LIBRARIES Qt6::WebEngineWidgets Qt6::WebChannel)
else()
  if (NOT Qt6WebEngineWidgets_FOUND OR NOT Qt6WebChannel_FOUND)
    message(WARNING "Optional components Qt6 WebEngineWidgets and/or Qt6 WebChannel not found, MAPPREVIEW is not available.")
  endif()
  target_compile_definitions(gpsbabelfe PRIVATE DISABLE_MAPPREVIEW)
endif()

if(UNIX AND NOT APPLE)
  set_target_properties(gpsbabelfe PROPERTIES RUNTIME_OUTPUT_DIRECTORY GPSBabelFE)
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

  target_sources(gpsbabelfe PRIVATE ${SOURCES} ${HEADERS} ${ICON_FILE} ${RESOURCES})

  # Info.plist has not been debugged with the cmake flow, it's a bit different than with the qmake flow.
  set_target_properties(gpsbabelfe PROPERTIES
    MACOSX_BUNDLE_BUNDLE_NAME GPSBabel
    MACOSX_BUNDLE_BUNDLE_VERSION ${GB.BUILD}
    MACOSX_BUNDLE_COPYRIGHT "© 2002-${GB.COPYRIGHT_YEAR} Robert Lipe"
    MACOSX_BUNDLE_GUI_IDENTIFIER org.gpsbabel.gpsbabelfe
    MACOSX_BUNDLE_ICON_FILE ${MACOSX_BUNDLE_ICON_FILE}
    MACOSX_BUNDLE_SHORT_VERSION_STRING ${PROJECT_VERSION}
  )
else()
  target_sources(gpsbabelfe PRIVATE ${SOURCES} ${HEADERS} ${RESOURCES})
endif()

target_link_libraries(gpsbabelfe ${QT_LIBRARIES})

get_target_property(Srcs gpsbabelfe SOURCES)
message(STATUS "Sources are: \"${Srcs}\"")
get_target_property(DirDefs gpsbabelfe COMPILE_DEFINITIONS)
message(STATUS "Defines are: \"${DirDefs}\"")
get_target_property(LnkLibs gpsbabelfe LINK_LIBRARIES)
message(STATUS "Libs are: \"${LnkLibs}\"")
get_target_property(IncDirs gpsbabelfe INCLUDE_DIRECTORIES)
message(STATUS "Include Directories are: \"${IncDirs}\"")

find_package(Qt6 QUIET COMPONENTS LinguistTools)
if (NOT Qt6LinguistTools_FOUND)
  message(WARNING "Qt6LinguistTools not found, gpsbabelfe translations cannot be updated or released, and application cannot be packaged.")
else()
# FIXME: translations updated and released in source directory (and under version control).
  list(APPEND TRANSLATIONS gpsbabelfe_de.ts)
  list(APPEND TRANSLATIONS gpsbabelfe_es.ts)
  list(APPEND TRANSLATIONS gpsbabelfe_fr.ts)
  list(APPEND TRANSLATIONS gpsbabelfe_hu.ts)
  list(APPEND TRANSLATIONS gpsbabelfe_it.ts)
  list(APPEND TRANSLATIONS gpsbabelfe_ru.ts)

  add_custom_target(gpsbabelfe_lupdate
                    COMMAND Qt6::lupdate ${SOURCES} ${FORMS} -ts ${TRANSLATIONS}
                    WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
                    VERBATIM
                    USES_TERMINAL)
  add_custom_target(gpsbabelfe_lrelease
                    COMMAND Qt6::lrelease ${TRANSLATIONS}
                    WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
                    DEPENDS gpsbabelfe_lupdate
                    VERBATIM
                    USES_TERMINAL)

  if(APPLE)
    get_target_property(_qmake_executable Qt6::qmake IMPORTED_LOCATION)
    add_custom_target(package_app
                      COMMAND ${CMAKE_CURRENT_SOURCE_DIR}/package_app -a $<TARGET_BUNDLE_DIR:gpsbabelfe> -q ${_qmake_executable} -g $<TARGET_FILE:gpsbabel> -s ${CMAKE_CURRENT_SOURCE_DIR}
                      COMMAND ${CMAKE_COMMAND} -E copy $<TARGET_BUNDLE_DIR:gpsbabelfe>/../GPSBabelFE.dmg ${CMAKE_CURRENT_BINARY_DIR}
                      DEPENDS gpsbabelfe gpsbabel gpsbabelfe_lrelease coretool_lrelease
                      VERBATIM
                      USES_TERMINAL)
  elseif(UNIX)
    get_target_property(_qmake_executable Qt6::qmake IMPORTED_LOCATION)
    add_custom_target(package_app
                      COMMAND ${CMAKE_CURRENT_SOURCE_DIR}/package_app -a $<TARGET_FILE_DIR:gpsbabelfe> -q ${_qmake_executable} -g $<TARGET_FILE:gpsbabel> -s ${CMAKE_CURRENT_SOURCE_DIR}
                      DEPENDS gpsbabelfe gpsbabel gpsbabelfe_lrelease coretool_lrelease
                      VERBATIM
                      USES_TERMINAL)
  elseif(WIN32)
    find_program(INNO_COMPILER NAMES iscc ISCC
                 PATHS "C:/Program Files (x86)/Inno Setup 6" "C:/Program Files/Inno Setup 6")
    if (INNO_COMPILER STREQUAL "INNO_COMPILER-NOTFOUND")
      message(WARNING "Inno compiler iscc not found, application cannot be packaged.")
    endif()

    # in 5.12.12 cmake doesn't know about windeployqt, look in directory that has qmake.
    get_target_property(_qmake_executable Qt6::qmake IMPORTED_LOCATION)
    get_filename_component(_qt_bin_dir "${_qmake_executable}" DIRECTORY)
    find_program(WINDEPLOYQT NAMES windeployqt PATHS "${_qt_bin_dir}" NO_DEFAULT_PATH)
    if (WINDEPLOYQT STREQUAL "WINDEPLOYQT-NOTFOUND")
      message(WARNING "windeployqt not found, application cannot be packaged.")
    endif()

    if((NOT WINDEPLOYQT STREQUAL "WINDEPLOYQT-NOTFOUND") AND (NOT INNO_COMPILER STREQUAL "INNO_COMPILER-NOTFOUND"))
      file(TO_NATIVE_PATH "${CMAKE_CURRENT_BINARY_DIR}" _win_binary_path)
      file(TO_NATIVE_PATH "${CMAKE_CURRENT_SOURCE_DIR}" _win_source_path)
      add_custom_target(package_app
                        # deploy to a clean directory as different build systems create differently named debris in release.
                        COMMAND ${CMAKE_COMMAND} -E remove_directory package
                        COMMAND ${CMAKE_COMMAND} -E make_directory package
                        COMMAND ${CMAKE_COMMAND} -E copy $<TARGET_FILE:gpsbabelfe> package
                        COMMAND ${CMAKE_COMMAND} -E copy $<TARGET_FILE:gpsbabel> package
                        # use --plugindir option to locate the plugins.
                        COMMAND ${WINDEPLOYQT} --verbose 1 --plugindir package\\plugins package\\GPSBabelFE.exe package\\GPSBabel.exe
                        # set location to location of generated setup.iss file.
                        COMMAND ${INNO_COMPILER} /Doutput_dir=${_win_binary_path} /Dsource_dir=${_win_source_path} setup.iss
                        DEPENDS gpsbabelfe gpsbabel gpsbabelfe_lrelease coretool_lrelease
                        VERBATIM
                        USES_TERMINAL)
    endif()
  endif()
endif()
