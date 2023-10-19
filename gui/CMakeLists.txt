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

add_library(gpsbabelfe_lib STATIC)

set(CMAKE_AUTOMOC OFF)
set(CMAKE_AUTOUIC OFF)

add_executable(gpsbabelfe WIN32 MACOSX_BUNDLE main.cc)
if(NOT UNIX OR APPLE)
  set_target_properties(gpsbabelfe PROPERTIES OUTPUT_NAME GPSBabelFE)
endif()

# Don't set WIN32 on TESTTARGET, we want to see qDebug messages.
add_executable(gpsbabelfe_test MACOSX_BUNDLE EXCLUDE_FROM_ALL test_main.cc test_main.h)

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
  target_compile_definitions(gpsbabelfe_lib PRIVATE DISABLE_MAPPREVIEW)
endif()

if (${QT_VERSION_MAJOR} EQUAL "5")
  qt5_wrap_cpp(TESTTARGET_MOC test_main.h TARGET gpsbabelfe_test)
else()
  qt_wrap_cpp(TESTTARGET_MOC test_main.h TARGET gpsbabelfe_test)
endif()
target_sources(gpsbabelfe_test PRIVATE ${TESTTARGET_MOC})

if(UNIX AND NOT APPLE)
  set_target_properties(gpsbabelfe PROPERTIES RUNTIME_OUTPUT_DIRECTORY GPSBabelFE)
  set_target_properties(gpsbabelfe_test PROPERTIES RUNTIME_OUTPUT_DIRECTORY GPSBabelFE_test)
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
  target_compile_definitions(gpsbabelfe PRIVATE HAVE_EMBEDDED_TRANSLATIONS)
  target_compile_definitions(gpsbabelfe_test PRIVATE HAVE_EMBEDDED_TRANSLATIONS)
endif()
if (GPSBABEL_EMBED_MAP)
  list(APPEND RESOURCES map.qrc)
  target_compile_definitions(gpsbabelfe PRIVATE HAVE_EMBEDDED_MAP)
  target_compile_definitions(gpsbabelfe_test PRIVATE HAVE_EMBEDDED_MAP)
endif()

target_sources(gpsbabelfe_lib PRIVATE ${SOURCES} ${HEADERS} ${RESOURCES})
if(APPLE)
  set(MACOSX_BUNDLE_ICON_FILE appicon.icns)
  set(ICON_FILE images/${MACOSX_BUNDLE_ICON_FILE})
  set_source_files_properties(${ICON_FILE} PROPERTIES MACOSX_PACKAGE_LOCATION Resources)

  target_sources(gpsbabelfe PRIVATE ${HEADERS} ${ICON_FILE})
  target_sources(gpsbabelfe_test PRIVATE ${HEADERS} ${ICON_FILE})

  # Info.plist has not been debugged with the cmake flow, it's a bit different than with the qmake flow.
  set_target_properties(gpsbabelfe PROPERTIES
    MACOSX_BUNDLE_BUNDLE_NAME GPSBabel
    MACOSX_BUNDLE_BUNDLE_VERSION ${GB.BUILD}
    MACOSX_BUNDLE_COPYRIGHT "© 2002-${GB.COPYRIGHT_YEAR} Robert Lipe"
    MACOSX_BUNDLE_GUI_IDENTIFIER org.gpsbabel.gpsbabelfe
    MACOSX_BUNDLE_ICON_FILE ${MACOSX_BUNDLE_ICON_FILE}
    MACOSX_BUNDLE_SHORT_VERSION_STRING ${PROJECT_VERSION}
  )
  set_target_properties(gpsbabelfe_test PROPERTIES
    MACOSX_BUNDLE_GUI_IDENTIFIER org.gpsbabel.gpsbabelfe_test
    MACOSX_BUNDLE_ICON_FILE ${MACOSX_BUNDLE_ICON_FILE}
  )
else()
  target_sources(gpsbabelfe PRIVATE ${HEADERS})
  target_sources(gpsbabelfe_test PRIVATE ${HEADERS})
endif()

get_property(_isMultiConfig GLOBAL PROPERTY GENERATOR_IS_MULTI_CONFIG)
if(_isMultiConfig)
  set(AUTOGEN_INCLUDE_DIR ${CMAKE_CURRENT_BINARY_DIR}/gpsbabelfe_lib_autogen/include_$<CONFIG>)
else()
  set(AUTOGEN_INCLUDE_DIR ${CMAKE_CURRENT_BINARY_DIR}/gpsbabelfe_lib_autogen/include)
endif()
target_include_directories(gpsbabelfe_lib INTERFACE $<BUILD_INTERFACE:${AUTOGEN_INCLUDE_DIR}>)

target_link_libraries(gpsbabelfe gpsbabelfe_lib ${QT_LIBRARIES})
target_link_libraries(gpsbabelfe_test gpsbabelfe_lib ${QT_LIBRARIES})
target_link_libraries(gpsbabelfe_lib ${QT_LIBRARIES})

get_target_property(idirs gpsbabelfe_lib INTERFACE_INCLUDE_DIRECTORIES)
message(STATUS "lib incs \"${idirs}\"")

get_target_property(Srcs gpsbabelfe SOURCES)
message(STATUS "Sources are: \"${Srcs}\"")
get_target_property(DirDefs gpsbabelfe COMPILE_DEFINITIONS)
message(STATUS "Defines are: \"${DirDefs}\"")
get_target_property(LnkLibs gpsbabelfe LINK_LIBRARIES)
message(STATUS "Libs are: \"${LnkLibs}\"")
get_target_property(IncDirs gpsbabelfe INCLUDE_DIRECTORIES)
message(STATUS "Include Directores are: \"${IncDirs}\"")

add_custom_command(
  TARGET gpsbabelfe_test POST_BUILD
  COMMAND ${CMAKE_COMMAND} -E copy "$<TARGET_FILE:gpsbabel>" "$<TARGET_FILE_DIR:gpsbabelfe_test>/gpsbabel"
  COMMAND ${CMAKE_COMMAND} -E copy "${CMAKE_CURRENT_SOURCE_DIR}/gmapbase.html" "$<TARGET_FILE_DIR:gpsbabelfe_test>"
  VERBATIM
)

add_test(NAME test-gui
         COMMAND gpsbabelfe_test
         WORKING_DIRECTORY ${CMAKE_SOURCE_DIR}
        )

find_package(Qt${QT_VERSION_MAJOR} QUIET COMPONENTS LinguistTools)
if (NOT Qt${QT_VERSION_MAJOR}LinguistTools_FOUND)
  message(WARNING "Qt${QT_VERSION_MAJOR}LinguistTools not found, gpsbabelfe translations cannot be updated or released, and application cannot be packaged.")
else()
# FIXME: translations updated and released in source directory (and under version control).
  list(APPEND TRANSLATIONS gpsbabelfe_de.ts)
  list(APPEND TRANSLATIONS gpsbabelfe_es.ts)
  list(APPEND TRANSLATIONS gpsbabelfe_fr.ts)
  list(APPEND TRANSLATIONS gpsbabelfe_hu.ts)
  list(APPEND TRANSLATIONS gpsbabelfe_it.ts)
  list(APPEND TRANSLATIONS gpsbabelfe_ru.ts)

  add_custom_target(gpsbabelfe_lupdate
                    COMMAND Qt${QT_VERSION_MAJOR}::lupdate ${SOURCES} ${FORMS} -ts ${TRANSLATIONS}
                    WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
                    VERBATIM
                    USES_TERMINAL)
  add_custom_target(gpsbabelfe_lrelease
                    COMMAND Qt${QT_VERSION_MAJOR}::lrelease ${TRANSLATIONS}
                    WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
                    DEPENDS gpsbabelfe_lupdate
                    VERBATIM
                    USES_TERMINAL)

  if(APPLE)
    get_target_property(_qmake_executable Qt${QT_VERSION_MAJOR}::qmake IMPORTED_LOCATION)
    add_custom_target(package_app
                      COMMAND ${CMAKE_CURRENT_SOURCE_DIR}/package_app -a $<TARGET_BUNDLE_DIR:gpsbabelfe> -q ${_qmake_executable} -g $<TARGET_FILE:gpsbabel> -s ${CMAKE_CURRENT_SOURCE_DIR}
                      COMMAND ${CMAKE_COMMAND} -E copy $<TARGET_BUNDLE_DIR:gpsbabelfe>/../GPSBabelFE.dmg ${CMAKE_CURRENT_BINARY_DIR}
                      DEPENDS gpsbabelfe gpsbabel gpsbabelfe_lrelease coretool_lrelease
                      VERBATIM
                      USES_TERMINAL)
  elseif(UNIX)
    get_target_property(_qmake_executable Qt${QT_VERSION_MAJOR}::qmake IMPORTED_LOCATION)
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
    get_target_property(_qmake_executable Qt${QT_VERSION_MAJOR}::qmake IMPORTED_LOCATION)
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
