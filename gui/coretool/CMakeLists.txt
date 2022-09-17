if(${CMAKE_SOURCE_DIR} STREQUAL ${CMAKE_CURRENT_SOURCE_DIR})
  message(FATAL_ERROR "Please use CMakeLists.txt in the project root directory to generate a build system.")
endif()

set(TARGET coretool)

add_executable(${TARGET} EXCLUDE_FROM_ALL)

find_package(QT NAMES Qt6 Qt5 COMPONENTS Core REQUIRED)
find_package(Qt${QT_VERSION_MAJOR} COMPONENTS Core Widgets REQUIRED)
list(APPEND QT_LIBRARIES Qt${QT_VERSION_MAJOR}::Core Qt${QT_VERSION_MAJOR}::Widgets)

list(APPEND SOURCES ../formatload.cc)
list(APPEND SOURCES coretool.cc)

list(APPEND HEADERS ../format.h)
list(APPEND HEADERS ../formatload.h)

target_compile_definitions(${TARGET} PRIVATE GENERATE_CORE_STRINGS)
target_include_directories(${TARGET} PRIVATE ..)
target_sources(${TARGET} PRIVATE ${SOURCES} ${HEADERS})
target_link_libraries(${TARGET} ${QT_LIBRARIES})

# FIXME: core_strings.h generated in source directory (and under version control).
# FIXME: translations updated and released in source directory (and under version control).

add_custom_target(core_strings
                  COMMAND ${CMAKE_COMMAND} -E copy $<TARGET_FILE:gpsbabel> ${CMAKE_CURRENT_BINARY_DIR}
                  COMMAND ${TARGET} 2> core_strings.h;
                  DEPENDS ${TARGET}
                  WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
                  VERBATIM
                  USES_TERMINAL)

list(APPEND TRANSLATIONS gpsbabel_de.ts)
list(APPEND TRANSLATIONS gpsbabel_es.ts)
list(APPEND TRANSLATIONS gpsbabel_fr.ts)
list(APPEND TRANSLATIONS gpsbabel_hu.ts)
list(APPEND TRANSLATIONS gpsbabel_it.ts)
list(APPEND TRANSLATIONS gpsbabel_ru.ts)

find_package(Qt${QT_VERSION_MAJOR} QUIET COMPONENTS LinguistTools)
if(Qt${QT_VERSION_MAJOR}LinguistTools_FOUND)
  add_custom_target(coretool_lupdate
                    COMMAND Qt${QT_VERSION_MAJOR}::lupdate -locations absolute core_strings.h -ts ${TRANSLATIONS}
                    DEPENDS core_strings
                    WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
                    VERBATIM
                    USES_TERMINAL)
  
  add_custom_target(coretool_lrelease
                    COMMAND Qt${QT_VERSION_MAJOR}::lrelease ${TRANSLATIONS}
                    DEPENDS coretool_lupdate
                    WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
                    VERBATIM
                    USES_TERMINAL)
else()
  message(WARNING "Qt${QT_VERSION_MAJOR}LinguistTools not found, coretool translations cannot be updated or released.")
endif()
