if(${CMAKE_SOURCE_DIR} STREQUAL ${CMAKE_CURRENT_SOURCE_DIR})
  message(FATAL_ERROR "Please use CMakeLists.txt in the project root directory to generate a build system.")
endif()

add_executable(coretool EXCLUDE_FROM_ALL)

find_package(Qt6 REQUIRED COMPONENTS Core Widgets)
list(APPEND QT_LIBRARIES Qt6::Core Qt6::Widgets)

list(APPEND SOURCES ../formatload.cc)
list(APPEND SOURCES coretool.cc)

list(APPEND HEADERS ../format.h)
list(APPEND HEADERS ../formatload.h)

target_compile_definitions(coretool PRIVATE GENERATE_CORE_STRINGS)
target_include_directories(coretool PRIVATE ..)
target_sources(coretool PRIVATE ${SOURCES} ${HEADERS})
target_link_libraries(coretool ${QT_LIBRARIES})

# FIXME: core_strings.h generated in source directory (and under version control).
# FIXME: translations updated and released in source directory (and under version control).

get_target_property(_qmake_executable Qt6::qmake IMPORTED_LOCATION)
get_filename_component(_qt_bin_dir "${_qmake_executable}" DIRECTORY)
add_custom_command(OUTPUT ${CMAKE_CURRENT_SOURCE_DIR}/core_strings.h
                  COMMAND ${CMAKE_COMMAND} -E copy $<TARGET_FILE:gpsbabel> ${CMAKE_CURRENT_BINARY_DIR}
                  COMMAND ${CMAKE_COMMAND} -DQTBINDIR=${_qt_bin_dir} -DCURRENT_BINARY_DIR=${CMAKE_CURRENT_BINARY_DIR} -P ${CMAKE_CURRENT_SOURCE_DIR}/coretool.cmake
                  DEPENDS coretool gpsbabel
                  WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
                  VERBATIM
                  USES_TERMINAL)

list(APPEND TRANSLATIONS gpsbabel_de.ts)
list(APPEND TRANSLATIONS gpsbabel_es.ts)
list(APPEND TRANSLATIONS gpsbabel_fr.ts)
list(APPEND TRANSLATIONS gpsbabel_hu.ts)
list(APPEND TRANSLATIONS gpsbabel_it.ts)
list(APPEND TRANSLATIONS gpsbabel_ru.ts)

find_package(Qt6 QUIET COMPONENTS LinguistTools)
if(Qt6LinguistTools_FOUND)
  # The line numbers are almost meaningless the way we generate corestrings.h, and we force everything to the same context.
  # With line numbers and the similartext heuristic enabled translations can be copied from an old message to a new message,
  # and marked as unfinished.  The threshold for similar is low.
  # These will be used by the application, even though they really need to be checked.
  # Disable the similartext heuristic to avoid these mistranslations.
  add_custom_target(coretool_lupdate
                    COMMAND Qt6::lupdate -disable-heuristic similartext core_strings.h -ts ${TRANSLATIONS}
                    DEPENDS core_strings.h
                    WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
                    VERBATIM
                    USES_TERMINAL)
  
  add_custom_target(coretool_lrelease
                    COMMAND Qt6::lrelease ${TRANSLATIONS}
                    DEPENDS coretool_lupdate
                    WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
                    VERBATIM
                    USES_TERMINAL)
else()
  message(WARNING "Qt6LinguistTools not found, coretool translations cannot be updated or released.")
endif()
