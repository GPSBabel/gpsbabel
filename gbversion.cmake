# Use GB variable to express ownership intention and avoid conflict with
# documented and undocumented cmake variables.

# Including this pri file from ${CMAKE_SOURCE_DIR} will generate the version
# file in the current build directory, i.e. ${CMAKE_CURRENT_BINARY_DIR}.

# Note some of these variables are also used in the gui to generate setup.iss.
# Note some of these variables are also used in the cli to generate documents.

# FIXME: when we have a hierarchical build we can set the version directly
# in the top level CMakeLists.txt file, and the version components will
# be automatically available in as variables.  But today we have multiple
# independent CMakeLists.txt files all with their own project commands.
# By defining the version here we minimize the number of locations
# containing the definition to gbversion.pri and gbversion.cmake.

set(GB.VERSION 1.8.0) # also change in gbversion.pri
string(REPLACE "." ";" VERSION_COMPONENTS ${GB.VERSION})
list(GET VERSION_COMPONENTS 0 GB.MAJOR)
list(GET VERSION_COMPONENTS 1 GB.MINOR)
list(GET VERSION_COMPONENTS 2 GB.MICRO)
# Increase GB.BUILD for a new release (why? Where is this ever used?)
# A: it's used by win32/gpsbabel.rc which includes gbversion.h
set(GB.BUILD 32 CACHE STRING "Fourth component of Windows VERSIONINFO resource FILEVERSION and PRODUCTVERSION parameters.")
set(GB.PACKAGE_RELEASE "" CACHE STRING "String to append to VERSION tuple.") # .e.g. "-beta20190413"
set(GB.SHA $ENV{GITHUB_SHA})
if(DEFINED ENV{GITHUB_SHA})
  find_program(GIT_EXECUTABLE NAMES git)
  if(NOT GIT_EXECUTABLE STREQUAL "GIT-NOTFOUND")
    execute_process(COMMAND ${GIT_EXECUTABLE} show -s --format=%aI
    WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
    OUTPUT_VARIABLE GB.DATE OUTPUT_STRIP_TRAILING_WHITESPACE)
  endif()
endif()
string(TIMESTAMP GB.COPYRIGHT_YEAR "%Y" UTC)

# may be overridden on cmake command line
set(DOCVERSION ${GB.VERSION} CACHE STRING "String appended to documentation location for www.gpsbabel.org.")
