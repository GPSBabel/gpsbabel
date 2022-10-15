# Use GB variable to express ownership intention and avoid conflict with
# documented and undocumented qmake variables.

# Until we do a hierarchical build the build directory for gpsbabel and
# the build directory for GPSBabelFE are independent.  Only the source
# directories have a known relationship.  Including this pri file from the
# source tree will generate the version file in the current build directory.

# Note some of these variables are also used in the gui to generate setup.iss.
# Note some of these variables are also used in the cli to generate documents.

# FIXME: Today we have multiple independent .pro files.  By defining
# the version here we minimize the number of locations containing the
# definition to gbversion.pri and gbversion.cmake. When we retire qmake
# we will be back to only one place for version to be defined.

GB.VERSION = 1.8.0 # Also change in gbversion.cmake
GB.VERSION_COMPONENTS = $$split(GB.VERSION, .)
GB.MAJOR = $$member(GB.VERSION_COMPONENTS, 0)
GB.MINOR = $$member(GB.VERSION_COMPONENTS, 1)
GB.MICRO = $$member(GB.VERSION_COMPONENTS, 2)
# Increase GB.BUILD for a new release (why? Where is this ever used?)
# A: it's used by win32/gpsbabel.rc which includes gbversion.h
GB.BUILD = 32
# GB.PACKAGE_RELEASE = "-beta20190413"
GB.SHA = $$(GITHUB_SHA)
GB.COPYRIGHT_YEAR = 2022

# may be overridden on qmake command line
!defined(DOCVERSION, var) {
DOCVERSION=$${GB.VERSION}
}

# use undocumented QMAKE_SUBSTITUTES variable to emulate AC_CONFIG_FILES
# Note $${PWD} is relative to the location of this file.
GB.versionfile.input = $${PWD}/gbversion.h.qmake.in
GB.versionfile.output = gbversion.h
QMAKE_SUBSTITUTES += GB.versionfile
