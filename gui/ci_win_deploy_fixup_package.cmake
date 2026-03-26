# windeployqt is known to copy icuuc.dll from the system32 folder into the package, which causes issues on some machines. This script checks if the icuuc.dll in the package matches the one in system32, and if so, removes it from the package to avoid conflicts.
# https://qt-project.atlassian.net/browse/QTBUG-142131

cmake_minimum_required(VERSION 3.14)

# Set default packagedir parameter
set(PACKAGEDIR "package" CACHE STRING "Path to the package directory")

# Define paths to the icuuc.dll in the package and in system32
set(PACKAGE_ICU "${PACKAGEDIR}/icuuc.dll")
set(SYSTEM_ICU "$ENV{SystemRoot}/System32/icuuc.dll")

# Check if both icuuc.dll files exist
if(EXISTS "${PACKAGE_ICU}" AND EXISTS "${SYSTEM_ICU}")
    # Compute the hashes of both files
    file(MD5 "${SYSTEM_ICU}" SYSTEM_HASH)
    file(MD5 "${PACKAGE_ICU}" PACKAGE_HASH)

    # If the hashes match, remove the package icuuc.dll
    if(PACKAGE_HASH STREQUAL SYSTEM_HASH)
        message(WARNING "Removing icuuc.dll that matches system32 copy from the package.")
        file(REMOVE "${PACKAGE_ICU}")
    endif()
endif()
