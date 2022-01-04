if(UNIX)
  if (NOT APPLE)
    set(GPSBABEL_WITH_LIBUSB "system" CACHE STRING "no|pkgconfig|system*|custom")
  else()
    set(GPSBABEL_WITH_LIBUSB "included" CACHE STRING "no|pkgconfig|included*|custom")
  endif()
  if(GPSBABEL_WITH_LIBUSB STREQUAL "no")
    message(STATUS "libusb-1.0 disabled.")
    list(APPEND JEEPS jeeps/gpsusbstub.cc)
  else()
    target_compile_definitions(gpsbabel PRIVATE HAVE_LIBUSB_1_0)
    list(APPEND JEEPS jeeps/gpslibusb.cc)
    if(GPSBABEL_WITH_LIBUSB STREQUAL "pkgconfig")
      message(STATUS "Using libusb-1.0 found by pkg-config.")
      find_package(PkgConfig REQUIRED)
      pkg_search_module(LIBUSB REQUIRED libusb-1.0 IMPORTED_TARGET)
      list(APPEND LIBS PkgConfig::LIBUSB)
      target_compile_definitions(gpsbabel PRIVATE LIBUSB_H_INCLUDE=<libusb.h>)
    else()
      if(UNIX AND NOT APPLE)
        if(GPSBABEL_WITH_LIBUSB STREQUAL "system")
          list(APPEND LIBS usb-1.0)
          target_compile_definitions(gpsbabel PRIVATE LIBUSB_H_INCLUDE=<libusb-1.0/libusb.h>)
        elseif(GPSBABEL_WITH_LIBUSB STREQUAL "custom")
          message("libusb-1.0 is enabled but but must be manually configured.")
          message("  e.g. GPSBABEL_WITH_LIBUSB=custom GPSBABEL_EXTRA_LINK_LIBRARIES:STRING==... GPSBABEL_EXTRA_INCLUDE_DIRECTORIES:STRING=...")
          target_compile_definitions(gpsbabel PRIVATE LIBUSB_H_INCLUDE=<libusb.h>)
        else()
          message(FATAL_ERROR "GPSBABEL_WITH_LIBUSB=no|pkgconfig|system*|custom")
        endif()
      else()
        if(GPSBABEL_WITH_LIBUSB STREQUAL "included")
          target_compile_definitions(gpsbabel PRIVATE "LIBUSB_H_INCLUDE=\"mac/libusb/libusb.h\"")
          add_library(usb-1.0 STATIC
            mac/libusb/core.c
            mac/libusb/descriptor.c
            mac/libusb/hotplug.c
            mac/libusb/io.c
            mac/libusb/strerror.c
            mac/libusb/sync.c
            mac/libusb/os/darwin_usb.c
            mac/libusb/os/events_posix.c
            mac/libusb/os/threads_posix.c
            mac/libusb/hotplug.h
            mac/libusb/libusb.h
            mac/libusb/libusbi.h
            mac/libusb/version.h
            mac/libusb/version_nano.h
            mac/libusb/os/darwin_usb.h
            mac/libusb/os/events_posix.h
            mac/libusb/os/threads_posix.h
            mac/libusb/XCode/config.h
          )
          target_include_directories(usb-1.0 PRIVATE mac/libusb/XCode PUBLIC mac/libusb)
          target_link_libraries(usb-1.0 INTERFACE objc "-framework IOKit" "-framework CoreFoundation")
          list(APPEND LIBS usb-1.0)
        elseif(GPSBABEL_WITH_LIBUSB STREQUAL "custom")
          message("libusb-1.0 is enabled but but must be manually configured.")
          message("  e.g. GPSBABEL_WITH_LIBUSB=custom GPSBABEL_EXTRA_LINK_LIBRARIES:STRING==... GPSBABEL_EXTRA_INCLUDE_DIRECTORIES:STRING=...")
          target_compile_definitions(gpsbabel PRIVATE LIBUSB_H_INCLUDE=<libusb.h>)
        else()
          message(FATAL_ERROR "GPSBABEL_WITH_LIBUSB=no|pkgconfig|included*|custom")
        endif()
      endif()
    endif()
  endif()
endif()
