foreach(var QTBINDIR CURRENT_BINARY_DIR)
  if(NOT DEFINED ${var})
    message(FATAL_ERROR "${var} must be passed on the command line")
  endif()
endforeach()

if(WIN32)
  # windows needs the Qt Binary directory in the path to find the DLLs.
  file(TO_NATIVE_PATH "${QTBINDIR}" _qt_win_path)
  set(ENV{PATH} "${_qt_win_path};$ENV{PATH}")
endif()
execute_process(
  COMMAND ${CURRENT_BINARY_DIR}/coretool
  ERROR_FILE core_strings.h)
