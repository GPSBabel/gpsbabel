add_library(strptime STATIC
  strptime/strptime_l.c
  strptime/strptime.h
)
if(MSVC)
  target_compile_definitions(strptime PRIVATE _CRT_SECURE_NO_WARNINGS)
  target_compile_options(strptime PRIVATE -wd4101 -wd4102 -wd4267)
endif()
target_include_directories(strptime INTERFACE strptime)
list(APPEND LIBS strptime)
