add_library(strptime STATIC
  strptime/strptime_l.c
  strptime/strptime.h
)
target_include_directories(strptime INTERFACE strptime)
list(APPEND LIBS strptime)
