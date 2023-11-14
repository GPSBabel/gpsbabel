#ifndef JEEPS_GPSUTIL_H_INCLUDED_
#define JEEPS_GPSUTIL_H_INCLUDED_


#include "jeeps/gps.h"

int32_t GPS_Util_Little();

  US     GPS_Util_Get_Short(const UC* s);
  void   GPS_Util_Put_Short(UC* s, US v);
int32_t GPS_Util_Get_Int(const UC* s);
  void   GPS_Util_Put_Int(UC* s, int32_t v);
  double GPS_Util_Get_Double(const UC* s);
  void   GPS_Util_Put_Double(UC* s, double v);
  float  GPS_Util_Get_Float(const UC* s);
  void   GPS_Util_Put_Float(UC* s, float v);
  void   GPS_Util_Canon(int32_t state);
int32_t GPS_Util_Block(int32_t fd, int32_t state);
  void   GPS_Util_Put_Uint(UC* s, uint32_t v);
uint32_t GPS_Util_Get_Uint(const UC* s);

  void   GPS_Warning(const char* s);
  [[gnu::format(printf, 1, 2)]] void   GPS_Error(const char* fmt, ...);
  [[gnu::format(printf, 1, 2)]] void   GPS_Serial_Error(const char* fmt, ...);
  [[noreturn]] void   GPS_Fatal(const char* s);
  void   GPS_Enable_Error();
  void   GPS_Enable_Warning();
  void   GPS_Disable_Error();
  void   GPS_Disable_Warning();
  [[gnu::format(printf, 1, 2)]] void   GPS_User(const char* fmt, ...);
  void   GPS_Disable_User();
  void   GPS_Enable_User();
  void   GPS_Diagnose(int32_t c);
  [[gnu::format(printf, 1, 2)]] void   GPS_Diag(const char* fmt, ...);

  void   GPS_Enable_Diagnose();
  void   GPS_Disable_Diagnose();


#endif // JEEPS_GPSUTIL_H_INCLUDED_
