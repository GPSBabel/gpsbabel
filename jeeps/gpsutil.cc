/********************************************************************
** @source JEEPS utility functions
**
** @author Copyright (C) 1999 Alan Bleasby
** @version 1.0
** @modified Dec 28 1999 Alan Bleasby. First version
** @@
**
** This library is free software; you can redistribute it and/or
** modify it under the terms of the GNU Library General Public
** License as published by the Free Software Foundation; either
** version 2 of the License, or (at your option) any later version.
**
** This library is distributed in the hope that it will be useful,
** but WITHOUT ANY WARRANTY; without even the implied warranty of
** MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
** Library General Public License for more details.
**
** You should have received a copy of the GNU Library General Public
** License along with this library; if not, write to the
** Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
** Boston, MA  02110-1301, USA.
********************************************************************/
#include "jeeps/gps.h"
#include <bit>
#include <cstdarg>
#include <cstdlib>

static constexpr bool GPS_Little = std::endian::native == std::endian::little;

int32_t gps_warning = 0;
int32_t gps_error = 0;
int32_t gps_user = 0;
int32_t gps_show_bytes = 0;
int32_t gps_errno = 0;


/* @func GPS_Util_Get_Short ********************************************
**
** Get a short from a string
**
** @return [US] value
************************************************************************/

US GPS_Util_Get_Short(const UC* s)
{
  static US ret;
  UC* p;

  p = (UC*)&ret;

  if constexpr(!GPS_Little) {
    *p++ = *(s+1);
    *p = *s;
  } else {
    *p++ = *s;
    *p = *(s+1);
  }

  return ret;
}



/* @func GPS_Util_Put_Short ********************************************
**
** Put a short to a string
**
** @param [w] s [UC *] string to write to
** @param [r] v [const US] short to write
**
** @return [void]
************************************************************************/

void GPS_Util_Put_Short(UC* s, const US v)
{
  const auto* p = reinterpret_cast<const UC*>(&v);

  if constexpr(!GPS_Little) {
    *s++ = *(p+1);
    *s = *p;
  } else {
    *s++ = *p;
    *s = *(p+1);
  }

  return;
}



/* @func GPS_Util_Get_Double ********************************************
**
** Get a double from a string
**
** @return [double] value
************************************************************************/

double GPS_Util_Get_Double(const UC* s)
{
  double ret;
  UC* p;
  int32_t i;

  p = (UC*)&ret;


  if constexpr(!GPS_Little)
    for (i=sizeof(double)-1; i>-1; --i) {
      *p++ = s[i];
    }
  else
    for (i=0; i<(int32_t)sizeof(double); ++i) {
      *p++ = s[i];
    }

  return ret;
}



/* @func GPS_Util_Put_Double ********************************************
**
** Put a double to a string
**
** @param [w] s [UC *] string to write to
** @param [r] v [const double] double to write
**
** @return [void]
************************************************************************/

void GPS_Util_Put_Double(UC* s, const double v)
{
	int32_t i;

  const auto* p = reinterpret_cast<const UC*>(&v);

  if constexpr(!GPS_Little)
    for (i=sizeof(double)-1; i>-1; --i) {
      s[i] = *p++;
    }
  else
    for (i=0; i<(int32_t)sizeof(double); ++i) {
      s[i] = *p++;
    }

  return;
}




/* @func GPS_Util_Get_Int ********************************************
**
** Get an int from a string
**
** @return [int32] value
************************************************************************/

int32_t GPS_Util_Get_Int(const UC* s)
{
	int32_t ret;
  UC* p;
  int32_t i;

  p = (UC*)&ret;


  if constexpr(!GPS_Little)
    for (i=sizeof(int32_t)-1; i>-1; --i) {
      *p++ = s[i];
    }
  else
    for (i=0; i<(int32_t)sizeof(int32_t); ++i) {
      *p++ = s[i];
    }

  return ret;
}



/* @func GPS_Util_Put_Int ********************************************
**
** Put a int to a string
**
** @param [w] s [UC *] string to write to
** @param [r] v [const int32] int to write
**
** @return [void]
************************************************************************/

void GPS_Util_Put_Int(UC* s, const int32_t v)
{
	int32_t i;

  const auto* p = reinterpret_cast<const UC*>(&v);

  if constexpr(!GPS_Little)
    for (i=sizeof(int32_t)-1; i>-1; --i) {
      s[i] = *p++;
    }
  else
    for (i=0; i<(int32_t)sizeof(int32_t); ++i) {
      s[i] = *p++;
    }

  return;
}



/* @func GPS_Util_Get_Uint ********************************************
**
** Get an unsigned int from a string
**
** @return [uint32] value
************************************************************************/

uint32_t GPS_Util_Get_Uint(const UC* s)
{
	uint32_t ret;
  UC*     p;
  int32_t i;

  p = (UC*)&ret;


  if constexpr(!GPS_Little)
    for (i=sizeof(uint32_t)-1; i>-1; --i) {
      *p++ = s[i];
    }
  else
    for (i=0; i<(int32_t)sizeof(uint32_t); ++i) {
      *p++ = s[i];
    }

  return ret;
}



/* @func GPS_Util_Put_Uint ********************************************
**
** Put an unsigned int to a string
**
** @param [w] s [UC *] string to write to
** @param [r] v [const uint32] unsigned int to write
**
** @return [void]
************************************************************************/

void GPS_Util_Put_Uint(UC* s, const uint32_t v)
{
	int32_t i;

  const auto* p = reinterpret_cast<const UC*>(&v);

  if constexpr(!GPS_Little)
    for (i=sizeof(uint32_t)-1; i>-1; --i) {
      s[i] = *p++;
    }
  else
    for (i=0; i<(int32_t)sizeof(uint32_t); ++i) {
      s[i] = *p++;
    }

  return;
}



/* @func GPS_Util_Get_Float ********************************************
**
** Get a float from a string
**
** @return [float] value
************************************************************************/

float GPS_Util_Get_Float(const UC* s)
{
  float ret;
  UC* p;
  int32_t i;

  p = (UC*)&ret;


  if constexpr(!GPS_Little)
    for (i=sizeof(float)-1; i>-1; --i) {
      *p++ = s[i];
    }
  else
    for (i=0; i<(int32_t)sizeof(float); ++i) {
      *p++ = s[i];
    }

  return ret;
}



/* @func GPS_Util_Put_Float ********************************************
**
** Put a float to a string
**
** @param [w] s [UC *] string to write to
** @param [r] v [const float] float to write
**
** @return [void]
************************************************************************/

void GPS_Util_Put_Float(UC* s, const float v)
{
	int32_t i;

  const auto* p = reinterpret_cast<const UC*>(&v);

  if constexpr(!GPS_Little)
    for (i=sizeof(float)-1; i>-1; --i) {
      s[i] = *p++;
    }
  else
    for (i=0; i<(int32_t)sizeof(float); ++i) {
      s[i] = *p++;
    }

  return;
}


/* @func GPS_Warning ********************************************************
**
** Prints warning if gps_warning is true
**
** @param [r] s [char *] warning
**
** @return [void]
** @@
****************************************************************************/

void GPS_Warning(const char* s)
{
  if (!gps_warning) {
    return;
  }

  fprintf(stderr,"[WARNING] %s\n",s);
  fflush(stderr);

  return;
}


/* @func GPS_Fatal ********************************************************
**
** Always prints error and exits program
** Bad thing for a library so the library doesn't call it.
**
** @param [r] s [char *] fatal error
**
** @return [void]
** @@
****************************************************************************/

[[noreturn]] void GPS_Fatal(const char* s)
{

  fprintf(stderr,"[FATAL] %s\n",s);
  exit(0);
}



/* @func GPS_Error **********************************************************
**
** Prints Error if gps_error is true
**
** @param [r] s [char *] error
**
** @return [void]
** @@
****************************************************************************/

void GPS_Error(const char* fmt, ...)
{
  va_list argp;
  va_start(argp, fmt);

  if (gps_error) {
    fprintf(stderr, "[ERROR] ");
    vfprintf(stderr, fmt, argp);
    fprintf(stderr, "\n");
  }

  va_end(argp);
  return;
}


/* @func GPS_Enable_Error ***************************************************
**
** Enable error message printing
**
** @return [void]
** @@
****************************************************************************/

void GPS_Enable_Error()
{
  gps_error = 1;
  return;
}



/* @func GPS_Enable_Warning ***************************************************
**
** Enable warning message printing
**
** @return [void]
** @@
****************************************************************************/

void GPS_Enable_Warning()
{
  gps_warning = 1;
  return;
}



/* @func GPS_Disable_Error ***************************************************
**
** Disable error message printing
**
** @return [void]
** @@
****************************************************************************/

void GPS_Disable_Error()
{
  gps_error = 0;
  return;
}



/* @func GPS_Disable_Warning ***********************************************
**
** Disable warning message printing
**
** @return [void]
** @@
****************************************************************************/

void GPS_Disable_Warning()
{
  gps_warning = 0;
  return;
}



/* @func GPS_User ********************************************************
**
** Prints a message if gps_user is true
**
** @param [r] s [char *] message
**
** @return [void]
** @@
****************************************************************************/

void GPS_User(const char* fmt, ...)
{
  va_list  argp;
  va_start(argp, fmt);

  if (gps_user) {
    vfprintf(stdout, fmt, argp);
    fflush(stdout);
  }

  va_end(argp);
}

/* @func GPS_Disable_User ***********************************************
**
** Disable message printing
**
** @return [void]
** @@
****************************************************************************/

void GPS_Disable_User()
{
  gps_user = 0;
  return;
}


/* @func GPS_Enable_User ***********************************************
**
** Disable warning message printing
**
** @return [void]
** @@
****************************************************************************/

void GPS_Enable_User()
{
  gps_user = 1;
  return;
}


/* @func GPS_Diagnose ********************************************************
**
** Prints bytes read from gps if gps_show_bytes is set
**
** @param [r] cs [int32] byte read
**
** @return [void]
** @@
****************************************************************************/

void GPS_Diagnose(int32_t c)
{
  if (!gps_show_bytes) {
    return;
  }

  fprintf(stdout,"%d\n",(int)c);
  fflush(stdout);

  return;
}

void GPS_Diag(const char* fmt, ...)
{
  va_list argp;
  va_start(argp, fmt);

  if (gps_show_bytes) {
    vfprintf(stdout, fmt, argp);
  }
  va_end(argp);
  return;

}

/* @func GPS_Enable_Diagnose ***********************************************
**
** Enable diagnosis mode
**
** @return [void]
** @@
****************************************************************************/

void GPS_Enable_Diagnose()
{
  gps_show_bytes = 1;
  return;
}



/* @func GPS_Disable_Diagnose ***********************************************
**
** Disable diagnosis mode
**
** @return [void]
** @@
****************************************************************************/

void GPS_Disable_Diagnose()
{
  gps_show_bytes = 0;
  return;
}
