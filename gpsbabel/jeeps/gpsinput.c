/********************************************************************
** @source JEEPS input functions
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
** Free Software Foundation, Inc., 59 Temple Place - Suite 330,
** Boston, MA  02111-1307, USA.
********************************************************************/
#include "gps.h"
#include <stdio.h>
#include <errno.h>
#include <string.h>
#include <stdlib.h>


static int32 GPS_Input_Load_String(char *t, int32 n, char *s);
static int32 GPS_Input_Load_Strnull(char *t, char *s);
static int32 GPS_Input_Read_Line(char *s, FILE *inf);

static int32 GPS_Input_Get_D100(GPS_PWay *way, FILE *inf);
static int32 GPS_Input_Get_D101(GPS_PWay *way, FILE *inf);
static int32 GPS_Input_Get_D102(GPS_PWay *way, FILE *inf);
static int32 GPS_Input_Get_D103(GPS_PWay *way, FILE *inf);
static int32 GPS_Input_Get_D104(GPS_PWay *way, FILE *inf);
static int32 GPS_Input_Get_D105(GPS_PWay *way, FILE *inf);
static int32 GPS_Input_Get_D106(GPS_PWay *way, FILE *inf);
static int32 GPS_Input_Get_D107(GPS_PWay *way, FILE *inf);
static int32 GPS_Input_Get_D108(GPS_PWay *way, FILE *inf);
static int32 GPS_Input_Get_D109(GPS_PWay *way, FILE *inf, int protonum);
static int32 GPS_Input_Get_D150(GPS_PWay *way, FILE *inf);
static int32 GPS_Input_Get_D151(GPS_PWay *way, FILE *inf);
static int32 GPS_Input_Get_D152(GPS_PWay *way, FILE *inf);
static int32 GPS_Input_Get_D154(GPS_PWay *way, FILE *inf);
static int32 GPS_Input_Get_D155(GPS_PWay *way, FILE *inf);

static int32 GPS_Input_Get_Track301(GPS_PTrack **trk, FILE *inf, int32 type,
                                    int32 n);
static int32 GPS_Input_Get_D300(GPS_PTrack *trk, FILE *inf, char *s);
static int32 GPS_Input_Get_D301(GPS_PTrack *trk, FILE *inf, char *s);

static int32 GPS_Input_Get_Route201(GPS_PWay **way, FILE *inf);


/* @funcstatic GPS_Input_Load_String ***********************************
**
** Load a GPS char type from an input line
** Remove trailing newline
**
** @param [w] t [char *] string to load
** @param [r] n [int32] maximum type length
** @param [r] s [char *] source line
**
** @return [int32] success
************************************************************************/
static int32 GPS_Input_Load_String(char *t, int32 n, char *s)
{
  char *p;
  char *q;

  int32 len;
  int32 i;

  gps_errno = INPUT_ERROR;

  p=s;
  if (!(p=strchr(p,':'))) {
    return gps_errno;
  }
  ++p;
  while (*p && (*p==' ' || *p=='\t')) {
    ++p;
  }
  if (!*p) {
    return 0;
  }

  len = strlen(p);
  q = p+len-1;
  while (*q==' ' || *q=='\t') {
    --q;
  }
  len = q-p+1;

  if (q-p+1 > n) {
    len = n;
    p[n]='\0';
  }
  for (i=0; i<len; ++i) {
    t[i]=*p++;
  }

  return 1;
}



/* @funcstatic GPS_Input_Load_Strnull **********************************
**
** Load a GPS variable length string type from an input line
** Remove trailing newline
**
** @param [w] t [char *] string to load
** @param [r] s [char *] source line
**
** @return [int32] success
************************************************************************/
static int32 GPS_Input_Load_Strnull(char *t, char *s)
{
  char *p;
  char *q;

  gps_errno = INPUT_ERROR;

  p=s;
  if (!(p=strchr(p,':'))) {
    return gps_errno;
  }
  ++p;
  while (*p && (*p==' ' || *p=='\t')) {
    ++p;
  }

  q = t;
  while ((*q++ = *p++));

  return 1;
}



/* @funcstatic GPS_Input_Read_Line   ************************************
**
** Read line from a file ignoring comments and newlines
** Remove trailing newline
**
** @param [w] s [char *] string
** @param [r] inf [FILE *] stream
**
** @return [int32] success
************************************************************************/

static int32 GPS_Input_Read_Line(char *s, FILE *inf)
{
  int32 len;

  while (fgets(s,GPS_ARB_LEN,inf)) {
    if (*s=='#' || *s=='\n') {
      continue;
    }
    len = strlen(s);
    if (s[len-1]=='\n') {
      s[len-1]='\0';
      return len-1;
    }
    return len;
  }

  return 0;
}



/* @func GPS_Input_Get_Almanac   ****************************************
**
** Construct an almanac array from a file
**
** @param [w] alm [GPS_PAlmanac **] pointer to almanac array
** @param [r] inf [FILE *] stream
**
** @return [int32] number of entries
************************************************************************/

int32 GPS_Input_Get_Almanac(GPS_PAlmanac **alm, FILE *inf)
{
  char s[GPS_ARB_LEN];
  int32 n;
  int32 type;
  int32 i;
  int32   d;
  float f;
  char *p;

  gps_errno = INPUT_ERROR;


  if (!GPS_Input_Read_Line(s,inf)) {
    return gps_errno;
  }

  if (sscanf(s,"Almanac %d%d",(int *)&n,(int *)&type)!=2) {
    return gps_errno;
  }

  if (!type) {
    if (!(*alm = (GPS_PAlmanac *) malloc(32*sizeof(GPS_PAlmanac *)))) {
      return MEMORY_ERROR;
    }
    for (i=0; i<32; ++i) {
      if (!((*alm)[i] = GPS_Almanac_New())) {
        return MEMORY_ERROR;
      }

      (*alm)[i]->svid = i;
      (*alm)[i]->wn = -1;
    }
  } else {
    if (!(*alm = (GPS_PAlmanac *) malloc(n*sizeof(GPS_PAlmanac *)))) {
      return MEMORY_ERROR;
    }
    for (i=0; i<32; ++i)
      if (!((*alm)[i] = GPS_Almanac_New())) {
        return MEMORY_ERROR;
      }
  }

  for (i=0; i<n; ++i) {
    if (!GPS_Input_Read_Line(s,inf)) {
      if (!type) {
        break;
      } else {
        return gps_error;
      }
    }

    p=strchr(s,':');
    if (sscanf(p+1,"%d",(int *)&d)!=1) {
      return gps_errno;
    }
    --d;

    if (!type)
      while ((*alm)[i]->svid!=d) {
        ++i;
      }
    (*alm)[i]->svid=d;

    if (!GPS_Input_Read_Line(s,inf)) {
      return gps_errno;
    }
    p=strchr(s,':');
    if (sscanf(p+1,"%d",(int *)&d)!=1) {
      return gps_errno;
    }
    (*alm)[i]->wn = d;

    if (!GPS_Input_Read_Line(s,inf)) {
      return gps_errno;
    }
    p=strchr(s,':');
    if (sscanf(p+1,"%f",&f)!=1) {
      return gps_errno;
    }
    (*alm)[i]->toa = f;

    if (!GPS_Input_Read_Line(s,inf)) {
      return gps_errno;
    }
    p=strchr(s,':');
    if (sscanf(p+1,"%f",&f)!=1) {
      return gps_errno;
    }
    (*alm)[i]->af0 = f;

    if (!GPS_Input_Read_Line(s,inf)) {
      return gps_errno;
    }
    p=strchr(s,':');
    if (sscanf(p+1,"%f",&f)!=1) {
      return gps_errno;
    }
    (*alm)[i]->af1 = f;

    if (!GPS_Input_Read_Line(s,inf)) {
      return gps_errno;
    }
    p=strchr(s,':');
    if (sscanf(p+1,"%f",&f)!=1) {
      return gps_errno;
    }
    (*alm)[i]->e = f;

    if (!GPS_Input_Read_Line(s,inf)) {
      return gps_errno;
    }
    p=strchr(s,':');
    if (sscanf(p+1,"%f",&f)!=1) {
      return gps_errno;
    }
    (*alm)[i]->sqrta = f;

    if (!GPS_Input_Read_Line(s,inf)) {
      return gps_errno;
    }
    p=strchr(s,':');
    if (sscanf(p+1,"%f",&f)!=1) {
      return gps_errno;
    }
    (*alm)[i]->m0 = f;

    if (!GPS_Input_Read_Line(s,inf)) {
      return gps_errno;
    }
    p=strchr(s,':');
    if (sscanf(p+1,"%f",&f)!=1) {
      return gps_errno;
    }
    (*alm)[i]->w = f;

    if (!GPS_Input_Read_Line(s,inf)) {
      return gps_errno;
    }
    p=strchr(s,':');
    if (sscanf(p+1,"%f",&f)!=1) {
      return gps_errno;
    }
    (*alm)[i]->omg0 = f;

    if (!GPS_Input_Read_Line(s,inf)) {
      return gps_errno;
    }
    p=strchr(s,':');
    if (sscanf(p+1,"%f",&f)!=1) {
      return gps_errno;
    }
    (*alm)[i]->odot = f;

    if (!GPS_Input_Read_Line(s,inf)) {
      return gps_errno;
    }
    p=strchr(s,':');
    if (sscanf(p+1,"%f",&f)!=1) {
      return gps_errno;
    }
    (*alm)[i]->i = f;

    if (!GPS_Input_Read_Line(s,inf)) {
      return gps_errno;
    }
    p=strchr(s,':');
    if (sscanf(p+1,"%d",(int *)&d)!=1) {
      return gps_errno;
    }
    (*alm)[i]->hlth=d;
  }

  if (!type) {
    n = 32;
  }

  return n;
}



/* @func GPS_Input_Get_Waypoint *****************************************
**
** Construct a waypoint array from a file
**
** @param [w] way [GPS_PWay **] pointer to waypoint array
** @param [r] inf [FILE *] stream
**
** @return [int32] number of entries
************************************************************************/

int32 GPS_Input_Get_Waypoint(GPS_PWay **way, FILE *inf)
{
  char s[GPS_ARB_LEN];
  int32 n;
  int32 type;
  int32 i;
  long pos;
  int32 ret;

  gps_errno = INPUT_ERROR;

  if (!GPS_Input_Read_Line(s,inf)) {
    return gps_errno;
  }
  if (sscanf(s,"Waypoints Type: %d",(int *)&type)!=1) {
    return gps_errno;
  }

  if (!GPS_Input_Read_Line(s,inf)) {
    return gps_errno;
  }
  if (strncmp(s,"Start",5)) {
    return gps_errno;
  }

  pos = ftell(inf);
  n = 0;
  while (strncmp(s,"End",3)) {
    if (!GPS_Input_Read_Line(s,inf)) {
      return gps_errno;
    }
    if (strstr(s,"Latitude")) {
      ++n;
    }
  }
  fseek(inf,pos,0);

  if (!(*way=(GPS_PWay *)malloc(n*sizeof(GPS_PWay *)))) {
    return MEMORY_ERROR;
  }
  for (i=0; i<n; ++i) {
    if (!((*way)[i]=GPS_Way_New())) {
      return MEMORY_ERROR;
    }
    (*way)[i]->prot = type;
  }


  for (i=0; i<n; ++i) {
    switch (type) {
    case 100:
      ret = GPS_Input_Get_D100(&((*way)[i]),inf);
      if (ret<0) {
        return gps_errno;
      }
      break;
    case 101:
      ret = GPS_Input_Get_D101(&((*way)[i]),inf);
      if (ret<0) {
        return gps_errno;
      }
      break;
    case 102:
      ret = GPS_Input_Get_D102(&((*way)[i]),inf);
      if (ret<0) {
        return gps_errno;
      }
      break;
    case 103:
      ret = GPS_Input_Get_D103(&((*way)[i]),inf);
      if (ret<0) {
        return gps_errno;
      }
      break;
    case 104:
      ret = GPS_Input_Get_D104(&((*way)[i]),inf);
      if (ret<0) {
        return gps_errno;
      }
      break;
    case 105:
      ret = GPS_Input_Get_D105(&((*way)[i]),inf);
      if (ret<0) {
        return gps_errno;
      }
      break;
    case 106:
      ret = GPS_Input_Get_D106(&((*way)[i]),inf);
      if (ret<0) {
        return gps_errno;
      }
      break;
    case 107:
      ret = GPS_Input_Get_D107(&((*way)[i]),inf);
      if (ret<0) {
        return gps_errno;
      }
      break;
    case 108:
      ret = GPS_Input_Get_D108(&((*way)[i]),inf);
      if (ret<0) {
        return gps_errno;
      }
      break;
    case 109:
      ret = GPS_Input_Get_D109(&((*way)[i]),inf, 109);
      if (ret<0) {
        return gps_errno;
      }
      break;
    case 110:
      ret = GPS_Input_Get_D109(&((*way)[i]),inf, 110);
      if (ret<0) {
        return gps_errno;
      }
      break;
    case 150:
      ret = GPS_Input_Get_D150(&((*way)[i]),inf);
      if (ret<0) {
        return gps_errno;
      }
      break;
    case 151:
      ret = GPS_Input_Get_D151(&((*way)[i]),inf);
      if (ret<0) {
        return gps_errno;
      }
      break;
    case 152:
      ret = GPS_Input_Get_D152(&((*way)[i]),inf);
      if (ret<0) {
        return gps_errno;
      }
      break;
    case 154:
      ret = GPS_Input_Get_D154(&((*way)[i]),inf);
      if (ret<0) {
        return gps_errno;
      }
      break;
    case 155:
      ret = GPS_Input_Get_D155(&((*way)[i]),inf);
      if (ret<0) {
        return gps_errno;
      }
      break;
    default:
      GPS_Error("Input_Get_Waypoints: Unknown protocol");
      return PROTOCOL_ERROR;
    }
  }

  return n;
}



/* @func GPS_Input_Get_Proximity   *************************************
**
** Construct a proximity waypoint array from a file
**
** @param [w] way [GPS_PWay **] pointer to proximity waypoint array
** @param [r] inf [FILE *] stream
**
** @return [int32] number of entries
************************************************************************/

int32 GPS_Input_Get_Proximity(GPS_PWay **way, FILE *inf)
{
  char s[GPS_ARB_LEN];
  int32 n;
  int32 type;
  int32 i;
  long pos;
  int32 ret;
  double f;
  char *p;

  gps_errno = INPUT_ERROR;

  if (!GPS_Input_Read_Line(s,inf)) {
    return gps_errno;
  }
  if (sscanf(s,"Waypoints Type: %d",(int *)&type)!=1) {
    return gps_errno;
  }

  if (!GPS_Input_Read_Line(s,inf)) {
    return gps_errno;
  }
  if (strncmp(s,"Start",5)) {
    return gps_errno;
  }

  pos = ftell(inf);
  n = 0;
  while (strncmp(s,"End",3)) {
    if (!GPS_Input_Read_Line(s,inf)) {
      return gps_errno;
    }
    if (strstr(s,"Latitude")) {
      ++n;
    }
  }
  fseek(inf,pos,0);

  if (!(*way=(GPS_PWay *)malloc(n*sizeof(GPS_PWay *)))) {
    return MEMORY_ERROR;
  }
  for (i=0; i<n; ++i) {
    if (!((*way)[i]=GPS_Way_New())) {
      return MEMORY_ERROR;
    }
    (*way)[i]->prot = type;
  }

  for (i=0; i<n; ++i) {
    switch (type) {
    case 400:
      ret = GPS_Input_Get_D100(&((*way)[i]),inf);
      if (ret<0) {
        return gps_errno;
      }
      break;
    case 101:
      ret = GPS_Input_Get_D101(&((*way)[i]),inf);
      if (ret<0) {
        return gps_errno;
      }
      break;
    case 102:
      ret = GPS_Input_Get_D102(&((*way)[i]),inf);
      if (ret<0) {
        return gps_errno;
      }
      break;
    case 403:
      ret = GPS_Input_Get_D103(&((*way)[i]),inf);
      if (ret<0) {
        return gps_errno;
      }
      break;
    case 104:
      ret = GPS_Input_Get_D104(&((*way)[i]),inf);
      if (ret<0) {
        return gps_errno;
      }
      break;
    case 105:
      ret = GPS_Input_Get_D105(&((*way)[i]),inf);
      if (ret<0) {
        return gps_errno;
      }
      break;
    case 106:
      ret = GPS_Input_Get_D106(&((*way)[i]),inf);
      if (ret<0) {
        return gps_errno;
      }
      break;
    case 107:
      ret = GPS_Input_Get_D107(&((*way)[i]),inf);
      if (ret<0) {
        return gps_errno;
      }
      break;
    case 108:
      ret = GPS_Input_Get_D108(&((*way)[i]),inf);
      if (ret<0) {
        return gps_errno;
      }
      break;
    case 109:
      ret = GPS_Input_Get_D109(&((*way)[i]),inf, 109);
      if (ret<0) {
        return gps_errno;
      }
      break;
    case 110:
      ret = GPS_Input_Get_D109(&((*way)[i]),inf, 110);
      if (ret<0) {
        return gps_errno;
      }
      break;
    case 450:
      ret = GPS_Input_Get_D150(&((*way)[i]),inf);
      if (ret<0) {
        return gps_errno;
      }
      break;
    case 151:
      ret = GPS_Input_Get_D151(&((*way)[i]),inf);
      if (ret<0) {
        return gps_errno;
      }
      break;
    case 152:
      ret = GPS_Input_Get_D152(&((*way)[i]),inf);
      if (ret<0) {
        return gps_errno;
      }
      break;
    case 154:
      ret = GPS_Input_Get_D154(&((*way)[i]),inf);
      if (ret<0) {
        return gps_errno;
      }
      break;
    case 155:
      ret = GPS_Input_Get_D155(&((*way)[i]),inf);
      if (ret<0) {
        return gps_errno;
      }
      break;
    default:
      GPS_Error("Input_Get_Waypoints: Unknown protocol");
      return PROTOCOL_ERROR;
    }

    if (!GPS_Input_Read_Line(s,inf)) {
      return gps_errno;
    }
    p=strchr(s,':');
    if (sscanf(p+1,"%lf",&f)!=1) {
      return gps_errno;
    }
    (*way)[i]->dst = f;
  }

  return n;
}



/* @funcstatic GPS_Input_Get_D100   ************************************
**
** Get a D100 Entry
**
** @param [w] way [GPS_PWay *] pointer to waypoint entry
** @param [r] inf [FILE *] stream
**
** @return [int32] number of entries
************************************************************************/
static int32 GPS_Input_Get_D100(GPS_PWay *way, FILE *inf)
{
  char s[GPS_ARB_LEN];
  char *p;

  double f;

  if (!GPS_Input_Read_Line(s,inf)) {
    return gps_errno;
  }
  GPS_Input_Load_String((*way)->ident,6,s);

  if (!GPS_Input_Read_Line(s,inf)) {
    return gps_errno;
  }
  p=strchr(s,':');
  if (sscanf(p+1,"%lf",&f)!=1) {
    return gps_errno;
  }
  (*way)->lat = f;

  if (!GPS_Input_Read_Line(s,inf)) {
    return gps_errno;
  }
  p=strchr(s,':');
  if (sscanf(p+1,"%lf",&f)!=1) {
    return gps_errno;
  }
  (*way)->lon = f;

  if (!GPS_Input_Read_Line(s,inf)) {
    return gps_errno;
  }
  GPS_Input_Load_String((*way)->cmnt,40,s);

  return 1;
}



/* @funcstatic GPS_Input_Get_D101   ************************************
**
** Get a D101 Entry
**
** @param [w] way [GPS_PWay *] pointer to waypoint entry
** @param [r] inf [FILE *] stream
**
** @return [int32] number of entries
************************************************************************/
static int32 GPS_Input_Get_D101(GPS_PWay *way, FILE *inf)
{
  char s[GPS_ARB_LEN];
  char *p;

  double f;
  int32 d;


  if (!GPS_Input_Read_Line(s,inf)) {
    return gps_errno;
  }
  GPS_Input_Load_String((*way)->ident,6,s);

  if (!GPS_Input_Read_Line(s,inf)) {
    return gps_errno;
  }
  p=strchr(s,':');
  if (sscanf(p+1,"%lf",&f)!=1) {
    return gps_errno;
  }
  (*way)->lat = f;

  if (!GPS_Input_Read_Line(s,inf)) {
    return gps_errno;
  }
  p=strchr(s,':');
  if (sscanf(p+1,"%lf",&f)!=1) {
    return gps_errno;
  }
  (*way)->lon = f;

  if (!GPS_Input_Read_Line(s,inf)) {
    return gps_errno;
  }
  GPS_Input_Load_String((*way)->cmnt,40,s);

  if (!GPS_Input_Read_Line(s,inf)) {
    return gps_errno;
  }
  p=strchr(s,':');
  if (sscanf(p+1,"%d",(int *)&d)!=1) {
    return gps_errno;
  }
  (*way)->smbl = d;

  return 1;
}



/* @funcstatic GPS_Input_Get_D102   ************************************
**
** Get a D102 Entry
**
** @param [w] way [GPS_PWay *] pointer to waypoint entry
** @param [r] inf [FILE *] stream
**
** @return [int32] number of entries
************************************************************************/
static int32 GPS_Input_Get_D102(GPS_PWay *way, FILE *inf)
{
  return GPS_Input_Get_D101(way,inf);
}



/* @funcstatic GPS_Input_Get_D103   ************************************
**
** Get a D103 Entry
**
** @param [w] way [GPS_PWay *] pointer to waypoint entry
** @param [r] inf [FILE *] stream
**
** @return [int32] number of entries
************************************************************************/
static int32 GPS_Input_Get_D103(GPS_PWay *way, FILE *inf)
{
  char s[GPS_ARB_LEN];
  char *p;

  double f;
  int32 d;


  if (!GPS_Input_Read_Line(s,inf)) {
    return gps_errno;
  }
  GPS_Input_Load_String((*way)->ident,6,s);

  if (!GPS_Input_Read_Line(s,inf)) {
    return gps_errno;
  }
  p=strchr(s,':');
  if (sscanf(p+1,"%lf",&f)!=1) {
    return gps_errno;
  }
  (*way)->lat = f;

  if (!GPS_Input_Read_Line(s,inf)) {
    return gps_errno;
  }
  p=strchr(s,':');
  if (sscanf(p+1,"%lf",&f)!=1) {
    return gps_errno;
  }
  (*way)->lon = f;

  if (!GPS_Input_Read_Line(s,inf)) {
    return gps_errno;
  }
  GPS_Input_Load_String((*way)->cmnt,40,s);

  if (!GPS_Input_Read_Line(s,inf)) {
    return gps_errno;
  }
  p=strchr(s,':');
  if (sscanf(p+1,"%d",(int *)&d)!=1) {
    return gps_errno;
  }
  (*way)->smbl = d;

  if (!GPS_Input_Read_Line(s,inf)) {
    return gps_errno;
  }
  p=strchr(s,':');
  if (sscanf(p+1,"%d",(int *)&d)!=1) {
    return gps_errno;
  }
  (*way)->dspl = d;

  return 1;
}



/* @funcstatic GPS_Input_Get_D104   ************************************
**
** Get a D104 Entry
**
** @param [w] way [GPS_PWay *] pointer to waypoint entry
** @param [r] inf [FILE *] stream
**
** @return [int32] number of entries
************************************************************************/
static int32 GPS_Input_Get_D104(GPS_PWay *way, FILE *inf)
{
  return GPS_Input_Get_D103(way,inf);
}



/* @funcstatic GPS_Input_Get_D105   ************************************
**
** Get a D105 Entry
**
** @param [w] way [GPS_PWay *] pointer to waypoint entry
** @param [r] inf [FILE *] stream
**
** @return [int32] number of entries
************************************************************************/
static int32 GPS_Input_Get_D105(GPS_PWay *way, FILE *inf)
{
  char s[GPS_ARB_LEN];
  char *p;

  double f;
  int32 d;


  if (!GPS_Input_Read_Line(s,inf)) {
    return gps_errno;
  }
  p=strchr(s,':');
  if (sscanf(p+1,"%lf",&f)!=1) {
    return gps_errno;
  }
  (*way)->lat = f;

  if (!GPS_Input_Read_Line(s,inf)) {
    return gps_errno;
  }
  p=strchr(s,':');
  if (sscanf(p+1,"%lf",&f)!=1) {
    return gps_errno;
  }
  (*way)->lon = f;

  if (!GPS_Input_Read_Line(s,inf)) {
    return gps_errno;
  }
  p=strchr(s,':');
  if (sscanf(p+1,"%d",(int *)&d)!=1) {
    return gps_errno;
  }
  (*way)->smbl = d;

  if (!GPS_Input_Read_Line(s,inf)) {
    return gps_errno;
  }
  GPS_Input_Load_Strnull((*way)->wpt_ident,s);

  return 1;
}



/* @funcstatic GPS_Input_Get_D106   ************************************
**
** Get a D106 Entry
**
** @param [w] way [GPS_PWay *] pointer to waypoint entry
** @param [r] inf [FILE *] stream
**
** @return [int32] number of entries
************************************************************************/
static int32 GPS_Input_Get_D106(GPS_PWay *way, FILE *inf)
{
  char s[GPS_ARB_LEN];
  char *p;

  double f;
  int32 d;

  if (!GPS_Input_Read_Line(s,inf)) {
    return gps_errno;
  }
  p=strchr(s,':');
  if (sscanf(p+1,"%d",(int *)&d)!=1) {
    return gps_errno;
  }
  (*way)->wpt_class = d;

  if (!GPS_Input_Read_Line(s,inf)) {
    return gps_errno;
  }
  GPS_Input_Load_String((char *)(*way)->subclass,13,s);


  if (!GPS_Input_Read_Line(s,inf)) {
    return gps_errno;
  }
  GPS_Input_Load_String((*way)->ident,6,s);

  if (!GPS_Input_Read_Line(s,inf)) {
    return gps_errno;
  }
  p=strchr(s,':');
  if (sscanf(p+1,"%lf",&f)!=1) {
    return gps_errno;
  }
  (*way)->lat = f;

  if (!GPS_Input_Read_Line(s,inf)) {
    return gps_errno;
  }
  p=strchr(s,':');
  if (sscanf(p+1,"%lf",&f)!=1) {
    return gps_errno;
  }
  (*way)->lon = f;

  if (!GPS_Input_Read_Line(s,inf)) {
    return gps_errno;
  }
  p=strchr(s,':');
  if (sscanf(p+1,"%d",(int *)&d)!=1) {
    return gps_errno;
  }
  (*way)->smbl = d;

  if (!GPS_Input_Read_Line(s,inf)) {
    return gps_errno;
  }
  GPS_Input_Load_Strnull((*way)->wpt_ident,s);

  if (!GPS_Input_Read_Line(s,inf)) {
    return gps_errno;
  }
  GPS_Input_Load_Strnull((*way)->lnk_ident,s);

  return 1;
}



/* @funcstatic GPS_Input_Get_D107   ************************************
**
** Get a D107 Entry
**
** @param [w] way [GPS_PWay *] pointer to waypoint entry
** @param [r] inf [FILE *] stream
**
** @return [int32] number of entries
************************************************************************/
static int32 GPS_Input_Get_D107(GPS_PWay *way, FILE *inf)
{
  char s[GPS_ARB_LEN];
  char *p;

  int32 d;
  int32 ret;

  if ((ret=GPS_Input_Get_D103(way,inf))<0) {
    return ret;
  }

  if (!GPS_Input_Read_Line(s,inf)) {
    return gps_errno;
  }
  p=strchr(s,':');
  if (sscanf(p+1,"%d",(int *)&d)!=1) {
    return gps_errno;
  }
  (*way)->colour = d;

  return 1;
}



/* @funcstatic GPS_Input_Get_D108   ************************************
**
** Get a D108 Entry
**
** @param [w] way [GPS_PWay *] pointer to waypoint entry
** @param [r] inf [FILE *] stream
**
** @return [int32] number of entries
************************************************************************/
static int32 GPS_Input_Get_D108(GPS_PWay *way, FILE *inf)
{
  char s[GPS_ARB_LEN];
  char *p;
  double f;
  int32 d;
  int32 xc;


  if (!GPS_Input_Read_Line(s,inf)) {
    return gps_errno;
  }
  GPS_Input_Load_Strnull((*way)->ident,s);

  if (!GPS_Input_Read_Line(s,inf)) {
    return gps_errno;
  }
  p=strchr(s,':');
  if (sscanf(p+1,"%lf",&f)!=1) {
    return gps_errno;
  }
  (*way)->lat = f;

  if (!GPS_Input_Read_Line(s,inf)) {
    return gps_errno;
  }
  p=strchr(s,':');
  if (sscanf(p+1,"%lf",&f)!=1) {
    return gps_errno;
  }
  (*way)->lon = f;

  if (!GPS_Input_Read_Line(s,inf)) {
    return gps_errno;
  }
  p=strchr(s,':');
  if (sscanf(p+1,"%d",(int *)&d)!=1) {
    return gps_errno;
  }
  (*way)->colour = d;

  if (!GPS_Input_Read_Line(s,inf)) {
    return gps_errno;
  }
  p=strchr(s,':');
  if (sscanf(p+1,"%d",(int *)&d)!=1) {
    return gps_errno;
  }
  (*way)->dspl = d;

  if (!GPS_Input_Read_Line(s,inf)) {
    return gps_errno;
  }
  p=strchr(s,':');
  if (sscanf(p+1,"%d",(int *)&d)!=1) {
    return gps_errno;
  }
  (*way)->smbl = d;

  if (!GPS_Input_Read_Line(s,inf)) {
    return gps_errno;
  }
  p=strchr(s,':');
  if (sscanf(p+1,"%d",(int *)&d)!=1) {
    return gps_errno;
  }
  (*way)->alt = d;

  if (!GPS_Input_Read_Line(s,inf)) {
    return gps_errno;
  }
  p=strchr(s,':');
  if (sscanf(p+1,"%lf",&f)!=1) {
    return gps_errno;
  }
  (*way)->dpth = f;

  if (!GPS_Input_Read_Line(s,inf)) {
    return gps_errno;
  }
  GPS_Input_Load_String((*way)->state,2,s);

  if (!GPS_Input_Read_Line(s,inf)) {
    return gps_errno;
  }
  GPS_Input_Load_String((*way)->cc,2,s);

  if (!GPS_Input_Read_Line(s,inf)) {
    return gps_errno;
  }
  p=strchr(s,':');
  if (sscanf(p+1,"%d",(int *)&d)!=1) {
    return gps_errno;
  }
  (*way)->wpt_class = d;
  xc = d;

  if (xc>=0x80 && xc<=0x85) {
    if (!GPS_Input_Read_Line(s,inf)) {
      return gps_errno;
    }
    GPS_Input_Load_String((char *)(*way)->subclass,18,s);
  } else {
    GPS_Util_Put_Short((*way)->subclass,0);
    GPS_Util_Put_Int((*way)->subclass+2,0);
    GPS_Util_Put_Uint((*way)->subclass+6,0xffffffff);
    GPS_Util_Put_Uint((*way)->subclass+10,0xffffffff);
    GPS_Util_Put_Uint((*way)->subclass+14,0xffffffff);
  }

  if (!xc) {
    if (!GPS_Input_Read_Line(s,inf)) {
      return gps_errno;
    }
    GPS_Input_Load_Strnull((*way)->cmnt,s);
  }

  if (xc>=0x40 && xc<=0x46) {
    if (!GPS_Input_Read_Line(s,inf)) {
      return gps_errno;
    }
    GPS_Input_Load_Strnull((*way)->facility,s);
    if (!GPS_Input_Read_Line(s,inf)) {
      return gps_errno;
    }
    GPS_Input_Load_Strnull((*way)->city,s);
  }

  if (xc==0x83) {
    if (!GPS_Input_Read_Line(s,inf)) {
      return gps_errno;
    }
    GPS_Input_Load_Strnull((*way)->addr,s);
  }

  if (xc==0x82) {
    if (!GPS_Input_Read_Line(s,inf)) {
      return gps_errno;
    }
    GPS_Input_Load_Strnull((*way)->cross_road,s);
  }

  return 1;
}



/* @funcstatic GPS_Input_Get_D109   ************************************
**
** Get a D109 Entry
**
** @param [w] way [GPS_PWay *] pointer to waypoint entry
** @param [r] inf [FILE *] stream
**
** @return [int32] number of entries
** D109's and D110's are so similar, we handle both with the same function.
************************************************************************/
static int32 GPS_Input_Get_D109(GPS_PWay *way, FILE *inf, int protonum)
{
  char s[GPS_ARB_LEN];
  char *p;
  double f;
  int32 d;
  int32 xc;


  if (!GPS_Input_Read_Line(s,inf)) {
    return gps_errno;
  }
  GPS_Input_Load_Strnull((*way)->ident,s);

  if (!GPS_Input_Read_Line(s,inf)) {
    return gps_errno;
  }
  p=strchr(s,':');
  if (sscanf(p+1,"%lf",&f)!=1) {
    return gps_errno;
  }
  (*way)->lat = f;

  if (!GPS_Input_Read_Line(s,inf)) {
    return gps_errno;
  }
  p=strchr(s,':');
  if (sscanf(p+1,"%lf",&f)!=1) {
    return gps_errno;
  }
  (*way)->lon = f;

  if (!GPS_Input_Read_Line(s,inf)) {
    return gps_errno;
  }
  p=strchr(s,':');
  if (sscanf(p+1,"%d",(int *)&d)!=1) {
    return gps_errno;
  }
  (*way)->colour = d;

  if (!GPS_Input_Read_Line(s,inf)) {
    return gps_errno;
  }
  p=strchr(s,':');
  if (sscanf(p+1,"%d",(int *)&d)!=1) {
    return gps_errno;
  }
  (*way)->dspl = d;

  if (!GPS_Input_Read_Line(s,inf)) {
    return gps_errno;
  }
  p=strchr(s,':');
  if (sscanf(p+1,"%d",(int *)&d)!=1) {
    return gps_errno;
  }
  (*way)->smbl = d;

  if (!GPS_Input_Read_Line(s,inf)) {
    return gps_errno;
  }
  p=strchr(s,':');
  if (sscanf(p+1,"%d",(int *)&d)!=1) {
    return gps_errno;
  }
  (*way)->alt = d;

  if (!GPS_Input_Read_Line(s,inf)) {
    return gps_errno;
  }
  p=strchr(s,':');
  if (sscanf(p+1,"%lf",&f)!=1) {
    return gps_errno;
  }
  (*way)->dpth = f;

  if (!GPS_Input_Read_Line(s,inf)) {
    return gps_errno;
  }
  GPS_Input_Load_String((*way)->state,2,s);

  if (!GPS_Input_Read_Line(s,inf)) {
    return gps_errno;
  }
  GPS_Input_Load_String((*way)->cc,2,s);

  if (!GPS_Input_Read_Line(s,inf)) {
    return gps_errno;
  }
  p=strchr(s,':');
  if (sscanf(p+1,"%d",(int *)&d)!=1) {
    return gps_errno;
  }
  (*way)->wpt_class = d;
  xc = d;

  if (xc>=0x80 && xc<=0x85) {
    if (!GPS_Input_Read_Line(s,inf)) {
      return gps_errno;
    }
    GPS_Input_Load_String((char *)(*way)->subclass,18,s);
  } else {
    GPS_Util_Put_Short((*way)->subclass,0);
    GPS_Util_Put_Int((*way)->subclass+2,0);
    GPS_Util_Put_Uint((*way)->subclass+6,0xffffffff);
    GPS_Util_Put_Uint((*way)->subclass+10,0xffffffff);
    GPS_Util_Put_Uint((*way)->subclass+14,0xffffffff);
  }

  if (!xc) {
    if (!GPS_Input_Read_Line(s,inf)) {
      return gps_errno;
    }
    GPS_Input_Load_Strnull((*way)->cmnt,s);
  }

  if (xc>=0x40 && xc<=0x46) {
    if (!GPS_Input_Read_Line(s,inf)) {
      return gps_errno;
    }
    GPS_Input_Load_Strnull((*way)->facility,s);
    if (!GPS_Input_Read_Line(s,inf)) {
      return gps_errno;
    }
    GPS_Input_Load_Strnull((*way)->city,s);
  }

  if (xc==0x83) {
    if (!GPS_Input_Read_Line(s,inf)) {
      return gps_errno;
    }
    GPS_Input_Load_Strnull((*way)->addr,s);
  }

  if (xc==0x82) {
    if (!GPS_Input_Read_Line(s,inf)) {
      return gps_errno;
    }
    GPS_Input_Load_Strnull((*way)->cross_road,s);
  }

  return 1;
}


/* @funcstatic GPS_Input_Get_D150   ************************************
**
** Get a D150 Entry
**
** @param [w] way [GPS_PWay *] pointer to waypoint entry
** @param [r] inf [FILE *] stream
**
** @return [int32] number of entries
************************************************************************/
static int32 GPS_Input_Get_D150(GPS_PWay *way, FILE *inf)
{
  char s[GPS_ARB_LEN];
  char *p;

  double f;
  int32 d;
  int32 cl=0;


  if (!GPS_Input_Read_Line(s,inf)) {
    return gps_errno;
  }
  GPS_Input_Load_String((*way)->ident,6,s);

  if (!GPS_Input_Read_Line(s,inf)) {
    return gps_errno;
  }
  p=strchr(s,':');
  if (sscanf(p+1,"%lf",&f)!=1) {
    return gps_errno;
  }
  (*way)->lat = f;

  if (!GPS_Input_Read_Line(s,inf)) {
    return gps_errno;
  }
  p=strchr(s,':');
  if (sscanf(p+1,"%lf",&f)!=1) {
    return gps_errno;
  }
  (*way)->lon = f;

  if (!GPS_Input_Read_Line(s,inf)) {
    return gps_errno;
  }
  GPS_Input_Load_String((*way)->cmnt,40,s);

  if (!GPS_Input_Read_Line(s,inf)) {
    return gps_errno;
  }
  p=strchr(s,':');
  if (sscanf(p+1,"%d",(int *)&d)!=1) {
    return gps_errno;
  }
  (*way)->wpt_class = cl = d;

  if (cl != 4) {
    if (!GPS_Input_Read_Line(s,inf)) {
      return gps_errno;
    }
    GPS_Input_Load_String((*way)->cc,2,s);

    if (!GPS_Input_Read_Line(s,inf)) {
      return gps_errno;
    }
    GPS_Input_Load_String((*way)->city,24,s);

    if (!GPS_Input_Read_Line(s,inf)) {
      return gps_errno;
    }
    GPS_Input_Load_String((*way)->state,2,s);

    if (!GPS_Input_Read_Line(s,inf)) {
      return gps_errno;
    }
    GPS_Input_Load_String((*way)->name,30,s);
  }

  if (!cl) {
    if (!GPS_Input_Read_Line(s,inf)) {
      return gps_errno;
    }
    p=strchr(s,':');
    if (sscanf(p+1,"%d",(int *)&d)!=1) {
      return gps_errno;
    }
    (*way)->alt = d;
  }

  return 1;
}



/* @funcstatic GPS_Input_Get_D151   ************************************
**
** Get a D151 Entry
**
** @param [w] way [GPS_PWay *] pointer to waypoint entry
** @param [r] inf [FILE *] stream
**
** @return [int32] number of entries
************************************************************************/
static int32 GPS_Input_Get_D151(GPS_PWay *way, FILE *inf)
{
  char s[GPS_ARB_LEN];
  char *p;

  double f;
  int32 d;
  int32 cl=0;


  if (!GPS_Input_Read_Line(s,inf)) {
    return gps_errno;
  }
  GPS_Input_Load_String((*way)->ident,6,s);

  if (!GPS_Input_Read_Line(s,inf)) {
    return gps_errno;
  }
  p=strchr(s,':');
  if (sscanf(p+1,"%lf",&f)!=1) {
    return gps_errno;
  }
  (*way)->lat = f;

  if (!GPS_Input_Read_Line(s,inf)) {
    return gps_errno;
  }
  p=strchr(s,':');
  if (sscanf(p+1,"%lf",&f)!=1) {
    return gps_errno;
  }
  (*way)->lon = f;

  if (!GPS_Input_Read_Line(s,inf)) {
    return gps_errno;
  }
  GPS_Input_Load_String((*way)->cmnt,40,s);

  if (!GPS_Input_Read_Line(s,inf)) {
    return gps_errno;
  }
  p=strchr(s,':');
  if (sscanf(p+1,"%d",(int *)&d)!=1) {
    return gps_errno;
  }
  (*way)->wpt_class = cl = d;

  if (cl != 2) {
    if (!GPS_Input_Read_Line(s,inf)) {
      return gps_errno;
    }
    GPS_Input_Load_String((*way)->cc,2,s);

    if (!GPS_Input_Read_Line(s,inf)) {
      return gps_errno;
    }
    GPS_Input_Load_String((*way)->city,24,s);

    if (!GPS_Input_Read_Line(s,inf)) {
      return gps_errno;
    }
    GPS_Input_Load_String((*way)->state,2,s);

    if (!GPS_Input_Read_Line(s,inf)) {
      return gps_errno;
    }
    GPS_Input_Load_String((*way)->name,30,s);
  }

  if (!cl) {
    if (!GPS_Input_Read_Line(s,inf)) {
      return gps_errno;
    }
    p=strchr(s,':');
    if (sscanf(p+1,"%d",(int *)&d)!=1) {
      return gps_errno;
    }
    (*way)->alt = d;
  }

  return 1;
}


/* @funcstatic GPS_Input_Get_D152   ************************************
**
** Get a D152 Entry
**
** @param [w] way [GPS_PWay *] pointer to waypoint entry
** @param [r] inf [FILE *] stream
**
** @return [int32] number of entries
************************************************************************/
static int32 GPS_Input_Get_D152(GPS_PWay *way, FILE *inf)
{
  return GPS_Input_Get_D150(way,inf);
}



/* @funcstatic GPS_Input_Get_D154   ************************************
**
** Get a D154 Entry
**
** @param [w] way [GPS_PWay *] pointer to waypoint entry
** @param [r] inf [FILE *] stream
**
** @return [int32] number of entries
************************************************************************/
static int32 GPS_Input_Get_D154(GPS_PWay *way, FILE *inf)
{
  char s[GPS_ARB_LEN];
  char *p;

  double f;
  int32 d;
  int32 cl=0;


  if (!GPS_Input_Read_Line(s,inf)) {
    return gps_errno;
  }
  GPS_Input_Load_String((*way)->ident,6,s);

  if (!GPS_Input_Read_Line(s,inf)) {
    return gps_errno;
  }
  p=strchr(s,':');
  if (sscanf(p+1,"%lf",&f)!=1) {
    return gps_errno;
  }
  (*way)->lat = f;

  if (!GPS_Input_Read_Line(s,inf)) {
    return gps_errno;
  }
  p=strchr(s,':');
  if (sscanf(p+1,"%lf",&f)!=1) {
    return gps_errno;
  }
  (*way)->lon = f;

  if (!GPS_Input_Read_Line(s,inf)) {
    return gps_errno;
  }
  GPS_Input_Load_String((*way)->cmnt,40,s);

  if (!GPS_Input_Read_Line(s,inf)) {
    return gps_errno;
  }
  p=strchr(s,':');
  if (sscanf(p+1,"%d",(int *)&d)!=1) {
    return gps_errno;
  }
  (*way)->smbl = d;

  if (!GPS_Input_Read_Line(s,inf)) {
    return gps_errno;
  }
  p=strchr(s,':');
  if (sscanf(p+1,"%d",(int *)&d)!=1) {
    return gps_errno;
  }
  (*way)->wpt_class = cl = d;

  if (cl != 4 && cl != 8) {
    if (!GPS_Input_Read_Line(s,inf)) {
      return gps_errno;
    }
    GPS_Input_Load_String((*way)->cc,2,s);

    if (!GPS_Input_Read_Line(s,inf)) {
      return gps_errno;
    }
    GPS_Input_Load_String((*way)->city,24,s);

    if (!GPS_Input_Read_Line(s,inf)) {
      return gps_errno;
    }
    GPS_Input_Load_String((*way)->state,2,s);

    if (!GPS_Input_Read_Line(s,inf)) {
      return gps_errno;
    }
    GPS_Input_Load_String((*way)->name,30,s);
  }

  if (!cl) {
    if (!GPS_Input_Read_Line(s,inf)) {
      return gps_errno;
    }
    p=strchr(s,':');
    if (sscanf(p+1,"%d",(int *)&d)!=1) {
      return gps_errno;
    }
    (*way)->alt = d;
  }

  return 1;
}



/* @funcstatic GPS_Input_Get_D155   ************************************
**
** Get a D155 Entry
**
** @param [w] way [GPS_PWay *] pointer to waypoint entry
** @param [r] inf [FILE *] stream
**
** @return [int32] number of entries
************************************************************************/
static int32 GPS_Input_Get_D155(GPS_PWay *way, FILE *inf)
{
  char s[GPS_ARB_LEN];
  char *p;

  double f;
  int32 d;
  int32 cl=0;


  if (!GPS_Input_Read_Line(s,inf)) {
    return gps_errno;
  }
  GPS_Input_Load_String((*way)->ident,6,s);

  if (!GPS_Input_Read_Line(s,inf)) {
    return gps_errno;
  }
  p=strchr(s,':');
  if (sscanf(p+1,"%lf",&f)!=1) {
    return gps_errno;
  }
  (*way)->lat = f;

  if (!GPS_Input_Read_Line(s,inf)) {
    return gps_errno;
  }
  p=strchr(s,':');
  if (sscanf(p+1,"%lf",&f)!=1) {
    return gps_errno;
  }
  (*way)->lon = f;

  if (!GPS_Input_Read_Line(s,inf)) {
    return gps_errno;
  }
  GPS_Input_Load_String((*way)->cmnt,40,s);

  if (!GPS_Input_Read_Line(s,inf)) {
    return gps_errno;
  }
  p=strchr(s,':');
  if (sscanf(p+1,"%d",(int *)&d)!=1) {
    return gps_errno;
  }
  (*way)->smbl = d;

  if (!GPS_Input_Read_Line(s,inf)) {
    return gps_errno;
  }
  p=strchr(s,':');
  if (sscanf(p+1,"%d",(int *)&d)!=1) {
    return gps_errno;
  }
  (*way)->dspl = d;

  if (!GPS_Input_Read_Line(s,inf)) {
    return gps_errno;
  }
  p=strchr(s,':');
  if (sscanf(p+1,"%d",(int *)&d)!=1) {
    return gps_errno;
  }
  (*way)->wpt_class = cl = d;

  if (cl != 4 && cl != 8) {
    if (!GPS_Input_Read_Line(s,inf)) {
      return gps_errno;
    }
    GPS_Input_Load_String((*way)->cc,2,s);

    if (!GPS_Input_Read_Line(s,inf)) {
      return gps_errno;
    }
    GPS_Input_Load_String((*way)->city,24,s);

    if (!GPS_Input_Read_Line(s,inf)) {
      return gps_errno;
    }
    GPS_Input_Load_String((*way)->state,2,s);

    if (!GPS_Input_Read_Line(s,inf)) {
      return gps_errno;
    }
    GPS_Input_Load_String((*way)->name,30,s);
  }

  if (!cl) {
    if (!GPS_Input_Read_Line(s,inf)) {
      return gps_errno;
    }
    p=strchr(s,':');
    if (sscanf(p+1,"%d",(int *)&d)!=1) {
      return gps_errno;
    }
    (*way)->alt = d;
  }

  return 1;
}



/* @func GPS_Input_Get_Track *******************************************
**
** Construct a travk array from a file
**
** @param [w] trk [GPS_PTrack **] pointer to track array
** @param [r] inf [FILE *] stream
**
** @return [int32] number of entries
************************************************************************/

int32 GPS_Input_Get_Track(GPS_PTrack **trk, FILE *inf)
{
  char s[GPS_ARB_LEN];
  int32 n;
  int32 i;
  long pos;
  int32 a;
  int32 d;

  gps_errno = INPUT_ERROR;

  if (!GPS_Input_Read_Line(s,inf)) {
    return gps_errno;
  }
  if (strncmp(s,"Track log",9)) {
    return gps_errno;
  }

  if (sscanf(s,"Track log %d%d",(int *)&a,(int *)&d)!=2) {
    return INPUT_ERROR;
  }


  if (!GPS_Input_Read_Line(s,inf)) {
    return gps_errno;
  }
  if (strncmp(s,"Start",5)) {
    return gps_errno;
  }

  pos = ftell(inf);
  n = 0;
  while (strncmp(s,"End",3)) {
    if (!GPS_Input_Read_Line(s,inf)) {
      return gps_errno;
    }
    if (strstr(s,"Latitude")) {
      ++n;
    }
    if (strstr(s,"Header")) {
      ++n;
    }
  }
  fseek(inf,pos,0);


  if (!(*trk=(GPS_PTrack *)malloc(n*sizeof(GPS_PTrack *)))) {
    return MEMORY_ERROR;
  }
  for (i=0; i<n; ++i)
    if (!((*trk)[i]=GPS_Track_New())) {
      return MEMORY_ERROR;
    }


  switch (a) {
  case pA300:
    break;
  case pA301:
    return GPS_Input_Get_Track301(trk, inf, d, n);
  default:
    return INPUT_ERROR;
  }



  for (i=0; i<n; ++i) {
    if (!GPS_Input_Read_Line(s,inf)) {
      return gps_errno;
    }

    if (!strcmp(s,"New track")) {
      (*trk)[i]->tnew=1;
      if (!GPS_Input_Read_Line(s,inf)) {
        return gps_errno;
      }
    } else {
      (*trk)[i]->tnew=0;
    }

    switch (d) {
    case pD300:
      GPS_Input_Get_D300(&((*trk)[i]),inf,s);
      break;
    case pD301:
      GPS_Input_Get_D301(&((*trk)[i]),inf,s);
      break;
    default:
      return PROTOCOL_ERROR;
    }
  }

  return n;
}



/* @funcstatic GPS_Input_Get_Track301 **********************************
**
** Construct a travk array from a file
**
** @param [w] trk [GPS_PTrack **] pointer to track array
** @param [r] inf [FILE *] stream
** @param [r] type [int32] data type
** @param [r] n [int32] number of tracks
**
** @return [int32] number of entries
************************************************************************/

static int32 GPS_Input_Get_Track301(GPS_PTrack **trk, FILE *inf, int32 type,
                                    int32 n)
{
  char s[GPS_ARB_LEN];
  int32 i;
  char *p;
  int32 x;

  for (i=0; i<n; ++i) {
    if (!GPS_Input_Read_Line(s,inf)) {
      return gps_errno;
    }
    if (!strcmp(s,"Header")) {
      (*trk)[i]->ishdr = 1;

      if (!GPS_Input_Read_Line(s,inf)) {
        return gps_errno;
      }
      GPS_Input_Load_Strnull((*trk)[i]->trk_ident,s);

      if (!GPS_Input_Read_Line(s,inf)) {
        return gps_errno;
      }
      p=strchr(s,':');
      if (sscanf(p+1,"%d",(int *)&x)!=1) {
        return INPUT_ERROR;
      }
      (*trk)[i]->dspl = x;

      if (!GPS_Input_Read_Line(s,inf)) {
        return gps_errno;
      }
      p=strchr(s,':');
      if (sscanf(p+1,"%d",(int *)&x)!=1) {
        return INPUT_ERROR;
      }
      (*trk)[i]->colour = x;

      continue;
    }

    (*trk)[i]->ishdr = 0;

    if (!strcmp(s,"New track")) {
      (*trk)[i]->tnew=1;
      if (!GPS_Input_Read_Line(s,inf)) {
        return gps_errno;
      }
    } else {
      (*trk)[i]->tnew=0;
    }

    switch (type) {
    case pD300:
      GPS_Input_Get_D300(&((*trk)[i]),inf,s);
      break;
    case pD301:
      GPS_Input_Get_D301(&((*trk)[i]),inf,s);
      break;
    default:
      return PROTOCOL_ERROR;
    }
  }

  return n;
}



/* @funcstatic GPS_Input_Get_D300 **********************************
**
** Construct a track from a file
**
** @param [w] trk [GPS_PTrack *] pointer to track
** @param [r] inf [FILE *] stream
** @param [w] s [char *] input line
**
** @return [int32] number of entries
************************************************************************/

static int32 GPS_Input_Get_D300(GPS_PTrack *trk, FILE *inf, char *s)
{
  char *p;
  double f;

  p=strchr(s,':');
  if (sscanf(p+1,"%lf",&f)!=1) {
    return gps_errno;
  }
  (*trk)->lat = f;

  if (!GPS_Input_Read_Line(s,inf)) {
    return gps_errno;
  }
  p=strchr(s,':');
  if (sscanf(p+1,"%lf",&f)!=1) {
    return gps_errno;
  }
  (*trk)->lon = f;

  if (!GPS_Input_Read_Line(s,inf)) {
    return gps_errno;
  }
  (*trk)->Time = 0;

  return 1;
}



/* @funcstatic GPS_Input_Get_D301 **********************************
**
** Construct a track from a file
**
** @param [w] trk [GPS_PTrack *] pointer to track
** @param [r] inf [FILE *] stream
** @param [w] s [char *] input line
**
** @return [int32] number of entries
************************************************************************/

static int32 GPS_Input_Get_D301(GPS_PTrack *trk, FILE *inf, char *s)
{
  char *p;
  double f;

  p=strchr(s,':');
  if (sscanf(p+1,"%lf",&f)!=1) {
    return gps_errno;
  }
  (*trk)->lat = f;

  if (!GPS_Input_Read_Line(s,inf)) {
    return gps_errno;
  }
  p=strchr(s,':');
  if (sscanf(p+1,"%lf",&f)!=1) {
    return gps_errno;
  }
  (*trk)->lon = f;

  if (!GPS_Input_Read_Line(s,inf)) {
    return gps_errno;
  }
  (*trk)->Time = 0;

  if (!GPS_Input_Read_Line(s,inf)) {
    return gps_errno;
  }
  p=strchr(s,':');
  if (sscanf(p+1,"%lf",&f)!=1) {
    return gps_errno;
  }
  (*trk)->alt = f;

  if (!GPS_Input_Read_Line(s,inf)) {
    return gps_errno;
  }
  p=strchr(s,':');
  if (sscanf(p+1,"%lf",&f)!=1) {
    return gps_errno;
  }
  (*trk)->dpth = f;

  return 1;
}



/* @func GPS_Input_Get_Route *******************************************
**
** Construct a route array from a file
**
** @param [w] way [GPS_PWay **] pointer to waypoint array
** @param [r] inf [FILE *] stream
**
** @return [int32] number of entries
************************************************************************/

int32 GPS_Input_Get_Route(GPS_PWay **way, FILE *inf)
{
  char s[GPS_ARB_LEN];
  int32 n;
  int32 type;
  int32 rtype=0;
  int32 atype=0;
  int32 i;
  long pos;
  int32 ret;
  char *p;
  int32 d;

  gps_errno = INPUT_ERROR;


  if (!GPS_Input_Read_Line(s,inf)) {
    return gps_errno;
  }
  if (sscanf(s,"Route log %d",(int *)&atype)!=1) {
    return gps_errno;
  }
  if (!GPS_Input_Read_Line(s,inf)) {
    return gps_errno;
  }

  switch (atype) {
  case pA200:
    break;
  case pA201:
    return GPS_Input_Get_Route201(way,inf);
  default:
    GPS_Error("GPS_Input_Get_Route: Unknown protocol");
    return PROTOCOL_ERROR;
  }


  if (!GPS_Input_Read_Line(s,inf)) {
    return gps_errno;
  }
  if (sscanf(s,"Route Type: %d",(int *)&rtype)!=1) {
    return gps_errno;
  }

  if (!GPS_Input_Read_Line(s,inf)) {
    return gps_errno;
  }
  if (sscanf(s,"Waypoints Type: %d",(int *)&type)!=1) {
    return gps_errno;
  }

  if (!GPS_Input_Read_Line(s,inf)) {
    return gps_errno;
  }
  if (strncmp(s,"Start",5)) {
    return gps_errno;
  }

  pos = ftell(inf);
  n = 1;
  while (strncmp(s,"End",3)) {
    if (!GPS_Input_Read_Line(s,inf)) {
      return gps_errno;
    }
    if (strstr(s,"Latitude") || strstr(s,"Route")) {
      ++n;
    }
  }
  fseek(inf,0L,0);


  if (!(*way=(GPS_PWay *)malloc(n*sizeof(GPS_PWay *)))) {
    return MEMORY_ERROR;
  }
  for (i=0; i<n; ++i) {
    if (!((*way)[i]=GPS_Way_New())) {
      return MEMORY_ERROR;
    }
    (*way)[i]->prot = type;
  }

  if (!GPS_Input_Read_Line(s,inf)) {
    return gps_errno;
  }
  if (!GPS_Input_Read_Line(s,inf)) {
    return gps_errno;
  }
  if (!GPS_Input_Read_Line(s,inf)) {
    return gps_errno;
  }

  for (i=0; i<n; ++i) {
    (*way)[i]->rte_prot = rtype;
    (*way)[i]->islink = 0;
    if (strstr(s,"Route")) {
      (*way)[i]->isrte = 1;
      switch (rtype) {
      case 200:
        p = strchr(s,':');
        p = strchr(p+1,':');
        if (sscanf(p+1,"%d",(int *)&d)!=1) {
          return gps_error;
        }
        (*way)[i]->rte_num=d;
        break;
      case 201:
        p = strchr(s,':');
        p = strchr(p+1,':');
        if (sscanf(p+1,"%d",(int *)&d)!=1) {
          return gps_error;
        }
        (*way)[i]->rte_num=d;
        ++p;
        GPS_Input_Load_String((*way)[i]->rte_cmnt,20,p+2);
        break;
      case 202:
        p = strchr(s,':');
        p = strchr(p+1,':');
        GPS_Input_Load_Strnull((*way)[i]->rte_ident,p+1);
        break;
      }
      if (!GPS_Input_Read_Line(s,inf)) {
        return gps_errno;
      }
      if (!GPS_Input_Read_Line(s,inf)) {
        return gps_errno;
      }
      continue;
    } else {
      (*way)[i]->isrte=0;
    }

    if (strstr(s,"End")) {
      if (!GPS_Input_Read_Line(s,inf)) {
        return gps_errno;
      }

      continue;
    }


    switch (type) {
    case 100:
      ret = GPS_Input_Get_D100(&((*way)[i]),inf);
      if (ret<0) {
        return gps_errno;
      }
      break;
    case 101:
      ret = GPS_Input_Get_D101(&((*way)[i]),inf);
      if (ret<0) {
        return gps_errno;
      }
      break;
    case 102:
      ret = GPS_Input_Get_D102(&((*way)[i]),inf);
      if (ret<0) {
        return gps_errno;
      }
      break;
    case 103:
      ret = GPS_Input_Get_D103(&((*way)[i]),inf);
      if (ret<0) {
        return gps_errno;
      }
      break;
    case 104:
      ret = GPS_Input_Get_D104(&((*way)[i]),inf);
      if (ret<0) {
        return gps_errno;
      }
      break;
    case 105:
      ret = GPS_Input_Get_D105(&((*way)[i]),inf);
      if (ret<0) {
        return gps_errno;
      }
      break;
    case 106:
      ret = GPS_Input_Get_D106(&((*way)[i]),inf);
      if (ret<0) {
        return gps_errno;
      }
      break;
    case 107:
      ret = GPS_Input_Get_D107(&((*way)[i]),inf);
      if (ret<0) {
        return gps_errno;
      }
      break;
    case 108:
      ret = GPS_Input_Get_D108(&((*way)[i]),inf);
      if (ret<0) {
        return gps_errno;
      }
      break;
    case 150:
      ret = GPS_Input_Get_D150(&((*way)[i]),inf);
      if (ret<0) {
        return gps_errno;
      }
      break;
    case 151:
      ret = GPS_Input_Get_D151(&((*way)[i]),inf);
      if (ret<0) {
        return gps_errno;
      }
      break;
    case 152:
      ret = GPS_Input_Get_D152(&((*way)[i]),inf);
      if (ret<0) {
        return gps_errno;
      }
      break;
    case 154:
      ret = GPS_Input_Get_D154(&((*way)[i]),inf);
      if (ret<0) {
        return gps_errno;
      }
      break;
    case 155:
      ret = GPS_Input_Get_D155(&((*way)[i]),inf);
      if (ret<0) {
        return gps_errno;
      }
      break;
    default:
      GPS_Error("Input_Get_Waypoints: Unknown protocol");
      return PROTOCOL_ERROR;
    }

  }

  return n;
}



/* @funcstatic GPS_Input_Get_Route201 ***********************************
**
** Construct a route array from a file
**
** @param [w] way [GPS_PWay **] pointer to waypoint array
** @param [r] inf [FILE *] stream
**
** @return [int32] number of entries
************************************************************************/

static int32 GPS_Input_Get_Route201(GPS_PWay **way, FILE *inf)
{
  char s[GPS_ARB_LEN];
  int32 n;
  int32 type;
  int32 rtype;
  int32 i;
  long pos;
  int32 ret;
  char *p;
  int32 d;

  gps_errno = INPUT_ERROR;

  if (!GPS_Input_Read_Line(s,inf)) {
    return gps_errno;
  }
  if (sscanf(s,"Route Type: %d",(int *)&rtype)!=1) {
    return gps_errno;
  }

  if (!GPS_Input_Read_Line(s,inf)) {
    return gps_errno;
  }
  if (sscanf(s,"Waypoints Type: %d",(int *)&type)!=1) {
    return gps_errno;
  }


  pos = ftell(inf);
  n = 1;
  while (strncmp(s,"End",3)) {
    if (!GPS_Input_Read_Line(s,inf)) {
      return gps_errno;
    }
    if (strstr(s,"Latitude") || strstr(s,"Route") || strstr(s,"Link Class")) {
      ++n;
    }
  }
  fseek(inf,0L,0);

  if (!(*way=(GPS_PWay *)malloc(n*sizeof(GPS_PWay *)))) {
    return MEMORY_ERROR;
  }
  for (i=0; i<n; ++i) {
    if (!((*way)[i]=GPS_Way_New())) {
      return MEMORY_ERROR;
    }
    (*way)[i]->prot = type;
  }

  if (!GPS_Input_Read_Line(s,inf)) {
    return gps_errno;
  }
  if (!GPS_Input_Read_Line(s,inf)) {
    return gps_errno;
  }
  if (!GPS_Input_Read_Line(s,inf)) {
    return gps_errno;
  }

  for (i=0; i<n; ++i) {
    (*way)[i]->rte_prot = rtype;
    (*way)[i]->islink = 0;
    if (strstr(s,"Route")) {
      (*way)[i]->isrte = 1;
      switch (rtype) {
      case 200:
        p = strchr(s,':');
        p = strchr(p+1,':');
        if (sscanf(p+1,"%d",(int *)&d)!=1) {
          return gps_error;
        }
        (*way)[i]->rte_num=d;
        break;
      case 201:
        p = strchr(s,':');
        p = strchr(p+1,':');
        if (sscanf(p+1,"%d",(int *)&d)!=1) {
          return gps_error;
        }
        (*way)[i]->rte_num=d;
        p = strchr(p+1,':');
        GPS_Input_Load_String((*way)[i]->rte_cmnt,20,p+1);
        break;
      case 202:
        p = strchr(s,':');
        p = strchr(p+1,':');
        GPS_Input_Load_Strnull((*way)[i]->rte_ident,p+1);
        break;
      }
      if (!GPS_Input_Read_Line(s,inf)) {
        return gps_errno;
      }
      continue;
    } else {
      (*way)[i]->isrte=0;
    }



    if (strstr(s,"Link Class")) {
      (*way)[i]->islink = 1;

      p = strchr(s,':');
      if (sscanf(p+1,"%d",(int *)&d)!=1) {
        return gps_error;
      }
      (*way)[i]->rte_link_class=d;

      if (!((*way)[i]->rte_link_class==3 || (*way)[i]->rte_link_class
            ==0xff)) {
        if (!GPS_Input_Read_Line(s,inf)) {
          return gps_errno;
        }
        GPS_Input_Load_String((*way)[i]->rte_link_subclass,18,s);
      } else {
        GPS_Util_Put_Short((UC *)(*way)[i]->rte_link_subclass,0);
        GPS_Util_Put_Int((UC *)(*way)[i]->rte_link_subclass+2,0);
        GPS_Util_Put_Uint((UC *)(*way)[i]->rte_link_subclass+6,
                          0xffffffff);
        GPS_Util_Put_Uint((UC *)(*way)[i]->rte_link_subclass+10,
                          0xffffffff);
        GPS_Util_Put_Uint((UC *)(*way)[i]->rte_link_subclass+14,
                          0xffffffff);
      }

      if (!GPS_Input_Read_Line(s,inf)) {
        return gps_errno;
      }
      GPS_Input_Load_Strnull((*way)[i]->rte_link_ident,s);

      continue;
    } else {
      (*way)[i]->islink=0;
    }


    if (strstr(s,"End")) {
      GPS_Error("Get_Route201: Unexpected End");
      return INPUT_ERROR;
    }


    switch (type) {
    case 100:
      ret = GPS_Input_Get_D100(&((*way)[i]),inf);
      if (ret<0) {
        return gps_errno;
      }
      break;
    case 101:
      ret = GPS_Input_Get_D101(&((*way)[i]),inf);
      if (ret<0) {
        return gps_errno;
      }
      break;
    case 102:
      ret = GPS_Input_Get_D102(&((*way)[i]),inf);
      if (ret<0) {
        return gps_errno;
      }
      break;
    case 103:
      ret = GPS_Input_Get_D103(&((*way)[i]),inf);
      if (ret<0) {
        return gps_errno;
      }
      break;
    case 104:
      ret = GPS_Input_Get_D104(&((*way)[i]),inf);
      if (ret<0) {
        return gps_errno;
      }
      break;
    case 105:
      ret = GPS_Input_Get_D105(&((*way)[i]),inf);
      if (ret<0) {
        return gps_errno;
      }
      break;
    case 106:
      ret = GPS_Input_Get_D106(&((*way)[i]),inf);
      if (ret<0) {
        return gps_errno;
      }
      break;
    case 107:
      ret = GPS_Input_Get_D107(&((*way)[i]),inf);
      if (ret<0) {
        return gps_errno;
      }
      break;
    case 108:
      ret = GPS_Input_Get_D108(&((*way)[i]),inf);
      if (ret<0) {
        return gps_errno;
      }
      break;
    case 150:
      ret = GPS_Input_Get_D150(&((*way)[i]),inf);
      if (ret<0) {
        return gps_errno;
      }
      break;
    case 151:
      ret = GPS_Input_Get_D151(&((*way)[i]),inf);
      if (ret<0) {
        return gps_errno;
      }
      break;
    case 152:
      ret = GPS_Input_Get_D152(&((*way)[i]),inf);
      if (ret<0) {
        return gps_errno;
      }
      break;
    case 154:
      ret = GPS_Input_Get_D154(&((*way)[i]),inf);
      if (ret<0) {
        return gps_errno;
      }
      break;
    case 155:
      ret = GPS_Input_Get_D155(&((*way)[i]),inf);
      if (ret<0) {
        return gps_errno;
      }
      break;
    default:
      GPS_Error("Input_Get_Waypoints: Unknown protocol");
      return PROTOCOL_ERROR;
    }
    if (!GPS_Input_Read_Line(s,inf)) {
      return gps_errno;
    }
  }

  return n;
}
