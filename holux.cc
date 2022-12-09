/*
    Access to holux wpo files.

    Copyright (C) 2002 Jochen Becker, jb@bepo.com

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.


History:
    2002-09-15      J. Becker       start programming


*/
/* This module is for the holux (gm-100) .wpo format */

#include <cstring>                 // for strncpy, memset, strcpy, strlen
#include <cstdio>                  // for snprintf
#include <ctime>                   // for gmtime, mktime, time_t, tm

#include <QDate>                   // for QDate
#include <QString>                 // for QString
#include <QTime>                   // for QTime
#include <QtGlobal>                // for qRound

#include "defs.h"                  // for Waypoint, le_write16, le_write32
#include "holux.h"
#include "gbfile.h"                // for gbfclose, gbfopen_le, gbfile, gbfread
#include "src/core/datetime.h"     // for DateTime


static gbfile* file_in;
static gbfile* file_out;
static unsigned char* HxWFile;
static short_handle mkshort_handle;

#define MYNAME "Holux"


static void rd_init(const QString& fname)
{
  file_in = gbfopen_le(fname, "rb", MYNAME);
}


static void rd_deinit()
{
  gbfclose(file_in);
}





static void
wr_init(const QString& fname)
{
  mkshort_handle = mkshort_new_handle();

  HxWFile = (unsigned char*) xcalloc(GM100_WPO_FILE_SIZE, 1);

  file_out = gbfopen_le(fname, "wb", MYNAME);
}





static void wr_deinit()
{
  mkshort_del_handle(&mkshort_handle);
  gbfclose(file_out);
}



static void data_read()
{
  char name[9];
  char desc[90];
  struct tm tm;
  struct tm* ptm;

  memset(&tm, 0, sizeof(tm));

  auto* HxWpt = (unsigned char*) xcalloc(GM100_WPO_FILE_SIZE, 1);

  /* read the wpo file to the data-array */
  int iDataRead = gbfread(HxWpt, 1, GM100_WPO_FILE_SIZE, file_in);

  if (iDataRead == 0) {
    fatal(MYNAME ": Error reading data from %s.\n", file_in->name);
  }

  int iWptNum = le_read16(&((WPTHDR*)HxWpt)->num);

  /* Get the waypoints */
  for (int iCount = 0; iCount < iWptNum ; iCount ++) {
    auto* wpt_tmp = new Waypoint;

    int iWptIndex = le_read16(&((WPTHDR*)HxWpt)->idx[iCount]);
    WPT* pWptHxTmp = (WPT*)&HxWpt[OFFS_WPT + (sizeof(WPT) * iWptIndex)];

    wpt_tmp->altitude = 0;
    strncpy(name,pWptHxTmp->name,sizeof(name));
    name[sizeof(pWptHxTmp->name)]=0;

    strncpy(desc,pWptHxTmp->comment,sizeof(desc));
    desc[sizeof(pWptHxTmp->comment)]=0;

    wpt_tmp->shortname = name;
    wpt_tmp->description = desc;

    wpt_tmp->SetCreationTime(0);
    if (pWptHxTmp->date.year) {
#if 0
      /* Unless there's some endian swapping that I don't see,
       * this can't be right.  Then again, the definition of the
       * the structure itself has a pretty serious disregard for
       * host word size issues... - rjl
       */
      ptm = gmtime((time_t*)&pWptHxTmp->time);
#else
      time_t wt = le_read32(&pWptHxTmp->time);
      ptm = gmtime(&wt);
#endif
      tm.tm_hour = ptm->tm_hour;
      tm.tm_min = ptm->tm_min;
      tm.tm_sec = ptm->tm_sec;

      tm.tm_mday = pWptHxTmp->date.day;
      tm.tm_mon = pWptHxTmp->date.month - 1;
      tm.tm_year = pWptHxTmp->date.year - 1900;
      wpt_tmp->SetCreationTime(mktime(&tm));
    }

    double lon = le_read32(&pWptHxTmp->pt.iLongitude) / 36000.0;
    double lat = (le_read32(&pWptHxTmp->pt.iLatitude)  / 36000.0) * -1.0;
    wpt_tmp->longitude = lon;
    wpt_tmp->latitude = lat;
    waypt_add(wpt_tmp);
  }
  xfree(HxWpt);
}




static const char* mknshort(const char* stIn,unsigned int sLen)
{
  constexpr int MAX_STRINGLEN = 255;
  static char strOut[MAX_STRINGLEN];
  char strTmp[MAX_STRINGLEN];

  if (sLen > MAX_STRINGLEN) {
    return (stIn);
  }

  if (stIn == nullptr) {
    return nullptr;
  }

  setshort_length(mkshort_handle, sLen);
  setshort_mustuniq(mkshort_handle, 0);
  setshort_defname(mkshort_handle, "");

  char* shortstr = mkshort(mkshort_handle, stIn, false);
  strcpy(strTmp,shortstr);
  xfree(shortstr);

  memset(strOut,' ', MAX_STRINGLEN);
  strncpy(strOut,strTmp,strlen(strTmp));
  return (strOut);
}




static void holux_disp(const Waypoint* wpt)
{
  double lon = wpt->longitude * 36000.0;
  double lat = wpt->latitude * -36000.0;

  short sIndex = le_read16(&((WPTHDR*)HxWFile)->num);

  if (sIndex >= MAXWPT) {
    fatal(MYNAME ": too many waypoints.  Max is %d.\n", MAXWPT);
  }

  ((WPTHDR*)HxWFile)->idx[sIndex] = sIndex;          /* set the waypoint index  */
  le_write16(&((WPTHDR*)HxWFile)->idx[sIndex], sIndex);          /* set the waypoint index  */
  ((WPTHDR*)HxWFile)->used[sIndex] = 0xff;            /* Waypoint used */


  /* set Waypoint */
  WPT* pWptHxTmp = (WPT*)&HxWFile[OFFS_WPT + (sizeof(WPT) * sIndex)];

  memset(pWptHxTmp->name,0x20,sizeof(pWptHxTmp->name));
  if (wpt->shortname != nullptr) {
    strncpy(pWptHxTmp->name, mknshort(CSTRc(wpt->shortname),sizeof(pWptHxTmp->name)),sizeof(pWptHxTmp->name));
  } else {
    snprintf(pWptHxTmp->name,sizeof(pWptHxTmp->name), "W%d",sIndex);
  }

  memset(pWptHxTmp->comment,0x20,sizeof(pWptHxTmp->comment));
  if (wpt->description != nullptr) {
    strncpy(pWptHxTmp->comment, mknshort(CSTRc(wpt->description),sizeof(pWptHxTmp->comment)),sizeof(pWptHxTmp->comment));
  }

  /*set the time */
  if (wpt->creation_time.isValid()) {
    /* tm = gmtime(&wpt->creation_time);*/  /* I get the wrong result with gmtime ???  */
    QDate date(wpt->GetCreationTime().date());
    QTime time(wpt->GetCreationTime().time());
    pWptHxTmp->time = (time.hour() * 3600) + (time.minute()* 60) + time.second();
    pWptHxTmp->date.day = date.day();
    pWptHxTmp->date.month = date.month();
    pWptHxTmp->date.year = date.year();
  } else {
    pWptHxTmp->time = 0;
    pWptHxTmp->date.day = 0;
    pWptHxTmp->date.month = 0;
    pWptHxTmp->date.year = 0;
  }


  // Note that conversions from double values to unsigned int
  // yield undefined results for negative values.
  // We intentionally convert to int, then do an implicit
  // conversion to unsigned in the call.
  le_write32(&pWptHxTmp->pt.iLatitude, qRound(lat));
  le_write32(&pWptHxTmp->pt.iLongitude, qRound(lon));
  pWptHxTmp->checked = 01;
  pWptHxTmp->vocidx = (short)0xffff;
  le_write16(&((WPTHDR*)HxWFile)->num, ++sIndex);
  le_write16(&((WPTHDR*)HxWFile)->next, ++sIndex);
}






static void data_write()
{
  short sCount;

  /* init the waypoint area*/
  le_write32(&((WPTHDR*)HxWFile)->id, WPT_HDR_ID);
  ((WPTHDR*)HxWFile)->num = 0;
  ((WPTHDR*)HxWFile)->next = 0;

  /* clear index list */
  for (sCount = 0; sCount < MAXWPT; sCount++) {
    ((WPTHDR*)HxWFile)->idx[sCount] = (signed short)-1;
  }
  for (sCount = 0; sCount < MAXWPT; sCount++) {
    ((WPTHDR*)HxWFile)->used[sCount] = 0;
  }

  /* init the route area */
  le_write32(&((RTEHDR*)&HxWFile[ROUTESTART])->id, RTE_HDR_ID);
  ((RTEHDR*)&HxWFile[ROUTESTART])->num = 0;
  le_write16(&((RTEHDR*)&HxWFile[ROUTESTART])->next, 1);
  ((RTEHDR*)&HxWFile[ROUTESTART])->rteno = (signed short)-1;

  /* clear index list */
  for (sCount = 0; sCount < MAXRTE; sCount++) {
    ((RTEHDR*)&HxWFile[ROUTESTART])->idx[sCount] = (signed short)-1;
  }
  for (sCount = 0; sCount < MAXRTE; sCount++) {
    ((RTEHDR*)&HxWFile[ROUTESTART])->used[sCount] = 0;
  }

  waypt_disp_all(holux_disp);

  int iWritten = gbfwrite(HxWFile, 1, GM100_WPO_FILE_SIZE,file_out);
  if (iWritten == 0) {
    fatal(MYNAME ": Error writing data to %s.\n", file_out->name);
  }
  xfree(HxWFile);
}




ff_vecs_t holux_vecs = {
  ff_type_file,
  FF_CAP_RW_WPT,
  rd_init,
  wr_init,
  rd_deinit,
  wr_deinit,
  data_read,
  data_write,
  nullptr,
  nullptr,
  CET_CHARSET_ASCII, 0	/* CET-REVIEW */
  , NULL_POS_OPS
};
