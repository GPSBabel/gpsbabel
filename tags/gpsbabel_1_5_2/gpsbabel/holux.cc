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
    Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111 USA


History:
    2002-09-15      J. Becker       start programming


*/
/* This module is for the holux (gm-100) .wpo format */

#include "defs.h"
#include "holux.h"
//#include <math.h>
#include <stdio.h>
#include <stdlib.h>

static  gbfile* file_in, *file_out;
static 	unsigned char* HxWFile;
static  short_handle mkshort_handle;

#define MYNAME "Holux"


static void rd_init(const char* fname)
{
  file_in = gbfopen_le(fname, "rb", MYNAME);
}


static void rd_deinit(void)
{
  gbfclose(file_in);
}





static void
wr_init(const char* fname)
{
  mkshort_handle = mkshort_new_handle();

  HxWFile = (unsigned char*) xcalloc(GM100_WPO_FILE_SIZE, 1);

  file_out = gbfopen_le(fname, "wb", MYNAME);
}





static void wr_deinit(void)
{
  mkshort_del_handle(&mkshort_handle);
  gbfclose(file_out);
}



static void data_read(void)
{
  char name[9], desc[90];
  double lat,lon;
  unsigned char* HxWpt;
  Waypoint* wpt_tmp;
  int iCount;
  int iDataRead;
  int iWptNum;
  int iWptIndex;
  WPT* pWptHxTmp;
  struct tm tm;
  struct tm* ptm;

  memset(&tm, 0, sizeof(tm));

  HxWpt = (unsigned char*) xcalloc(GM100_WPO_FILE_SIZE, 1);

  /* read the wpo file to the data-array */
  iDataRead = gbfread(HxWpt, 1, GM100_WPO_FILE_SIZE, file_in);

  if (iDataRead == 0) {
    fatal(MYNAME ": Error reading data from %s.\n", file_in->name);
  }

  iWptNum = le_read16(&((WPTHDR*)HxWpt)->num);

  /* Get the waypoints */
  for (iCount = 0; iCount < iWptNum ; iCount ++) {
    wpt_tmp = new Waypoint;

    iWptIndex = le_read16(&((WPTHDR*)HxWpt)->idx[iCount]);
    pWptHxTmp = (WPT*)&HxWpt[OFFS_WPT + (sizeof(WPT) * iWptIndex)];

    wpt_tmp->altitude = 0;
    strncpy(name,pWptHxTmp->name,sizeof(pWptHxTmp->name));
    name[sizeof(pWptHxTmp->name)]=0;

    strncpy(desc,pWptHxTmp->comment,sizeof(pWptHxTmp->comment));
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

    lon = le_read32(&pWptHxTmp->pt.iLongitude) / 36000.0;
    lat = (le_read32(&pWptHxTmp->pt.iLatitude)  / 36000.0) * -1.0;
    wpt_tmp->longitude = lon;
    wpt_tmp->latitude = lat;
    waypt_add(wpt_tmp);
  }
  xfree(HxWpt);
}




const char* mknshort(const char* stIn,unsigned int sLen)
{
#define MAX_STRINGLEN 255
  static char strOut[MAX_STRINGLEN];
  char strTmp[MAX_STRINGLEN];
  char* shortstr = NULL;

  if (sLen > MAX_STRINGLEN) {
    return (stIn);
  }

  if (stIn == NULL) {
    return NULL;
  }

  setshort_length(mkshort_handle, sLen);
  setshort_mustuniq(mkshort_handle, 0);

  shortstr = mkshort(mkshort_handle, stIn);
  strcpy(strTmp,shortstr);
  xfree(shortstr);

  memset(strOut,' ', MAX_STRINGLEN);
  strncpy(strOut,strTmp,strlen(strTmp));
  return (strOut);
}




static void holux_disp(const Waypoint* wpt)
{
  double lon,lat;
  short sIndex;
  WPT* pWptHxTmp;

  lon =(double)wpt->longitude * 36000;
  lat =(double)wpt->latitude * -36000;


  /* round it to increase the accuracy */
  if (lon != 0) {
    lon += (double)((int)lon/abs((int)lon)) * .5;
  }
  if (lat != 0) {
    lat += (double)((int)lat/abs((int)lat)) * .5;
  }

  sIndex =  le_read16(&((WPTHDR*)HxWFile)->num);
  ((WPTHDR*)HxWFile)->idx[sIndex] = sIndex;          /* set the waypoint index  */
  le_write16(&((WPTHDR*)HxWFile)->idx[sIndex], sIndex);          /* set the waypoint index  */
  ((WPTHDR*)HxWFile)->used[sIndex] = 0xff;            /* Waypoint used */


  /* set Waypoint */
  pWptHxTmp = (WPT*)&HxWFile[OFFS_WPT + (sizeof(WPT) * sIndex)];

  memset(pWptHxTmp->name,0x20,sizeof(pWptHxTmp->name));
  if (wpt->shortname != NULL) {
    strncpy(pWptHxTmp->name, mknshort(CSTRc(wpt->shortname),sizeof(pWptHxTmp->name)),sizeof(pWptHxTmp->name));
  } else {
    sprintf(pWptHxTmp->name,"W%d",sIndex);
  }

  memset(pWptHxTmp->comment,0x20,sizeof(pWptHxTmp->comment));
  if (wpt->description != NULL) {
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


  le_write32(&pWptHxTmp->pt.iLatitude,(unsigned int) lat);
  le_write32(&pWptHxTmp->pt.iLongitude,(unsigned int) lon);
  pWptHxTmp->checked = 01;
  pWptHxTmp->vocidx = (short)0xffff;
  le_write16(&((WPTHDR*)HxWFile)->num, ++sIndex);
  le_write16(&((WPTHDR*)HxWFile)->next, ++sIndex);
}






static void data_write(void)
{
  int iWritten;
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

  iWritten = gbfwrite(HxWFile, 1, GM100_WPO_FILE_SIZE,file_out);
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
  NULL,
  NULL,
  CET_CHARSET_ASCII, 0	/* CET-REVIEW */
};
