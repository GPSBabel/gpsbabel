/*
    Read Vito SmartMap .vtt tracks

    Copyright (C) 2007 Jeremy Ehrhardt, jeremye@caltech.edu

based on vitostc.c, which is
Copyright (C) 2005 Etienne TASSE

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

 */

#include "defs.h"
#include <cmath>

#define MYNAME "vitovtt"

#define TM_YEAR_ZERO 1900
#define TM_MONTH_ZERO 1

static gbfile* infile = nullptr;
static int count = 0;

static const int vitovtt_version = 3;
// static const size_tvitovtt_headersize = 16;
// static const size_tvitovtt_datasize = 32;

static const double vitovtt_latitudescale = 20000000.0;
static const double vitovtt_longitudescale = 10000000.0;
static const int vitovtt_secondscale = 30000000;
static const int vitovtt_microsecondscale = 30;

static void
rd_init(const QString& fname)
{
  infile = gbfopen_le(fname, "rb", MYNAME);
}

static void
rd_deinit()
{
  gbfclose(infile);
}

static void
vitovtt_read()
{
  struct tm tmStruct;

  memset(&tmStruct, 0, sizeof(tmStruct));

  auto* rte = new route_head;
  track_add_head(rte);

  /* Read the header. */
  int version = gbfgetint32(infile);
  count = gbfgetint32(infile);

  if (version != vitovtt_version) {
    fatal("%s (%d) reading file.  Unsupported version %d\n",
          MYNAME, __LINE__, version);
  }

  while (count) {
    /* Read an entry. */
    int scaled_lat = gbfgetint32(infile);
    int scaled_lon = gbfgetint32(infile);
    double altitude = gbfgetflt(infile);
    tmStruct.tm_year = gbfgetint16(infile) - TM_YEAR_ZERO;
    tmStruct.tm_mon = gbfgetc(infile) - TM_MONTH_ZERO;
    tmStruct.tm_mday = gbfgetc(infile);
    tmStruct.tm_hour = gbfgetc(infile);
    tmStruct.tm_min = gbfgetc(infile);
    int scaled_sec = gbfgetint32(infile);
    double speed = gbfgetflt(infile);
    int course = gbfgetint16(infile);
    int status = gbfgetint32(infile);

    auto* wpt_tmp = new Waypoint;

    wpt_tmp->latitude= scaled_lat / vitovtt_latitudescale;
    wpt_tmp->longitude= scaled_lon / vitovtt_longitudescale;
    wpt_tmp->altitude= altitude;

    tmStruct.tm_sec = scaled_sec / vitovtt_secondscale;
    int microseconds = (scaled_sec % vitovtt_secondscale) / vitovtt_microsecondscale;
    wpt_tmp->SetCreationTime(mkgmtime(&tmStruct), lround(microseconds/1000.0));
    /*
     * TODO: interpret speed, course, status
     */
    (void) speed;
    (void) course;
    (void) status;

    track_add_wpt(rte, wpt_tmp);

    count--;
  }
}

ff_vecs_t vitovtt_vecs = {
  ff_type_file,
  { ff_cap_none, ff_cap_read, ff_cap_none },
  rd_init,
  nullptr,
  rd_deinit,
  nullptr,
  vitovtt_read,
  nullptr,
  nullptr,
  nullptr,
  CET_CHARSET_UTF8, 1/* do nothing | CET-REVIEW */
  , NULL_POS_OPS,
  nullptr
};
