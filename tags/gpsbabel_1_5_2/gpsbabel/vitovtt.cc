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
    Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111 USA

 */

#include "defs.h"
#include <cmath>

#define MYNAME "vitovtt"

#define TM_YEAR_ZERO	1900
#define TM_MONTH_ZERO	1

static gbfile*			infile	= 0;
static int				count	= 0;

static const int			vitovtt_version				= 3;
// static const size_t			vitovtt_headersize			= 16;
// static const size_t			vitovtt_datasize			= 32;

static const double			vitovtt_latitudescale		= 20000000.0;
static const double			vitovtt_longitudescale		= 10000000.0;
static const int			vitovtt_secondscale			= 30000000;
static const int			vitovtt_microsecondscale	= 30;

static void
rd_init(const char* fname)
{
  infile = gbfopen_le(fname, "rb", MYNAME);
}

static void
rd_deinit(void)
{
  gbfclose(infile);
}

static void
vitovtt_read(void)
{
  int				version			= 0;
  route_head*		route_head		= 0;
  Waypoint*		wpt_tmp		= 0;
  int				scaled_lat		= 0;
  int				scaled_lon		= 0;
  double			altitude		= 0;
  struct tm		tmStruct;
  int				scaled_sec		= 0;
  double			speed			= 0;
  int				course			= 0;
  int				status			= 0;

  memset(&tmStruct, 0, sizeof(tmStruct));

  route_head = route_head_alloc();
  track_add_head(route_head);

  /* Read the header. */
  version		= gbfgetint32(infile);
  count		= gbfgetint32(infile);

  if (version!=vitovtt_version) {

    fatal("%s (%d) reading file.  Unsupported version %d\n",
          MYNAME, __LINE__, version);
  }

  while (count) {
    /* Read an entry. */
    scaled_lat			= gbfgetint32(infile);
    scaled_lon			= gbfgetint32(infile);
    altitude			= gbfgetflt(infile);
    tmStruct.tm_year	= gbfgetint16(infile) - TM_YEAR_ZERO;
    tmStruct.tm_mon		= gbfgetc(infile) - TM_MONTH_ZERO;
    tmStruct.tm_mday	= gbfgetc(infile);
    tmStruct.tm_hour	= gbfgetc(infile);
    tmStruct.tm_min		= gbfgetc(infile);
    scaled_sec			= gbfgetint32(infile);
    speed				= gbfgetflt(infile);
    course				= gbfgetint16(infile);
    status				= gbfgetint32(infile);

    wpt_tmp = new Waypoint;

    wpt_tmp->latitude	= scaled_lat / vitovtt_latitudescale;
    wpt_tmp->longitude	= scaled_lon / vitovtt_longitudescale;
    wpt_tmp->altitude	= altitude;

    tmStruct.tm_sec = scaled_sec / vitovtt_secondscale;
    int microseconds = (scaled_sec % vitovtt_secondscale) / vitovtt_microsecondscale;
    wpt_tmp->SetCreationTime(mkgmtime(&tmStruct), lround(microseconds/1000.0));
    /*
     * TODO: interpret speed, course, status
     */

    track_add_wpt(route_head, wpt_tmp);

    count--;
  }
}

ff_vecs_t vitovtt_vecs = {
  ff_type_file,
  { ff_cap_none, ff_cap_read, ff_cap_none },
  rd_init,
  NULL,
  rd_deinit,
  NULL,
  vitovtt_read,
  NULL,
  NULL,
  NULL,
  CET_CHARSET_UTF8, 1	/* do nothing | CET-REVIEW */
};
