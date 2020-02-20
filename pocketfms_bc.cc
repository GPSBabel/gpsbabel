/*
    Read and write PocketFMS files.

	Copyright (C) 2009 Tobias Kretschmar, tobias.kretschmar@gmx.de

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

#define MYNAME			"PocketFMS Breadcrumbs"

static char header_id[] = "BRC";

struct breadcrumb {
  // header
  char		id[4];		  // 0x42 0x52 0x43 0x00 <=> "BRC"
  uint16_t	version;	// 0x0100
  uint16_t	reserve1;	// 0x0000
  // data
  float		latitude;
  float		longitude;
  float		altitude;	// meter
  float		speed;		// m/s
  float		course;		// degrees
  float		magvar;		// degrees
  float		separation;	// meter
  float		ehpe;		// estimated horizontal position error
  float		evpe;		// estimated vertical position error
  float		espe;		// estimated speed position error
  uint16_t	fix;		// 1..none, 2..2D, 3..3D, 4..dgps, 5pps
  uint16_t	year;		// 1999..2999
  uint16_t	month;		// 1..12
  uint16_t	day;		// 0..31
  uint16_t	hour;		// 0.23
  uint16_t	minute;		// 0..59
  uint16_t	second;		// 0..59
  uint16_t	reserve2;	// 0x0000
};

static gbfile* file_in, *file_out;

static void
rd_init(const QString& fname)
{
  file_in = gbfopen_le(fname, "rb", MYNAME);
}

static void
rd_deinit()
{
  gbfclose(file_in);
}

static void
wr_init(const QString& fname)
{
  file_out = gbfopen_le(fname, "wb", MYNAME);
}

static void
wr_deinit()
{
  gbfclose(file_out);
}

static void
read_tracks()
{
  breadcrumb bc;
  auto* trk_head = new route_head;
  trk_head->rte_num = 1;
  trk_head->rte_name = "PocketFMS";
  trk_head->rte_desc = "Breadcrumb";
  trk_head->rte_urls.AddUrlLink(UrlLink("www.pocketfms.com"));
  track_add_head(trk_head);

  while (1 == gbfread(&bc, sizeof(bc), 1, file_in)) {
    struct tm tm;

    if (strcmp(bc.id, header_id) != 0) {
      fatal(MYNAME ": invalid breadcrumb header in input file.\n");
    }

    memset(&tm, 0, sizeof(tm));
    tm.tm_year = le_readu16(&bc.year)-1900;
    tm.tm_mon = le_readu16(&bc.month)-1;
    tm.tm_mday = le_readu16(&bc.day);
    tm.tm_hour = le_readu16(&bc.hour);
    tm.tm_min = le_readu16(&bc.minute);
    tm.tm_sec = le_readu16(&bc.second);

    auto* wpt = new Waypoint;
    wpt->latitude = le_read_float(&bc.latitude);
    wpt->longitude = le_read_float(&bc.longitude);
    wpt->altitude = FEET_TO_METERS(le_read_float(&bc.altitude));
    wpt->SetCreationTime(mkgmtime(&tm));
    wpt->hdop = le_read_float(&bc.ehpe);
    wpt->vdop = le_read_float(&bc.evpe);
    wpt->pdop = le_read_float(&bc.espe);
    wpt->course = le_read_float(&bc.course);
    wpt->speed = le_read_float(&bc.speed);
    wpt->fix = (fix_type)(le_readu16(&bc.fix) - 1);

    track_add_wpt(trk_head, wpt);
  }
}

static void
pocketfms_waypt_disp(const Waypoint* wpt)
{
  breadcrumb bc;

  memset(&bc, 0, sizeof(bc));
  const time_t tt = wpt->GetCreationTime().toTime_t();
  struct tm* tm = localtime(&tt);
  if (wpt->creation_time.isValid()) {
    const time_t tt = wpt->GetCreationTime().toTime_t();
    tm = gmtime(&tt);
  }

  strcpy(bc.id, header_id);
  le_write16(&bc.version, 1);
  le_write_float(&bc.latitude, wpt->latitude);
  le_write_float(&bc.longitude, wpt->longitude);
  le_write_float(&bc.altitude, METERS_TO_FEET(wpt->altitude));
  if (tm) {
    le_write16(&bc.year, tm->tm_year + 1900);
    le_write16(&bc.month, tm->tm_mon + 1);
    le_write16(&bc.day, tm->tm_mday);
    le_write16(&bc.hour, tm->tm_hour);
    le_write16(&bc.minute, tm->tm_min);
    le_write16(&bc.second, tm->tm_sec);
  }
  le_write_float(&bc.ehpe, wpt->hdop);
  le_write_float(&bc.evpe, wpt->vdop);
  le_write_float(&bc.espe, wpt->pdop);
  le_write_float(&bc.course, wpt->course);
  le_write_float(&bc.speed, wpt->speed);
  le_write16(&bc.fix, wpt->fix+1);

  gbfwrite(&bc, sizeof(bc), 1, file_out);
}

static void
data_read()
{
  read_tracks();
}

static void
data_write()
{
  track_disp_all(nullptr, nullptr, pocketfms_waypt_disp);
}

ff_vecs_t pocketfms_bc_vecs = {
  ff_type_file,
  {
    ff_cap_none,				/* waypoints */
    (ff_cap)(ff_cap_read | ff_cap_write),	/* tracks */
    ff_cap_none					/* routes */
  },
  rd_init,
  wr_init,
  rd_deinit,
  wr_deinit,
  data_read,
  data_write,
  nullptr,
  nullptr,
  CET_CHARSET_ASCII, 0	/* CET-REVIEW */
  , NULL_POS_OPS,
  nullptr
};
