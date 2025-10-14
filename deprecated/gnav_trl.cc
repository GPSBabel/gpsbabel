/*

    Support for Google Navigator tracklines (.trl).

    Copyright (C) 2008 Olaf Klein

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

#define MYNAME "gnav_trl"

static
QVector<arglist_t> gnav_trl_args = {
};

struct gnav_trl_t {
  uint32_t time;
  float lat;
  float lon;
  uint32_t alt;
};

static gbfile* fin, *fout;

/*******************************************************************************
* %%%        global callbacks called by gpsbabel main process              %%% *
*******************************************************************************/

static void
gnav_trl_rd_init(const QString& fname)
{
  fin = gbfopen_le(fname, "rb", MYNAME);
}

static void
gnav_trl_rw_init(const QString& fname)
{
  fout = gbfopen_le(fname, "wb", MYNAME);
}

static void
gnav_trl_rd_deinit()
{
  gbfclose(fin);
}

static void
gnav_trl_rw_deinit()
{
  gbfclose(fout);
}

static double
read_altitude(void* ptr)
{
  auto* i = (unsigned char*) ptr;
  char buf[sizeof(float)];
  le_write32(&buf, i[2] << 24 | i[1] << 16 | i[0] <<8 | i[3]);
  return le_read_float(&buf);
}

static void
write_altitude(void* ptr, const float alt)
{
  char buf[sizeof(float)];
  auto* i = (unsigned char*) &buf;
  le_write_float(&buf, alt);
  le_write32(ptr, i[0] << 24 | i[3] << 16 | i[2] << 8 | i[1]);
}

static void
gnav_trl_read()
{
  route_head* trk = nullptr;

  while (! gbfeof(fin)) {
    gnav_trl_t rec;

    if (gbfread(&rec, sizeof(rec), 1, fin) != 1) {
      fatal(MYNAME ": Unexpected EOF (end of file)!\n");
    }

    auto* wpt = new Waypoint;

    wpt->SetCreationTime(le_read32(&rec.time));
    wpt->latitude = le_read_float(&rec.lat);
    wpt->longitude = le_read_float(&rec.lon);
    wpt->altitude = read_altitude(&rec.alt);

    if (trk == nullptr) {
      trk = new route_head;
      track_add_head(trk);
    }
    track_add_wpt(trk, wpt);
  }
}

static void
gnav_trl_write_trkpt(const Waypoint* wpt)
{
  gnav_trl_t rec;

  le_write32(&rec.time, wpt->GetCreationTime().toTime_t());
  le_write_float(&rec.lat, wpt->latitude);
  le_write_float(&rec.lon, wpt->longitude);
  if (wpt->altitude != unknown_alt) {
    write_altitude(&rec.alt, wpt->altitude);
  } else {
    write_altitude(&rec.alt, 0);
  }

  gbfwrite(&rec, sizeof(rec), 1, fout);
}

static void
gnav_trl_write()
{
  track_disp_all(nullptr, nullptr, gnav_trl_write_trkpt);
}


/**************************************************************************/

ff_vecs_t gnav_trl_vecs = {
  ff_type_file,
  {
    ff_cap_none			/* waypoints */,
    (ff_cap)(ff_cap_read | ff_cap_write)	/* tracks */,
    ff_cap_none			/* routes */
  },
  gnav_trl_rd_init,
  gnav_trl_rw_init,
  gnav_trl_rd_deinit,
  gnav_trl_rw_deinit,
  gnav_trl_read,
  gnav_trl_write,
  nullptr,
  &gnav_trl_args,
  CET_CHARSET_UTF8, 1	/* CET - do nothing ! */
  , NULL_POS_OPS
};

/**************************************************************************/
