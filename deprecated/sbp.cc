/*
    Locosys NaviGPS GT-31/BGT-31 binary datalog format (SBP)

    Copyright (C) 2008  Rodney Lorrimar
    Copyright (C) 2005  Robert Lipe

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

#include <cstddef>     // for size_t

#include <QString>     // for QString
#include <QVector>     // for QVector

#include "defs.h"
#include "gbfile.h"    // for gbfread, gbfclose, gbfopen, gbfile
#include "navilink.h"  // for locosys_decode_file_id, navilink_decode_logpoint


#define MYNAME "sbp"

static gbfile* file_handle = nullptr;

static
QVector<arglist_t> sbp_args = {
};

/*******************************************************************************
* %%%        global callbacks called by gpsbabel main process              %%% *
*******************************************************************************/

static void
sbp_rd_init(const QString& fname)
{
  file_handle = gbfopen(fname, "r", MYNAME);
}

static void
sbp_rd_deinit()
{
  gbfclose(file_handle);
}

static void
read_sbp_header(route_head*)
{
  /*
   * A complete SBP file contains 64 bytes header,
   *
   * Here is the definition of the SBP header
   * BYTE 0 ~1 : true SBP header length
   * BYTE 2~63:  MID_FILE_ID(0xfd)
   *             will stuff 0xff for remaining bytes
   */

#define HEADER_SKIP 7

  bool success;
  char header[64];

  if (gbfread(header, sizeof(header), 1, file_handle) == 1) {
    size_t len = le_read16(header) - HEADER_SKIP;
    if (len > sizeof(header)) {
      len = sizeof(header);
    }

    success = locosys_decode_file_id(header + HEADER_SKIP, len);
  } else {
    success = false;
  }

  if (!success) {
    fatal(MYNAME ": Format error: Couldn't read SBP header."
          "This probably isn't a SBP file.\n");
  }
}

static Waypoint*
read_logpoint()
{
  unsigned char buffer[SBP_RECORD_LEN];

  if (gbfread(buffer, sizeof(buffer), 1, file_handle) == 1) {
    return navilink_decode_logpoint(buffer);
  }

  return nullptr;
}

static void
sbp_read()
{
  Waypoint* logpoint;

  auto* track = new route_head;
  track_add_head(track);

  read_sbp_header(track);

  while ((logpoint = read_logpoint())) {
    track_add_wpt(track, logpoint);
  }
}

/**************************************************************************/

/* ascii is the expected character set */
/* not fixed, can be changed through command line parameter */

ff_vecs_t sbp_vecs = {
  ff_type_file,
  {
    ff_cap_none         /* waypoints */,
    ff_cap_read					/* tracks */,
    ff_cap_none					/* routes */
  },
  sbp_rd_init,
  nullptr,
  sbp_rd_deinit,
  nullptr,
  sbp_read,
  nullptr,
  nullptr,
  &sbp_args,
  NULL_POS_OPS
};
/**************************************************************************/
