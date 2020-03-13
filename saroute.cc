/*
    Read various Delorme routes including anr, rte, and rtd.

    Copyright (C) 2003 Ron Parker and Robert Lipe.

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


#define MYNAME "saroute"
#include "defs.h"
#include "grtcirc.h"
#include <cstddef>

static gbfile* infile;

static char* turns_important = nullptr;
static char* turns_only = nullptr;
static char* controls = nullptr;
static char* split = nullptr;
static char* timesynth = nullptr;

static int control = 0;

static
QVector<arglist_t> saroute_args = {
  {
    "turns_important", &turns_important,
    "Keep turns if simplify filter is used",
    nullptr, ARGTYPE_BOOL, ARG_NOMINMAX, nullptr
  },
  {
    "turns_only", &turns_only, "Only read turns; skip all other points",
    nullptr, ARGTYPE_BOOL, ARG_NOMINMAX, nullptr
  },
  {
    "split", &split, "Split into multiple routes at turns",
    nullptr, ARGTYPE_BOOL, ARG_NOMINMAX, nullptr
  },
  {
    "controls", &controls, "Read control points as waypoint/route/none",
    "none", ARGTYPE_STRING, ARG_NOMINMAX, nullptr
  },
  {
    "times", &timesynth, "Synthesize track times",
    nullptr, ARGTYPE_BOOL, ARG_NOMINMAX, nullptr
  },
};

#define ReadShort(f) gbfgetint16(f)
#define ReadLong(f) gbfgetint32(f)

static unsigned char*
ReadRecord(gbfile* f, gbsize_t size)
{
  auto* result = (unsigned char*) xmalloc(size);

  (void)gbfread(result, size, 1, f);
  return result;
}

static void
Skip(gbfile* f, gbsize_t distance)
{
  gbfseek(f, distance, SEEK_CUR);
}

static void
rd_init(const QString& fname)
{
  infile = gbfopen(fname, "rb", MYNAME);
  if (split && (turns_important || turns_only)) {
    fatal(MYNAME
          ": turns options are not compatible with split\n");
  }
  if (controls) {
    switch (controls[0]) {
    case 'n':
      control = 0;
      break;
    case 'r':
      control = 1;
      break;
    case 'w':
      control = 2;
      break;
    default:
      fatal(MYNAME
            ": unrecognized value for 'controls'\n");
      break;
    }
  }
}

static void
rd_deinit()
{
  gbfclose(infile);
}

static void
my_read()
{
  static int serial = 0;
  struct ll {
    int32_t lat;
    int32_t lon;
  } *latlon;
  struct ll mylatlon;
  route_head* track_head = nullptr;
  Waypoint* wpt_tmp;
  char* routename = nullptr;
  double seglen = 0.0;
  int32_t  starttime = 0;
  int32_t  transittime = 0;
  double totaldist = 0.0;
  double oldlat = 0;
  double oldlon = 0;

  ReadShort(infile);		/* magic */
  uint16_t version = ReadShort(infile);

  ReadLong(infile);
  if (version >= 6) {
    ReadLong(infile);
    ReadLong(infile);
  }

  /*
   * end of header
   */

  ReadShort(infile);
  uint32_t recsize = ReadLong(infile);
  /*
   * the first recsize, oddly, doesn't include the filename string
   * but it does include the header.
   */
  unsigned char* record = ReadRecord(infile, recsize);

  uint16_t stringlen = le_read16((uint16_t*)(record + 0x1a));
  if (stringlen) {
    routename = (char*)xmalloc(stringlen + 1);
    routename[stringlen] = '\0';
    memcpy(routename, record+0x1c, stringlen);
  }
  Skip(infile, stringlen - 4);
  xfree(record);

  /*
   * end of filename record
   */

  /*
   * here lie the route description records
   */
  if (version < 6 || (control == 1)) {
    track_head = new route_head;
    route_add_head(track_head);
    if (control) {
      track_head->rte_name = "control points";
    } else {
      track_head->rte_name = routename;
    }
  }
  uint32_t count = ReadLong(infile);
  while (count) {
    ReadShort(infile);
    recsize = ReadLong(infile);
    if (version < 6 || control) {
      record = ReadRecord(infile, recsize);
      latlon = (struct ll*)(record);

      /* These records are backwards for some reason */
      double lat = (0x80000000UL -
        le_read32(&latlon->lon)) / (double)(0x800000);
      double lon = (0x80000000UL -
        le_read32(&latlon->lat)) / (double)(0x800000);

      wpt_tmp = new Waypoint;
      wpt_tmp->latitude = lat;
      wpt_tmp->longitude = -lon;
      if (control) {
        int obase;

        /* Somewhere around TopoUSA 6.0, these moved  */
        /* This block also seems to get miscompiled
         * at -O0 on Linux.  I tried rewriting it to
         * reduce/eliminate some of the really funky
         * pointer math and casting that was here.
         */
        if (version >= 11) {
          obase = 20;
        } else {
          obase = 18;
        }

        int addrlen = le_read16(&record[obase]);
        int cmtlen = le_read16(&record[obase+2+addrlen]);
        (void) cmtlen;
        // That we've had no bugreports on this strongly indicates this code
        // is never used... Look in revision history if anyone cares.
        wpt_tmp->shortname = "booger";
        wpt_tmp->notes = "goober";
      } else {
        wpt_tmp->shortname = QString::asprintf("\\%5.5x", serial++);
      }
      if (control == 2) {
        waypt_add(wpt_tmp);
      } else {
        route_add_wpt(track_head, wpt_tmp);
      }
      xfree(record);
      if (version >= 6) {
        /*
             * two longs of scrap after each record, don't know why
         */
        ReadLong(infile);
        ReadLong(infile);
      }
    } else {
      Skip(infile, recsize);
      /*
       * two longs of scrap after each record, don't know why
       */
      ReadLong(infile);
      ReadLong(infile);
    }
    count--;
  }
  /*
   * end of route desc records
   */

  /*
   * outercount is the number of route segments (start+end+stops+vias-1)
   */

  uint32_t outercount = ReadLong(infile);
  while (outercount) {

    /*
     * unknown record (route params?) lives here
     */
    ReadShort(infile);
    recsize = ReadLong(infile);
    Skip(infile, recsize);

    /*
     * end of unknown record
     */

    /*
     * routing begins here
     */
    count = ReadLong(infile);
    if (count) {
      track_head = new route_head;
      if (timesynth) {
        track_add_head(track_head);
      } else {
        route_add_head(track_head);
      }
      if (routename && !split) {
        track_head->rte_name = routename;
      }
    }
    while (count) {
      route_head* old_track_head = nullptr;
      ReadShort(infile);
      recsize = ReadLong(infile);
      record = ReadRecord(infile, recsize);
      stringlen = le_read16((uint16_t*)record);
      if (split && stringlen) {
        if (track_head->rte_waypt_ct) {
          old_track_head = track_head;
          track_head = new route_head;
          if (timesynth) {
            track_add_head(track_head);
          } else {
            route_add_head(track_head);
          }
        } // end if
        if (track_head->rte_name.isEmpty()) {
          track_head->rte_name = "Track";
        }
      }

      if (timesynth) {
        seglen = le_read_double(
                   record + 2 + stringlen + 0x08);
        starttime = le_read32((uint32_t*)
                              (record + 2 + stringlen + 0x30));
        transittime = le_read32((uint32_t*)
                                (record + 2 + stringlen + 0x10));
        seglen *= kMilesPerKilometer; /* to miles */
      }

      uint16_t coordcount = le_read16((uint16_t*)
        (record + 2 + stringlen + 0x3c));
      latlon = (struct ll*)(record + 2 + stringlen + 0x3c + 2);
      count--;
      if (count) {
        coordcount--;
      }

      int first = 1;

      while (coordcount) {
        wpt_tmp = new Waypoint;

        // copy to make sure we don't violate alignment restrictions.
        memcpy(&mylatlon,latlon,sizeof(mylatlon));
        double lat = (0x80000000UL -
            le_read32(&mylatlon.lat)) /
          (double)(0x800000);
        double lon = (0x80000000UL -
            le_read32(&mylatlon.lon)) /
          (double)(0x800000);

        wpt_tmp->latitude = lat;
        wpt_tmp->longitude = -lon;
        if (stringlen && ((coordcount>1) || count)) {
          wpt_tmp->shortname = QString(((char*)record)+2);
        } else {
          wpt_tmp->shortname = QString::asprintf("\\%5.5x", serial++);
        }
        if (timesynth) {
          if (!first) {
            double dist = radtomiles(gcdist(
                                       RAD(lat), RAD(-lon),
                                       RAD(oldlat),
                                       RAD(-oldlon)));
            totaldist += dist;
            if (totaldist > seglen) {
              totaldist = seglen;
            }
            wpt_tmp->SetCreationTime(
              gpsbabel_time+starttime+
              transittime * totaldist/seglen);
          } else {
            wpt_tmp->SetCreationTime(gpsbabel_time+starttime);
            totaldist = 0;
          }
          oldlat = lat;
          oldlon = lon;
        }
        if (turns_important && stringlen) {
          wpt_tmp->route_priority=1;
        }
        if (!turns_only || stringlen) {
          if (timesynth) {
            track_add_wpt(track_head,wpt_tmp);
          } else {
            route_add_wpt(track_head, wpt_tmp);
          }
          if (old_track_head) {
            if (timesynth) {
              track_add_wpt(old_track_head,
                            new Waypoint(*wpt_tmp));
            } else {
              route_add_wpt(old_track_head,
                            new Waypoint(*wpt_tmp));
            }
            old_track_head = nullptr;
          }
        }

        latlon++;
        coordcount--;
        stringlen = 0;
        /* the stop point is a "turn" */
        if (coordcount == 1 && count == 0) {
          stringlen = 1;
        }
        first = 0;
      }
      if (version > 10) {
        Skip(infile,2*sizeof(uint32_t));
      }
      xfree(record);
    }
    /*
     * end of routing
     */
    outercount--;
  }
  if (routename) {
    xfree(routename);
  }

}

ff_vecs_t saroute_vecs = {
  ff_type_file,
  { ff_cap_none, ff_cap_read, ff_cap_none},
  rd_init,
  nullptr,
  rd_deinit,
  nullptr,
  my_read,
  nullptr,
  nullptr,
  &saroute_args,
  CET_CHARSET_UTF8, 1	/* do nothing | CET-REVIEW */
  , NULL_POS_OPS,
  nullptr
};
