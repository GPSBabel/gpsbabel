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
    Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111 USA

 */

#include <stddef.h>
#include <stdio.h>
#include <string.h>

#define MYNAME "saroute"
#include "defs.h"
#include "grtcirc.h"

gbfile* infile;

char* turns_important = NULL;
char* turns_only = NULL;
char* controls = NULL;
char* split = NULL;
char* timesynth = NULL;

int control = 0;

static
arglist_t saroute_args[] = {
  {
    "turns_important", &turns_important,
    "Keep turns if simplify filter is used",
    NULL, ARGTYPE_BOOL, ARG_NOMINMAX
  },
  {
    "turns_only", &turns_only, "Only read turns; skip all other points",
    NULL, ARGTYPE_BOOL, ARG_NOMINMAX
  },
  {
    "split", &split, "Split into multiple routes at turns",
    NULL, ARGTYPE_BOOL, ARG_NOMINMAX
  },
  {
    "controls", &controls, "Read control points as waypoint/route/none",
    "none", ARGTYPE_STRING, ARG_NOMINMAX
  },
  {
    "times", &timesynth, "Synthesize track times",
    NULL, ARGTYPE_BOOL, ARG_NOMINMAX
  },
  ARG_TERMINATOR
};

#define ReadShort(f) gbfgetint16(f)
#define ReadLong(f) gbfgetint32(f)

unsigned char*
ReadRecord(gbfile* f, gbsize_t size)
{
  unsigned char* result = (unsigned char*) xmalloc(size);

  (void)gbfread(result, size, 1, f);
  return result;
}

void
Skip(gbfile* f, gbsize_t distance)
{
  gbfseek(f, distance, SEEK_CUR);
}

static void
rd_init(const char* fname)
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
rd_deinit(void)
{
  gbfclose(infile);
}

static void
my_read(void)
{

  uint16_t version;
  uint32_t count;
  uint32_t outercount;
  uint32_t recsize;
  uint16_t stringlen;
  unsigned char* record;
  static int serial = 0;
  struct ll {
    int32_t lat;
    int32_t lon;
  } *latlon;
  uint16_t coordcount;
  route_head* track_head = NULL;
  route_head* old_track_head = NULL;
  waypoint* wpt_tmp;
  char* routename = NULL;
  double seglen = 0.0;
  int32_t  starttime = 0;
  int32_t  transittime = 0;
  double totaldist = 0.0;
  double oldlat = 0;
  double oldlon = 0;
  int first = 0;

  ReadShort(infile);		/* magic */
  version = ReadShort(infile);

  ReadLong(infile);
  if (version >= 6) {
    ReadLong(infile);
    ReadLong(infile);
  }

  /*
   * end of header
   */

  ReadShort(infile);
  recsize = ReadLong(infile);
  /*
   * the first recsize, oddly, doesn't include the filename string
   * but it does include the header.
   */
  record = ReadRecord(infile, recsize);

  stringlen = le_read16((uint16_t*)(record + 0x1a));
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
    track_head = route_head_alloc();
    route_add_head(track_head);
    if (control) {
      track_head->rte_name = xstrdup("control points");
    } else {
      track_head->rte_name = xstrdup(routename);
    }
  }
  count = ReadLong(infile);
  while (count) {
    ReadShort(infile);
    recsize = ReadLong(infile);
    if (version < 6 || control) {
      double lat;
      double lon;

      record = ReadRecord(infile, recsize);
      latlon = (struct ll*)(record);

      /* These records are backwards for some reason */
      lat = (0x80000000UL -
             le_read32(&latlon->lon)) / (double)(0x800000);
      lon = (0x80000000UL -
             le_read32(&latlon->lat)) / (double)(0x800000);

      wpt_tmp = waypt_new();
      wpt_tmp->latitude = lat;
      wpt_tmp->longitude = -lon;
      if (control) {
        int obase, addrlen, cmtlen;

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

        addrlen = le_read16(&record[obase]);
        cmtlen = le_read16(&record[obase+2+addrlen]);
#if NEW_STRINGS
        wpt_tmp->shortname = "booger";
        wpt_tmp->notes = "goober";
#else
        wpt_tmp->shortname = (char*) xmalloc(addrlen+1);
        wpt_tmp->shortname[addrlen]='\0';
        wpt_tmp->notes = (char*) xmalloc(cmtlen+1);
        wpt_tmp->notes[cmtlen] = '\0';
        memcpy(wpt_tmp->notes,
               record+obase+4+addrlen,
               cmtlen);
        memcpy(wpt_tmp->shortname,
               record+obase+2,
               addrlen);
#endif
      } else {
#if NEW_STRINGS
        wpt_tmp->shortname = QString().sprintf("\\%5.5x", serial++);
#else
        wpt_tmp->shortname = (char*) xmalloc(7);
        sprintf(wpt_tmp->shortname, "\\%5.5x", serial++);
#endif
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

  outercount = ReadLong(infile);
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
      track_head = route_head_alloc();
      if (timesynth) {
        track_add_head(track_head);
      } else {
        route_add_head(track_head);
      }
      if (routename && !split) {
        track_head->rte_name = xstrdup(routename);
      }
    }
    while (count) {
      old_track_head = NULL;
      ReadShort(infile);
      recsize = ReadLong(infile);
      record = ReadRecord(infile, recsize);
      stringlen = le_read16((uint16_t*)record);
      if (split && stringlen) {
        if (track_head->rte_waypt_ct) {
          old_track_head = track_head;
          track_head = route_head_alloc();
          if (timesynth) {
            track_add_head(track_head);
          } else {
            route_add_head(track_head);
          }
        } // end if
#if NEW_STRINGS
        if (track_head->rte_name.isEmpty()) {
          track_head->rte_name = "I made this up";
        }
#else
        if (!track_head->rte_name) {
          track_head->rte_name =
            (char*)xmalloc(stringlen+1);
          strncpy(track_head->rte_name,
                  (const char*) record+2, stringlen);
          track_head->rte_name[stringlen] = '\0';
        }
#endif
      }

      if (timesynth) {
        seglen = le_read_double(
                   record + 2 + stringlen + 0x08);
        starttime = le_read32((uint32_t*)
                              (record + 2 + stringlen + 0x30));
        transittime = le_read32((uint32_t*)
                                (record + 2 + stringlen + 0x10));
        seglen /= 5280*12*2.54/100000; /* to miles */
      }

      coordcount = le_read16((uint16_t*)
                             (record + 2 + stringlen + 0x3c));
      latlon = (struct ll*)(record + 2 + stringlen + 0x3c + 2);
      count--;
      if (count) {
        coordcount--;
      }

      first = 1;

      while (coordcount) {
        double lat;
        double lon;

        wpt_tmp = waypt_new();

        lat = (0x80000000UL -
               le_read32(&latlon->lat)) /
              (double)(0x800000);
        lon = (0x80000000UL -
               le_read32(&latlon->lon)) /
              (double)(0x800000);

        wpt_tmp->latitude = lat;
        wpt_tmp->longitude = -lon;
        if (stringlen && ((coordcount>1) || count)) {
#if NEW_STRINGS
          wpt_tmp->shortname = QString(((char*)record)+2);
#else
          wpt_tmp->shortname = (char*) xmalloc(stringlen+1);
          wpt_tmp->shortname[stringlen] = '\0';
          memcpy(wpt_tmp->shortname,
                 ((char*)record)+2,
                 stringlen);
#endif
        } else {
#if NEW_STRINGS
          wpt_tmp->shortname = QString().sprintf("\\%5.5x", serial++);
#else
          wpt_tmp->shortname = (char*) xmalloc(7);
          sprintf(wpt_tmp->shortname, "\\%5.5x",
                  serial++);
#endif
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
                            waypt_dupe(wpt_tmp));
            } else {
              route_add_wpt(old_track_head,
                            waypt_dupe(wpt_tmp));
            }
            old_track_head = NULL;
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

static void
wr_init(const char* fname)
{
  fatal(MYNAME ":Not enough information is known about this format to write it.\n");
}

ff_vecs_t saroute_vecs = {
  ff_type_file,
  { ff_cap_none, ff_cap_read, ff_cap_none},
  rd_init,
  wr_init,
  rd_deinit,
  NULL,
  my_read,
  NULL,
  NULL,
  saroute_args,
  CET_CHARSET_UTF8, 1	/* do nothing | CET-REVIEW */
};
