/*
    Read Netstumbler data files.

    Copyright (C) 2004, 2005 Robert Lipe, robertlipe@usa.net and
    John Temples; gpsns@xargs.com

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
#include "csv_util.h"
#include <ctype.h>

static gbfile* file_in;
static char* nseicon = NULL;
static char* nsneicon = NULL;
static char* seicon = NULL;
static char* sneicon = NULL;
static char* snmac = NULL;
static int macstumbler;

static void	fix_netstumbler_dupes(void);

#define MYNAME "NETSTUMBLER"

static
arglist_t netstumbler_args[] = {
  {
    "nseicon", &nseicon, "Non-stealth encrypted icon name",
    "Red Square", ARGTYPE_STRING, ARG_NOMINMAX
  },
  {
    "nsneicon", &nsneicon, "Non-stealth non-encrypted icon name",
    "Green Square", ARGTYPE_STRING, ARG_NOMINMAX
  },
  {
    "seicon", &seicon, "Stealth encrypted icon name",
    "Red Diamond", ARGTYPE_STRING, ARG_NOMINMAX
  },
  {
    "sneicon", &sneicon, "Stealth non-encrypted icon name",
    "Green Diamond", ARGTYPE_STRING, ARG_NOMINMAX
  },
  {
    "snmac", &snmac, "Shortname is MAC address", NULL, ARGTYPE_BOOL,
    ARG_NOMINMAX
  },
  ARG_TERMINATOR
};

static void
rd_init(const char* fname)
{
  file_in = gbfopen(fname, "rb", MYNAME);
  macstumbler = 0;
}

static void
rd_deinit(void)
{
  gbfclose(file_in);
}

static void
data_read(void)
{
  char* ibuf;
  char ssid[2 + 32 + 2 + 1];			/* "( " + SSID + " )" + null */
  char mac[2 + 17 + 2 + 1];			/* "( " + MAC + " )" + null */
  char desc[sizeof ssid - 1 + 15 + 1];	/* room for channel/speed */
  double lat = 0, lon = 0;
  waypoint* wpt_tmp;
  int line_no = 0;
  int stealth_num = 0, whitespace_num = 0;
  long flags = 0;
  int speed = 0, channel = 0;
  struct tm tm;
  int line = 0;

  memset(&tm, 0, sizeof(tm));

  while ((ibuf = gbfgetstr(file_in))) {
    char* field;
    int field_num, len, i, stealth = 0;

    if ((line++ == 0) && file_in->unicode) {
      cet_convert_init(CET_CHARSET_UTF8, 1);
    }
    ibuf = lrtrim(ibuf);
    /* A sharp in column zero might be a comment.  Or it might be
     * something useful, like the date.
    */

    if (ibuf[0] == '#') {
      if (strncmp(&ibuf[2], "$DateGMT:", 9) == 0) {
        tm.tm_year = atoi(&ibuf[12]) - 1900;
        tm.tm_mon = atoi(&ibuf[17]) - 1;
        tm.tm_mday = atoi(&ibuf[20]);
      }

      /*
       * Mac stumbler files are the same, except
       * use DDMM.mmm instad of DD.DDDD.
       */
      if (strstr(ibuf, "Creator: MacStumbler")) {
        macstumbler = 1;
      }

      continue;
    }

    field_num = 0;
    line_no++;
    field = csv_lineparse(ibuf, "\t", "", line_no);

    while (field) {
      switch (field_num) {
      case 0:				/* lat */
        lat = atof(&field[2]);
        if (field[0] == 'S') {
          lat = -lat;
        }
        if (macstumbler) {
          lat = ddmm2degrees(lat);
        }
        break;

      case 1:				/* long */
        lon = atof(&field[2]);
        if (field[0] == 'W') {
          lon = -lon;
        }
        if (macstumbler) {
          lon = ddmm2degrees(lon);
        }
        break;

      case 2:				/* ( SSID ) */
        strcpy(ssid, &field[2]); /* zap "( " */
        len = strlen(ssid) - 2;
        ssid[len] = 0;	 	 /* zap " )" */
        stealth = (len == 0);
        /* don't alter SSID in snmac mode */
        if (!snmac) {
          int found = 0;
          /* check for all whitespace */
          for (i = 0; i < len && !found; i++) {
            if (!isspace(ssid[i])) {
              found = 1;
            }
          }

          if (!stealth && !found) {
            snprintf(ssid, sizeof ssid, "Whitespace/%d", ++whitespace_num);
          }
        }
        break;

      case 4:			/* ( MAC address ) */
        strcpy(mac, &field[2]);	/* zap "( " */
        mac[strlen(mac) - 2] = 0;/* zap " )" */
        break;

      case 5:			/* time */
        tm.tm_hour = atoi(field);
        tm.tm_min = atoi(&field[3]);
        tm.tm_sec = atoi(&field[6]);
        break;

      case 8:					/* flags */
        flags = strtol(field, NULL, 16);
        break;

      case 11:				/* data rate */
        speed = atoi(field) / 10;
        break;

      case 12:				/* last channel */
        channel = atoi(field);
        break;

      case 3:			/* type */
      case 6:			/* SNR/sig/noise */
      case 7:			/* name */
      case 9:			/* channel bits */
      case 10:		/* beacon interval */
      default:
        break;
      }

      field_num++;
      field = csv_lineparse(NULL, "\t", "", line_no);
    }

    if (lat == 0 && lon == 0) {	/* skip records with no GPS data */
      continue;
    }

    wpt_tmp = waypt_new();

    if (stealth) {
      if (!snmac) {
        snprintf(ssid, sizeof ssid, "Stealth/%d", ++stealth_num);
      }

      if (flags & 0x0010) {	/* encrypted? */
        wpt_tmp->icon_descr = seicon;
      } else {
        wpt_tmp->icon_descr = sneicon;
      }
    } else {
      if (flags & 0x0010) {	/* encrypted? */
        wpt_tmp->icon_descr = nseicon;
      } else {
        wpt_tmp->icon_descr = nsneicon;
      }
    }

    if (snmac) {
      snprintf(desc, sizeof desc, "%s/%d Mbps/Ch %d", ssid, speed, channel);
      wpt_tmp->shortname = xstrdup(mac);
    } else {
      snprintf(desc, sizeof desc, "%d Mbps/Ch %d/%s", speed, channel, mac);
      wpt_tmp->shortname = xstrdup(ssid);
    }

    wpt_tmp->description = xstrdup(desc);
    wpt_tmp->longitude = lon;
    wpt_tmp->latitude = lat;
    wpt_tmp->creation_time = mktime(&tm);

    waypt_add(wpt_tmp);
  }
  fix_netstumbler_dupes();
}

typedef struct {
  unsigned long crc;
  waypoint* wpt;
} htable_t;

static
int
compare(const void* a, const void* b)
{
  unsigned long crc_a = ((const htable_t*)a)->crc;
  unsigned long crc_b = ((const htable_t*)b)->crc;

  /* we can't just return crc_a - crc_b because the return type is
   * signed.
   * */

  if (crc_a < crc_b) {
    return -1;
  } else if (crc_a > crc_b) {
    return 1;
  } else {
    /* CRCs are equal; we need to subsort on the description (which
     * includes the MAC address) to guarantee the same ordering of
     * the output for any qsort() implementation.  this is strictly
     * to make the testo script happy.
     * */

    waypoint* wpt_a = ((const htable_t*)a)->wpt;
    waypoint* wpt_b = ((const htable_t*)b)->wpt;

    return strcmp(wpt_a->description, wpt_b->description);
  }
}

/* netstumbler data will have a lot of duplicate shortnames if the SSID
 * is used as the shortname (the default).  salvage the dupes by
 * synthesizing a (hopefully) unique shortname.
 * */

static
void
fix_netstumbler_dupes(void)
{
  int i, ct = waypt_count(), serial = 0;
  htable_t* htable, *bh;
  queue* elem, *tmp;
  extern queue waypt_head;
  const char* snptr;
  char* tmp_sn;
  unsigned long last_crc;
  char ssid[32 + 5 + 1];

  htable = (htable_t*) xmalloc(ct * sizeof *htable);
  bh = htable;

  i = 0;
  QUEUE_FOR_EACH(&waypt_head, elem, tmp) {
    bh->wpt = (waypoint*) elem;
    snptr = bh->wpt->shortname;
    tmp_sn = strlower(xstrdup(snptr));
    bh->crc = get_crc32(tmp_sn, strlen(snptr));
    xfree(tmp_sn);
    i ++;
    bh ++;
  }

  qsort(htable, ct, sizeof *htable, compare);

  last_crc = htable[0].crc + 1;		/* force mismatch */

  for (i = 0, bh = htable; i < ct; i++, bh++) {
    if (last_crc == bh->crc) {
      snprintf(ssid, sizeof ssid, "%s/%d", bh->wpt->shortname, ++serial);
      xfree(bh->wpt->shortname);
      bh->wpt->shortname = xstrdup(ssid);
    } else {
      last_crc = bh->crc;
    }
  }

  xfree(htable);
}

ff_vecs_t netstumbler_vecs = {
  ff_type_file,
  { ff_cap_read, ff_cap_none, ff_cap_none },
  rd_init,
  NULL,
  rd_deinit,
  NULL,
  data_read,
  NULL,
  NULL,
  netstumbler_args,
  CET_CHARSET_ASCII, 0	/* CET-REVIEW */
};
