/*
    random - GPS data generator

    Copyright (C) 2007 Olaf Klein, o.b.klein@gpsbabel.org

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
#include "garmin_fs.h"
#include "jeeps/gpsmath.h"
#include <time.h>

#define MYNAME "random"

static char *opt_points, *opt_seed;

static arglist_t random_args[] = {
  {
    "points", &opt_points, "Generate # points", NULL,
    ARGTYPE_INT, "1", NULL
  },
  {
    "seed", &opt_seed, "Starting seed of the internal number generator", NULL,
    ARGTYPE_INT, "1", NULL
  },
  ARG_TERMINATOR
};


static double
rand_dbl(const double max)
{
  return max * rand() / (((double)RAND_MAX) + 1);
}

static int
rand_int(const int max)
{
  return (int)((double)max * rand() / (((double)RAND_MAX) + 1));
}

/* rand_str always returns a valid string with len >= 0 */

static char *
rand_str(const int maxlen, const char *fmt)
{
  char *res;
  int i, len;

  len = rand_int(maxlen) + 1;

  res = (char*) xmalloc(len + 1);
  res[len] = '\0';

  for (i = 0; i < len; i++) {
    int c = rand_int(26 + 26 + 10);
    if (c < 26) {
      c += 'a';
    } else if (c < 52) {
      c = (c - 26) + 'A';
    } else {
      c = (c - 52) + '0';
    }
    res[i] = c;
  }
  if (fmt) {
    char *tmp;
    xasprintf(&tmp, fmt, res);
    xfree(res);
    return tmp;
  } else {
    return res;
  }
}

static void
random_rd_init(const char *fname)
{
}

static void
random_rd_deinit(void)
{
}

static void
random_read(void)
{
#define RND(a) (rand_int(a) > 0)

  int i, points;
  route_head *head;
  waypoint *prev = NULL;
  time_t time = gpsbabel_time;

  if (opt_seed) {
    srand(atoi(opt_seed));
  } else {
    srand(gpsbabel_now);
  }


  points = (opt_points) ? atoi(opt_points) : rand_int(128) + 1;
  if (doing_trks || doing_rtes) {
    head = route_head_alloc();
    if (doing_trks) {
      head->rte_name = rand_str(8, "Trk_%s");
      track_add_head(head);
    } else {
      head->rte_name = rand_str(8, "Rte_%s");
      route_add_head(head);
    }
    head->rte_desc = rand_str(16, NULL);
  } else {
    head = NULL;
  }

  for (i = 0; i < points; i++) {

    waypoint *wpt;
    garmin_fs_t *gmsd;

    wpt = waypt_new();
    gmsd = garmin_fs_alloc(-1);
    fs_chain_add(&wpt->fs, (format_specific_data *) gmsd);

    do {
      wpt->shortname = rand_str(8, "Wpt_%s");
    } while (wpt->shortname == NULL);

    wpt->latitude = rand_dbl(180) - 90;
    wpt->longitude = rand_dbl(360) - 180;

    /* !!! "if RND(3) ..." produces some leaks in generated data !!! */

    if RND(3) {
      wpt->altitude = rand_int(1000) / 10;
    }
    if RND(3) {
      WAYPT_SET(wpt, temperature, rand_int(320) / 10.0);
    }
    if RND(3) {
      WAYPT_SET(wpt, proximity, rand_int(10000) / 10.0);
    }
    if RND(3) {
      WAYPT_SET(wpt, depth, rand_int(10000) / 10.0);
    }

    wpt->creation_time = time;
    if RND(3) {
      wpt->microseconds = rand_int(1000) * 1000;
    }
    time += rand_int(10) + 1;

    if (doing_trks) {
      if (i > 0) {
        wpt->latitude = prev->latitude + (rand_dbl(1) / 1000);
        wpt->longitude = prev->longitude + (rand_dbl(1) / 1000);
        WAYPT_SET(wpt, course, waypt_course(prev, wpt));
        WAYPT_SET(wpt, speed, waypt_speed(prev, wpt));
      }
      wpt->sat = rand_int(12 + 1);
      wpt->hdop = (rand_int(500)) / 10.0;
      wpt->vdop = (rand_int(500)) / 10.0;
      wpt->pdop = (rand_int(500)) / 10.0;
      wpt->fix = (fix_type)(rand_int(6) - 1);
      if RND(3) {
        wpt->cadence = rand_int(255);
      }
      if RND(3) {
        wpt->heartrate = rand_int(255);
      }
    } else {
      if (doing_rtes && (i > 0)) {
        wpt->latitude = prev->latitude + (rand_dbl(1) / 100);
        wpt->longitude = prev->longitude + (rand_dbl(1) / 100);
      }
      if RND(3) {
        wpt->description = rand_str(16, "Des_%s");
      }
      if RND(3) {
        wpt->notes = rand_str(16, "Nts_%s");
      }
      if RND(3) {
        GMSD_SET(addr, rand_str(8, "Adr_%s"));
      }
      if RND(3) {
        GMSD_SET(city, rand_str(8, "Cty_%s"));
      }
      if RND(3) {
        GMSD_SET(facility, rand_str(8, "Fac_%s"));
      }
      if RND(3) {
        GMSD_SET(country, rand_str(8, "Ctr_%s"));
      }
      if RND(3) {
        GMSD_SET(state, rand_str(8, "Sta_%s"));
      }
      if RND(3) {
        GMSD_SET(phone_nr, rand_str(8, "Pnr_%s"));
      }
      if RND(3) {
        GMSD_SET(postal_code, rand_str(8, "Pcd_%s"));
      }
    }

    if (doing_trks) {
      track_add_wpt(head, wpt);
    } else if (doing_rtes) {
      route_add_wpt(head, wpt);
    } else {
      waypt_add(wpt);
    }

    prev = wpt;
  }
}


ff_vecs_t random_vecs = {
  ff_type_internal,
  {
    ff_cap_read /* waypoints */,
    ff_cap_read /* tracks */,
    ff_cap_read /* routes */
  },
  random_rd_init,
  NULL,	/* wr_init */
  random_rd_deinit,
  NULL,	/* wr_deinit */
  random_read,
  NULL,	/* write */
  NULL,	/* exit */
  random_args,
  CET_CHARSET_ASCII, 1			/* fixed */
};
