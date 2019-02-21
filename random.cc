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

#include <cstdlib>           // for atoi
#include <random>            // for mt19937

#include <QtCore/QDateTime>  // for QDateTime
#include <QtCore/QString>    // for QString
#include <QtCore/QThread>    // for QThread

#include "defs.h"
#include "garmin_fs.h"       // for garmin_fs_t, GMSD_SET, garmin_fs_flags_t, garmin_fs_alloc

#define MYNAME "random"

static char* opt_points, *opt_seed, *opt_nodelay;

static arglist_t random_args[] = {
  {
    "points", &opt_points, "Generate # points", nullptr,
    ARGTYPE_INT, "1", nullptr, nullptr
  },
  {
    "seed", &opt_seed, "Starting seed of the internal number generator", nullptr,
    ARGTYPE_INT, "1", nullptr, nullptr
  },
  {
    "nodelay", &opt_nodelay, "Output realtime points without delay", nullptr,
    ARGTYPE_BOOL, ARG_NOMINMAX, nullptr
  },
  ARG_TERMINATOR
};

//  this generator is invariant across platforms.
static std::mt19937* generator;

// we do this cheesy distribution function because we need it to be invariant across platforms.
// note uniform_int_distribution is not invariant.
template <typename T>
static T
rand_num(const T max)
{
  T retval;
  // scalefactor expression assumes generator is mt19937.
  constexpr double scalefactor = 1.0 / std::mt19937::max();
  do {
    retval = static_cast<T>(static_cast<double>(max) * scalefactor * (*generator)());
  } while (retval >= max);
  return retval;
}

static double
rand_dbl(const double max)
{
  return rand_num(max);
}

static float
rand_flt(const float max)
{
  return rand_num(max);
}

static int
rand_int(const int max)
{
  return rand_num(max);
}

/* rand_str always returns a valid string with len >= 0 */

static char*
rand_str(const int maxlen, const char* fmt)
{
  int len = rand_int(maxlen) + 1;

  auto res = (char*) xmalloc(len + 1);
  res[len] = '\0';

  for (int i = 0; i < len; i++) {
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
    char* tmp;
    xasprintf(&tmp, fmt, res);
    xfree(res);
    return tmp;
  } else {
    return res;
  }
}

static QString 
rand_qstr(const int maxlen, const char* fmt)
{
  char * str = rand_str(maxlen, fmt);
  QString qstr = QString(str);
  xfree(str);
  return qstr;
}

static void
random_rd_init(const QString&)
{
  generator = new std::mt19937;
  if (opt_seed) {
    generator->seed(atoi(opt_seed));
  } else {
    generator->seed(gpsbabel_now);
  }
}

static void
random_rd_deinit()
{
  delete generator;
}

#define RND(a) (rand_int(a) > 0)

static Waypoint*
random_generate_wpt(int i, const QDateTime& time, const Waypoint* prev)
{
    auto wpt = new Waypoint;
    garmin_fs_t* gmsd = garmin_fs_alloc(-1);
    fs_chain_add(&wpt->fs, (format_specific_data*) gmsd);

    do {
      wpt->shortname = rand_qstr(8, "Wpt_%s");
    } while (wpt->shortname == nullptr);

    wpt->latitude = rand_dbl(180.0) - 90.0;
    wpt->longitude = rand_dbl(360.0) - 180.0;

    /* !!! "if RND(3) ..." produces some leaks in generated data !!! */

    if RND(3) {
      wpt->altitude = rand_dbl(100.0);
    }
    if RND(3) {
      WAYPT_SET(wpt, temperature, rand_flt(32.0f));
    }
    if RND(3) {
      WAYPT_SET(wpt, proximity, rand_dbl(1000.0));
    }
    if RND(3) {
      WAYPT_SET(wpt, depth, rand_dbl(1000.0));
    }
    if RND(3) {
      wpt->AddUrlLink(rand_qstr(8, "http://link1.example.com/%s"));
      if RND(3) {
        wpt->AddUrlLink(rand_qstr(8, "http://link2.example.com/%s"));
      }
    }
    if RND(3) {
      wpt->icon_descr = rand_qstr(3, "Icon_%s");
    }

    wpt->SetCreationTime(time);

    if (doing_trks || doing_posn) {
      if (i > 0) {
        wpt->latitude = prev->latitude + rand_dbl(0.001);
        wpt->longitude = prev->longitude + rand_dbl(0.001);
        WAYPT_SET(wpt, course, waypt_course(prev, wpt));
        WAYPT_SET(wpt, speed, waypt_speed(prev, wpt));
      }
      wpt->sat = rand_int(12 + 1);
      wpt->hdop = rand_flt(50.0f);
      wpt->vdop = rand_flt(50.0f);
      wpt->pdop = rand_flt(50.0f);
      wpt->fix = (fix_type)(rand_int(6) - 1);
      if RND(3) {
        wpt->cadence = rand_int(255);
      }
      if RND(3) {
        wpt->heartrate = rand_int(255);
      }
    } else {
      if (doing_rtes && (i > 0)) {
        wpt->latitude = prev->latitude + rand_dbl(0.01);
        wpt->longitude = prev->longitude + rand_dbl(0.01);
      }
      if RND(3) {
        wpt->description = rand_qstr(16, "Des_%s");
      }
      if RND(3) {
        wpt->notes = rand_qstr(16, "Nts_%s");
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

  return wpt;
}

static void
random_read()
{

  route_head* head;
  Waypoint* prev = nullptr;
  QDateTime time = QDateTime::fromTime_t(gpsbabel_time);

  int points = (opt_points) ? atoi(opt_points) : rand_int(128) + 1;
  if (doing_trks || doing_rtes) {
    head = route_head_alloc();
    if (doing_trks) {
      head->rte_name = rand_qstr(8, "Trk_%s");
      track_add_head(head);
    } else {
      head->rte_name = rand_qstr(8, "Rte_%s");
      route_add_head(head);
    }
    head->rte_desc = rand_qstr(16, nullptr);
	if RND(3) {
      head->rte_urls.AddUrlLink(UrlLink(rand_qstr(8, "http://rteurl.example.com/%s")));
    }
  } else {
    head = nullptr;
  }

  for (int i = 0; i < points; i++) {
    Waypoint* wpt = random_generate_wpt(i, time, prev);
    if (doing_trks) {
      track_add_wpt(head, wpt);
    } else if (doing_rtes) {
      route_add_wpt(head, wpt);
    } else {
      waypt_add(wpt);
    }

    time = time.addMSecs(1000 + rand_int(10000));
    prev = wpt;
  }
}

struct realtime_data {
  QDateTime time;
  int points{-1};
  int point_count{0};
  Waypoint prev;
};
static realtime_data* realtime;

void
random_rd_posn_init(const QString&)
{
  generator = new std::mt19937;
  if (opt_seed) {
    generator->seed(atoi(opt_seed));
  } else {
    generator->seed(gpsbabel_now);
  }
  realtime = new realtime_data;
  if (opt_points) {
    realtime->points = atoi(opt_points);
  }
  realtime->time = QDateTime::fromTime_t(gpsbabel_time);
}

void
random_rd_posn_deinit()
{
  delete generator;
  delete realtime;
}

static Waypoint*
random_rd_posn(posn_status* p_status)
{
  Waypoint* wpt = random_generate_wpt(realtime->point_count, realtime->time, &(realtime->prev));

  if (p_status && (realtime->points > 0) && (realtime->point_count >= realtime->points)) {
    p_status->request_terminate = 1;
  }
  ++realtime->point_count;

  int delta_msecs= 1000 + rand_int(1000);
  realtime->time = realtime->time.addMSecs(delta_msecs);
  if (!opt_nodelay) {
    QThread::msleep(delta_msecs);
  }

  // copy the waypoint as main will delete the returned waypt
  // after write and we need it to generate the next wpt to
  // simulate realtime tracking data.
  realtime->prev = *wpt;

  return wpt;
}

ff_vecs_t random_vecs = {
  ff_type_internal,
  {
    ff_cap_read /* waypoints */,
    ff_cap_read /* tracks */,
    ff_cap_read /* routes */
  },
  random_rd_init,
  nullptr,	/* wr_init */
  random_rd_deinit,
  nullptr,	/* wr_deinit */
  random_read,
  nullptr,	/* write */
  nullptr,	/* exit */
  random_args,
  CET_CHARSET_ASCII, 1,			/* fixed */
  {
  random_rd_posn_init, random_rd_posn, random_rd_posn_deinit,
  nullptr, nullptr, nullptr,
  },
  nullptr
};
