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
    Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.
*/

#include <random>               // for mt19937

#include <QByteArray>           // for QByteArray
#include <QDateTime>            // for QDateTime
#include <QString>              // for QString
#include <QThread>              // for QThread

#include "defs.h"
#include "random.h"
#include "formspec.h"           // for FormatSpecificDataList
#include "garmin_fs.h"          // for garmin_fs_t, GMSD_SET, garmin_fs_flags_t, garmin_fs_alloc
#include "src/core/datetime.h"  // for DateTime


double
RandomFormat::rand_dbl(const double max)
{
  return rand_num(max);
}

float
RandomFormat::rand_flt(const float max)
{
  return rand_num(max);
}

int
RandomFormat::rand_int(const int max)
{
  return rand_num(max);
}

/* rand_str always returns a valid string with len >= 0 */

QString
RandomFormat::rand_str(const int maxlen, const char* fmt)
{
  int len = rand_int(maxlen) + 1;

  QByteArray res;
  res.resize(len);

  for (int i = 0; i < len; ++i) {
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
  return (fmt != nullptr)?
         QString::asprintf(fmt, res.constData()) : QString(res);
}

void
RandomFormat::random_set_generator()
{
  generator = new std::mt19937;
  if (opt_seed) {
    generator->seed(xstrtoi(opt_seed, nullptr, 10));
  } else {
    generator->seed(gpsbabel_time);
  }
}

void
RandomFormat::rd_init(const QString&)
{
  random_set_generator();
}

void
RandomFormat::rd_deinit()
{
  delete generator;
}

#define RND(a) (rand_int(a) > 0)

Waypoint*
RandomFormat::random_generate_wpt(int i, const QDateTime& time, const Waypoint* prev)
{
  auto* wpt = new Waypoint;
  garmin_fs_t* gmsd = garmin_fs_alloc(-1);
  wpt->fs.FsChainAdd(gmsd);

  do {
    wpt->shortname = rand_str(8, "Wpt_%s");
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
    wpt->AddUrlLink(rand_str(8, "http://link1.example.com/%s"));
    if RND(3) {
      wpt->AddUrlLink(rand_str(8, "http://link2.example.com/%s"));
    }
  }
  if RND(3) {
    wpt->icon_descr = rand_str(3, "Icon_%s");
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
    if RND(3) {
      wpt->power = rand_flt(500.0);
    }
  } else {
    if (doing_rtes && (i > 0)) {
      wpt->latitude = prev->latitude + rand_dbl(0.01);
      wpt->longitude = prev->longitude + rand_dbl(0.01);
    }
    if RND(3) {
      wpt->description = rand_str(16, "Des_%s");
    }
    if RND(3) {
      wpt->notes = rand_str(16, "Nts_%s");
    }
    if RND(3) {
      garmin_fs_t::set_addr(gmsd, rand_str(8, "Adr_%s"));
    }
    if RND(3) {
      garmin_fs_t::set_city(gmsd, rand_str(8, "Cty_%s"));
    }
    if RND(3) {
      garmin_fs_t::set_facility(gmsd, rand_str(8, "Fac_%s"));
    }
    if RND(3) {
      garmin_fs_t::set_country(gmsd, rand_str(8, "Ctr_%s"));
    }
    if RND(3) {
      garmin_fs_t::set_state(gmsd, rand_str(8, "Sta_%s"));
    }
    if RND(3) {
      garmin_fs_t::set_phone_nr(gmsd, rand_str(8, "Pnr_%s"));
    }
    if RND(3) {
      garmin_fs_t::set_postal_code(gmsd, rand_str(8, "Pcd_%s"));
    }
  }

  return wpt;
}

void
RandomFormat::read()
{
  route_head* head;
  Waypoint* prev = nullptr;
  QDateTime time = current_time().toUTC();

  int points = (opt_points) ? xstrtoi(opt_points, nullptr, 10) : rand_int(128) + 1;
  if (doing_trks || doing_rtes) {
    head = new route_head;
    if (doing_trks) {
      head->rte_name = rand_str(8, "Trk_%s");
      track_add_head(head);
    } else {
      head->rte_name = rand_str(8, "Rte_%s");
      route_add_head(head);
    }
    head->rte_desc = rand_str(16, nullptr);
    if RND(3) {
      head->rte_urls.AddUrlLink(UrlLink(rand_str(8, "http://rteurl.example.com/%s")));
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

void
RandomFormat::rd_position_init(const QString&)
{
  random_set_generator();
  realtime = new realtime_data;
  if (opt_points) {
    realtime->points = xstrtoi(opt_points, nullptr, 10);
  }
  realtime->time = current_time().toUTC();
}

void
RandomFormat::rd_position_deinit()
{
  delete generator;
  delete realtime;
}

Waypoint*
RandomFormat::rd_position(posn_status* status)
{
  Waypoint* wpt = random_generate_wpt(realtime->point_count, realtime->time, &(realtime->prev));

  if (status && (realtime->points > 0) && (realtime->point_count >= realtime->points)) {
    status->request_terminate = 1;
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
