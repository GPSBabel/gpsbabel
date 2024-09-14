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
#ifndef RANDOM_H_INCLUDED_
#define RANDOM_H_INCLUDED_

#include <random>            // for mt19937

#include <QDateTime>         // for QDateTime
#include <QString>           // for QString
#include <QVector>           // for QVector

#include "defs.h"
#include "format.h"


class RandomFormat : public Format
{
public:
  /* Member Functions */

  QVector<arglist_t>* get_args() override
  {
    return &random_args;
  }

  ff_type get_type() const override
  {
    return ff_type_internal;
  }

  QVector<ff_cap> get_cap() const override
  {
    return {
      ff_cap_read /* waypoints */,
      ff_cap_read /* tracks */,
      ff_cap_read /* routes */
    };
  }

  void rd_init(const QString& fname) override;
  void read() override;
  void rd_deinit() override;
  void rd_position_init(const QString& fname) override;
  Waypoint* rd_position(posn_status* status) override;
  void rd_position_deinit() override;

private:
  /* Types */

  struct realtime_data {
    QDateTime time;
    int points{-1};
    int point_count{0};
    Waypoint prev;
  };

  /* Member Functions */

  double rand_dbl(double max);
  float rand_flt(float max);
  int rand_int(int max);
  QString rand_str(int maxlen, const char* fmt);
  void random_set_generator();
  Waypoint* random_generate_wpt(int i, const QDateTime& time, const Waypoint* prev);

  /* Data Members */

  char* opt_points{nullptr};
  char* opt_seed{nullptr};
  char* opt_nodelay{nullptr};

  QVector<arglist_t> random_args = {
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
  };

//  this generator is invariant across platforms.
  std::mt19937* generator{nullptr};

// we do this cheesy distribution function because we need it to be invariant across platforms.
// note uniform_int_distribution is not invariant.
  template <typename T>
  T
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

  realtime_data* realtime{nullptr};
};
#endif // RANDOM_H_INCLUDED_
