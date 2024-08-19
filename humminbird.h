/*
    Copyright (C) 2008  Björn Augustsson, oggust@gmail.com
    Copyright (C) 2008  Olaf Klein, o.b.klein@gpsbabel.org
    Copyright (C) 2005-2013 Robert Lipe, robertlipe+source@gpsbabel.org

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
#ifndef HUMMINBIRD_H_INCLUDED_
#define HUMMINBIRD_H_INCLUDED_

#include <QHash>     // for QHash
#include <QString>   // for QString
#include <QVector>   // for QVector

#include <cstdint>   // for int32_t, uint32_t

#include "defs.h"    // for ff_cap, arglist_t, ff_cap_read, Waypoint, route_head, ff_cap_write, short_handle, ff_type, ff_type_file
#include "format.h"  // for Format
#include "gbfile.h"  // for gbfile
#include "mkshort.h" // for MakeShort


class HumminbirdBase
{
protected:
  /* Types */

  struct humminbird_waypt_t;
  struct humminbird_rte_t;
  struct humminbird_trk_header_t;
  struct humminbird_trk_point_t;
  struct humminbird_trk_header_old_t;
  struct humminbird_trk_point_old_t;
  struct group_header_t;
  struct group_body_t;

  /* Constants */

  // constants related to position conversions.
  static constexpr double i1924_equ_axis = 6378388.0;
  static constexpr double EAST_SCALE = 20038297.0; /* this is i1924_equ_axis*pi */
  // static constexpr double i1924_polar_axis = 6356911.946;
  // We use a modified international 1924 ellipse with a different flattening,
  // defined by cos_ae = cos(angular eccentricity).
  static constexpr double cos_ae = 0.9966349016452;
  static constexpr double cos2_ae = cos_ae * cos_ae;

  static constexpr const char* humminbird_icons[] = {
    "Normal",       /*  0 */
    "House",        /*  1 */
    "Red cross",    /*  2 */
    "Fish",         /*  3 */
    "Duck",         /*  4 */
    "Anchor",       /*  5 */
    "Buoy",         /*  6 */
    "Airport",      /*  7 */
    "Camping",      /*  8 */
    "Danger",       /*  9 */
    "Fuel",         /* 10 */
    "Rock",         /* 11 */
    "Weed",         /* 12 */
    "Wreck",        /* 13 */
    "Phone",        /* 14 */
    "Coffee",       /* 15 */
    "Beer",         /* 16 */
    "Mooring",      /* 17 */
    "Pier",         /* 18 */
    "Slip",         /* 19 */
    "Ramp",         /* 20 */
    "Circle",       /* 21 */
    "Diamond",      /* 22 */
    "Flag",         /* 23 */
    "Pattern",      /* 24 */
    "Shower",       /* 25 */
    "Water tap",    /* 26 */
    "Tree",         /* 27 */
    "Recording",    /* 28 */
    "Snapshot"      /* 29 */
  };

  /* Member Functions */

  static double geodetic_to_geocentric_hwr(double gd_lat);
  static double geocentric_to_geodetic_hwr(double gc_lat);
  static double gudermannian_i1924(double x);
  static double inverse_gudermannian_i1924(double x);
  void humminbird_rd_init(const QString& fname);
  void humminbird_rd_deinit() const;
  void humminbird_read_wpt(gbfile* fin);
  void humminbird_read_route(gbfile* fin) const;
  static void humminbird_read_track(gbfile* fin);
  static void humminbird_read_track_old(gbfile* fin);
  void humminbird_read();
  void humminbird_wr_init(const QString& fname);
  void humminbird_wr_deinit();

  /* Data Members */

  gbfile* fin_{};
  gbfile* fout_{};
  int waypoint_num{};
  MakeShort* wptname_sh{};
  MakeShort* rtename_sh{};
  MakeShort* trkname_sh{};
  humminbird_rte_t* humrte{};
  int rte_num_{};
  QHash<unsigned int, const Waypoint*> wpt_num_to_wpt_hash;
  QHash<QString, unsigned int> wpt_id_to_wpt_num_hash;

  humminbird_trk_header_t* trk_head{};
  humminbird_trk_point_t* trk_points{};
  int32_t last_east{};
  int32_t last_north{};
  uint32_t last_time{};
};

class HumminbirdFormat : public Format, private HumminbirdBase
{
public:
  QVector<arglist_t>* get_args() override
  {
    return &humminbird_args;
  }

  ff_type get_type() const override
  {
    return ff_type_file;
  }

  QVector<ff_cap> get_cap() const override
  {
    return {
      (ff_cap)(ff_cap_read | ff_cap_write) 	/* waypoints */,
      ff_cap_read 			/* tracks */,
      (ff_cap)(ff_cap_read | ff_cap_write)	/* routes */
    };
  }

  void rd_init(const QString& fname) override {humminbird_rd_init(fname);}
  void read() override {humminbird_read();}
  void rd_deinit() override {humminbird_rd_deinit();}
  void wr_init(const QString& fname) override {humminbird_wr_init(fname);}
  void write() override;
  void wr_deinit() override {humminbird_wr_deinit();}

private:
  /* Member Functions */

  void humminbird_rte_head(const route_head* rte);
  void humminbird_rte_tail(const route_head* rte);
  static QString wpt_to_id(const Waypoint*);
  void humminbird_write_rtept(const Waypoint* wpt) const;
  void humminbird_write_waypoint(const Waypoint* wpt);
  void humminbird_write_waypoint_wrapper(const Waypoint* wpt);

  /* Data Members */

  QVector<arglist_t> humminbird_args = {
  };
};

class HumminbirdHTFormat : public Format, private HumminbirdBase
{
public:
  QVector<arglist_t>* get_args() override
  {
    return &humminbirdht_args;
  }

  ff_type get_type() const override
  {
    return ff_type_file;
  }

  QVector<ff_cap> get_cap() const override
  {
    return {
      ff_cap_read		 	/* waypoints */,
      (ff_cap)(ff_cap_read | ff_cap_write)	/* tracks */,
      ff_cap_read			/* routes */
    };
  }

  void rd_init(const QString& fname) override {humminbird_rd_init(fname);}
  void read() override {humminbird_read();}
  void rd_deinit() override {humminbird_rd_deinit();}
  void wr_init(const QString& fname) override {humminbird_wr_init(fname);}
  void write() override;
  void wr_deinit() override {humminbird_wr_deinit();}

private:
  /* Member Functions */

  void humminbird_track_head(const route_head* trk);
  void humminbird_track_tail(const route_head* unused);
  void humminbird_track_cb(const Waypoint* wpt);

  /* Data Members */

  QVector<arglist_t> humminbirdht_args = {
  };
};

#endif // HUMMINBIRD_H_INCLUDED_
