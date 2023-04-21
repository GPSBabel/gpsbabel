/*
	Garmin GPS Database Reader/Writer

	Copyright (C) 2005-2008 Olaf Klein, o.b.klein@gpsbabel.org
	Mainly based on mapsource.c,
	Copyright (C) 2005 Robert Lipe, robertlipe+source@gpsbabel.org


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

  A format description obtained from reverse-engineering is at
  https://www.memotech.franken.de/FileFormats/Garmin_MPS_GDB_and_GFI_Format.pdf
*/
#ifndef GDB_H_INCLUDED_
#define GDB_H_INCLUDED_

#include <QList>            // for QList
#include <QString>          // for QString
#include <QStringView>      // for QStringView
#include <QVector>          // for QVector

#include "defs.h"           // for arglist_t, Waypoint, route_head, ARGTYPE_BOOL, ARGTYPE_INT, ARG_NOMINMAX, bounds, FF_CAP_RW_ALL, ff_cap, ff_type, ff_type_file, short_handle
#include "format.h"         // for Format
#include "garmin_fs.h"      // for garmin_fs_t
#include "garmin_tables.h"  // for gt_waypt_classes_e
#include "gbfile.h"         // for gbfile


class GdbFormat : public Format
{
public:
  QVector<arglist_t>* get_args() override
  {
    return &gdb_args;
  }

  ff_type get_type() const override
  {
    return ff_type_file;
  }

  QVector<ff_cap> get_cap() const override
  {
    return FF_CAP_RW_ALL;
  }

  void rd_init(const QString& fname) override;
  void read() override;
  void rd_deinit() override;
  void wr_init(const QString& fname) override;
  void write() override;
  void wr_deinit() override;

private:
  /* Constants */

  static constexpr int kGDBVer1 = 1;
  static constexpr int kGDBVer2 = 2;
  static constexpr int kGDBVer3 = 3;

  static constexpr int kGDBVerUTF8 = kGDBVer3;
  static constexpr int kGDBVerMin = kGDBVer1;
  static constexpr int kGDBVerMax = kGDBVer3;

  static constexpr int kGDBDefIcon = 18;

  static constexpr int kGDBNameBufferLen = 1024;

  /* static constexpr char gdb_release[] = "$Revision: 1.74 $"; */
  static constexpr char gdb_release_date[] = "$Date: 2011-04-14 01:30:01 $";

  /* Member Functions */

  static void gdb_flush_waypt_queue(QList<Waypoint*>* Q);
  void disp_summary(const gbfile* f) const;
  QString fread_cstr() const;
  static char* gdb_fread_cstr(gbfile* file_in);
  QString gdb_fread_strlist() const;
  static Waypoint* gdb_find_wayptq(const QList<Waypoint*>* Q, const Waypoint* wpt, char exact);
  Waypoint* gdb_reader_find_waypt(const Waypoint* wpt, char exact) const;
  Waypoint* gdb_add_route_waypt(route_head* rte, Waypoint* ref, int wpt_class) const;
  static QString gdb_to_ISO8601_duration(unsigned int seconds);
  void gdb_write_cstr(QStringView a = QStringView()) const;
  void gdb_write_cstr_list(QStringView str = QStringView()) const;
  void gdb_write_dbl(double value, double def) const;
  void gdb_write_time(int time) const;
  void read_file_header();
  Waypoint* read_waypoint(gt_waypt_classes_e* waypt_class_out);
  route_head* read_route();
  route_head* read_track();
  void reset_short_handle(const char* defname);
  void write_header() const;
  static void gdb_check_waypt(Waypoint* wpt);
  void write_waypoint(const Waypoint* wpt, const QString& shortname, garmin_fs_t* gmsd, int icon, int display);
  static void route_compute_bounds(const route_head* rte, bounds* bounds);
  void route_write_bounds(bounds* bounds) const;
  void write_route(const route_head* rte, const QString& rte_name);
  void write_track(const route_head* trk, const QString& trk_name);
  void finalize_item(gbfile* origin, char identifier);
  void write_waypoint_cb(const Waypoint* refpt);
  void write_route_cb(const route_head* rte);
  void write_track_cb(const route_head* trk);

  /* Data Members */

  gbfile* fin{}, *fout{}, *ftmp{};
  int gdb_ver{}, gdb_category{};
  bool gdb_roadbook{};
  bool gdb_hide_wpt{};
  bool gdb_hide_rpt{};

  QList<Waypoint*> wayptq_in, wayptq_out, wayptq_in_hidden;
  short_handle short_h{};

  char* gdb_opt_category{};
  char* gdb_opt_ver{};
  char* gdb_opt_via{};
  char* gdb_opt_roadbook{};
  char* gdb_opt_bitcategory{};
  char* gdb_opt_drop_hidden_wpt{};

  int waypt_flag{};
  int route_flag{};

  int waypt_ct{};	/* informational: total number of waypoints in/out */
  int waypth_ct{};	/* informational: total number of hidden waypoints in/out */
  int rtept_ct{};	/* informational: total number of route points in/out */
  int trkpt_ct{};	/* informational: total number of track points in/out */
  int rte_ct{};	/* informational: total number of routes in/out */
  int trk_ct{};	/* informational: total number of tracks in/out */

  QVector<arglist_t> gdb_args = {
    {
      "cat", &gdb_opt_category,
      "Default category on output (1..16)",
      nullptr, ARGTYPE_INT, "1", "16", nullptr
    },
    {
      "bitscategory", &gdb_opt_bitcategory, "Bitmap of categories",
      nullptr, ARGTYPE_INT, "1", "65535", nullptr
    },
    {
      "ver", &gdb_opt_ver,
      "Version of gdb file to generate (1..3)",
      "2", ARGTYPE_INT, "1", "3", nullptr
    },
    {
      "via", &gdb_opt_via,
      "Drop route points that do not have an equivalent waypoint (hidden points)",
      nullptr, ARGTYPE_BOOL, ARG_NOMINMAX, nullptr
    },
    {
      "dropwpt", &gdb_opt_drop_hidden_wpt,
      "Don't create waypoints for non-user points",
      nullptr, ARGTYPE_BOOL, ARG_NOMINMAX, nullptr
    },
    {
      "roadbook", &gdb_opt_roadbook,
      "Include major turn points (with description) from calculated route",
      nullptr, ARGTYPE_BOOL, ARG_NOMINMAX, nullptr
    }
  };
};
#endif // GDB_H_INCLUDED_
