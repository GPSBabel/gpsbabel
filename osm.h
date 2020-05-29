/*

	Support for "OpenStreetMap" data files (.xml)

	Copyright (C) 2008 Olaf Klein, o.b.klein@gpsbabel.org

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
#ifndef OSM_H_INCLUDED_
#define OSM_H_INCLUDED_

#include <QtCore/QHash>                 // for QHash
#include <QtCore/QList>                 // for QList
#include <QtCore/QPair>                 // for QPair
#include <QtCore/QString>               // for QString
#include <QtCore/QVector>               // for QVector
#include <QtCore/QXmlStreamAttributes>  // for QXmlStreamAttributes

#include "defs.h"
#include "format.h"                     // for Format
#include "gbfile.h"                     // for gbfile
#include "xmlgeneric.h"                 // for xg_functor_map_entry, cb_start, cb_end, xg_string


class OsmFormat : public Format
{
public:
  /* Member Functions */

  QVector<arglist_t>* get_args() override
  {
    return &osm_args;
  }

  ff_type get_type() const override
  {
    return ff_type_file;
  }

  QVector<ff_cap> get_cap() const override
  {
    return {
      (ff_cap)(ff_cap_read | ff_cap_write)	/* waypoints */,
      ff_cap_write 			/* tracks */,
      (ff_cap)(ff_cap_read | ff_cap_write) 	/* routes */,
    };
  }

  QString get_encode() const override
  {
    return CET_CHARSET_UTF8;
  }

  int get_fixed_encode() const override
  {
    return 0;
  }

  void rd_init(const QString& fname) override;
  void read() override;
  void rd_deinit() override;
  void wr_init(const QString& fname) override;
  void write() override;
  void wr_deinit() override;
  void exit() override;

private:
  /* Types */

  struct osm_icon_mapping_t {
    int key;
    const char* value;
    const char* icon;
  };

  /* Constants */

  static const char* const osm_features[];
  static const osm_icon_mapping_t osm_icon_mappings[];

  /* Member Functions */

  void osm_features_init();
  char osm_feature_ikey(const QString& key) const;
  QString osm_feature_symbol(int ikey, const char* value) const;
  static char* osm_strip_html(const char* str);
  QString osm_strip_html(const QString& str) const;
  void osm_node_end(xg_string /* unused */, const QXmlStreamAttributes* /* unused */);
  void osm_node(xg_string /* unused */, const QXmlStreamAttributes* attrv);
  void osm_node_tag(xg_string /* unused */, const QXmlStreamAttributes* attrv);
  void osm_way(xg_string /* unused */, const QXmlStreamAttributes* attrv);
  void osm_way_nd(xg_string /* unused */, const QXmlStreamAttributes* attrv);
  void osm_way_tag(xg_string /* unused */, const QXmlStreamAttributes* attrv);
  void osm_way_center(xg_string /* unused */, const QXmlStreamAttributes* attrv);
  void osm_way_end(xg_string /* unused */, const QXmlStreamAttributes* /* unused */);
  void osm_init_icons();
  void osm_write_tag(const QString& key, const QString& value) const;
  void osm_disp_feature(const Waypoint* waypoint) const;
  void osm_write_opt_tag(const char* atag);
  static void osm_release_ids(const Waypoint* waypoint);
  static QString osm_name_from_wpt(const Waypoint* waypoint);
  void osm_waypt_disp(const Waypoint* waypoint);
  void osm_rte_disp_head(const route_head* route);
  void osm_rtept_disp(const Waypoint* wpt_ref) const;
  void osm_rte_disp_trail(const route_head* route);

  /* Data Members */

  char* opt_tag{};
  char* opt_tagnd{};
  char* created_by{};

  QVector<arglist_t> osm_args = {
    { "tag", &opt_tag, 	"Write additional way tag key/value pairs", nullptr, ARGTYPE_STRING, ARG_NOMINMAX, nullptr},
    { "tagnd", &opt_tagnd,	"Write additional node tag key/value pairs", nullptr, ARGTYPE_STRING, ARG_NOMINMAX, nullptr },
    { "created_by", &created_by, "Use this value as custom created_by value","GPSBabel", ARGTYPE_STRING, ARG_NOMINMAX, nullptr },
  };

  QHash<QString, const Waypoint*> waypoints;

  QHash<QString, int> keys;
  QHash<QPair<int, QString>, const osm_icon_mapping_t*> values;
  QHash<QString, const osm_icon_mapping_t*> icons;

  gbfile* fout{};
  int node_id{};
  int skip_rte{};

  route_head* rte{};
  Waypoint* wpt{};

  QList<xg_functor_map_entry<OsmFormat>> osm_map = {
    {&OsmFormat::osm_node,	cb_start,	"/osm/node"},
    {&OsmFormat::osm_node_tag,	cb_start,	"/osm/node/tag"},
    {&OsmFormat::osm_node_end,	cb_end,		"/osm/node"},
    {&OsmFormat::osm_way,	cb_start,	"/osm/way"},
    {&OsmFormat::osm_way_nd,	cb_start,	"/osm/way/nd"},
    {&OsmFormat::osm_way_tag,	cb_start,	"/osm/way/tag"},
    {&OsmFormat::osm_way_center,	cb_start,	"/osm/way/center"},
    {&OsmFormat::osm_way_end,	cb_end,		"/osm/way"}
  };
};
#endif // OSM_H_INCLUDED_
