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

#include <QHash>                       // for QHash
#include <QList>                       // for QList
#include <QPair>                       // for QPair
#include <QString>                     // for QString
#include <QVector>                     // for QVector
#include <QXmlStreamAttributes>        // for QXmlStreamAttributes

#include "defs.h"
#include "format.h"                    // for Format
#include "src/core/file.h"             // for File
#include "src/core/xmlstreamwriter.h"  // for XmlStreamWriter
#include "xmlgeneric.h"                // for xg_functor_map_entry, cb_start, cb_end


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
    QString value;
    QString icon;
  };

  /* Constants */

  static const QStringList osm_features;
  static const QVector<osm_icon_mapping_t> osm_icon_mappings;

  /* Member Functions */

  void osm_features_init();
  int osm_feature_ikey(const QString& key) const;
  QString osm_feature_symbol(int ikey, const char* value) const;
  static QString osm_strip_html(const QString& str);
  void osm_node_end(const QString& /* unused */, const QXmlStreamAttributes* /* unused */);
  void osm_node(const QString& /* unused */, const QXmlStreamAttributes* attrv);
  void osm_node_tag(const QString& /* unused */, const QXmlStreamAttributes* attrv);
  void osm_way(const QString& /* unused */, const QXmlStreamAttributes* attrv);
  void osm_way_nd(const QString& /* unused */, const QXmlStreamAttributes* attrv);
  void osm_way_tag(const QString& /* unused */, const QXmlStreamAttributes* attrv);
  void osm_way_center(const QString& /* unused */, const QXmlStreamAttributes* attrv);
  void osm_way_end(const QString& /* unused */, const QXmlStreamAttributes* /* unused */);
  void osm_init_icons();
  void osm_write_tag(const QString& key, const QString& value) const;
  void osm_disp_feature(const Waypoint* waypoint) const;
  void osm_write_opt_tag(const QString& atag);
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

  gpsbabel::File* ofile{nullptr};
  gpsbabel::XmlStreamWriter* fout{nullptr};
  int node_id{};
  int skip_rte{};

  route_head* rte{};
  Waypoint* wpt{};

  QList<XmlGenericReader::xg_fmt_map_entry<OsmFormat>> osm_map  {
    {&OsmFormat::osm_node, xg_cb_type::cb_start,	"/osm/node"},
    {&OsmFormat::osm_node_tag, xg_cb_type::cb_start,	"/osm/node/tag"},
    {&OsmFormat::osm_node_end, xg_cb_type::cb_end,		"/osm/node"},
    {&OsmFormat::osm_way, xg_cb_type::cb_start,	"/osm/way"},
    {&OsmFormat::osm_way_nd, xg_cb_type::cb_start,	"/osm/way/nd"},
    {&OsmFormat::osm_way_tag, xg_cb_type::cb_start,	"/osm/way/tag"},
    {&OsmFormat::osm_way_center, xg_cb_type::cb_start,	"/osm/way/center"},
    {&OsmFormat::osm_way_end, xg_cb_type::cb_end,		"/osm/way"}
  };
  XmlGenericReader* xml_reader{nullptr};
};
#endif // OSM_H_INCLUDED_
