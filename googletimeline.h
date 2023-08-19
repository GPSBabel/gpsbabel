/*
  Support reading Google Takeout Timeline Location History JSON format.

  Copyright (C) 2023 Tyler MacDonald, tyler@macdonald.name

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
  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301,
  USA.
*/
#ifndef _GOOGLETIMELINE_H
#define _GOOGLETIMELINE_H

#include <QVector>

#include "defs.h"
#include "format.h"

/*
 * Reads Location History JSON files and return each timelineObject
 * that should be processed.
 *
 * TODO: Allow date ranges
 */
class GoogleTimelineInputStream
{
public:
  GoogleTimelineInputStream();
  GoogleTimelineInputStream(const QString& source);
  // Returns the next timelineObject, or a null QJsonValue if we're at the end
  QJsonValue next();
private:
  QList<QString> sources;
  QList<QJsonObject> timelineObjects;
  void loadSource(const QString& source);
  QList<QJsonObject> readJson(const QString& source);
  QList<QString> readDir(const QString& source);
};

/* Read-only Google Timeline Location History gpsbabel Format */
class GoogleTimelineFormat : public Format
{
public:
  /* Member functions */
  QVector<arglist_t>* get_args() override
  {
    return &googletimeline_args;
  }

  ff_type get_type() const override
  {
   return ff_type_file; 
  }

  QVector<ff_cap> get_cap() const override
  {
    return { ff_cap_read, ff_cap_read, ff_cap_none };
  }

  void rd_init(const QString& fname) override;
  void read() override;
 
private:
  GoogleTimelineInputStream inputStream;
  QVector<arglist_t> googletimeline_args = {};

  void add_place_visit(const QJsonObject& placeVisit);
  int add_activity_segment(const QJsonObject& activitySegment);

  Waypoint* _waypoint(
    int lat_e7,
    int lon_e7,
    const QString* shortname,
    const QString* description,
    const QString* start_str
  );
};


#endif /* _GOOGLETIMELINE_H */
