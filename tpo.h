/*
    National Geographic Topo! TPO file support.
    2.x support contributed to gpsbabel by Steve Chamberlin.
    3.x support contributed to gpsbabel by Curt Mills.
    4.x files read properly when treated as 3.x (final release was 4.5)
    track parsing bugs fixed by Steve Eckert in 2012 and 2020

    Topo! version 2.x:      Tracks are implemented.
    Topo! version 3.x/4.x:  Reading of Tracks/Waypoints/Routes is
                            implemented.  Also extracts Map Notes/
                            Symbols/Text Labels as Waypoints.

    Copyright (C) 2005 Steve Chamberlin, slc at alum.mit.edu
    Portions Copyright (C) 2006 Curtis E. Mills, archer at eskimo dot com

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

/*
 TPO format notes:
 -----------------
 Most of the ASCII strings embedded in the text will have a
 byte-count prepended to the string.  Unknown yet whether other
 fields have this same byte-count, but so far it doesn't look like
 it.

 New format (3.x and later) files begin with a string byte-count
 byte and then a string starting with "TOPO! Ver. 3.", like "TOPO!
 Ver.  3.3.4".  Can contain routes/tracks/waypoints, embedded
 images, Map Notes, Symbols, Text Labels, Compass symbols, and
 several others.

 Older (pre-3.0) format does not have the above string.  Contains
 only tracks.  Waypoints are saved in a separate .TPG file.

 Track parsing has been problematic and may still not be right!
 See further notes and clues in tpo_process_tracks()
 (can #define Tracks2012 to revert to pre-2020 code in tpo_process_tracks)

 May contain these strings:
    Frmt:   String:
    -----   --------------------------------
    2.x     "CTopoAzimuth"
    2.x     "CTopoBookmark"
    2.x     "CTopoGpsRoute".  Saved in .tpg files (see tpg.c)
    2.x     "CTopoRoute".  The actual tracks we parse here.
    2.x     "CTopoSymbol"
    2.x/3.x "CTopoText"
    2.x     "CTopoWaypoint".  Saved in .tpg files (see tpg.c)
    3.x     "Notes"
    3.x     "PNG."  Embedded PNG image containing 2 rows of 40
              symbols each.  Starts with signature: 89 50 4e 47 0d
              0a 1a 0a, ends with signature 49 45 4e 44 ae 42 60 82.
    3.x     "shapes"
    3.x     "arrows"
    3.x     "recreation"
*/
#ifndef TPO_H_INCLUDED_
#define TPO_H_INCLUDED_

#include <QList>     // for QList
#include <QString>   // for QString
#include <QVector>   // for QVector
#include <cstdint>   // for uint8_t
#include "defs.h"    // for ff_cap, arglist_t, ff_cap_none, ff_cap_read, Waypoint, ff_type, ff_type_file
#include "format.h"  // for Format
#include "gbfile.h"  // for gbfile


class TpoFormatBase
{
protected:
  /* Constants */

  static constexpr int TRACKNAMELENGTH = 255;

  /* Types */

  class StyleInfo
  {
  public:
    QString name;
    uint8_t color[3] {0, 0, 0}; // keep R/G/B values separate because line_color needs BGR
    uint8_t wide{0};
    uint8_t dash{0};
  };

  /* Member Functions */

  void tpo_check_version_string();
  void tpo_dump_header_bytes(int header_size);
  void tpo_read_until_section(const char* section_name, int seek_bytes);
  void tpo_read_2_x();
  int tpo_read_int();
  int tpo_find_block(unsigned int block_desired);
  static Waypoint* tpo_convert_ll(int lat, int lon);
  void tpo_process_tracks();
  void tpo_process_waypoints(QList<Waypoint>& tpo_wp_index);
  void tpo_process_map_notes();
  void tpo_process_symbols();
  void tpo_process_text_labels();
  void tpo_process_routes(const QList<Waypoint>& tpo_wp_index);
  void tpo_read_3_x();
  void tpo_rd_init(const QString& fname);
  void tpo_rd_deinit();
  void tpo_read();

  /* Data Members */

  char* dumpheader = nullptr;
  gbfile* tpo_file_in{};

  // Define a global here that we can query from multiple places.
  float tpo_version = 0.0;
};

class Tpo2Format : public Format, private TpoFormatBase
{
public:
  QVector<arglist_t>* get_args() override
  {
    return nullptr;
  }

  ff_type get_type() const override
  {
    return ff_type_file;
  }

  QVector<ff_cap> get_cap() const override
  {
    /* TPO 2.x format can read tracks only */
    /*         waypoints,      tracks,      routes */
    return { ff_cap_none, ff_cap_read,  ff_cap_none };
  }

  void rd_init(const QString& fname) override
  {
    tpo_rd_init(fname);
  }
  void read() override
  {
    tpo_read();
  }
  void rd_deinit() override
  {
    tpo_rd_deinit();
  }
};

class Tpo3Format : public Format, private TpoFormatBase
{
public:
  QVector<arglist_t>* get_args() override
  {
    return nullptr;
  }

  ff_type get_type() const override
  {
    return ff_type_file;
  }

  QVector<ff_cap> get_cap() const override
  {
    /* TPO 3.x format can read waypoints/tracks/routes */
    /*         waypoints,      tracks,      routes */
    return { ff_cap_read, ff_cap_read, ff_cap_read };
  }

  void rd_init(const QString& fname) override
  {
    tpo_rd_init(fname);
  }
  void read() override
  {
    tpo_read();
  }
  void rd_deinit() override
  {
    tpo_rd_deinit();
  }
};
#endif // TPO_H_INCLUDED_
