/*
    Copyright (C) 2002 Robert Lipe, robertlipe+source@gpsbabel.org
    Copyright (C) 2012 Guilhem Bonnefille, guilhem.bonnefille@gmail.com

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
/*
 * Parse the output of the following command:
 * curl "http://maps.googleapis.com/maps/api/directions/xml?origin='Albi,%20france'&destination='toulouse,%20france'&sensor=false" > google-direction.xml
 *
 * For more information, check:
 * https://developers.google.com/maps/documentation/directions/
 */

#include <QtCore/QXmlStreamAttributes>

#include "defs.h"
#include "xmlgeneric.h"

static QString encoded_points;
static QString instructions;
static short_handle desc_handle;

#define MYNAME "googledir"

static xg_callback      goog_points, goog_poly_e;
static xg_callback      goog_instr;

static
xg_tag_mapping google_map[] = {
  { goog_points,  cb_cdata,       "/DirectionsResponse/route/overview_polyline/points" },
  { goog_poly_e,  cb_end,         "/DirectionsResponse/route/overview_polyline" },
  { goog_points,  cb_cdata,       "/DirectionsResponse/route/leg/step/polyline/points" },
  { goog_poly_e,  cb_end,         "/DirectionsResponse/route/leg/step" },
  { goog_instr,   cb_cdata,       "/DirectionsResponse/route/leg/step/html_instructions" },
  { NULL, (xg_cb_type)0,              NULL }
};

void
goog_points(xg_string args, const QXmlStreamAttributes*)
{
  encoded_points += args;
}

void
goog_instr(xg_string args, const QXmlStreamAttributes*)
{
  instructions += args;
}

static int goog_step = 0;

long
decode_goog64(const QByteArray& str, int& pos)
{
  long result = 0;
  unsigned char shift = 0;

  if (pos >= str.size()) {
    return 0;
  }

  unsigned char c;
  do {
    c = str.at(pos++) - '?';
    result |= (c & 31) << shift;
    shift += 5;
  } while (c & ~31);

  if (result & 1) {
    result = ~result;
  }
  return result/2;
}

static void
goog_poly_e(xg_string args, const QXmlStreamAttributes*)
{
  long lat = 0;
  long lon = 0;
  const QByteArray qbstr = encoded_points.toUtf8();

  route_head* routehead = route_head_alloc();
  if (args == "overview_polyline") {
    routehead->rte_name = "overview";
    routehead->rte_desc = "Overview";
  } else {
    goog_step++;
    routehead->rte_name = QString("step%1").arg(goog_step, 3, 10, QChar('0'));
    if (instructions == NULL) {
      routehead->rte_desc = QString("Step %1").arg(goog_step);
    } else {
      utf_string utf;
      utf.is_html = 1;
      utf.utfstring = instructions;
      char *s = strip_html(&utf);
      routehead->rte_desc = s;
      xfree(s);
      instructions = QString();
    }
  }
  route_add_head(routehead);

  for (int qbpos=0; qbpos < qbstr.size();) {
    lat += decode_goog64(qbstr, qbpos);
    lon += decode_goog64(qbstr, qbpos);

    {
      Waypoint* wpt_tmp = new Waypoint;
      wpt_tmp->latitude = lat / 100000.0;
      wpt_tmp->longitude = lon / 100000.0;
      route_add_wpt(routehead, wpt_tmp);
    }
  }
  encoded_points = QString();
  instructions = QString();
}

static void
google_rd_init(const char* fname)
{
  desc_handle = mkshort_new_handle();
  setshort_length(desc_handle, 12);

  // leave default of UTF-8 unless xml file overrides with encoding=
  xml_init(fname, google_map, NULL);
}

static void
google_read(void)
{
  xml_read();

  encoded_points = QString();
  instructions = QString();
}

static void
google_rd_deinit(void)
{
  xml_deinit();
  mkshort_del_handle(&desc_handle);
}

ff_vecs_t google_dir_vecs = {
  ff_type_file,
  { ff_cap_none, ff_cap_read, ff_cap_none},
  google_rd_init,
  NULL,
  google_rd_deinit,
  NULL,
  google_read,
  NULL,
  NULL,
  NULL,
  CET_CHARSET_UTF8, 1	/* CET-REVIEW */
};
