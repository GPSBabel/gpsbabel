/*
	Support for XML based "TourExchangeFormat",
	found in Map & Guide Motorrad-Tourenplaner 2005/06

	Copyright (C) 2005 Olaf Klein, o.b.klein@gpsbabel.org

	Based on kml.c, Keyhole "kml" format.
	Copyright (C) 2002-2014 Robert Lipe, robertlipe+source@gpsbabel.org

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

#include "tef_xml.h"

#include <QLatin1String>         // for QLatin1String
#include <QString>               // for QString, QStringView::toString
#include <QXmlStreamAttribute>   // for QXmlStreamAttribute
#include <QXmlStreamAttributes>  // for QXmlStreamAttributes
#include <Qt>                    // for CaseInsensitive

#include <type_traits>           // for add_const<>::type

#include "defs.h"                // for Waypoint, fatal, wp_flags, route_add_head, route_add_wpt, route_head, waypt_add
#include "xmlgeneric.h"          // for xg_string, build_xg_tag_map, xml_deinit, xml_init, xml_read


#define MYNAME "TourExchangeFormat"

/*
 * tef_start: check for comment "TourExchangeFormat"
 */

void
TefXMLFormat::tef_start(xg_string /*unused*/, const QXmlStreamAttributes* attrv)
{
  bool valid = false;

  for (const auto& attr : *attrv) {
    if (attr.name().compare(QLatin1String("Comment"), Qt::CaseInsensitive) == 0) {
      if (attr.value().compare(QLatin1String("TourExchangeFormat"), Qt::CaseInsensitive) == 0) {
        valid = true;
      }
    } else if (attr.name().compare(QLatin1String("Version"), Qt::CaseInsensitive) == 0) {
      version = attr.value().toString().toDouble();
    }
  }

  if (!valid) {
    fatal(MYNAME ": Error in source file.\n");
  }
}

/*
 * tef_header: "Name" > Route name, "Software" > Route descr.
 */

void
TefXMLFormat::tef_header(xg_string /*unused*/, const QXmlStreamAttributes* attrv)
{
  route = new route_head;
  for (const auto& attr : *attrv) {
    if (attr.name().compare(QLatin1String("Name"), Qt::CaseInsensitive) == 0) {
      route->rte_name = attr.value().toString().trimmed();
    } else if (attr.name().compare(QLatin1String("Software"), Qt::CaseInsensitive) == 0) {
      route->rte_desc = attr.value().toString().trimmed();
    }
  }
  route_add_head(route);
}

void
TefXMLFormat::tef_list_start(xg_string /*unused*/, const QXmlStreamAttributes* attrv)
{
  if (attrv->hasAttribute("ItemCount")) {
    item_count = attrv->value("ItemCount").toString().toUInt();
  }
}

#if OMG

/*
 * TODO: this whole horrible mess is not covered at all in the test suite,
 * so just stub it all out until someone cares. (TEF is rarely used from
 * what we can tell.)
 */


/* in "TourExchangeFormat" the following can happen:
 *
 * SegDescription="L34\Wittlicher Strasse"
 * PointDescription="Wittlicher Strasse (  "
 *
 * fix_notes tries to create a new PointDescription, which
 * should be "Wittlicher Strasse (L34)" for the example above
 */
// FIXME: the calling convention here is screwy.  notes is an input AND
// output argument and may be modified.
char*
TefXMLFormat::fix_notes(const char* name, char* notes)
{
  const char* cleft, *cright, *cback;
  char* ctmp;

  if ((! name) || (! notes)) {
    return notes;
  }

  /* do we have a BACKSLASH in shortname ? */
  cback = strchr(name, '\\');
  if ((! cback) || (cback == name)) {
    return notes;
  }

  /* do we have left, but no right parenthesis in notes ? */
  if (!(cleft = strchr(notes, '('))) {
    return notes;
  }
  cright = strchr(notes, ')');
  if (cright && (cright > cleft)) {
    return notes;
  }

  /* now contruct the new name */
  ctmp = lrtrim(xstrndup(notes, cleft - notes));
  xfree(notes);
  xasprintf(&notes, "%s (%*.*s)", ctmp, (int)(cback - name), (int)(cback - name), name);
  xfree(ctmp);

  return notes;
}

char*
TefXMLFormat::Xfix_notes(const QString& name, const QString& notes)
{

  char* cname = xstrdup(name);
  char* cnotes = xstrdup(notes);
  char *r =  fix_notes(cname, cnotes);
  xfree(cname);
// WTH?  fix_notes() modifies the note string...and
// may reallocate it.
//  xfree(cnotes);
  return r;
}
#else
QString
TefXMLFormat::fix_notes(const QString& /*unused*/, const QString& notes){
    return notes;
}
#endif

void
TefXMLFormat::waypoint_final()
{
  if (wpt_tmp == nullptr) {
    return;
  }

  int via = wpt_tmp->wpt_flags.fmt_use;
  wpt_tmp->wpt_flags.fmt_use  = 0;

  if (version < 2) {	/* keep the old behaviour */
    wpt_tmp->notes = wpt_tmp->description;
    wpt_tmp->description = QString();
  }

  wpt_tmp->notes = fix_notes(wpt_tmp->shortname, wpt_tmp->notes);

  if (via != 0) {
    waypt_add(wpt_tmp);
  }

  if (route != nullptr) {
    if ((via != 0) || (routevia == nullptr)) {
      auto* wpt = new Waypoint(*wpt_tmp);
      route_add_wpt(route, wpt);
    }
  }

  if (via == 0) {
    delete wpt_tmp;
  }

  wpt_tmp = nullptr;
}

void
TefXMLFormat::tef_item_end(xg_string /*unused*/, const QXmlStreamAttributes* /*unused*/)
{
  waypoint_final();
}

void
TefXMLFormat::tef_list_end(xg_string /*unused*/, const QXmlStreamAttributes* /*unused*/)
{
  waypoint_final();
  if (waypoints != item_count)
    fatal(MYNAME ": waypoint count differs to internal \"ItemCount\"! (%d to %d)\n",
          waypoints, item_count);
}

void
TefXMLFormat::tef_item_start(xg_string /*unused*/, const QXmlStreamAttributes* attrv)
{
  waypoints++;

  wpt_tmp = new Waypoint;
  if ((waypoints == 1) || (waypoints == item_count)) {
    wpt_tmp->wpt_flags.fmt_use ++;
  }

  for (const auto& attr : *attrv) {
    QString attrstr = attr.value().toString();

    if (attr.name().compare(QLatin1String("SegDescription"), Qt::CaseInsensitive) == 0) {
      wpt_tmp->shortname = attrstr.trimmed();
    } else if (attr.name().compare(QLatin1String("PointDescription"), Qt::CaseInsensitive) == 0) {
      wpt_tmp->description = attrstr.trimmed();
    } else if (attr.name().compare(QLatin1String("ViaStation"), Qt::CaseInsensitive) == 0 &&
               attr.value().compare(QLatin1String("true"), Qt::CaseInsensitive) == 0) {
      wpt_tmp->wpt_flags.fmt_use = 1;  /* only a flag */

      /* new in TEF V2 */
    } else if (attr.name().compare(QLatin1String("Instruction"), Qt::CaseInsensitive) == 0) {
      wpt_tmp->description = attrstr.trimmed();
    } else if (attr.name().compare(QLatin1String("Altitude"), Qt::CaseInsensitive) == 0) {
      wpt_tmp->altitude = attrstr.toDouble();
    } else if (attr.name().compare(QLatin1String("TimeStamp"), Qt::CaseInsensitive) == 0) {
      /* nothing for the moment */
    }
  }
}

double
TefXMLFormat::tef_read_comma_float(QStringView value)
{
  QString svalue = value.toString();

  int cidx = svalue.indexOf(',');
  if (cidx == -1) {
    return svalue.toDouble();
  }

  QString fixed = svalue.replace(cidx, 1, '.');
  return fixed.toDouble();
}

void
TefXMLFormat::tef_point(xg_string /*unused*/, const QXmlStreamAttributes* attrv)
{
  if (!wpt_tmp) {
    return;
  }

  if (attrv->hasAttribute("y")) {
    wpt_tmp->latitude = tef_read_comma_float(attrv->value("y"));
  }

  if (attrv->hasAttribute("x")) {
    wpt_tmp->longitude = tef_read_comma_float(attrv->value("x"));
  }
}

void
TefXMLFormat::rd_init(const QString& fname)
{
  wpt_tmp = nullptr;
  waypoints = 0;
  item_count = -1;
  version = 1.5;

  xml_init(fname, build_xg_tag_map(this, tef_xml_map), nullptr, nullptr, nullptr, true);
}

void
TefXMLFormat::read()
{
  xml_read();
}

void
TefXMLFormat::rd_deinit()
{
  xml_deinit();
}
