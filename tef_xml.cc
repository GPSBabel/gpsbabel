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
	Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111 USA

*/

#include <QtCore/QXmlStreamAttributes>

#include "defs.h"
#include "xmlgeneric.h"

static Waypoint* wpt_tmp;
static int item_count;
static int waypoints;
static double version;
static route_head* route = NULL;

static char* routevia = NULL;

static arglist_t tef_xml_args[] = {
  {
    "routevia", &routevia, "Include only via stations in route",
    NULL, ARGTYPE_BOOL, ARG_NOMINMAX
  },
  ARG_TERMINATOR
};

#define MYNAME "TourExchangeFormat"

static xg_callback	tef_start, tef_header, tef_list_start, tef_list_end;
static xg_callback	tef_item_start, tef_point, tef_item_end;

static
xg_tag_mapping tef_xml_map[] = {
  { tef_start,		cb_start,	"/TEF" },
  { tef_header,		cb_start,	"/TEF/Header" },
  { tef_list_start,	cb_start,	"/TEF/WaypointList" },
  { tef_item_start,	cb_start,	"/TEF/WaypointList/Item" },
  { tef_point,		cb_start,	"/TEF/WaypointList/Item/Point" },
  { tef_item_end,		cb_end,		"/TEF/WaypointList/Item" },
  { tef_list_end,		cb_end,		"/TEF/WaypointList" },
  { NULL,	(xg_cb_type)0,		NULL }
};


/*
 * tef_start: check for comment "TourExchangeFormat"
 */

void
tef_start(xg_string args, const QXmlStreamAttributes* attrv)
{
  bool valid = false;

  foreach(QXmlStreamAttribute attr, *attrv) {
    if (attr.name().compare("Comment", Qt::CaseInsensitive) == 0) {
      if (attr.value().compare("TourExchangeFormat", Qt::CaseInsensitive) == 0) {
        valid = true;
      }
    } else if (attr.name().compare("Version", Qt::CaseInsensitive) == 0) {
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

static void
tef_header(xg_string args, const QXmlStreamAttributes* attrv)
{
  route = route_head_alloc();
  foreach(QXmlStreamAttribute attr, *attrv) {
    if (attr.name().compare("Name", Qt::CaseInsensitive) == 0) {
      route->rte_name = attr.value().toString().trimmed();
    } else if (attr.name().compare("Software", Qt::CaseInsensitive) == 0) {
      route->rte_desc = attr.value().toString().trimmed();
    }
  }
  route_add_head(route);
}

static void
tef_list_start(xg_string args, const QXmlStreamAttributes* attrv)
{
  if (attrv->hasAttribute("ItemCount")) {
    item_count = attrv->value("ItemCount").toString().toUInt();
  }
}

#if OMG

TODO: this whole horrible mess is not covered at all in the test suite,
so just stub it all out until someone cares. (TEF is rarely used from 
what we can tell.)


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
static char*
fix_notes(const char* name, char* notes)
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

static char*
Xfix_notes(const QString& name, const QString& notes)
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
static QString 
fix_notes(const QString& name, const QString& notes){
    return notes;
}
#endif

static void
waypoint_final()
{
  int via;
  if (wpt_tmp == NULL) {
    return;
  }

  via = wpt_tmp->wpt_flags.fmt_use ;
  wpt_tmp->wpt_flags.fmt_use  = 0;

  if (version < 2) {	/* keep the old behaviour */
    wpt_tmp->notes = wpt_tmp->description;
    wpt_tmp->description = QString();
  }

  wpt_tmp->notes = fix_notes(wpt_tmp->shortname, wpt_tmp->notes);

  if (via != 0) {
    waypt_add(wpt_tmp);
  }

  if (route != NULL) {
    if ((via != 0) || (routevia == NULL)) {
      Waypoint* wpt = new Waypoint(*wpt_tmp);
      route_add_wpt(route, wpt);
    }
  }

  if (via == 0) {
    delete wpt_tmp;
  }

  wpt_tmp = NULL;
}

static void
tef_item_end(xg_string args, const QXmlStreamAttributes*)
{
  waypoint_final();
}

static void
tef_list_end(xg_string args, const QXmlStreamAttributes*)
{
  waypoint_final();
  if (waypoints != item_count)
    fatal(MYNAME ": waypoint count differs to internal \"ItemCount\"! (%d to %d)\n",
          waypoints, item_count);
}

static void
tef_item_start(xg_string args, const QXmlStreamAttributes* attrv)
{
  waypoints++;

  wpt_tmp = new Waypoint;
  if ((waypoints == 1) || (waypoints == item_count)) {
    wpt_tmp->wpt_flags.fmt_use ++;
  }

  foreach(QXmlStreamAttribute attr, *attrv) {
    QString attrstr = attr.value().toString();
    QByteArray attrtext = attrstr.toUtf8();

    if (attr.name().compare("SegDescription", Qt::CaseInsensitive) == 0) {
      wpt_tmp->shortname = attrstr.trimmed();
    } else if (attr.name().compare("PointDescription", Qt::CaseInsensitive) == 0) {
      wpt_tmp->description = attrstr.trimmed();
    } else if (attr.name().compare("ViaStation", Qt::CaseInsensitive) == 0 &&
               attr.value().compare("true", Qt::CaseInsensitive) == 0) {
      wpt_tmp->wpt_flags.fmt_use = 1;  /* only a flag */

      /* new in TEF V2 */
    } else if (attr.name().compare("Instruction", Qt::CaseInsensitive) == 0) {
      wpt_tmp->description = attrstr.trimmed();
    } else if (attr.name().compare("Altitude", Qt::CaseInsensitive) == 0) {
      wpt_tmp->altitude = attrstr.toDouble();
    } else if (attr.name().compare("TimeStamp", Qt::CaseInsensitive) == 0) {
      /* nothing for the moment */
    }
  }
}

static double
tef_read_comma_float(const QStringRef& value)
{
  QString svalue = value.toString();
  int cidx;

  cidx = svalue.indexOf(',');
  if (cidx == -1) {
    return svalue.toDouble();
  }

  QString fixed = svalue.replace(cidx, 1, '.');
  return fixed.toDouble();
}

static void
tef_point(xg_string args, const QXmlStreamAttributes* attrv)
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

static void
tef_xml_rd_init(const char* fname)
{
  wpt_tmp = NULL;
  waypoints = 0;
  item_count = -1;
  version = 1.5;

  xml_init(fname, tef_xml_map, NULL);
}

static void
tef_xml_read(void)
{
  xml_read();
}

static void
tef_xml_rd_deinit(void)
{
  xml_deinit();
}

ff_vecs_t tef_xml_vecs = {
  ff_type_file,
  { ff_cap_none, ff_cap_none, ff_cap_read },
  tef_xml_rd_init,
  NULL,
  tef_xml_rd_deinit,
  NULL,
  tef_xml_read,
  NULL,
  NULL,
  tef_xml_args,
  CET_CHARSET_UTF8, 1
};
