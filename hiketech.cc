/*
    Access Hiketech XML data files.

    Copyright (C) 2004,2005 Robert Lipe, robertlipe+source@gpsbabel.org

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

#include "defs.h"
#include "src/core/xmlstreamwriter.h"
#include "xmlgeneric.h"
#include <QtCore/QXmlStreamAttributes>
#include <cstdio>

static gbfile* ofd;
static QString ostring;
static gpsbabel::XmlStreamWriter writer(&ostring);

static Waypoint* wpt_tmp;
static route_head* trk_head;

#define MYNAME "hiketech"

static
arglist_t hiketech_args[] = {
  ARG_TERMINATOR
};

/* Waypoints */
static xg_callback 	ht_wpt_s;
static xg_callback 	ht_wpt_e;
static xg_callback 	ht_ident;
static xg_callback 	ht_sym;
static xg_callback 	ht_lat;
static xg_callback 	ht_long;
static xg_callback 	ht_alt;

/* Tracks */
static xg_callback	ht_trk_s, ht_trk_e;
static xg_callback	ht_trk_ident;
static xg_callback	ht_trk_pnt_s, ht_trk_pnt_e;
static xg_callback	ht_trk_utc;
static xg_callback	ht_trk_lat;
static xg_callback	ht_trk_long;
static xg_callback	ht_trk_alt;

static xg_tag_mapping ht_map[] = {
  { ht_wpt_s, 	cb_start, "/hiketech/gpsdata/wpt" },
  { ht_wpt_e, 	cb_end,   "/hiketech/gpsdata/wpt" },
  { ht_ident, 	cb_cdata, "/hiketech/gpsdata/wpt/ident" },
  { ht_sym, 	cb_cdata, "/hiketech/gpsdata/wpt/sym" },
  { ht_lat, 	cb_cdata, "/hiketech/gpsdata/wpt/lat" },
  { ht_long, 	cb_cdata, "/hiketech/gpsdata/wpt/long" },
  { ht_alt, 	cb_cdata, "/hiketech/gpsdata/wpt/alt" },

  { ht_trk_s, 	cb_start, "/hiketech/gpsdata/trk" },
  { ht_trk_e, 	cb_end,   "/hiketech/gpsdata/trk" },
  { ht_trk_ident,	cb_cdata, "/hiketech/gpsdata/trk/ident" },
  { ht_trk_pnt_s,	cb_start, "/hiketech/gpsdata/trk/pnt" },
  { ht_trk_pnt_e,	cb_end,   "/hiketech/gpsdata/trk/pnt" },
  { ht_trk_utc, 	cb_cdata, "/hiketech/gpsdata/trk/pnt/utc" },
  { ht_trk_lat, 	cb_cdata, "/hiketech/gpsdata/trk/pnt/lat" },
  { ht_trk_long, 	cb_cdata, "/hiketech/gpsdata/trk/pnt/long" },
  { ht_trk_alt, 	cb_cdata, "/hiketech/gpsdata/trk/pnt/alt" },
  { nullptr,	(xg_cb_type)0,         nullptr}
};

static void
hiketech_rd_init(const QString& fname)
{
  xml_init(fname, ht_map, nullptr);
}

static void
hiketech_read()
{
  xml_read();
}

static void
hiketech_rd_deinit()
{
  xml_deinit();
}

static void
hiketech_wr_init(const QString& fname)
{
  ofd = gbfopen(fname, "w", MYNAME);

}

static void
hiketech_wr_deinit()
{
  writer.writeEndDocument();
  gbfputs(ostring, ofd);
  gbfclose(ofd);
  ofd = nullptr;
}

static void
hiketech_trk_hdr(const route_head* rte)
{
  writer.writeStartElement(QStringLiteral("trk"));
  writer.setAutoFormattingIndent(1);
  writer.writeOptionalTextElement(QStringLiteral("ident"), rte->rte_name);
}

static void
hiketech_trk_tlr(const route_head*)
{
  writer.writeEndElement(); // trk
}

static QString
hiketech_format_time(const QDateTime& t)
{
  // FIXME: Find out why these two blocks of code aren't equivalent.
  // it produces times that are 12 hours too late.  Double TZ bump?
  // for now, just go back to the way we've done it for a decade.  -- robert
#if 0
  return t.toString("yyyy-MM-dd hh:mm:ss");
#else
  char tbuf[80];
  time_t tm = t.toTime_t();
  strftime(tbuf, sizeof(tbuf), "%Y-%m-%d %I:%M:%S", gmtime(&tm));
  return QString(tbuf);
#endif
}

static void
hiketech_trkpt_pr(const Waypoint* waypointp)
{
  writer.writeStartElement(QStringLiteral("pnt")); 
  if (waypointp->creation_time.isValid()) {
    writer.writeTextElement(QStringLiteral("utc"), 
                            hiketech_format_time(waypointp->GetCreationTime()));
  }
  writer.writeTextElement(QStringLiteral("lat"), QString::number(waypointp->latitude,'f', 6));
  writer.writeTextElement(QStringLiteral("long"), QString::number(waypointp->longitude,'f', 6));
  if (waypointp->altitude != unknown_alt) {
    writer.writeTextElement(QStringLiteral("alt"), QString::number(waypointp->altitude,'f', 6));
  }
  writer.writeEndElement(); // png
}

static void
hiketech_waypt_pr(const Waypoint* wpt)
{
  writer.writeStartElement(QStringLiteral("wpt")); 
  writer.setAutoFormattingIndent(-1);
  writer.writeTextElement(QStringLiteral("ident"), wpt->shortname);
  writer.writeTextElement(QStringLiteral("sym"), wpt->icon_descr);
  writer.writeTextElement(QStringLiteral("lat"), QString::number(wpt->latitude, 'f', 6));
  writer.writeTextElement(QStringLiteral("long"), QString::number(wpt->longitude, 'f', 6));
  writer.writeStartElement(QStringLiteral("color")); 
  writer.writeTextElement(QStringLiteral("lbl"), QStringLiteral("FAFFB4"));
  writer.writeTextElement(QStringLiteral("obj"), QStringLiteral("FF8000"));
  writer.writeEndElement(); // color

  writer.writeEndElement(); // wpt
}

static void
hiketech_write()
{
  writer.writeStartElement(QStringLiteral("hiketech"));
  writer.writeAttribute(QStringLiteral("version"), QStringLiteral("1.2"));
  writer.writeAttribute(QStringLiteral("url"), QStringLiteral("http://www.hiketech.com"));
  writer.setAutoFormatting(true);
  writer.writeStartElement(QStringLiteral("gpsdata"));
  track_disp_all(hiketech_trk_hdr, hiketech_trk_tlr, hiketech_trkpt_pr);
  track_disp_all(nullptr, nullptr, hiketech_trkpt_pr);
  waypt_disp_all(hiketech_waypt_pr);
  writer.writeEndElement(); // gpsdata
  writer.writeEndElement(); // hiketech
}

static
void	 ht_wpt_s(xg_string, const QXmlStreamAttributes*)
{
  wpt_tmp = new Waypoint;
}

static
void  	ht_ident(xg_string args, const QXmlStreamAttributes*)
{
  wpt_tmp->shortname = args;
}

static
void 	ht_sym(xg_string args, const QXmlStreamAttributes*)
{
  wpt_tmp->icon_descr = args;
}

static
void  	ht_lat(xg_string args, const QXmlStreamAttributes*)
{
  wpt_tmp->latitude = args.toDouble();
}

static
void  	ht_long(xg_string args, const QXmlStreamAttributes*)
{
  wpt_tmp->longitude = args.toDouble();
}

static
void  	ht_alt(xg_string args, const QXmlStreamAttributes*)
{
  wpt_tmp->altitude = args.toDouble();
}

static
void  	ht_wpt_e(xg_string, const QXmlStreamAttributes*)
{
  waypt_add(wpt_tmp);
  wpt_tmp = nullptr;
}

static
void	ht_trk_s(xg_string, const QXmlStreamAttributes*)
{
  trk_head = route_head_alloc();
  track_add_head(trk_head);
}

static
void	ht_trk_e(xg_string, const QXmlStreamAttributes*)
{

}

static
void	ht_trk_ident(xg_string args, const QXmlStreamAttributes*)
{
  trk_head->rte_name = args;
}

static
void	ht_trk_pnt_s(xg_string, const QXmlStreamAttributes*)
{
  wpt_tmp = new Waypoint;
}

static
void	ht_trk_pnt_e(xg_string, const QXmlStreamAttributes*)
{
  track_add_wpt(trk_head, wpt_tmp);
}

static
void	ht_trk_utc(xg_string args, const QXmlStreamAttributes*)
{
  struct tm tm;

  sscanf(CSTRc(args), "%d-%d-%d %d:%d:%d",
         &tm.tm_year, &tm.tm_mon,
         &tm.tm_mday, &tm.tm_hour,
         &tm.tm_min, &tm.tm_sec);
  tm.tm_mon -= 1;
  tm.tm_year -= 1900;
  tm.tm_isdst = 0;

  time_t utc = mkgmtime(&tm);

  wpt_tmp->SetCreationTime(utc);
}

static
void	ht_trk_lat(xg_string args, const QXmlStreamAttributes*)
{
  wpt_tmp->latitude = args.toDouble();
}

static
void	ht_trk_long(xg_string args, const QXmlStreamAttributes*)
{
  wpt_tmp->longitude = args.toDouble();
}

static
void	ht_trk_alt(xg_string args, const QXmlStreamAttributes*)
{
  wpt_tmp->altitude = args.toDouble();
}



ff_vecs_t hiketech_vecs = {
  ff_type_file,
  { (ff_cap)(ff_cap_read | ff_cap_write), (ff_cap)(ff_cap_read | ff_cap_write) },
  hiketech_rd_init,
  hiketech_wr_init,
  hiketech_rd_deinit,
  hiketech_wr_deinit,
  hiketech_read,
  hiketech_write,
  nullptr,
  hiketech_args,
  CET_CHARSET_ASCII, 0	/* CET-REVIEW */
  , NULL_POS_OPS,
  nullptr
};

