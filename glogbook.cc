/*
    Access Garmin Logbook (Forerunner/Foretracker) data files.

    Copyright (C) 2004, 2005, 2006, 2007  Robert Lipe, robertlipe+source@gpsbabel.org

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
#include "src/core/file.h"
#include "xmlgeneric.h"

#include <QtCore/QXmlStreamAttributes>
#include <QtCore/QXmlStreamWriter>

static gbfile* ofd;
static QString ostring;
static QXmlStreamWriter writer(&ostring);

static Waypoint* wpt_tmp;
static route_head* trk_head;

#define MYNAME "glogbook"

static
arglist_t glogbook_args[] = {
  ARG_TERMINATOR
};

/* Tracks */
static xg_callback	gl_trk_s;
// static xg_callback	gl_trk_ident;
static xg_callback	gl_trk_pnt_s, gl_trk_pnt_e;
static xg_callback	gl_trk_utc;
static xg_callback	gl_trk_lat;
static xg_callback	gl_trk_long;
static xg_callback	gl_trk_alt;

static xg_tag_mapping gl_map[] = {
  { gl_trk_s,    cb_start, "/History/Run/Track" },
  { gl_trk_pnt_s,cb_start, "/History/Run/Track/Trackpoint/Position" },
  { gl_trk_pnt_e,cb_end,   "/History/Run/Track/Trackpoint/Position" },
  { gl_trk_lat,  cb_cdata, "/History/Run/Track/Trackpoint/Position/Latitude" },
  { gl_trk_long, cb_cdata, "/History/Run/Track/Trackpoint/Position/Longitude" },
  { gl_trk_alt,  cb_cdata, "/History/Run/Track/Trackpoint/Position/Altitude" },
  { gl_trk_utc,  cb_cdata, "/History/Run/Track/Trackpoint/Time" },
  { nullptr,	(xg_cb_type)0,         nullptr}
};

static void
glogbook_rd_init(const QString& fname)
{
  xml_init(fname, gl_map, nullptr);
}

static void
glogbook_read()
{
  xml_read();
}

static void
glogbook_rd_deinit()
{
  xml_deinit();
}

static void
glogbook_wr_init(const QString& fname)
{
  ofd = gbfopen(fname, "w", MYNAME);
  writer.setAutoFormatting(true);
  writer.setAutoFormattingIndent(4);
  writer.writeStartDocument();
}

static void
glogbook_wr_deinit()
{
  writer.writeEndDocument();
  gbfputs(ostring,ofd);
  gbfclose(ofd);
  ofd = nullptr;
}

static void
glogbook_waypt_pr(const Waypoint* wpt)
{
  writer.writeStartElement(QStringLiteral("Trackpoint"));

  writer.writeStartElement(QStringLiteral("Position"));
  writer.writeTextElement(QStringLiteral("Latitude"), QString::number(wpt->latitude,'f', 5));
  writer.writeTextElement(QStringLiteral("Longitude"), QString::number(wpt->longitude,'f', 5));
  writer.writeTextElement(QStringLiteral("Altitude"), QString::number(wpt->altitude,'f', 3));
  writer.writeEndElement(); // Position

  writer.writeTextElement(QStringLiteral("Time"), wpt->GetCreationTime().toPrettyString());
  writer.writeEndElement(); // Trackpoint
}

static void
glogbook_hdr(const route_head*)
{
  writer.writeStartElement(QStringLiteral("Track"));
}

static void
glogbook_ftr(const route_head*)
{
  writer.writeEndElement();
}

static void
glogbook_write()
{
#if 0
  gbfprintf(ofd, "<?xml version=\"1.0\" ?>\n");
  gbfprintf(ofd, "<History xmlns=\"http://www.garmin.com/xmlschemas/ForerunnerLogbook\" xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\" xsi:schemaLocation=\"http://www.garmin.com/xmlschemas/ForerunnerLogbook http://www.garmin.com/xmlschemas/ForerunnerLogbookv1.xsd\" version=\"1\">\n");
  gbfprintf(ofd, "    <Run>\n");
#else
  writer.writeStartElement(QStringLiteral("History"));
  writer.writeStartElement(QStringLiteral("Run"));
#endif
  track_disp_all(glogbook_hdr, glogbook_ftr, glogbook_waypt_pr);
  writer.writeEndElement(); // Run
  writer.writeEndElement(); // History
}

void	gl_trk_s(xg_string, const QXmlStreamAttributes*)
{
  trk_head = route_head_alloc();
  track_add_head(trk_head);
}

void	gl_trk_pnt_s(xg_string, const QXmlStreamAttributes*)
{
  wpt_tmp = new Waypoint;
}

void	gl_trk_pnt_e(xg_string, const QXmlStreamAttributes*)
{
  track_add_wpt(trk_head, wpt_tmp);
}

void	gl_trk_utc(xg_string args, const QXmlStreamAttributes*)
{
  wpt_tmp->SetCreationTime(xml_parse_time(args));
}

void	gl_trk_lat(xg_string args, const QXmlStreamAttributes*)
{
  wpt_tmp->latitude = args.toDouble();
}

void	gl_trk_long(xg_string args, const QXmlStreamAttributes*)
{
  wpt_tmp->longitude = args.toDouble();
}

void	gl_trk_alt(xg_string args, const QXmlStreamAttributes*)
{
  wpt_tmp->altitude = args.toDouble();
}



ff_vecs_t glogbook_vecs = {
  ff_type_file,
  { ff_cap_none, (ff_cap)(ff_cap_read | ff_cap_write), ff_cap_none},
  glogbook_rd_init,
  glogbook_wr_init,
  glogbook_rd_deinit,
  glogbook_wr_deinit,
  glogbook_read,
  glogbook_write,
  nullptr,
  glogbook_args,
  CET_CHARSET_ASCII, 0	/* CET-REVIEW */
  , NULL_POS_OPS,
  nullptr
};
