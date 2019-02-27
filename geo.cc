/*
    Copyright (C) 2002 Robert Lipe, robertlipe+source@gpsbabel.org

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
#include <QtCore/QDebug>
#include <QtCore/QXmlStreamReader>
#include <QtCore/QXmlStreamWriter>

static char* deficon = nullptr;
static char* nuke_placer;
static gbfile* ofd;
static QString ostring;
static QXmlStreamWriter writer(&ostring);

static
arglist_t geo_args[] = {
  {"deficon", &deficon, "Default icon name", nullptr, ARGTYPE_STRING, ARG_NOMINMAX, nullptr },
  {"nuke_placer", &nuke_placer, "Omit Placer name", nullptr, ARGTYPE_BOOL, ARG_NOMINMAX, nullptr },
  ARG_TERMINATOR
};

#define MYNAME "geo"

// This really should be class-local...
static QXmlStreamReader reader;
static QString geo_read_fname;

static geocache_container wpt_container(const QString&);

static void GeoReadLoc()
{
  Waypoint* wpt = nullptr;
  QString current_tag;

  while (!reader.atEnd()) {
    QStringRef tag_name = reader.name();
    if (reader.tokenType()==QXmlStreamReader::StartElement) {
      current_tag.append("/");
      current_tag.append(tag_name);
      if (current_tag == "/loc/waypoint") {
        wpt = new Waypoint;
        wpt->AllocGCData();
        // There is no 'unknown' alt value and so many reference files have
        // leaked it that we just paper over that here.
        wpt->altitude = 0;
      } else if (current_tag == "/loc/waypoint/name") {
        QXmlStreamAttributes a = reader.attributes();
        wpt->shortname = a.value("id").toString();
        wpt->description = reader.readElementText();
      } else if (current_tag == "/loc/waypoint/coord") {
        QXmlStreamAttributes a = reader.attributes();
        wpt->latitude = a.value("lat").toString().toDouble();
        wpt->longitude = a.value("lon").toString().toDouble();
      } else if (current_tag == "/loc/waypoint/type") {
        wpt->icon_descr = reader.readElementText();
      } else if (current_tag == "/loc/waypoint/link") {
        QXmlStreamAttributes a = reader.attributes();
        waypt_add_url(wpt,
                      reader.readElementText(), a.value("text").toString());
      } else if (current_tag == "/loc/waypoint/difficulty") {
        wpt->gc_data->diff = reader.readElementText().toDouble() * 10;
      } else if (current_tag == "/loc/waypoint/terrain") {
        wpt->gc_data->terr = reader.readElementText().toDouble() * 10;
      } else if (current_tag == "/loc/waypoint/container") {
        wpt->gc_data->container = wpt_container(reader.readElementText());
      }
    }

    // The tokenType may have changed to EndElement as a result of readElementText.
    if (reader.tokenType() == QXmlStreamReader::EndElement) {
      if (current_tag == "/loc/waypoint") {
        waypt_add(wpt);
      }
      current_tag.chop(tag_name.length() + 1);
    }

    reader.readNext();
  }
}

static void
geo_rd_init(const QString& fname)
{
  geo_read_fname = fname;
}

static void
geo_read()
{
  gpsbabel::File file(geo_read_fname);
  file.open(QIODevice::ReadOnly);
  reader.setDevice(&file);

  GeoReadLoc();
  if (reader.hasError())  {
    fatal(MYNAME ":Read error: %s (%s, line %ld, col %ld)\n",
          qPrintable(reader.errorString()),
          qPrintable(file.fileName()),
          (long) reader.lineNumber(),
          (long) reader.columnNumber());
  }
}

geocache_container wpt_container(const QString& args)
{
  geocache_container v;

  switch (args.toInt()) {
  case 1:
    v = gc_unknown;
    break;
  case 2:
    v = gc_micro;
    break;
  case 3:
    v = gc_regular;
    break;
  case 4:
    v = gc_large;
    break;
  case 5:
    v = gc_virtual;
    break;
  case 6:
    v = gc_other;
    break;
  case 8:
    v = gc_small;
    break;
  default:
    v = gc_unknown;
    break;
  }
  return v;
}

static void
geo_rd_deinit()
{

}

static void
geo_wr_init(const QString& fname)
{
  ofd = gbfopen(fname, "w", MYNAME);

  //writer.setAutoFormatting(true);
  writer.setAutoFormattingIndent(0);
  writer.writeStartDocument();

}

static void
geo_wr_deinit()
{
  writer.writeEndDocument();
  gbfputs(ostring,ofd);
  gbfclose(ofd);
  ofd = nullptr;
}

static void
geo_waypt_pr(const Waypoint* waypointp)
{
  writer.writeStartElement(QStringLiteral("waypoint"));

  writer.writeStartElement(QStringLiteral("name"));
  writer.writeAttribute(QStringLiteral("id"), waypointp->shortname);
  // TODO: this could be writeCharacters, but it's here for compat with pre
  // Qt writer.
  writer.writeCDATA(waypointp->description);
  writer.writeEndElement();

  writer.writeStartElement(QStringLiteral("coord"));
  writer.writeAttribute(QStringLiteral("lat"), QString::number(waypointp->latitude, 'f'));
  writer.writeAttribute(QStringLiteral("lon"), QString::number(waypointp->longitude, 'f'));
  writer.writeEndElement();

  writer.writeTextElement(QStringLiteral("type"), deficon ? deficon : waypointp->icon_descr);

  if (waypointp->HasUrlLink()) {
    writer.writeStartElement(QStringLiteral("link"));
    writer.writeAttribute(QStringLiteral("text "), QStringLiteral("Cache Details"));
    UrlLink link = waypointp->GetUrlLink();
    writer.writeCharacters(link.url_);
    writer.writeEndElement();
  }

  if (waypointp->gc_data && waypointp->gc_data->diff) {
    writer.writeTextElement(QStringLiteral("difficulty"),
                            QString::number(waypointp->gc_data->diff/10));
    writer.writeTextElement(QStringLiteral("terrain"),
                            QString::number(waypointp->gc_data->terr/10));

    int v = 1;
    switch (waypointp->gc_data->container) {
    case gc_unknown:
      v = 1;
      break;
    case gc_micro:
      v = 2;
      break;
    case gc_regular:
      v = 3;
      break;
    case gc_large:
      v = 4;
      break;
    case gc_virtual:
      v = 5;
      break;
    case gc_other:
      v = 6;
      break;
    case gc_small:
      v = 8;
      break;
    default:
      v = 1;
      break;
    }
    writer.writeTextElement(QStringLiteral("container"),
                            QString::number(v));
  }

  writer.writeEndElement();
}

static void
geo_write()
{
  writer.writeStartElement(QStringLiteral("loc"));
  writer.writeAttribute(QStringLiteral("version"), QStringLiteral("1.0"));
  // TODO: This could be moved to wr_init, but the pre GPX version put the two
  // lines above this, so mimic that behaviour exactly.
  writer.setAutoFormatting(true);
  writer.writeAttribute(QStringLiteral("src"), QStringLiteral("EasyGPS"));
  waypt_disp_all(geo_waypt_pr);
  writer.writeEndElement();
}

ff_vecs_t geo_vecs = {
  ff_type_file,
  { (ff_cap)(ff_cap_read | ff_cap_write), ff_cap_none, ff_cap_none },
  geo_rd_init,
  geo_wr_init,
  geo_rd_deinit,
  geo_wr_deinit,
  geo_read,
  geo_write,
  nullptr,
  geo_args,
  CET_CHARSET_UTF8, 0,	/* CET-REVIEW */
  NULL_POS_OPS,
  nullptr
};
