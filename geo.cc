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
    Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.

 */

#include "geo.h"

#include <QByteArray>            // for QByteArray
#include <QIODevice>             // for QIODevice
#include <QString>               // for QString, operator==, QStringView::to...
#include <QStringView>           // for QStringView
#include <QXmlStreamAttributes>  // for QXmlStreamAttributes
#include <QtCore>                // for qPrintable, QIODeviceBase::ReadOnly

#include "defs.h"
#include "geocache.h"            // for Geocache, Geocache::container_t, Geo...
#include "src/core/file.h"       // for File


void GeoFormat::GeoReadLoc(QXmlStreamReader& reader)
{
  Waypoint* wpt = nullptr;
  QString current_tag;

  while (!reader.atEnd()) {
    auto tag_name = reader.name();
    if (reader.tokenType()==QXmlStreamReader::StartElement) {
      current_tag.append("/");
      current_tag.append(tag_name);
      if (current_tag == u"/loc/waypoint") {
        wpt = new Waypoint;
        wpt->AllocGCData();
        // There is no 'unknown' alt value and so many reference files have
        // leaked it that we just paper over that here.
        wpt->altitude = 0;
      } else if (current_tag == u"/loc/waypoint/name") {
        QXmlStreamAttributes a = reader.attributes();
        wpt->shortname = a.value("id").toString();
        wpt->description = reader.readElementText();
      } else if (current_tag == u"/loc/waypoint/coord") {
        QXmlStreamAttributes a = reader.attributes();
        wpt->latitude = a.value("lat").toDouble();
        wpt->longitude = a.value("lon").toDouble();
      } else if (current_tag == u"/loc/waypoint/type") {
        wpt->icon_descr = reader.readElementText();
      } else if (current_tag == u"/loc/waypoint/link") {
        QXmlStreamAttributes a = reader.attributes();
        waypt_add_url(wpt,
                      reader.readElementText(), a.value("text").toString());
      } else if (current_tag == u"/loc/waypoint/difficulty") {
        wpt->gc_data->diff = reader.readElementText().toDouble() * 10;
      } else if (current_tag == u"/loc/waypoint/terrain") {
        wpt->gc_data->terr = reader.readElementText().toDouble() * 10;
      } else if (current_tag == u"/loc/waypoint/container") {
        wpt->gc_data->container = wpt_container(reader.readElementText());
      }
    }

    // The tokenType may have changed to EndElement as a result of readElementText.
    if (reader.tokenType() == QXmlStreamReader::EndElement) {
      if (current_tag == u"/loc/waypoint") {
        waypt_add(wpt);
      }
      current_tag.chop(tag_name.length() + 1);
    }

    reader.readNext();
  }
}

void GeoFormat::read()
{
  gpsbabel::File ifile = gpsbabel::File(fname);
  ifile.open(QIODevice::ReadOnly);
  QXmlStreamReader reader = QXmlStreamReader(&ifile);

  GeoReadLoc(reader);
  if (reader.hasError())  {
    fatal("Read error: %s (%s, line %ld, col %ld)\n",
          qPrintable(reader.errorString()),
          qPrintable(ifile.fileName()),
          (long) reader.lineNumber(),
          (long) reader.columnNumber());
  }
}

Geocache::container_t GeoFormat::wpt_container(const QString& args)
{
  Geocache::container_t v;

  switch (args.toInt()) {
  case 1:
    v = Geocache::container_t::gc_unknown;
    break;
  case 2:
    v = Geocache::container_t::gc_micro;
    break;
  case 3:
    v = Geocache::container_t::gc_regular;
    break;
  case 4:
    v = Geocache::container_t::gc_large;
    break;
  case 5:
    v = Geocache::container_t::gc_virtual;
    break;
  case 6:
    v = Geocache::container_t::gc_other;
    break;
  case 8:
    v = Geocache::container_t::gc_small;
    break;
  default:
    v = Geocache::container_t::gc_unknown;
    break;
  }
  return v;
}

void GeoFormat::geo_waypt_pr(const Waypoint* waypointp, QXmlStreamWriter& writer)
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
    const UrlLink& link = waypointp->GetUrlLink();
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
    case Geocache::container_t::gc_unknown:
      v = 1;
      break;
    case Geocache::container_t::gc_micro:
      v = 2;
      break;
    case Geocache::container_t::gc_regular:
      v = 3;
      break;
    case Geocache::container_t::gc_large:
      v = 4;
      break;
    case Geocache::container_t::gc_virtual:
      v = 5;
      break;
    case Geocache::container_t::gc_other:
      v = 6;
      break;
    case Geocache::container_t::gc_small:
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

void GeoFormat::write()
{
  gpsbabel::File ofile = gpsbabel::File(fname);
  ofile.open(QIODevice::WriteOnly | QIODevice::Text);
  QXmlStreamWriter writer = QXmlStreamWriter(&ofile);

  writer.setAutoFormatting(true);
  writer.setAutoFormattingIndent(0);
  writer.writeStartDocument();
  writer.writeStartElement(QStringLiteral("loc"));
  writer.writeAttribute(QStringLiteral("version"), QStringLiteral("1.0"));
  writer.writeAttribute(QStringLiteral("src"), QStringLiteral("EasyGPS"));
  auto geo_waypt_pr_lambda = [this, &writer](const Waypoint* waypointp)->void {
    geo_waypt_pr(waypointp, writer);
  };
  waypt_disp_all(geo_waypt_pr_lambda);
  writer.writeEndElement(); // loc
  writer.writeEndDocument();
}
