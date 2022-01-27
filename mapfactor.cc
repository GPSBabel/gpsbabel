/*
    Copyright (C) 2014 Robert Lipe

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
#include "mapfactor.h"

#include <QByteArray>                  // for QByteArray
#include <QIODevice>                   // for QIODevice, operator|, QIODevice::ReadOnly, QIODevice::Text, QIODevice::WriteOnly
#include <QtGlobal>                    // for qPrintable
#include <QStringLiteral>              // for QStringLiteral
#include <QXmlStreamAttributes>        // for QXmlStreamAttributes
#include <QXmlStreamReader>            // for QXmlStreamReader, QXmlStreamReader::EndElement, QXmlStreamReader::StartElement
#include <QXmlStreamWriter>            // for QXmlStreamWriter

#include "defs.h"                      // for Waypoint, fatal, waypt_add, waypt_disp_all
#include "src/core/file.h"             // for File
#include "src/core/xmlstreamwriter.h"  // for XmlStreamWriter


#define MYNAME "mapfactor"

void MapfactorFormat::MapfactorRead()
{
  Waypoint* wpt = nullptr;

  while (!reader.atEnd()) {
    auto tag_name = reader.name();
    if (reader.tokenType()==QXmlStreamReader::StartElement) {
      if (tag_name == u"item") {
        wpt = new Waypoint;

        QXmlStreamAttributes a = reader.attributes();
        wpt->shortname = a.value("name").toString();
        wpt->latitude = a.value("lat").toString().toDouble() / milliarcseconds;
        wpt->longitude = a.value("lon").toString().toDouble() / milliarcseconds;
      }
    }

    if (reader.tokenType() == QXmlStreamReader::EndElement) {
      if (wpt && reader.name() == u"item") {
        waypt_add(wpt);
      }
    }

    reader.readNext();
  }
}

void
MapfactorFormat::rd_init(const QString& fname)
{
  mapfactor_read_fname = fname;
}

void
MapfactorFormat::read()
{
  gpsbabel::File file(mapfactor_read_fname);
  file.open(QIODevice::ReadOnly);
  reader.setDevice(&file);

  MapfactorRead();
  if (reader.hasError())  {
    fatal(MYNAME ":Read error: %s (%s, line %ld, col %ld)\n",
          qPrintable(reader.errorString()),
          qPrintable(file.fileName()),
          (long) reader.lineNumber(),
          (long) reader.columnNumber());
  }
}

void
MapfactorFormat::wr_init(const QString& fname)
{
  oqfile = new gpsbabel::File(fname);
  oqfile->open(QIODevice::WriteOnly | QIODevice::Text);
  writer = new gpsbabel::XmlStreamWriter(oqfile);

  writer->setAutoFormatting(true);
  writer->setAutoFormattingIndent(2);
  writer->writeStartDocument();
}

void
MapfactorFormat::wr_deinit()
{
  writer->writeEndDocument();
  delete writer;
  writer = nullptr;
  oqfile->close();
  delete oqfile;
  oqfile = nullptr;
}

void
MapfactorFormat::mapfactor_waypt_pr(const Waypoint* waypointp)
{
  writer->writeStartElement(QStringLiteral("item"));

  writer->writeAttribute(QStringLiteral("name"), waypointp->shortname);
  writer->writeAttribute(QStringLiteral("lat"), QString::number(waypointp->latitude * milliarcseconds, 'f', 0));
  writer->writeAttribute(QStringLiteral("lon"), QString::number(waypointp->longitude * milliarcseconds, 'f', 0));
  writer->writeEndElement();
}

void
MapfactorFormat::write()
{
  writer->writeStartElement(QStringLiteral("favourites"));
  writer->writeAttribute(QStringLiteral("version"), QStringLiteral("1"));
  // TODO: This could be moved to wr_init, but the pre GPX version put the two
  // lines above this, so mimic that behaviour exactly.
  writer->setAutoFormatting(true);
  auto mapfactor_waypt_pr_lambda = [this](const Waypoint* waypointp)->void {
    mapfactor_waypt_pr(waypointp);
  };
  waypt_disp_all(mapfactor_waypt_pr_lambda);
  writer->writeEndElement();
}
