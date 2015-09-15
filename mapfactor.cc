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
    Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111 USA

 */
#include "defs.h"
#include "src/core/file.h"
#include "src/core/xmlstreamwriter.h"
#include <QtCore/QXmlStreamReader>
#include <QtCore/QXmlStreamWriter>
#include <QtCore/QDebug>

static gpsbabel::File* oqfile;
static QXmlStreamWriter* writer;

static
arglist_t mapfactor_args[] = {
  ARG_TERMINATOR
};

#define MYNAME "mapfactor"

// This really should be class-local...
static QXmlStreamReader reader;
static QString mapfactor_read_fname;
static  const double milliarcseconds = 60.0 * 60.0 * 1000.0;

geocache_container wpt_container(const QString&);

void MapfactorRead()
{
  Waypoint* wpt = NULL;

  while (!reader.atEnd()) {
    QStringRef tag_name = reader.name();
    if (reader.tokenType()==QXmlStreamReader::StartElement) {
      if (tag_name == "item") {
        wpt = new Waypoint;

        QXmlStreamAttributes a = reader.attributes();
        wpt->shortname = a.value("name").toString();
        wpt->latitude = a.value("lat").toString().toDouble() / milliarcseconds;
        wpt->longitude = a.value("lon").toString().toDouble() / milliarcseconds;
      }
    }

    if (reader.tokenType() == QXmlStreamReader::EndElement) {
      if (wpt && reader.name() == "item") {
        waypt_add(wpt);
      }
    }

    reader.readNext();
  }
}

static void
mapfactor_rd_init(const char* fname)
{
  mapfactor_read_fname = fname;
}

static void
mapfactor_read(void)
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


static void
mapfactor_rd_deinit(void)
{

}

static void
mapfactor_wr_init(const char* fname)
{
  oqfile = new gpsbabel::File(fname);
  oqfile->open(QIODevice::WriteOnly | QIODevice::Text);
  writer = new gpsbabel::XmlStreamWriter(oqfile);

  // Override the "UTF-8-XML" with ... the default.
  writer->setCodec("utf-8");

  writer->setAutoFormatting(true);
  writer->setAutoFormattingIndent(2);
  writer->writeStartDocument();
}

static void
mapfactor_wr_deinit(void)
{
  writer->writeEndDocument();
  delete writer;
  writer = NULL;
  oqfile->close();
  delete oqfile;
  oqfile = NULL;
}

static void
mapfactor_waypt_pr(const Waypoint* waypointp)
{
  writer->writeStartElement("item");

  writer->writeAttribute("name", waypointp->shortname);
  writer->writeAttribute("lat", QString::number(waypointp->latitude * milliarcseconds, 'f', 0));
  writer->writeAttribute("lon", QString::number(waypointp->longitude * milliarcseconds, 'f', 0));
  writer->writeEndElement();
}

static void
mapfactor_write(void)
{
  writer->writeStartElement("favourites");
  writer->writeAttribute("version", "1");
  // TODO: This could be moved to wr_init, but the pre GPX version put the two
  // lines above this, so mimic that behaviour exactly.
  writer->setAutoFormatting(true);
  waypt_disp_all(mapfactor_waypt_pr);
  writer->writeEndElement();
}

ff_vecs_t mapfactor_vecs = {
  ff_type_file,
  { (ff_cap)(ff_cap_read | ff_cap_write), ff_cap_none, ff_cap_none },
  mapfactor_rd_init,
  mapfactor_wr_init,
  mapfactor_rd_deinit,
  mapfactor_wr_deinit,
  mapfactor_read,
  mapfactor_write,
  NULL,
  mapfactor_args,
  CET_CHARSET_UTF8, 0	/* CET-REVIEW */
};
