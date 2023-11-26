/*
    Common utilities for XML-based formats.

    Copyright (C) 2004, 2005, 2006, 2007 Robert Lipe, robertlipe+source@gpsbabel.org

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

#include "xmlgeneric.h"

#include <QByteArray>            // for QByteArray
#include <QHash>                 // for QHash
#include <QIODevice>             // for QIODevice
#include <QLatin1Char>           // for QLatin1Char
#include <QStringView>           // for QStringView
#include <QTextCodec>            // for QTextCodec
#include <QXmlStreamAttributes>  // for QXmlStreamAttributes, QXmlStreamReader::Characters, QXmlStreamReader::EndElement, QXmlStreamReader::IncludeChildElements, QXmlStreamReader::StartDocument, QXmlStreamReader::StartElement
#include <QXmlStreamReader>      // for QXmlStreamReader
//#include <QtCore>                // for QHash, QIODeviceBase::ReadOnly
#include <QtGlobal>              // for qPrintable

#include "defs.h"                // for fatal
#include "src/core/file.h"       // for File


#define MYNAME "XML Reader"

/***********************************************************************
 * These implement a simple interface for "generic" XML that
 * maps reasonably close to  1:1 between XML tags and internal data
 * structures.
 *
 * It doesn't work well for formats (like GPX) that really are "real"
 * XML with extended namespaces and such, but it handles many simpler
 * xml strains and insulates us from a lot of the grubbiness of expat.
 */

XgCallbackBase*
XmlGenericReader::xml_tbl_lookup(const QString& tag, xg_cb_type cb_type)
{
  for (const auto& tm : *xg_tag_tbl) {
    if (cb_type == tm.cb_type) {
      QRegularExpressionMatch match = tm.tag_re.match(tag);
      if (match.hasMatch()) {
        return tm.tag_cb;
      }
    }
  }
  return nullptr;
}

void
XmlGenericReader::xml_init(const QString& fname, const QList<xg_tag_map_entry>* tbl, const char* encoding,
         const char* const* ignorelist, const char* const* skiplist)
{
  xg_tag_tbl = tbl;

  rd_fname = fname;

  if (encoding != nullptr) {
    codec = QTextCodec::codecForName(encoding);
    if (codec == nullptr) {
      fatal(MYNAME ": codec \"%s\" is not available.\n", encoding);
    }
  } else {
    codec = QTextCodec::codecForName("UTF-8");
  }

  xg_shortcut_taglist = new QHash<QString, xg_shortcut>;
  if (ignorelist != nullptr) {
    for (; ignorelist && *ignorelist; ++ignorelist) {
      xg_shortcut_taglist->insert(QString::fromUtf8(*ignorelist), xg_shortcut_ignore);
    }
  }
  if (skiplist != nullptr) {
    for (; skiplist && *skiplist; ++skiplist) {
      xg_shortcut_taglist->insert(QString::fromUtf8(*skiplist), xg_shortcut_skip);
    }
  }
}

void
XmlGenericReader::xml_deinit()
{
  for (const auto& tm : *xg_tag_tbl) {
    delete tm.tag_cb;
  }
  delete xg_tag_tbl;
  xg_tag_tbl = nullptr;

  reader_data.clear();
  rd_fname.clear();

  codec = nullptr;

  delete xg_shortcut_taglist;
  xg_shortcut_taglist = nullptr;
}

XmlGenericReader::~XmlGenericReader()
{
  for (const auto& tm : *xg_tag_tbl) {
    delete tm.tag_cb;
  }
  delete xg_tag_tbl;

  delete xg_shortcut_taglist;
}

XmlGenericReader::xg_shortcut
XmlGenericReader::xml_shortcut(QStringView name)
{
  QString key = name.toString();
  if (xg_shortcut_taglist->contains(key)) {
    return xg_shortcut_taglist->value(key);
  }
  return xg_shortcut_none;
}

void
XmlGenericReader::xml_run_parser(QXmlStreamReader& reader)
{
  XgCallbackBase* cb;
  QString current_tag;

  while (!reader.atEnd()) {
    switch (reader.tokenType()) {
    case QXmlStreamReader::StartDocument:
      if (!reader.documentEncoding().isEmpty()) {
        codec = QTextCodec::codecForName(reader.documentEncoding().toUtf8());
      }
      if (codec == nullptr) {
        // According to http://www.opentag.com/xfaq_enc.htm#enc_default , we
        // should assume UTF-8 in absence of other information. Users can
        // EASILY override this with xml_init().
        codec = QTextCodec::codecForName("UTF-8");
      }
      break;

    case QXmlStreamReader::StartElement:
      switch (xml_shortcut(reader.name())) {
      case xg_shortcut_skip:
        reader.skipCurrentElement();
        goto readnext;
      case xg_shortcut_ignore:
        goto readnext;
      default:
        break;
      }

      current_tag.append(QLatin1Char('/'));
      current_tag.append(reader.qualifiedName());

      cb = xml_tbl_lookup(current_tag, cb_start);
      if (cb) {
        const QXmlStreamAttributes attrs = reader.attributes();
        (*cb)(nullptr, &attrs);
      }

      cb = xml_tbl_lookup(current_tag, cb_cdata);
      if (cb) {
        QString c = reader.readElementText(QXmlStreamReader::IncludeChildElements);
        // readElementText advances the tokenType to QXmlStreamReader::EndElement,
        // thus we will not process the EndElement case as we will issue a readNext first.
        // does a caller ever expect to be able to use both a cb_cdata and a
        // cb_end callback?
        (*cb)(c, nullptr);
        current_tag.chop(reader.qualifiedName().length() + 1);
      }
      break;

    case QXmlStreamReader::EndElement:
      if (xml_shortcut(reader.name()) == xg_shortcut_skip) {
        goto readnext;
      }

      cb = xml_tbl_lookup(current_tag, cb_end);
      if (cb) {
        (*cb)(reader.name().toString(), nullptr);
      }
      current_tag.chop(reader.qualifiedName().length() + 1);
      break;

    case QXmlStreamReader::Characters:
      break;

    default:
      break;
    }

readnext:
    // readNextStartElement will cause a "Premature end of document." error
    // if you use it to try to read past the end of the document.
    // See https://bugreports.qt-project.org/browse/QTBUG-25944,
    // which is a bug report from 2012 that doesn't seem to be going anywhere.
    reader.readNext();
  }
}

void XmlGenericReader::xml_read()
{
  gpsbabel::File file(rd_fname);

  file.open(QIODevice::ReadOnly);

  QXmlStreamReader reader(&file);

  xml_run_parser(reader);
  if (reader.hasError())  {
    fatal(MYNAME ":Read error: %s (%s, line %lld, col %lld)\n",
          qPrintable(reader.errorString()),
          qPrintable(file.fileName()),
          reader.lineNumber(),
          reader.columnNumber());
  }
}

// Chucks some bytes into the global QByteArray buffer and waits for
// xml_readstring() to parse.
void XmlGenericReader::xml_readprefixstring(const char* str)
{
  reader_data.append(str);
}

// Parses a bytestream as if it were a file. Looks for an <?xml encoding= to
// determine file encoding, falls back to UTF-8 if unspecified.
void XmlGenericReader::xml_readstring(const char* str)
{
  reader_data.append(str);

  QXmlStreamReader reader(reader_data);

  xml_run_parser(reader);
  if (reader.hasError())  {
    fatal(MYNAME ":Read error: %s (%s, line %lld, col %lld)\n",
          qPrintable(reader.errorString()),
          "unknown",
          reader.lineNumber(),
          reader.columnNumber());
  }
}

// This is quite different from xml_readstring(). It doesn't have to interpret
// encoding because the source is already Qt's internal UTF-16.
void XmlGenericReader::xml_readunicode(const QString& str)
{
  QXmlStreamReader reader(str);

  xml_run_parser(reader);
}

/******************************************/
