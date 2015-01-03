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
    Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111 USA

 */

#include "defs.h"
#include "xmlgeneric.h"
#include "cet_util.h"
#include "src/core/file.h"

#include <QtCore/QByteArray>
#include <QtCore/QDebug>
#include <QtCore/QTextCodec>
#include <QtCore/QXmlStreamAttributes>
#include <QtCore/QXmlStreamReader>

#define DEBUG_TAG 0
#if DEBUG_TAG
#include <QtCore/QDebug>
#endif

static QString current_tag;
static xg_tag_mapping* xg_tag_tbl;
static QSet<QString> xg_ignore_taglist;

static const char* rd_fname;
static QByteArray reader_data;
static const char* xg_encoding;
static QTextCodec* utf8_codec = QTextCodec::codecForName("UTF-8");
static QTextCodec* codec = utf8_codec;  // Qt has no vanilla ASCII encoding =(

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

xg_callback*
xml_tbl_lookup(const QString& tag, xg_cb_type cb_type)
{
  xg_tag_mapping* tm;
  for (tm = xg_tag_tbl; tm->tag_cb != NULL; tm++) {
    if (str_match(CSTR(tag), tm->tag_name) && (cb_type == tm->cb_type)) {
      return tm->tag_cb;
    }
  }
  return NULL;
}

void
xml_init(const char* fname, xg_tag_mapping* tbl, const char* encoding)
{
  rd_fname = fname;
  xg_tag_tbl = tbl;
  xg_encoding = encoding;
  if (encoding) {
    QTextCodec* tcodec = QTextCodec::codecForName(encoding);
    if (tcodec) {
      codec = tcodec;
    }
  }
}

void
xml_deinit(void)
{
  reader_data.clear();
  rd_fname = NULL;
  xg_tag_tbl = NULL;
  xg_encoding = NULL;
  codec = utf8_codec;
}

static bool
xml_consider_ignoring(const QStringRef& name)
{
  return xg_ignore_taglist.contains(name.toString());
}

static void
xml_run_parser(QXmlStreamReader& reader, QString& current_tag)
{
  xg_callback* cb;

  while (!reader.atEnd()) {
    switch (reader.tokenType()) {
    case QXmlStreamReader::StartDocument:
      if (!reader.documentEncoding().isEmpty()) {
        codec = QTextCodec::codecForName(CSTR(reader.documentEncoding().toString()));
      }
      if (codec == NULL) {
        // According to http://www.opentag.com/xfaq_enc.htm#enc_default , we
        // should assume UTF-8 in absense of other informations. Users can
        // EASILY override this with xml_init().
        codec = QTextCodec::codecForName("UTF-8");
      }
      break;

    case QXmlStreamReader::StartElement:
      if (xml_consider_ignoring(reader.name())) {
        goto readnext;
      }

      current_tag.append("/");
      current_tag.append(reader.qualifiedName());

      cb = xml_tbl_lookup(current_tag, cb_start);
      if (cb) {
        const QXmlStreamAttributes attrs = reader.attributes();
        cb(NULL, &attrs);
      }

      cb = xml_tbl_lookup(current_tag, cb_cdata);
      if (cb) {
        QString c = reader.readElementText(QXmlStreamReader::IncludeChildElements);
        // readElementText advances the tokenType to QXmlStreamReader::EndElement,
        // thus we will not process the EndElement case as we will issue a readNext first.
        // does a caller ever expect to be able to use both a cb_cdata and a
        // cb_end callback?
        cb(c, NULL);
        current_tag.chop(reader.qualifiedName().length() + 1);
      }
      break;

    case QXmlStreamReader::EndElement:
      if (xml_consider_ignoring(reader.name())) {
        goto readnext;
      }

      cb = xml_tbl_lookup(current_tag, cb_end);
      if (cb) {
        cb(reader.name().toString(), NULL);
      }
      current_tag.chop(reader.qualifiedName().length() + 1);
      break;

    case QXmlStreamReader::Characters:
      break;

    default:
      break;
    };

readnext:
    // readNextStartElement will cause a "Premature end of document." error
    // if you use it to try to read past the end of the document.
    // See https://bugreports.qt-project.org/browse/QTBUG-25944,
    // which is a bug report from 2012 that doesn't seem to be going anywhere.
    reader.readNext();
  }
}

void xml_read(void)
{
  gpsbabel::File file(rd_fname);
  QString current_tag;

  file.open(QIODevice::ReadOnly);

  QXmlStreamReader reader(&file);

  xml_run_parser(reader, current_tag);
  if (reader.hasError())  {
    fatal(MYNAME ":Read error: %s (%s, line %ld, col %ld)\n",
          qPrintable(reader.errorString()),
          qPrintable(file.fileName()),
          (long) reader.lineNumber(),
          (long) reader.columnNumber());
  }
}

void xml_ignore_tags(const char** taglist)
{
  for (; taglist && *taglist; taglist++) {
    xg_ignore_taglist.insert(QString::fromUtf8(*taglist));
  }
}

// Chucks some bytes into the global QByteArray buffer and waits for
// xml_readstring() to parse.
void xml_readprefixstring(const char* str)
{
  reader_data.append(str);
}

// Parses a bytestream as if it were a file. Looks for an <?xml encoding= to
// determine file encoding, falls back to UTF-8 if unspecified.
void xml_readstring(const char* str)
{
  QString current_tag;

  reader_data.append(str);

  QXmlStreamReader reader(reader_data);

  xml_run_parser(reader, current_tag);
  if (reader.hasError())  {
    fatal(MYNAME ":Read error: %s (%s, line %ld, col %ld)\n",
          qPrintable(reader.errorString()),
          "unknown",
          (long) reader.lineNumber(),
          (long) reader.columnNumber());
  }
}

// This is quite different from xml_readstring(). It doesn't have to interpret
// encoding because the source is already Qt's internal UTF-16.
void xml_readunicode(const QString& str)
{
  QString current_tag;
  QXmlStreamReader reader(str);

  xml_run_parser(reader, current_tag);
}

/******************************************/
