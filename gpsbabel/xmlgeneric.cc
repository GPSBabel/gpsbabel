/*
    Common utilities for XML-based formats.

    Copyright (C) 2004, 2005, 2006, 2007 Robert Lipe, robertlipe@usa.net

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

#include <QtCore/QByteArray>
#include <QtCore/QDebug>
#include <QtCore/QTextCodec>
#include <QtCore/QXmlStreamAttributes>
#include <QtCore/QXmlStreamReader>

#include "defs.h"
#include "xmlgeneric.h"
#include "cet_util.h"
#include "src/core/file.h"

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

#define MY_CBUF 4096

#define MYNAME "XML Reader"

void
write_xml_header(gbfile* ofd)
{
  char buff[128];
  cet_cs_vec_t* cs = cet_find_cs_by_name(CET_CHARSET_ASCII);

  if ((global_opts.charset != NULL) && (global_opts.charset != cs)) {
    snprintf(buff, sizeof(buff), " encoding=\"%s\"", global_opts.charset_name);
    QTextCodec* tcodec = QTextCodec::codecForName(global_opts.charset_name);
    if (tcodec) {
      codec = tcodec;
    }
  } else {
    buff[0] = 0;
  }

  gbfprintf(ofd, "<?xml version=\"1.0\"%s?>\n", buff);
}

void
write_xml_entity(gbfile* ofd, const QString& indent,
                 const QString& tag, const QString& value)
{
  char* tmp_ent = xml_entitize(CSTRE(value));
  gbfprintf(ofd, "%s<%s>%s</%s>\n", CSTRE(indent), CSTRE(tag), tmp_ent,
            CSTRE(tag));
  xfree(tmp_ent);
}

void
write_optional_xml_entity(gbfile* ofd, const QString& indent,
                          const QString& tag, const QString& value)
{
  if (!value.isEmpty()) {
    write_xml_entity(ofd, indent, tag, value);
  }
}

void
write_xml_entity_begin0(gbfile* ofd, const QString& indent,
                        const QString& tag)
{
  gbfprintf(ofd, "%s<%s>\n", CSTRE(indent), CSTRE(tag));
}

void
write_xml_entity_begin1(gbfile* ofd, const QString& indent,
                        const QString& tag, const QString& attr,
                        const QString& attrval)
{
  gbfprintf(ofd, "%s<%s %s=\"%s\">\n", CSTRE(indent), CSTRE(tag), CSTRE(attr),
            CSTRE(attrval));
}

void
write_xml_entity_begin2(gbfile* ofd, const QString& indent,
                        const QString& tag, const QString& attr1,
                        const QString& attrval1, const QString& attr2,
                        const QString& attrval2)
{
  gbfprintf(ofd, "%s<%s %s=\"%s\" %s=\"%s\">\n", CSTRE(indent), CSTRE(tag),
            CSTRE(attr1), CSTRE(attrval1), CSTRE(attr2), CSTRE(attrval2));
}

void
write_xml_entity_end(gbfile* ofd, const QString& indent,
                     const QString& tag)
{
  gbfprintf(ofd, "%s</%s>\n", CSTRE(indent), CSTRE(tag));
}

void
xml_write_time(gbfile* ofd, gpsbabel::DateTime dt, const char* elname)
{
  gbfprintf(ofd, "<%s>%s</%s>\n",
            elname,
            CSTRE(dt.toPrettyString()),
            elname
           );
}

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
    if (str_match(tag.toUtf8().data(), tm->tag_name) && (cb_type == tm->cb_type)) {
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
  bool started = false;

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
      started = true;
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
        cb(CSTRE(c), NULL);
        current_tag.chop(reader.qualifiedName().length() + 1);
      }
      break;

    case QXmlStreamReader::EndElement:
      if (xml_consider_ignoring(reader.name())) {
        goto readnext;
      }

      cb = xml_tbl_lookup(current_tag, cb_end);
      if (cb) {
        cb(CSTRE(reader.name().toString()), NULL);
      }
      current_tag.chop(reader.qualifiedName().length() + 1);
      break;

    case QXmlStreamReader::Characters:
      break;

    default:
      break;
    };

readnext:
    if (started) {
      reader.readNextStartElement();
    } else {
      reader.readNext();
    }
  }
}

void xml_read(void)
{
  gpsbabel::File file(rd_fname);
  QString current_tag;

  file.open(QIODevice::ReadOnly);

  QXmlStreamReader reader(&file);

  xml_run_parser(reader, current_tag);
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
