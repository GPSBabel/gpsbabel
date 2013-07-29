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

#include "defs.h"
#include "xmlgeneric.h"
#include "cet_util.h"
#include <QtCore/QDebug>
#include <QtCore/QXmlStreamReader>
#include <QtCore/QFile>

#define DEBUG_TAG 0
#if DEBUG_TAG
#include <QtCore/QDebug>
#endif

#if HAVE_LIBEXPAT
#include <expat.h>
static XML_Parser psr;
#endif

static QString current_tag;
static vmem_t cdatastr;
static gbfile *ifd;
static xg_tag_mapping *xg_tag_tbl;
static const char **xg_ignore_taglist;

#define MY_CBUF 4096

#define MYNAME "XML Reader"

void
write_xml_header(gbfile *ofd)
{
  char buff[128];
  cet_cs_vec_t *cs = cet_find_cs_by_name(CET_CHARSET_ASCII);

  if ((global_opts.charset != NULL) && (global_opts.charset != cs)) {
    snprintf(buff, sizeof(buff), " encoding=\"%s\"", global_opts.charset_name);
  } else {
    buff[0] = 0;
  }
  gbfprintf(ofd, "<?xml version=\"1.0\"%s?>\n", buff);
}

void
write_xml_entity(gbfile *ofd, const QString& indent,
                 const QString& tag, const QString& value)
{
  char *tmp_ent = xml_entitize(value.toLatin1().data());
  gbfprintf(ofd, "%s<%s>%s</%s>\n", qPrintable(indent), qPrintable(tag), tmp_ent, qPrintable(tag));
  xfree(tmp_ent);
}

void
write_optional_xml_entity(gbfile *ofd, const QString& indent,
                          const QString& tag, const QString& value)
{
  if (!value.isEmpty()) {
    write_xml_entity(ofd, indent, tag, value);
  }
}

void
write_xml_entity_begin0(gbfile *ofd, const QString& indent,
                        const QString& tag)
{
  gbfprintf(ofd, "%s<%s>\n", indent.toLatin1().data(), tag.toLatin1().data());
}

void
write_xml_entity_begin1(gbfile *ofd, const QString& indent,
                        const QString& tag, const QString& attr,
                        const QString& attrval)
{
  gbfprintf(ofd, "%s<%s %s=\"%s\">\n", indent.toLatin1().data(), tag.toLatin1().data(), attr.toLatin1().data(), attrval.toLatin1().data());
}

void
write_xml_entity_begin2(gbfile *ofd, const QString& indent,
                        const QString& tag, const QString& attr1,
                        const QString& attrval1, const QString& attr2,
                        const QString& attrval2)
{
  gbfprintf(ofd, "%s<%s %s=\"%s\" %s=\"%s\">\n", indent.toLatin1().data(), tag.toLatin1().data(), attr1.toLatin1().data(), attrval1.toLatin1().data(), attr2.toLatin1().data(), attrval2.toLatin1().data());
}

void
write_xml_entity_end(gbfile *ofd, const QString& indent,
                     const QString& tag)
{
  gbfprintf(ofd, "%s</%s>\n", indent.toLatin1().data(), tag.toLatin1().data());
}

void
xml_write_time(gbfile *ofd, gpsbabel::DateTime dt, const char *elname)
{
  gbfprintf(ofd, "<%s>%s</%s>\n",
            elname,
            dt.toPrettyString().toUtf8().data(),
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

xg_callback *
xml_tbl_lookup(const QString& tag, xg_cb_type cb_type)
{
  xg_tag_mapping *tm;
  for (tm = xg_tag_tbl; tm->tag_cb != NULL; tm++) {
    if (str_match(tag.toUtf8().data(), tm->tag_name) && (cb_type == tm->cb_type)) {
      return tm->tag_cb;
    }
  }
  return NULL;
}

/*
 * See if tag element 't' is in our list of things to ignore.
 * Returns 0 if it is not on the list.
 */
static int
xml_consider_ignoring(const char *t)
{
  const char **il;

  if (!xg_ignore_taglist) {
    return 0;
  }

  for (il = xg_ignore_taglist; *il; il++) {
    if (0 == strcmp(*il, t)) {
      return 1;
    }
  }
  return 0;
}


static void
xml_start(void *data, const XML_Char *xml_el, const XML_Char **xml_attr)
{
  xg_callback *cb;
  const char *el;
  const char **attrs;

  el = xml_convert_to_char_string(xml_el);
  attrs = xml_convert_attrs_to_char_string(xml_attr);

  if (xml_consider_ignoring(el)) {
    return;
  }

  current_tag.append("/");
  current_tag.append(el);

  memset(cdatastr.mem, 0, cdatastr.size);

  cb = xml_tbl_lookup(current_tag, cb_start);
  if (cb) {
#if DEBUG_TAG
    fprintf(stderr, "Start: %s\n", xml_el);
#endif
    (*cb)(NULL, attrs);
  }
  xml_free_converted_string(el);
  xml_free_converted_attrs(attrs);
}

#if 1
static void
xml_cdata(void *dta, const XML_Char *xml_s, int len)
{
  char *estr;
  const char *s = xml_convert_to_char_string_n(xml_s, &len);

  vmem_realloc(&cdatastr,  1 + len + strlen(cdatastr.mem));
  estr = (char *) cdatastr.mem + strlen(cdatastr.mem);
  memcpy(estr, s, len);
  estr[len]  = 0;
  xml_free_converted_string(s);
}

static void
xml_end(void *data, const XML_Char *xml_el)
{
  int pos = current_tag.lastIndexOf('/');
  QString s = current_tag.mid(pos + 1);
  const char *el = xml_convert_to_char_string(xml_el);
  xg_callback *cb;

  if (xml_consider_ignoring(el)) {
    return;
  }

  if (s.compare(el)) {
    fprintf(stderr, "Mismatched tag %s\n", el);
  }
#if DEBUG_TAG
  qDebug() << current_tag;
#endif
  cb = xml_tbl_lookup(current_tag, cb_cdata);
  if (cb) {
    (*cb)((char *) cdatastr.mem, NULL);
  }

  cb = xml_tbl_lookup(current_tag, cb_end);
  if (cb) {
    (*cb)(el, NULL);
  }
  current_tag.truncate(pos);
  xml_free_converted_string(el);
}

void xml_read(void)
{
  int len;
  char buf[MY_CBUF];

  while ((len = gbfread(buf, 1, sizeof(buf), ifd))) {
    char *str = buf;
    if (ifd->unicode) {
      str = cet_str_uni_to_utf8((short *)&buf, len >> 1);
      len = strlen(str);
    }
    if (!XML_Parse(psr, str, len, gbfeof(ifd))) {
      fatal(MYNAME ":Parse error at %d: %s\n",
            (int) XML_GetCurrentLineNumber(psr),
            XML_ErrorString(XML_GetErrorCode(psr)));
    }
    if (str != buf) {
      xfree(str);
    }
  }
  XML_ParserFree(psr);

}

void xml_readstring(char *str)
{
  int len = strlen(str);
  if (!XML_Parse(psr, str, len, 1)) {
    fatal(MYNAME ":Parse error at %d: %s\n",
          (int) XML_GetCurrentLineNumber(psr),
          XML_ErrorString(XML_GetErrorCode(psr)));
  }
  XML_ParserFree(psr);
}

void xml_readprefixstring(const char *str)
{
  int len = strlen(str);
  if (!XML_Parse(psr, str, len, 0)) {
    fatal(MYNAME ":Parse error at %d: %s\n",
          (int) XML_GetCurrentLineNumber(psr),
          XML_ErrorString(XML_GetErrorCode(psr)));
  }
}

void xml_ignore_tags(const char **taglist)
{
  xg_ignore_taglist = taglist;
}

void
xml_init0(const char *fname, xg_tag_mapping *tbl, const char *encoding,
          gbsize_t offset)
{
  if (fname) {
    ifd = gbfopen(fname, "r", MYNAME);
    if (offset) {
      gbfseek(ifd, offset, SEEK_SET);
    }
  } else {
    ifd = NULL;
  }

  current_tag.clear();

  psr = XML_ParserCreate((const XML_Char *)encoding);
  if (!psr) {
    fatal(MYNAME ": Cannot create XML Parser\n");
  }

  cdatastr = vmem_alloc(1, 0);
  *((char *)cdatastr.mem) = '\0';

  xg_tag_tbl = tbl;

  cet_convert_init(CET_CHARSET_UTF8, 1);

  XML_SetUnknownEncodingHandler(psr, cet_lib_expat_UnknownEncodingHandler, NULL);
  XML_SetElementHandler(psr, xml_start, xml_end);
  XML_SetCharacterDataHandler(psr, xml_cdata);
}

/* xml_init0 iwth a default seek argument of zero */
void
xml_init(const char *fname, xg_tag_mapping *tbl, const char *encoding)
{
  xml_init0(fname, tbl, encoding, 0);
}

void
xml_init_offset(const char *fname, xg_tag_mapping *tbl, const char *encoding,
                gbsize_t offset)
{
  xml_init0(fname, tbl, encoding, offset);
}

void
xml_deinit(void)
{
  vmem_free(&cdatastr);
  if (ifd) {
    gbfclose(ifd);
    ifd = NULL;
  }
  xg_ignore_taglist = NULL;
}

#else

static const char *rd_fname;
static QXmlStreamReader reader;

void
xml_init(const char *fname, xg_tag_mapping *tbl, const char *encoding)
{
  rd_fname = fname;
  xg_tag_tbl = tbl;
}

void xml_read(void)
{
  QFile file(rd_fname);
  file.open(QIODevice::ReadOnly);
  reader.setDevice(&file);

  QString current_tag;
  while (!reader.atEnd()) {
    xg_callback *cb = xml_tbl_lookup(current_tag, cb_start);
    switch (reader.tokenType()) {
      case QXmlStreamReader::StartElement: {
        if (cb) {
          const char **attrs;
          (*cb)(NULL, attrs);
        }
        current_tag.append("/");
        current_tag.append(reader.name());
        cb = xml_tbl_lookup(current_tag, cb_cdata);
        if (cb) {
          QString c = reader.readElementText();
          (*cb)(c.toUtf8().data(), NULL);
          current_tag.chop(reader.name().length() + 1);
        }
        }
        break;
      case QXmlStreamReader::Characters:
        break;
      case QXmlStreamReader::EndElement:
        cb = xml_tbl_lookup(current_tag, cb_end);
        if (cb) {
          (*cb)(NULL, NULL);
        }
        current_tag.chop(reader.name().length() + 1);
        break;
      default:
        break;
    };
    reader.readNextStartElement();
  }

}

void xml_ignore_tags(const char **taglist)
{
  xg_ignore_taglist = taglist;
}

void xml_readprefixstring(const char *str)
{
}

void xml_readstring(char *str)
{
}

void
xml_deinit(void)
{
}
#endif

/******************************************/
