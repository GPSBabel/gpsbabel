/*
    Header for our common utilities for XML-based formats.

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



typedef enum {
  cb_start = 1,
  cb_cdata,
  cb_end,
} xg_cb_type;

typedef void (xg_callback)(const char*, const char**);

typedef struct xg_tag_mapping {
  xg_callback* tag_cb;
  xg_cb_type cb_type;
  const char* tag_name;
} xg_tag_mapping;

extern char* xhtml_entities;

void write_xml_entity(gbfile* ofd, const QString& indent,
                      const QString& tag, const QString& value);
void write_xml_entity_begin0(gbfile* ofd, const QString& indent,
                             const QString& tag);
void write_xml_entity_begin1(gbfile* ofd, const QString& indent, 
                             const QString& tag, const QString& attr1, 
                             const QString& attrval1);
void write_xml_entity_begin2(gbfile* ofd, const QString& indent, const QString& tag,
                             const QString& attr1, const QString& attrval1,
                             const QString& attr2, const QString& attrval2);
void write_xml_entity_end(gbfile* ofd, const QString& indent, const QString& tag);

void write_optional_xml_entity(gbfile* ofd, const QString& indent,
                               const QString& tag, const QString& value);
void xml_write_time(gbfile* ofd, gpsbabel::DateTime time, const char* elname);
void xml_fill_in_time(char* time_string, const gpsbabel::DateTime timep, int long_or_short);
void write_xml_header(gbfile* ofd);
void xml_ignore_tags(const char** taglist);

void xml_init(const char* fname, xg_tag_mapping* tbl,const char* encoding);
void xml_init_offset(const char* fname, xg_tag_mapping* tbl,
                     const char* encoding, gbsize_t offset);
void xml_read(void);
void xml_readstring(char* str);
void xml_readprefixstring(const char* str);
void xml_deinit(void);
