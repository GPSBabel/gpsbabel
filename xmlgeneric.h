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
    Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.

 */

#ifndef XMLGENERIC_H_INCLUDED_
#define XMLGENERIC_H_INCLUDED_

#include <QtCore/QString>               // for QString
#include <QtCore/QXmlStreamAttributes>  // for QXmlStreamAttributes

// Maybe the XmlGeneric string callback really shouldn't have a type
// of its own; this was a crutch during the move from char* to QString.
// It's "just" a search and replace to make it go away, but it might
// be convenient to overload some day.
using xg_string = const QString&;


enum xg_cb_type {
  cb_start = 1,
  cb_cdata,
  cb_end,
};

using xg_callback = void (xg_string, const QXmlStreamAttributes*);

struct xg_tag_mapping {
  xg_callback* tag_cb;
  xg_cb_type cb_type;
  const char* tag_name;
};

extern const char* xhtml_entities;

void xml_init(const QString& fname, xg_tag_mapping* tbl,const char* encoding,
              const char** ignorelist = nullptr,
              const char** skiplist = nullptr);
void xml_read();
void xml_readstring(const char* str);
void xml_readprefixstring(const char* str);
void xml_readunicode(const QString& str);
void xml_deinit();

#endif  // XMLGENERIC_H_INCLUDED_
