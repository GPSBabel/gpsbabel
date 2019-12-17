/*
    Functions to deal with xml_tags

    Copyright (C) 2005 Ron Parker and Robert Lipe.

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

#include <cstring>                      // for memcpy

#include <QtCore/QString>               // for QString
#include <QtCore/QXmlStreamAttributes>  // for QXmlStreamAttributes

#include "defs.h"
#include "src/core/xmltag.h"

static void
free_xml_tag(xml_tag* tag)
{
  while (tag) {
    if (tag->child) {
      free_xml_tag(tag->child);
    }

    xml_tag* next = tag->sibling;
    delete tag;
    tag = next;
  }
}

// FIXME: at some point, this becomes a plain ole copy constructor.
static void
copy_xml_tag(xml_tag** copy, xml_tag* src, xml_tag* parent)
{
  if (!src) {
    *copy = nullptr;
    return;
  }

  xml_tag* res = new xml_tag;
  *copy = res;

//  memcpy(res, src, sizeof(xml_tag));
  res->tagname = (src->tagname);
  res->cdata = (src->cdata);
  res->parentcdata = (src->parentcdata);
  res->attributes = src->attributes;
  res->parent = parent;
  copy_xml_tag(&(res->sibling), src->sibling, parent);
  copy_xml_tag(&(res->child), src->child, res);
}

static void
fs_xml_destroy(void* fs)
{
  fs_xml* xml = (fs_xml*)fs;
  if (xml) {
    free_xml_tag(xml->tag);
  }
  delete xml;
}

static void
fs_xml_copy(void** copy, void* source)
{
  fs_xml* src = (fs_xml*)source;
  if (!source) {
    *copy = nullptr;
    return;
  }
  *copy = (void*)fs_xml_alloc(src->fs.type);
  memcpy(*copy, source, sizeof(fs_xml));
  copy_xml_tag(&(((fs_xml*)(*copy))->tag), src->tag, nullptr);
}

fs_xml* fs_xml_alloc(long type)
{
  fs_xml* result = new fs_xml;
  result->fs.type = type;
  result->fs.copy = fs_xml_copy;
  result->fs.destroy = fs_xml_destroy;
  result->fs.convert = nullptr;
  return result;
}
