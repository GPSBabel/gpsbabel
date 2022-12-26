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

#include <QString>                      // for QString
#include <QStringView>                  // for QStringView
#include <Qt>                           // for CaseInsensitive
#include <QXmlStreamAttribute>          // for QXmlStreamAttribute
#include <QXmlStreamAttributes>         // for QXmlStreamAttributes

#include "defs.h"
#include "formspec.h"                   // for FsType
#include "src/core/xmltag.h"


/*
 * xml_tag utilities
 */

xml_tag* xml_tag::xml_next(const xml_tag* root)
{
  xml_tag* cur = this;
  if (cur->child) {
    cur = cur->child;
  } else if (cur->sibling) {
    cur = cur->sibling;
  } else {
    cur = cur->parent;
    if (cur == root) {
      cur = nullptr;
    }
    if (cur) {
      cur = cur->sibling;
    }
  }
  return cur;
}

xml_tag* xml_tag::xml_findnext(xml_tag* root, xml_tag* cur, const QString& tagname)
{
  xml_tag* result = cur;
  do {
    result = result->xml_next(root);
  } while (result && result->tagname.compare(tagname, Qt::CaseInsensitive));
  return result;
}

xml_tag* xml_tag::xml_findfirst(xml_tag* root, const QString& tagname)
{
  return xml_findnext(root, root, tagname);
}

QString xml_tag::xml_attribute(const QXmlStreamAttributes& attributes, const QString& attrname)
{
  for (const auto& attribute : attributes) {
    if (attribute.qualifiedName().compare(attrname, Qt::CaseInsensitive) == 0) {
      return attribute.value().toString();
    }
  }
  return QString();
}

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

  auto* res = new xml_tag;
  *copy = res;

  res->tagname = (src->tagname);
  res->cdata = (src->cdata);
  res->parentcdata = (src->parentcdata);
  res->attributes = src->attributes;
  res->parent = parent;
  copy_xml_tag(&(res->sibling), src->sibling, parent);
  copy_xml_tag(&(res->child), src->child, res);
}

fs_xml::~fs_xml()
{
  free_xml_tag(tag);
}

fs_xml* fs_xml::clone() const
{
  auto* copy = new fs_xml(*this);
  copy_xml_tag(&(copy->tag), tag, nullptr);
  return copy;
}

fs_xml* fs_xml_alloc(FsType type)
{
  auto* result = new fs_xml(type);
  return result;
}
