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

#include "src/core/xmltag.h"


/*
 * xml_tag utilities
 */

XmlTag* XmlTag::xml_next(const XmlTag* root)
{
  XmlTag* cur = this;
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

XmlTag* XmlTag::xml_findnext(const XmlTag* root, QStringView name)
{
  XmlTag* result = this;
  do {
    result = result->xml_next(root);
  } while (result && result->tagname.compare(name, Qt::CaseInsensitive));
  return result;
}

XmlTag* XmlTag::xml_findfirst(QStringView name)
{
  return xml_findnext(this, name);
}

QString XmlTag::xml_attribute(QStringView attrname) const
{
  for (const auto& attribute : this->attributes) {
    if (attribute.qualifiedName().compare(attrname, Qt::CaseInsensitive) == 0) {
      return attribute.value().toString();
    }
  }
  return QString();
}

static void
free_xml_tag(XmlTag* tag)
{
  while (tag) {
    if (tag->child) {
      free_xml_tag(tag->child);
    }

    XmlTag* next = tag->sibling;
    delete tag;
    tag = next;
  }
}

// FIXME: at some point, this becomes a plain ole copy constructor.
static void
copy_xml_tag(XmlTag** copy, XmlTag* src, XmlTag* parent)
{
  if (!src) {
    *copy = nullptr;
    return;
  }

  auto* res = new XmlTag;
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
