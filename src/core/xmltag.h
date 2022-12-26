/*
    Copyright (C) 2002-2013 Robert Lipe, gpsbabel.org

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
#ifndef SRC_CORE_XMLTAG_H
#define SRC_CORE_XMLTAG_H

#include <QString>                                 // for QString
#include <QXmlStreamAttributes>                    // for QXmlStreamAttributes

#include "defs.h"

class xml_tag
{
public:

  /* Member Functions */

  static xml_tag* xml_findnext(xml_tag* root, xml_tag* cur, const QString& tagname);
  static xml_tag* xml_findfirst(xml_tag* root, const QString& tagname);
  static QString xml_attribute(const QXmlStreamAttributes& attributes, const QString& attrname);

  /* Data Members */

  QString tagname;
  QString cdata;
  QString parentcdata;
  QXmlStreamAttributes attributes;
  xml_tag* parent{nullptr};
  xml_tag* sibling{nullptr};
  xml_tag* child{nullptr};

private:

  /* Member Functions */

  static xml_tag* xml_next(xml_tag* root, xml_tag* cur);
};

struct fs_xml : FormatSpecificData {
  explicit fs_xml(FsType type) : FormatSpecificData(type) {}
private:
  fs_xml(const fs_xml&) = default;
public:
  fs_xml& operator=(const fs_xml&) = delete;
  fs_xml(fs_xml&&) = delete;
  fs_xml& operator=(fs_xml&&) = delete;
  ~fs_xml() override;

  fs_xml* clone() const override;

  xml_tag* tag{nullptr};
};

fs_xml* fs_xml_alloc(FsType type);

#endif // SRC_CORE_XMLTAG_H
