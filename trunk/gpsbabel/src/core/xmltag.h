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
    Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111 USA

 */
class xml_tag {
 public:
  xml_tag() :
    tagname(NULL),
    cdata(NULL),
    parentcdata(NULL),
    attributes(NULL),
    parent(NULL),
    sibling(NULL),
    child(NULL) {}

  char* tagname;
  char* cdata;
  char* parentcdata;
  char** attributes;
  xml_tag* parent;
  xml_tag* sibling;
  xml_tag* child;
};

xml_tag* xml_findfirst(xml_tag* root, const char* tagname);
xml_tag* xml_findnext(xml_tag* root, xml_tag* cur, const char* tagname);
char* xml_attribute(xml_tag* tag, const char* attrname);
void free_gpx_extras(xml_tag* tag);

typedef struct fs_xml {
  format_specific_data fs;
  xml_tag* tag;
} fs_xml;

fs_xml* fs_xml_alloc(long type);
