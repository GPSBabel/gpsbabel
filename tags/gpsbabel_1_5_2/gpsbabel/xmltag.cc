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
    Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111 USA

 */

#include "defs.h"
#include "cet_util.h"
#include "src/core/xmltag.h"
#include <stddef.h>
#include <stdio.h>
#include <string.h>

static void
free_xml_tag(xml_tag* tag)
{
  xml_tag* next = NULL;
  char** ap;

  while (tag) {
    if (tag->child) {
      free_gpx_extras(tag->child);
    }
    if (tag->attributes) {
      ap = tag->attributes;

      while (*ap) {
        xfree(*ap++);
      }

      xfree(tag->attributes);
    }

    next = tag->sibling;
    delete tag;
    tag = next;
  }
}

// FIXME: at some point, this becomes a plain ole copy constructor.
static void
copy_xml_tag(xml_tag** copy, xml_tag* src, xml_tag* parent)
{
  xml_tag* res = NULL;
  char** ap = NULL;
  char** ap2 = NULL;
  int count = 0;

  if (!src) {
    *copy = NULL;
    return;
  }

  res = new xml_tag;
  *copy = res;

//  memcpy(res, src, sizeof(xml_tag));
  res->tagname = (src->tagname);
  res->cdata = (src->cdata);
  res->parentcdata = (src->parentcdata);
  if (src->attributes) {
    ap = src->attributes;
    while (*ap) {
      count++;
      ap++;
    }
    res->attributes = (char**)xcalloc(count+1, sizeof(char*));
    ap = src->attributes;
    ap2 = res->attributes;
    while (*ap) {
      *ap2 = xstrdup(*ap);
      ap++;
      ap2++;
    }
  }
  res->parent = parent;
  copy_xml_tag(&(res->sibling), src->sibling, parent);
  copy_xml_tag(&(res->child), src->child, res);
}

static void
convert_xml_tag(xml_tag* tag)
{
  char** ap = NULL;

  if (tag == NULL) {
    return;
  }

//  tag->cdata = cet_convert_string(tag->cdata);
//  tag->parentcdata = cet_convert_string(tag->parentcdata);
  tag->cdata = tag->cdata;
  tag->parentcdata = tag->parentcdata;

  ap = tag->attributes;
  while (*ap) {
    *ap = cet_convert_string(*ap);
    ap++;
  }
  convert_xml_tag(tag->sibling);
  convert_xml_tag(tag->child);
}

fs_xml* fs_xml_alloc(long type);

static void
fs_xml_destroy(void* fs)
{
  fs_xml* xml = (fs_xml*)fs;
  if (xml) {
    free_xml_tag(xml->tag);
  }
  xfree(fs);
}

static void
fs_xml_copy(void** copy, void* source)
{
  fs_xml* src = (fs_xml*)source;
  if (!source) {
    *copy = NULL;
    return;
  }
  *copy = (void*)fs_xml_alloc(src->fs.type);
  memcpy(*copy, source, sizeof(fs_xml));
  copy_xml_tag(&(((fs_xml*)(*copy))->tag), src->tag, NULL);
}

static void
fs_xml_convert(void* fs)
{
  fs_xml* xml = (fs_xml*)fs;
  if (xml) {
    convert_xml_tag(xml->tag);
  }
}

fs_xml* fs_xml_alloc(long type)
{
  fs_xml* result = NULL;

  result = (fs_xml*)xcalloc(1, sizeof(fs_xml));
  result->fs.type = type;
  result->fs.copy = fs_xml_copy;
  result->fs.destroy = fs_xml_destroy;
  result->fs.convert = fs_xml_convert;
  return result;
}
