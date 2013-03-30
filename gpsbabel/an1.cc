/*
    Read DeLorme drawing files (.an1)

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

#include <stddef.h>
#include <stdio.h>
#include <string.h>
#include <limits.h>

#define MYNAME "an1"
#include "defs.h"

static gbfile* infile;
static gbfile* outfile;

static char* output_type = NULL;
static char* road_changes = NULL;
static char* nogc = NULL;
static char* nourl = NULL;
static char* opt_symbol = NULL;
static char* opt_color = NULL;
static char* opt_zoom  = NULL;
static char* opt_wpt_type = NULL;
static char* opt_radius = NULL;

static short output_type_num = 0;
static short opt_zoom_num = 0;
static long opt_color_num = 0;
static short wpt_type_num = 0;
static short last_read_type = 0;
static double radius = 0.0;

static long serial=10000;
static long rtserial=1;

typedef struct roadchange {
  long type;
  char* name;
} roadchange;

roadchange* roadchanges = NULL;

static
arglist_t an1_args[] = {
  {
    "type", &output_type, "Type of .an1 file",
    "", ARGTYPE_STRING, ARG_NOMINMAX
  },
  {
    "road", &road_changes, "Road type changes",
    "", ARGTYPE_STRING, ARG_NOMINMAX
  },
  {
    "nogc", &nogc, "Do not add geocache data to description",
    NULL, ARGTYPE_BOOL, ARG_NOMINMAX
  },
  {
    "nourl", &nourl, "Do not add URLs to description",
    NULL, ARGTYPE_BOOL, ARG_NOMINMAX
  },
  {
    "deficon", &opt_symbol, "Symbol to use for point data",
    "Red Flag", ARGTYPE_STRING, ARG_NOMINMAX
  },
  {
    "color", &opt_color, "Color for lines or mapnotes",
    "red", ARGTYPE_STRING, ARG_NOMINMAX
  },
  {
    "zoom", &opt_zoom, "Zoom level to reduce points",
    NULL, ARGTYPE_INT, ARG_NOMINMAX
  },
  {
    "wpt_type", &opt_wpt_type,
    "Waypoint type",
    "", ARGTYPE_STRING, ARG_NOMINMAX
  },
  {
    "radius", &opt_radius, "Radius for circles",
    NULL, ARGTYPE_STRING, ARG_NOMINMAX
  },
  ARG_TERMINATOR
};

typedef struct guid {
  unsigned long l;
  unsigned short s[3];
  unsigned char c[6];
} GUID;

#include "an1sym.h"

#define ReadShort(f) gbfgetint16(f)
#define WriteShort(f,s) gbfputint16((s),f)
#define ReadLong(f) gbfgetint32(f)
#define WriteLong(f,l) gbfputint32((l),f)
#define ReadDouble(f) gbfgetdbl(f)
#define WriteDouble(f,d) gbfputdbl((d),f)

static char*
ReadString(gbfile* f, short len)
{
  char* result = NULL;
  result = (char*)xcalloc(1, len + 1);
  if (len) {
    gbfread(result, 1, len, f);
  }
  return result;
}

#define ReadChar(f) (unsigned char) gbfgetc(f)
#define WriteChar(f,c) gbfputc((unsigned char)(c),f)
#define WriteString(f,s) gbfputs((s),f)

static void
ReadGuid(gbfile* f, GUID* guid)
{
  int i = 0;
  guid->l = ReadLong(f);
  for (i = 0; i < 3; i++) {
    guid->s[i] = ReadShort(f);
  }
  for (i = 0; i < 6; i++) {
    guid->c[i] = ReadChar(f);
  }
}

static void
WriteGuid(gbfile* f, GUID* guid)
{
  int i = 0;
  WriteLong(f, guid->l);
  for (i = 0; i < 3; i++) {
    WriteShort(f, guid->s[i]);
  }
  for (i = 0; i < 6; i++) {
    WriteChar(f, guid->c[i]);
  }
}

static void
Skip(gbfile* f,
     unsigned long distance)
{
  gbfseek(f, distance, SEEK_CUR);
}

static double
DecodeOrd(long ord)
{
  return (double)((gbint32)(0x80000000 - ord)) / 0x800000;
}

static long
EncodeOrd(double ord)
{
  return (gbint32)(0x80000000 - (gbint32)(ord * 0x800000));
}

typedef struct {
  short hotspotxhi;
  long hotspoty;
  long unk1;
  GUID guid;
  char* name;
} an1_symbol_record;

typedef struct {
  format_specific_data fs;
  short magic;
  long unk1;
  long lon;
  long lat;
  short type;
  long height;
  long width;
  short unk2;
  short unk3;
  short serial;
  short unk4;
  unsigned char create_zoom;
  unsigned char visible_zoom;
  short unk5;
  double radius; /* in km */
  char* name;
  char* fontname;
  GUID guid;
  long fontcolor;
  long fontstyle;
  long fontsize;
  long outlineweight;
  long outlinecolor;
  long outlineflags;
  long fillcolor;
  long unk6;
  long fillflags;

  /* Added in SA2006/Topo 6.0 */
  short unk6_1;
  char* url;
  char* comment;
  long creation_time;
  long modification_time;
  char* image_name;
} an1_waypoint_record;

typedef struct {
  format_specific_data fs;
  short magic;
  long unk0;
  long lon;
  long lat;
  short unk1;
} an1_vertex_record;

typedef struct {
  format_specific_data fs;
  long roadtype;
  short serial;
  long unk2;
  short unk3;
  short type;
  long unk4;
  char* name;
  long lineweight;
  long linestyle;
  long linecolor;
  long opacity;
  long polyfillcolor;
  long unk6;
  long unk7;
  short unk8;
  long pointcount;
} an1_line_record;

static an1_waypoint_record* Alloc_AN1_Waypoint();

void Destroy_AN1_Waypoint(void* vwpt)
{

  an1_waypoint_record* wpt = (an1_waypoint_record*)vwpt;
  xfree(wpt->name);
  xfree(wpt->fontname);
  if (wpt->url) {
    xfree(wpt->url);
  }
  if (wpt->comment) {
    xfree(wpt->comment);
  }
  if (wpt->image_name) {
    xfree(wpt->image_name);
  }
  xfree(vwpt);
}

void Copy_AN1_Waypoint(void** vdwpt, void* vwpt)
{
  an1_waypoint_record* wpt = (an1_waypoint_record*)vwpt;
  an1_waypoint_record* dwpt = Alloc_AN1_Waypoint();
  memcpy(dwpt, wpt, sizeof(an1_waypoint_record));
  dwpt->name = xstrdup(wpt->name);
  dwpt->fontname = xstrdup(wpt->fontname);
  dwpt->url = xstrdup(wpt->url);
  dwpt->comment = xstrdup(wpt->comment);
  dwpt->image_name = xstrdup(wpt->image_name);
  *vdwpt = (void*)dwpt;
}

static an1_waypoint_record* Alloc_AN1_Waypoint()
{
  an1_waypoint_record* result = NULL;
  result = (an1_waypoint_record*)xcalloc(sizeof(*result), 1);
  result->fs.type = FS_AN1W;
  result->fs.copy = Copy_AN1_Waypoint;
  result->fs.destroy = Destroy_AN1_Waypoint;
  result->fs.convert = NULL;
  return result;
}

static an1_vertex_record* Alloc_AN1_Vertex();

void Destroy_AN1_Vertex(void* vvertex)
{
  xfree(vvertex);
}

void Copy_AN1_Vertex(void** vdvert, void* vvert)
{
  an1_vertex_record* vert = (an1_vertex_record*)vvert;
  an1_vertex_record* dvert = Alloc_AN1_Vertex();
  memcpy(dvert, vert, sizeof(an1_vertex_record));
  *vdvert = (void*)dvert;
}

static an1_vertex_record* Alloc_AN1_Vertex()
{
  an1_vertex_record* result = NULL;
  result = (an1_vertex_record*)xcalloc(sizeof(*result), 1);
  result->fs.type = FS_AN1V;
  result->fs.copy = Copy_AN1_Vertex;
  result->fs.destroy = Destroy_AN1_Vertex;
  result->fs.convert = NULL;
  return result;
}


static an1_line_record* Alloc_AN1_Line();

void Destroy_AN1_Line(void* vline)
{
  an1_line_record* line = (an1_line_record*)vline;
  xfree(line->name);
  xfree(vline);
}

void Copy_AN1_Line(void** vdline, void* vline)
{
  an1_line_record* line = (an1_line_record*)vline;
  an1_line_record* dline = Alloc_AN1_Line();
  memcpy(dline, line, sizeof(an1_line_record));
  dline->name = xstrdup(line->name);
  *vdline = (void*)dline;
}

static an1_line_record* Alloc_AN1_Line()
{
  an1_line_record* result = NULL;
  result = (an1_line_record*)xcalloc(sizeof(*result), 1);
  result->fs.type = FS_AN1L;
  result->fs.copy = Copy_AN1_Line;
  result->fs.destroy = Destroy_AN1_Line;
  result->fs.convert = NULL;
  return result;
}


static void Destroy_AN1_Symbol(an1_symbol_record* symbol)
{
  xfree(symbol->name);
}

static void Read_AN1_Waypoint(gbfile* f, an1_waypoint_record* wpt)
{
  short len;

  wpt->magic = ReadShort(f);
  wpt->unk1 = ReadLong(f);
  wpt->lon = ReadLong(f);
  wpt->lat = ReadLong(f);
  wpt->type = ReadShort(f);
  wpt->height = ReadLong(f);
  wpt->width = ReadLong(f);
  wpt->unk2 = ReadShort(f);
  wpt->unk3 = ReadShort(f);
  wpt->serial = ReadShort(f);
  wpt->unk4 = ReadShort(f);
  wpt->create_zoom = ReadChar(f);
  wpt->visible_zoom = ReadChar(f);
  wpt->unk5 = ReadShort(f);
  wpt->radius = ReadDouble(f);
  len = ReadShort(f);
  wpt->name = ReadString(f, len);

  if (len != strlen(wpt->name)) {
    /* This happens in 06/6.0 files that put extra data in the
     * name record for backward compatibility's sake */
    char* ofs = wpt->name + strlen(wpt->name) + 1;
    wpt->unk6_1 = le_read16(ofs);
    ofs += 2;

    len = le_read16(ofs);
    ofs += 2;

    if (len) {
      char* oldurlstr;
      /*
       * Trust URL encoded in new format over one in
       * old format if both are present.  Whack the
       * name starting at '{URL='.
       */
      oldurlstr = strstr(wpt->name, "{URL=");
      if (oldurlstr) {
        *oldurlstr = 0;
      }

      wpt->url = (char*) xcalloc(len+1, 1);
      memcpy(wpt->url, ofs, len);
      ofs += len;
    }

    len = le_read16(ofs);
    ofs += 2;

    if (len) {
      wpt->comment = (char*) xcalloc(len+1, 1);
      memcpy(wpt->comment, ofs, len);
      ofs += len;
    }

    /* these are quadwords, presumably for year-2038 compat. */
    wpt->creation_time = le_read32(ofs);
    ofs += 8;

    wpt->modification_time = le_read32(ofs);
    ofs += 8;
  }

  if (wpt->type == 0x12) {
    /* 'image' type */
    ReadShort(f);    /* length of font + filename */
    len = ReadShort(f);
    wpt->fontname = ReadString(f, len);
    len = ReadShort(f);
    wpt->image_name = ReadString(f, len);
  } else {
    len = ReadShort(f);
    wpt->fontname = ReadString(f, len);
    wpt->image_name = NULL;
  }
  ReadGuid(f, &wpt->guid);
  wpt->fontcolor = ReadLong(f);
  wpt->fontstyle = ReadLong(f);
  wpt->fontsize = ReadLong(f);
  wpt->outlineweight = ReadLong(f);
  wpt->outlinecolor = ReadLong(f);
  wpt->outlineflags = ReadLong(f);
  wpt->fillcolor = ReadLong(f);
  wpt->unk6 = ReadLong(f);
  wpt->fillflags = ReadLong(f);
}

static void Write_AN1_Waypoint(gbfile* f, an1_waypoint_record* wpt)
{
  short len;

  WriteShort(f, wpt->magic);
  WriteLong(f, wpt->unk1);
  WriteLong(f, wpt->lon);
  WriteLong(f, wpt->lat);
  WriteShort(f, wpt->type);
  WriteLong(f, wpt->height);
  WriteLong(f, wpt->width);
  WriteShort(f, wpt->unk2);
  WriteShort(f, wpt->unk3);
  WriteShort(f, wpt->serial);
  WriteShort(f, wpt->unk4);
  WriteChar(f, wpt->create_zoom);
  WriteChar(f, wpt->visible_zoom);
  WriteShort(f, wpt->unk5);
  WriteDouble(f, wpt->radius);

  len = strlen(wpt->name) + 1 + 2 + 2 +
        (wpt->url ? strlen(wpt->url) : 0) + 2 +
        (wpt->comment ? strlen(wpt->comment) : 0) + 8 + 8;
  WriteShort(f, len);
  WriteString(f, wpt->name);
  WriteChar(f, 0);    /* name string terminator */

  WriteShort(f, wpt->unk6_1);

  if (wpt->url) {
    WriteShort(f, strlen(wpt->url));
    WriteString(f, wpt->url);
  } else {
    WriteShort(f, 0);
  }

  if (wpt->comment) {
    WriteShort(f, strlen(wpt->comment));
    WriteString(f, wpt->comment);
  } else {
    WriteShort(f, 0);
  }

  WriteLong(f, wpt->creation_time);
  WriteLong(f, 0);

  WriteLong(f, wpt->modification_time);
  WriteLong(f, 0);

  if (wpt->type == 0x12) {   /* image */
    len = 2 + (wpt->fontname ? strlen(wpt->fontname) : 0) +
          2 + (wpt->image_name ? strlen(wpt->image_name) : 0);
    WriteShort(f, len);
    if (wpt->fontname) {
      len = strlen(wpt->fontname);
      WriteShort(f, len);
      WriteString(f, wpt->fontname);
    } else {
      WriteShort(f, 0);
    }
    if (wpt->image_name) {
      len = strlen(wpt->image_name);
      WriteShort(f, len);
      WriteString(f, wpt->image_name);
    } else {
      WriteShort(f, 0);
    }
  } else {
    len = strlen(wpt->fontname);
    WriteShort(f, len);
    WriteString(f, wpt->fontname);
  }
  WriteGuid(f, &wpt->guid);
  WriteLong(f, wpt->fontcolor);
  WriteLong(f, wpt->fontstyle);
  WriteLong(f, wpt->fontsize);
  WriteLong(f, wpt->outlineweight);
  WriteLong(f, wpt->outlinecolor);
  WriteLong(f, wpt->outlineflags);
  WriteLong(f, wpt->fillcolor);
  WriteLong(f, wpt->unk6);
  WriteLong(f, wpt->fillflags);
}

static void Read_AN1_Vertex(gbfile* f, an1_vertex_record* vertex)
{

  vertex->magic = ReadShort(f);
  vertex->unk0 = ReadLong(f);
  vertex->lon = ReadLong(f);
  vertex->lat = ReadLong(f);
  vertex->unk1 = ReadShort(f);
}

static void Write_AN1_Vertex(gbfile* f, an1_vertex_record* vertex)
{
  WriteShort(f, vertex->magic);
  WriteLong(f, vertex->unk0);
  WriteLong(f, vertex->lon);
  WriteLong(f, vertex->lat);
  WriteShort(f, vertex->unk1);
}

static void Read_AN1_Line(gbfile* f, an1_line_record* line)
{

  short len;

  line->roadtype = ReadLong(f);
  line->serial = ReadShort(f);
  line->unk2 = ReadLong(f);
  line->unk3 = ReadShort(f);
  line->type = ReadShort(f);
  line->unk4 = ReadLong(f);
  len = ReadShort(f);
  line->name = ReadString(f, len);
  line->lineweight = ReadShort(f);
  line->linestyle = ReadLong(f);
  line->linecolor = ReadLong(f);
  line->opacity = ReadLong(f);
  line->polyfillcolor = ReadLong(f);
  line->unk6 = ReadLong(f);
  line->unk7 = ReadLong(f);
  line->unk8 = ReadShort(f);
  line->pointcount = ReadLong(f);
}

static void Write_AN1_Line(gbfile* f, an1_line_record* line)
{
  short len;

  WriteLong(f, line->roadtype);
  WriteShort(f, line->serial);
  WriteLong(f, line->unk2);
  WriteShort(f, line->unk3);
  WriteShort(f, line->type);
  WriteLong(f, line->unk4);
  len = strlen(line->name);
  WriteShort(f, len);
  WriteString(f, line->name);
  WriteShort(f, (short) line->lineweight);
  WriteLong(f, line->linestyle);
  WriteLong(f, line->linecolor);
  WriteLong(f, line->opacity);
  WriteLong(f, line->polyfillcolor);
  WriteLong(f, line->unk6);
  WriteLong(f, line->unk7);
  WriteShort(f, line->unk8);
  WriteLong(f, line->pointcount);
}

static void Skip_AN1_IL(gbfile* f)
{
  Skip(f, 26);
}

static void Skip_AN1_BM(gbfile* f)
{
  unsigned long bmsize;
  unsigned long palettesize;
  unsigned long bmisize;
  unsigned long bitoffset;

  Skip(f, 8);    /* BITMAPFILEHEADER fields 1-3 */
  bitoffset = ReadLong(f);

  bmisize = ReadLong(f);
  Skip(f, 16);    /* BITMAPINFOHEADER fields 2-6 */
  bmsize = ReadLong(f);
  Skip(f, 16);    /* BITMAPINFOHEADER fields 8-11 */

  palettesize = bitoffset - bmisize - 14;
  Skip(f, bmsize + palettesize);
}

static void Read_AN1_Symbol(gbfile* f, an1_symbol_record* symbol)
{
  short len;

  /* This is just the high word of a long; we ate the low
   * word in the caller.  Fortunately, we don't care. */
  symbol->hotspotxhi = ReadShort(f);
  symbol->hotspoty = ReadLong(f);
  symbol->unk1 = ReadLong(f);
  ReadGuid(f, &symbol->guid);
  len = ReadChar(f);
  symbol->name = ReadString(f, len);
}

static void Read_AN1_Header(gbfile* f)
{
  unsigned short magic;
  unsigned short type;

  magic = ReadShort(f);
  type = ReadShort(f);

  last_read_type = type;
}

static void Write_AN1_Header(gbfile* f)
{
  WriteShort(f, 11557);
  WriteShort(f, output_type_num);
}

static void Read_AN1_Bitmaps(gbfile* f)
{
  long count;
  unsigned short magic;
  an1_symbol_record symbol;

  count = ReadLong(f);

  while (count) {
    magic = ReadShort(f);
    switch (magic) {
    case 0x4d42:
      Skip_AN1_BM(f);
      break;
    case 0x4c49:
      Skip_AN1_IL(f);
      break;
    default:
      Read_AN1_Symbol(f, &symbol);
      Destroy_AN1_Symbol(&symbol);
      count--;
      break;
    }
  }

  /* Read the symbol table */
}

static void Write_AN1_Bitmaps(gbfile* f)
{
  /* On write, we don't output any bitmaps, so writing them
   * is just a matter of writing a count of zero */
  WriteLong(f, 0);
}

static void Read_AN1_Waypoints(gbfile* f)
{
  unsigned long count = 0;
  unsigned long i = 0;
  an1_waypoint_record* rec = NULL;
  waypoint* wpt_tmp;
  char* icon = NULL;
  char* url = NULL;
  ReadShort(f);
  count = ReadLong(f);
  for (i = 0; i < count; i++) {
    rec = Alloc_AN1_Waypoint();
    Read_AN1_Waypoint(f, rec);
    wpt_tmp = waypt_new();

    if (rec->creation_time) {
      wpt_tmp->creation_time = rec->creation_time;
    }
    wpt_tmp->longitude = -DecodeOrd(rec->lon);
    wpt_tmp->latitude = DecodeOrd(rec->lat);
    wpt_tmp->notes = xstrdup(rec->comment);
    wpt_tmp->description = xstrdup(rec->name);
    if (rec->url) {
      wpt_tmp->url = xstrdup(rec->url);
    } else if (NULL != (url=strstr(wpt_tmp->description, "{URL="))) {
      *url = '\0';
      url += 5;
      url[strlen(url)-1] = '\0';
      wpt_tmp->url = xstrdup(url);
    }

    if (rec->image_name) {
      wpt_tmp->icon_descr = xstrdup(rec->image_name);
    } else if (FindIconByGuid(&rec->guid, &icon)) {
      wpt_tmp->icon_descr = icon;
    }

    fs_chain_add(&(wpt_tmp->fs), (format_specific_data*)rec);
    rec = NULL;
    waypt_add(wpt_tmp);
  }
}

static void
Write_One_AN1_Waypoint(const waypoint* wpt)
{
  an1_waypoint_record* rec;
  int local;
  format_specific_data* fs = NULL;

  fs = fs_chain_find(wpt->fs, FS_AN1W);
  if (fs) {
    rec = (an1_waypoint_record*)fs;
    xfree(rec->name);
    local = 0;
    if (opt_zoom) {
      rec->visible_zoom = opt_zoom_num;
    }
  } else {
    rec = Alloc_AN1_Waypoint();
    local = 1;
    rec->magic = 1;
    rec->type = wpt_type_num;
    rec->unk2 = 3;
    rec->unk3 = 18561;
    rec->radius = radius;
    rec->fillcolor = opt_color_num;
    rec->fillflags = 3;
    if (wpt_type_num == 5) {
      rec->fillflags = 0x8200;
    }
    rec->height = -50;
    rec->width = 20;
    rec->fontname = xstrdup("Arial");
    FindIconByName(opt_symbol, &rec->guid);
    rec->fontsize = 10;
    rec->visible_zoom = opt_zoom?opt_zoom_num:10;
    rec->unk6_1 = 1;
  }
  rec->name = xstrdup(wpt->description);

  if (!nogc && wpt->gc_data->id) {
    char* extra = (char*) xmalloc(25 + strlen(wpt->gc_data->placer.toUtf8().data()) + strlen(wpt->shortname));
    sprintf(extra, "\r\nBy %s\r\n%s (%1.1f/%1.1f)",
            wpt->gc_data->placer.toUtf8().data(),
            wpt->shortname, wpt->gc_data->diff/10.0,
            wpt->gc_data->terr/10.0);
    rec->name = xstrappend(rec->name, extra);
    xfree(extra);
  }

  if (!nourl && wpt->hasLink()) {
    int len = 7 + wpt->url.length();
    char* extra = (char*)xmalloc(len);
    sprintf(extra, "{URL=%s}", wpt->url.toUtf8().data());
    rec->name = xstrappend(rec->name, extra);
    xfree(extra);
    rec->url = xstrdup(wpt->url.toUtf8().data());
  }

  if (wpt->notes) {
    if (rec->comment) {
      xfree(rec->comment);
    }
    rec->comment = xstrdup(wpt->notes);
  }


  rec->creation_time = rec->modification_time = wpt->creation_time;
  rec->lat = EncodeOrd(wpt->latitude);
  rec->lon = EncodeOrd(-wpt->longitude);
  rec->serial = serial++;

  if (rec->type == 0x12) {    /* image */
    if (wpt->icon_descr.contains(":\\")) {
      rec->image_name = xstrdup(wpt->icon_descr.toUtf8().data());
      rec->height = -244;
      rec->width = -1;
    }
  }
  if (!rec->image_name && !wpt->icon_descr.isNull()) {
// FIXME: WTH?
char* t = xstrdup(wpt->icon_descr.toUtf8().data());
    FindIconByName(t, &rec->guid);
xfree(t);
  }

  Write_AN1_Waypoint(outfile, rec);
  if (local) {
    Destroy_AN1_Waypoint(rec);
  }
}

static void Write_AN1_Waypoints(gbfile* f)
{
  WriteShort(f, 2);
  WriteLong(f, waypt_count());
  waypt_disp_all(Write_One_AN1_Waypoint);
}

static void Read_AN1_Lines(gbfile* f)
{
  unsigned long count = 0;
  unsigned long i = 0;
  unsigned long j = 0;
  an1_line_record* rec = NULL;
  an1_vertex_record* vert = NULL;
  route_head* rte_head;
  waypoint* wpt_tmp;

  ReadShort(f);
  count = ReadLong(f);
  for (i = 0; i < count; i++) {
    rec = Alloc_AN1_Line();
    Read_AN1_Line(f, rec);
    /* create route rec */
    rte_head = route_head_alloc();
    rte_head->line_color.bbggrr = rec->linecolor;
    if (rec->opacity == 0x8200) {
      rte_head->line_color.opacity = 128;
    }
    // lineweight isn't set for dashed/dotted lines
    // Since we don't have a way to represent this internally yet,
    // use leave line_width at the default.
    if (rec->lineweight) {
      rte_head->line_width = rec->lineweight;
    }
    rte_head->rte_name = xstrdup(rec->name);
    fs_chain_add(&rte_head->fs, (format_specific_data*)rec);
    route_add_head(rte_head);
    for (j = 0; j < (unsigned) rec->pointcount; j++) {
      vert = Alloc_AN1_Vertex();
      Read_AN1_Vertex(f, vert);

      /* create route point */
      wpt_tmp = waypt_new();
      wpt_tmp->latitude = DecodeOrd(vert->lat);
      wpt_tmp->longitude = -DecodeOrd(vert->lon);
      wpt_tmp->shortname = (char*) xmalloc(7);
      sprintf(wpt_tmp->shortname, "\\%5.5lx", rtserial++);
      fs_chain_add(&wpt_tmp->fs,
                   (format_specific_data*)vert);
      route_add_wpt(rte_head, wpt_tmp);
    }
  }
}

static void
Make_Road_Changes(an1_line_record* rec)
{
  int i = 0;

  if (!rec) {
    return;
  }

  if (!roadchanges) {
    return;
  }

  while (roadchanges[i].name) {
    if (!case_ignore_strcmp(roadchanges[i].name, rec->name)) {
      rec->roadtype = roadchanges[i].type;
      break;
    }
    i++;
  }
}

static void
Write_One_AN1_Line(const route_head* rte)
{
  an1_line_record* rec;
  int local;
  format_specific_data* fs = NULL;

  fs = fs_chain_find(rte->fs, FS_AN1L);

  if (fs) {
    rec = (an1_line_record*)(void*)fs;
    local = 0;
    switch (output_type_num) {
    case 1:
      if (rec->type != 14) {
        rec = Alloc_AN1_Line();
        memcpy(rec, fs, sizeof(an1_line_record));
        local = 1;
        rec->roadtype = 0x11100541;
        rec->unk2 = 655360;
        rec->type = 14;
        rec->unk8 = 2;
      } // end if
      Make_Road_Changes(rec);
      break;
    case 2:
      if (rec->type != 15) {
        rec = Alloc_AN1_Line();
        memcpy(rec, fs, sizeof(an1_line_record));
        local = 1;
        rec->type = 15;
      } // end if
      break;
    case 4:
      if (rec->type != 16) {
        rec = Alloc_AN1_Line();
        memcpy(rec, fs, sizeof(an1_line_record));
        local = 1;
        rec->type = 16;
      } // end if
      break;
    }
  } else {
    rec = Alloc_AN1_Line();
    local = 1;
    rec->name = NULL;
    switch (output_type_num) {
      /*  drawing road trail waypoint track  */
    case 1: /* road */
      rec->roadtype = 0x11100541;
      rec->unk2 = 655360;
      rec->type = 14;
      rec->unk8 = 2;
      rec->name = xstrdup(rte->rte_name);
      break;

    case 2: /* trail */
      rec->roadtype = 0x11071c50;
      rec->unk2 = 917504;
      rec->type = 15;
      rec->unk8 = 2;
      break;

    case 4: /* track */
      rec->roadtype = 0x48800015;
      rec->unk2 = 917504;
      rec->type = 16;
      rec->unk4 = 2;
      rec->unk8 = 2;
      break;

    case 0: /* drawing */
    case 3: /* waypoint - shouldn't have lines */
    default:
      rec->roadtype = 0x48800015;
      rec->unk2 = 1048576;
      rec->type = 2;
      rec->unk4 = 2;
      rec->lineweight = 6;
      rec->linecolor = opt_color_num; /* red */
      rec->opacity = 3;
      rec->unk8 = 2;
      break;
    }
    if (!rec->name) {
      rec->name = xstrdup("");
    }

  }
  rec->serial = serial++;
  rec->pointcount = rte->rte_waypt_ct;
  Write_AN1_Line(outfile, rec);
  if (local) {
    Destroy_AN1_Line(rec);
  }
}

static void
Write_One_AN1_Vertex(const waypoint* wpt)
{
  an1_vertex_record* rec;
  int local;
  format_specific_data* fs = NULL;

  fs = fs_chain_find(wpt->fs, FS_AN1V);

  if (fs) {
    rec = (an1_vertex_record*)(void*)fs;
    local = 0;
  } else {
    rec = Alloc_AN1_Vertex();
    local = 1;
    rec->magic = 1;
  }
  rec->lat = EncodeOrd(wpt->latitude);
  rec->lon = EncodeOrd(-wpt->longitude);

  Write_AN1_Vertex(outfile, rec);
  if (local) {
    Destroy_AN1_Vertex(rec);
  }
}

static void Write_AN1_Lines(gbfile* f)
{
  WriteShort(f, 2);
  WriteLong(f, route_count()+track_count());

  route_disp_all(Write_One_AN1_Line, NULL, Write_One_AN1_Vertex);
  track_disp_all(Write_One_AN1_Line, NULL, Write_One_AN1_Vertex);
}

static void
Init_Wpt_Type(void)
{
  if (!opt_wpt_type || !opt_wpt_type[0]) {
    wpt_type_num = 1; /* marker */
    return;
  }
  if ((opt_wpt_type[0] & 0xf0) == 0x30) {
    wpt_type_num = atoi(opt_wpt_type);
  } else {
    wpt_type_num = 1; /* marker */
    if (!case_ignore_strcmp(opt_wpt_type, "marker")) {
      wpt_type_num = 1;
    } else if (!case_ignore_strcmp(opt_wpt_type, "symbol")) {
      wpt_type_num = 1; /* symbol and marker are synonyms */
    } else if (!case_ignore_strcmp(opt_wpt_type, "text")) {
      wpt_type_num = 4;
    } else if (!case_ignore_strcmp(opt_wpt_type, "mapnote")) {
      wpt_type_num = 6;
    } else if (!case_ignore_strcmp(opt_wpt_type, "circle")) {
      wpt_type_num = 5;
    } else if (!case_ignore_strcmp(opt_wpt_type, "image")) {
      wpt_type_num = 18;
    } else {
      fatal(MYNAME ": wpt_type must be "
            "symbol, text, mapnote, circle, or image\n");
    }
  }
}

static void
Init_Output_Type(void)
{
  if (!output_type || !output_type[0]) {
    output_type_num = last_read_type;
    return;
  }
  if ((output_type[0] & 0xf0) == 0x30) {
    output_type_num = atoi(output_type);
  } else {
    output_type_num = 0;
    if (!case_ignore_strcmp(output_type, "drawing")) {
      output_type_num = 0;
    } else if (!case_ignore_strcmp(output_type, "road")) {
      output_type_num = 1;
    } else if (!case_ignore_strcmp(output_type, "trail")) {
      output_type_num = 2;
    } else if (!case_ignore_strcmp(output_type, "waypoint")) {
      output_type_num = 3;
    } else if (!case_ignore_strcmp(output_type, "track")) {
      output_type_num = 4;
    } else {
      fatal(MYNAME ": type must be "
            "drawing, road, trail, waypoint, or track\n");
    }
  }
  last_read_type = output_type_num;
}

static long
Parse_Change_Type(char* type)
{
  long retval = 0x11100541;

  if (!case_ignore_strcmp(type, "limited")) {
    retval = 0x11070430;
  } else if (!case_ignore_strcmp(type, "toll")) {
    retval = 0x11070470;
  } else if (!case_ignore_strcmp(type, "us")) {
    retval = 0x11070870;
  } else if (!case_ignore_strcmp(type, "state")) {
    retval = 0x11070c10;
  } else if (!case_ignore_strcmp(type, "primary")) {
    /* primary state/provincial routes */
    retval = 0x11070840;
  } else if (!case_ignore_strcmp(type, "major")) {
    retval = 0x11070c30;
  } else if (!case_ignore_strcmp(type, "local")) {
    retval = 0x11071010;
  } else if (!case_ignore_strcmp(type, "ramp")) {
    retval = 0x11070cb0;
  } else if (!case_ignore_strcmp(type, "ferry")) {
    retval = 0x11070ca0;
  } else if (!case_ignore_strcmp(type, "editable")) {
    retval = 0x11100541;
  } else {
    fatal(MYNAME ": unknown road type for road changes\n");
  }
  return retval;
}

static void
Free_Road_Changes(void)
{
  int i = 0;
  if (roadchanges) {
    while (roadchanges[i].name) {
      xfree(roadchanges[i].name);
      i++;
    }
    xfree(roadchanges);
  }
  roadchanges = NULL;
}

static void
Init_Road_Changes(void)
{
  int count = 0;
  char* strType = NULL;
  char* name = NULL;
  char* bar = NULL;
  char* copy = NULL;
  Free_Road_Changes();

  if (!road_changes || !road_changes[0]) {
    return;
  }
  bar = strchr(road_changes, '!');
  while (bar) {
    count++;
    bar = strchr(bar+1, '!');
  }
  if (!(count&1)) {
    fatal(MYNAME ": invalid format for road changes\n");
  }
  count = 1 + count / 2;
  roadchanges = (roadchange*)xmalloc((count+1) * sizeof(roadchange));

  roadchanges[count].type = 0;
  roadchanges[count].name = NULL;

  copy = xstrdup(road_changes);
  bar = copy;

  while (count) {
    count--;
    name = bar;
    bar = strchr(name, '!');
    *bar = '\0';
    bar++;
    strType = bar;
    bar = strchr(strType, '!');
    if (bar) {
      *bar = '\0';
      bar++;
    }
    roadchanges[count].name = xstrdup(name);
    roadchanges[count].type = Parse_Change_Type(strType);
  }

  xfree(copy);
}

static void
rd_init(const char* fname)
{
  infile = gbfopen_le(fname, "rb", MYNAME);
}

static void
rd_deinit(void)
{
  gbfclose(infile);
}

static void
my_read(void)
{
  Read_AN1_Header(infile);
  Read_AN1_Bitmaps(infile);
  Read_AN1_Waypoints(infile);
  Read_AN1_Lines(infile);
}

static void
wr_init(const char* fname)
{
  outfile = gbfopen_le(fname, "wb", MYNAME);
  Init_Output_Type();
  Init_Road_Changes();
  opt_color_num = color_to_bbggrr(opt_color);
  Init_Wpt_Type();
  if (opt_zoom) {
    opt_zoom_num = atoi(opt_zoom);
  }
  radius = .1609344; /* 1/10 mi */
  if (opt_radius) {
    radius = atof(opt_radius);
    if (!strchr(opt_radius,'k') && !strchr(opt_radius,'K')) {
      radius *= 5280*12*2.54/100000;
    }
  }
}

static void
wr_deinit(void)
{
  Free_Road_Changes();
  gbfclose(outfile);
}

static void
my_write(void)
{
  Write_AN1_Header(outfile);
  Write_AN1_Bitmaps(outfile);
  Write_AN1_Waypoints(outfile);
  Write_AN1_Lines(outfile);
}

ff_vecs_t an1_vecs = {
  ff_type_file,
  {
    (ff_cap)(ff_cap_read | ff_cap_write)	/* waypoints */,
    ff_cap_write 			/* tracks */,
    (ff_cap)(ff_cap_read | ff_cap_write) 	/* routes */,
  },
  rd_init,
  wr_init,
  rd_deinit,
  wr_deinit,
  my_read,
  my_write,
  NULL,
  an1_args,
  CET_CHARSET_ASCII, 0	/* CET-REVIEW */
};
