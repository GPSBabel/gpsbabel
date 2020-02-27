/*
    Read DeLorme drawing files (.an1)

    Copyright (C) 2005-2014 Ron Parker and Robert Lipe.

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

#include <cstdio>               // for sprintf, SEEK_CUR
#include <cstdint>              // for int32_t
#include <cstdlib>              // for atoi, atof
#include <cstring>              // for strlen, strchr, memcpy, strstr

#include <QtCore/QString>       // for QString
#include <QtCore/QVector>       // for QVector

#include "defs.h"
#include "formspec.h"           // for FsChainAdd, FsChainFind, FormatSpecificData, kFsAn1L, kFsAn1V, kFsAn1W
#include "gbfile.h"             // for gbfgetint32, gbfputint32, gbfputint16, gbfgetint16, gbfile, gbfputs, gbfgetc, gbfputc, gbfclose, gbfopen_le, gbfgetdbl, gbfputdbl, gbfread, gbfseek
#include "src/core/datetime.h"  // for DateTime


#define MYNAME "an1"

static gbfile* infile;
static gbfile* outfile;

static char* output_type = nullptr;
static char* road_changes = nullptr;
static char* nogc = nullptr;
static char* nourl = nullptr;
static char* opt_symbol = nullptr;
static char* opt_color = nullptr;
static char* opt_zoom  = nullptr;
static char* opt_wpt_type = nullptr;
static char* opt_radius = nullptr;

static short output_type_num = 0;
static short opt_zoom_num = 0;
static long opt_color_num = 0;
static short wpt_type_num = 0;
static short last_read_type = 0;
static double radius = 0.0;

static long serial=10000;
static long rtserial=1;

struct roadchange {
  long type;
  char* name;
};

static roadchange* roadchanges = nullptr;

static
QVector<arglist_t> an1_args = {
  {
    "type", &output_type, "Type of .an1 file",
    "", ARGTYPE_STRING, ARG_NOMINMAX, nullptr
  },
  {
    "road", &road_changes, "Road type changes",
    "", ARGTYPE_STRING, ARG_NOMINMAX, nullptr
  },
  {
    "nogc", &nogc, "Do not add geocache data to description",
    nullptr, ARGTYPE_BOOL, ARG_NOMINMAX, nullptr
  },
  {
    "nourl", &nourl, "Do not add URLs to description",
    nullptr, ARGTYPE_BOOL, ARG_NOMINMAX, nullptr
  },
  {
    "deficon", &opt_symbol, "Symbol to use for point data",
    "Red Flag", ARGTYPE_STRING, ARG_NOMINMAX, nullptr
  },
  {
    "color", &opt_color, "Color for lines or mapnotes",
    "red", ARGTYPE_STRING, ARG_NOMINMAX, nullptr
  },
  {
    "zoom", &opt_zoom, "Zoom level to reduce points",
    nullptr, ARGTYPE_INT, ARG_NOMINMAX, nullptr
  },
  {
    "wpt_type", &opt_wpt_type,
    "Waypoint type",
    "", ARGTYPE_STRING, ARG_NOMINMAX, nullptr
  },
  {
    "radius", &opt_radius, "Radius for circles",
    nullptr, ARGTYPE_STRING, ARG_NOMINMAX, nullptr
  },
};

struct guid_t {
  unsigned long l;
  unsigned short s[3];
  unsigned char c[6];
};

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
  auto* result = (char*)xcalloc(1, len + 1);
  if (len) {
    gbfread(result, 1, len, f);
  }
  return result;
}

#define ReadChar(f) (unsigned char) gbfgetc(f)
#define WriteChar(f,c) gbfputc((unsigned char)(c),f)
#define WriteString(f,s) gbfputs((s),f)

static void
ReadGuid(gbfile* f, guid_t* guid)
{
  guid->l = ReadLong(f);
  for (unsigned short& i : guid->s) {
    i = ReadShort(f);
  }
  for (unsigned char& i : guid->c) {
    i = ReadChar(f);
  }
}

static void
WriteGuid(gbfile* f, guid_t* guid)
{
  WriteLong(f, guid->l);
  for (int i = 0; i < 3; i++) {
    WriteShort(f, guid->s[i]);
  }
  for (int i = 0; i < 6; i++) {
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
  return (double)((int32_t)(0x80000000 - ord)) / 0x800000;
}

static long
EncodeOrd(double ord)
{
  return (int32_t)(0x80000000 - (int32_t)(ord * 0x800000));
}

struct an1_symbol_record {
  short hotspotxhi{0};
  long hotspoty{0};
  long unk1{0};
  guid_t guid{};
  char* name{nullptr};
};

struct an1_waypoint_record : FormatSpecificData {
  an1_waypoint_record() : FormatSpecificData(kFsAn1W) {}
private:
  an1_waypoint_record(const an1_waypoint_record&) = default;
public:
  an1_waypoint_record& operator=(const an1_waypoint_record&) = delete;
  an1_waypoint_record(an1_waypoint_record&&) = delete;
  an1_waypoint_record& operator=(an1_waypoint_record&&) = delete;
  ~an1_waypoint_record() override
  {
    xfree(name);
    xfree(fontname);
    xfree(url);
    xfree(comment);
    xfree(image_name);
  }

  an1_waypoint_record* clone() const override
  {
    auto* copy = new an1_waypoint_record(*this);
    copy->name = xstrdup(name);
    copy->fontname = xstrdup(fontname);
    copy->url = xstrdup(url);
    copy->comment = xstrdup(comment);
    copy->image_name = xstrdup(image_name);
    return copy;
  }

  short magic{0};
  long unk1{0};
  long lon{0};
  long lat{0};
  short type{0};
  long height{0};
  long width{0};
  short unk2{0};
  short unk3{0};
  short serial{0};
  short unk4{0};
  unsigned char create_zoom{0};
  unsigned char visible_zoom{0};
  short unk5{0};
  double radius{0.0}; /* in km */
  char* name{nullptr};
  char* fontname{nullptr};
  guid_t guid{};
  long fontcolor{0};
  long fontstyle{0};
  long fontsize{0};
  long outlineweight{0};
  long outlinecolor{0};
  long outlineflags{0};
  long fillcolor{0};
  long unk6{0};
  long fillflags{0};

  /* Added in SA2006/Topo 6.0 */
  short unk6_1{0};
  char* url{nullptr};
  char* comment{nullptr};
  long creation_time{0};
  long modification_time{0};
  char* image_name{nullptr};
};

struct an1_vertex_record : FormatSpecificData {
  an1_vertex_record() : FormatSpecificData(kFsAn1V) {}
  an1_vertex_record* clone() const override
  {
    return new an1_vertex_record(*this);
  }

  short magic{0};
  long unk0{0};
  long lon{0};
  long lat{0};
  short unk1{0};
};

struct an1_line_record : FormatSpecificData {
  an1_line_record() : FormatSpecificData(kFsAn1L) {}
private:
  an1_line_record(const an1_line_record&) = default;
public:
  an1_line_record& operator=(const an1_line_record&) = delete;
  an1_line_record(an1_line_record&&) = delete;
  an1_line_record& operator=(an1_line_record&&) = delete;
  ~an1_line_record() override
  {
    xfree(name);
  }

  an1_line_record* clone() const override
  {
    auto* copy = new an1_line_record(*this);
    copy->name = xstrdup(name);
    return copy;
  }

  long roadtype{0};
  short serial{0};
  long unk2{0};
  short unk3{0};
  short type{0};
  long unk4{0};
  char* name{nullptr};
  long lineweight{0};
  long linestyle{0};
  long linecolor{0};
  long opacity{0};
  long polyfillcolor{0};
  long unk6{0};
  long unk7{0};
  short unk8{0};
  long pointcount{0};
};

static void Destroy_AN1_Symbol(an1_symbol_record* symbol)
{
  xfree(symbol->name);
}

static void Read_AN1_Waypoint(gbfile* f, an1_waypoint_record* wpt)
{
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
  unsigned short len = ReadShort(f);
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
      /*
       * Trust URL encoded in new format over one in
       * old format if both are present.  Whack the
       * name starting at '{URL='.
       */
      char* oldurlstr = strstr(wpt->name, "{URL=");
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
    wpt->image_name = nullptr;
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

  short len = strlen(wpt->name) + 1 + 2 + 2 +
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
  line->roadtype = ReadLong(f);
  line->serial = ReadShort(f);
  line->unk2 = ReadLong(f);
  line->unk3 = ReadShort(f);
  line->type = ReadShort(f);
  line->unk4 = ReadLong(f);
  short len = ReadShort(f);
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
  WriteLong(f, line->roadtype);
  WriteShort(f, line->serial);
  WriteLong(f, line->unk2);
  WriteShort(f, line->unk3);
  WriteShort(f, line->type);
  WriteLong(f, line->unk4);
  short len = strlen(line->name);
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
  Skip(f, 8);    /* BITMAPFILEHEADER fields 1-3 */
  unsigned long bitoffset = ReadLong(f);

  unsigned long bmisize = ReadLong(f);
  Skip(f, 16);    /* BITMAPINFOHEADER fields 2-6 */
  unsigned long bmsize = ReadLong(f);
  Skip(f, 16);    /* BITMAPINFOHEADER fields 8-11 */

  unsigned long palettesize = bitoffset - bmisize - 14;
  Skip(f, bmsize + palettesize);
}

static void Read_AN1_Symbol(gbfile* f, an1_symbol_record* symbol)
{
  /* This is just the high word of a long; we ate the low
   * word in the caller.  Fortunately, we don't care. */
  symbol->hotspotxhi = ReadShort(f);
  symbol->hotspoty = ReadLong(f);
  symbol->unk1 = ReadLong(f);
  ReadGuid(f, &symbol->guid);
  short len = ReadChar(f);
  symbol->name = ReadString(f, len);
}

static void Read_AN1_Header(gbfile* f)
{
  unsigned short magic = ReadShort(f);
  (void) magic; // hush warning.
  unsigned short type = ReadShort(f);

  last_read_type = type;
}

static void Write_AN1_Header(gbfile* f)
{
  WriteShort(f, 11557);
  WriteShort(f, output_type_num);
}

static void Read_AN1_Bitmaps(gbfile* f)
{
  an1_symbol_record symbol;

  long count = ReadLong(f);

  while (count) {
    unsigned short magic = ReadShort(f);
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
  const char* icon = nullptr;
  ReadShort(f);
  unsigned long count = ReadLong(f);
  for (unsigned long i = 0; i < count; i++) {
    auto* rec = new an1_waypoint_record;
    Read_AN1_Waypoint(f, rec);
    auto* wpt_tmp = new Waypoint;

    if (rec->creation_time) {
      wpt_tmp->SetCreationTime(rec->creation_time);
    }
    wpt_tmp->longitude = -DecodeOrd(rec->lon);
    wpt_tmp->latitude = DecodeOrd(rec->lat);
    wpt_tmp->notes = rec->comment;
    wpt_tmp->description = rec->name;

    if (rec->url) {
      wpt_tmp->AddUrlLink(rec->url);
    } else {
      int u = wpt_tmp->description.indexOf("{URL=");
      if (u != -1) {
        QString us = wpt_tmp->description.mid(u);
        us.remove(0,5); // throw away anything up to and including "{URL="
        us.chop(1); // throw away final character, assumed to be "}"
        if (!us.isEmpty()) {
          wpt_tmp->AddUrlLink(us);
        }
      }
    }

    if (rec->image_name) {
      wpt_tmp->icon_descr = rec->image_name;
    } else if (FindIconByGuid(&rec->guid, &icon)) {
      wpt_tmp->icon_descr = icon;
    }

    wpt_tmp->fs.FsChainAdd(rec);
    rec = nullptr;
    waypt_add(wpt_tmp);
  }
}

static void
Write_One_AN1_Waypoint(const Waypoint* wpt)
{
  an1_waypoint_record* rec;

  const auto* source_rec = reinterpret_cast<an1_waypoint_record*>(wpt->fs.FsChainFind(kFsAn1W));

  if (source_rec != nullptr) {
    rec = source_rec->clone();
    if (opt_zoom) {
      rec->visible_zoom = opt_zoom_num;
    }
  } else {
    rec = new an1_waypoint_record;
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
  xfree(rec->name);
  rec->name = xstrdup(wpt->description);

  if (!nogc && wpt->gc_data->id) {
    // FIXME: this whole mess should be qstring concatenation
    auto* extra = (char*) xmalloc(25 + wpt->gc_data->placer.length() + wpt->shortname.length());
    sprintf(extra, "\r\nBy %s\r\n%s (%1.1f/%1.1f)",
            CSTR(wpt->gc_data->placer),
            CSTRc(wpt->shortname), wpt->gc_data->diff/10.0,
            wpt->gc_data->terr/10.0);
    rec->name = xstrappend(rec->name, extra);
    xfree(extra);
  }

  if (!nourl && wpt->HasUrlLink()) {
    UrlLink l = wpt->GetUrlLink();
    int len = 7 + l.url_.length();
    auto* extra = (char*)xmalloc(len);
    sprintf(extra, "{URL=%s}", CSTR(l.url_));
    rec->name = xstrappend(rec->name, extra);
    xfree(extra);
    if (rec->url) {
      xfree(rec->url);
    }
    rec->url = xstrdup(l.url_);
  }
  if (!wpt->notes.isEmpty()) {
    if (rec->comment) {
      xfree(rec->comment);
    }
    rec->comment = xstrdup(wpt->notes);
  }


  rec->creation_time = rec->modification_time = wpt->GetCreationTime().toTime_t();
  rec->lat = EncodeOrd(wpt->latitude);
  rec->lon = EncodeOrd(-wpt->longitude);
  rec->serial = serial++;

  if (rec->type == 0x12) {    /* image */
    if (wpt->icon_descr.contains(":\\")) {
      rec->image_name = xstrdup(wpt->icon_descr);
      rec->height = -244;
      rec->width = -1;
    }
  }
  if (!rec->image_name && !wpt->icon_descr.isNull()) {
    FindIconByName(CSTR(wpt->icon_descr), &rec->guid);
  }

  Write_AN1_Waypoint(outfile, rec);
  delete rec;
}

static void Write_AN1_Waypoints(gbfile* f)
{
  WriteShort(f, 2);
  WriteLong(f, waypt_count());
  waypt_disp_all(Write_One_AN1_Waypoint);
}

static void Read_AN1_Lines(gbfile* f)
{
  ReadShort(f);
  unsigned long count = ReadLong(f);
  for (unsigned long i = 0; i < count; i++) {
    auto* rec = new an1_line_record;
    Read_AN1_Line(f, rec);
    /* create route rec */
    auto* rte_head = new route_head;
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
    rte_head->rte_name = rec->name;
    rte_head->fs.FsChainAdd(rec);
    route_add_head(rte_head);
    for (unsigned long j = 0; j < (unsigned) rec->pointcount; j++) {
      auto* vert = new an1_vertex_record;
      Read_AN1_Vertex(f, vert);

      /* create route point */
      auto* wpt_tmp = new Waypoint;
      wpt_tmp->latitude = DecodeOrd(vert->lat);
      wpt_tmp->longitude = -DecodeOrd(vert->lon);
      wpt_tmp->shortname = QString::asprintf("\\%5.5lx", rtserial++);
      wpt_tmp->fs.FsChainAdd(vert);
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

  const auto* source_rec = reinterpret_cast<an1_line_record*>(rte->fs.FsChainFind(kFsAn1L));

  if (source_rec != nullptr) {
    rec = source_rec->clone();
    switch (output_type_num) {
    case 1:
      if (rec->type != 14) {
        rec->roadtype = 0x11100541;
        rec->unk2 = 655360;
        rec->type = 14;
        rec->unk8 = 2;
      } // end if
      Make_Road_Changes(rec);
      break;
    case 2:
      if (rec->type != 15) {
        rec->type = 15;
      } // end if
      break;
    case 4:
      if (rec->type != 16) {
        rec->type = 16;
      } // end if
      break;
    }
  } else {
    rec = new an1_line_record;
    rec->name = nullptr;
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
  delete rec;
}

static void
Write_One_AN1_Vertex(const Waypoint* wpt)
{
  an1_vertex_record* rec;

  const auto* source_rec = reinterpret_cast<an1_vertex_record*>(wpt->fs.FsChainFind(kFsAn1V));

  if (source_rec != nullptr) {
    rec = source_rec->clone();
  } else {
    rec = new an1_vertex_record;
    rec->magic = 1;
  }
  rec->lat = EncodeOrd(wpt->latitude);
  rec->lon = EncodeOrd(-wpt->longitude);

  Write_AN1_Vertex(outfile, rec);
  delete rec;
}

static void Write_AN1_Lines(gbfile* f)
{
  WriteShort(f, 2);
  WriteLong(f, route_count()+track_count());

  route_disp_all(Write_One_AN1_Line, nullptr, Write_One_AN1_Vertex);
  track_disp_all(Write_One_AN1_Line, nullptr, Write_One_AN1_Vertex);
}

static void
Init_Wpt_Type()
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
Init_Output_Type()
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
Free_Road_Changes()
{
  int i = 0;
  if (roadchanges) {
    while (roadchanges[i].name) {
      xfree(roadchanges[i].name);
      i++;
    }
    xfree(roadchanges);
  }
  roadchanges = nullptr;
}

static void
Init_Road_Changes()
{
  int count = 0;
  Free_Road_Changes();

  if (!road_changes || !road_changes[0]) {
    return;
  }
  char* bar = strchr(road_changes, '!');
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
  roadchanges[count].name = nullptr;

  char* copy = xstrdup(road_changes);
  bar = copy;

  while (count) {
    count--;
    char* name = bar;
    bar = strchr(name, '!');
    *bar = '\0';
    bar++;
    char* strType = bar;
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
rd_init(const QString& fname)
{
  infile = gbfopen_le(fname, "rb", MYNAME);
}

static void
rd_deinit()
{
  gbfclose(infile);
}

static void
my_read()
{
  Read_AN1_Header(infile);
  Read_AN1_Bitmaps(infile);
  Read_AN1_Waypoints(infile);
  Read_AN1_Lines(infile);
}

static void
wr_init(const QString& fname)
{
  outfile = gbfopen_le(fname, "wb", MYNAME);
  Init_Output_Type();
  Init_Road_Changes();
  opt_color_num = color_to_bbggrr(opt_color);
  Init_Wpt_Type();
  if (opt_zoom) {
    opt_zoom_num = atoi(opt_zoom);
  }
  radius = .1609344; /* 1/10 mi in kilometers */
  if (opt_radius) {
    radius = atof(opt_radius);
    if (!strchr(opt_radius,'k') && !strchr(opt_radius,'K')) {
      radius *= kKilometersPerMile;
    }
  }
}

static void
wr_deinit()
{
  Free_Road_Changes();
  gbfclose(outfile);
}

static void
my_write()
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
  nullptr,
  &an1_args,
  CET_CHARSET_ASCII, 0	/* CET-REVIEW */
  , NULL_POS_OPS,
  nullptr
};
