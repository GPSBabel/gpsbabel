/*

    Handle Geogrid-Viewer binary overlay file format (.ovl)

    Copyright (C) 2016 Ralf Horstmann <ralf@ackstorm.de>
    Copyright (C) 2016 Robert Lipe, robertlipe+source@gpsbabel.org

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
#include <QtCore/QtEndian>
#include <QtCore/QByteArray>
#include <QtCore/QDebug>
#include <QtCore/QFile>

#define MYNAME "ggv_bin"

static QString read_fname;

/***************************************************************************
 *           local helper functions                                        *
 ***************************************************************************/

static void
ggv_bin_read_bytes(QFile& file, QByteArray& buf, qint64 size, const char* descr = NULL)
{
  if (size > file.size())
    fatal(MYNAME ": Read size error (%s)\n", descr ? descr : "");
  buf = file.read(size);
  if (buf.size() < size)
    fatal(MYNAME ": Read error (%s)\n", descr ? descr : "");
}

static quint16
ggv_bin_read16(QFile& file, const char* descr = NULL)
{
  QByteArray buf;
  quint16 res;

  ggv_bin_read_bytes(file, buf, 2, descr);
  res = qFromLittleEndian<quint16>((const uchar*)buf.constData());
  if (global_opts.debug_level > 1)
    qDebug("ovl: %-15s %5u (0x%04x)", descr, res, res);
  return res;
}

static quint32
ggv_bin_read32(QFile& file, const char* descr = NULL)
{
  QByteArray buf;
  quint32 res;

  ggv_bin_read_bytes(file, buf, 4, descr);
  res = qFromLittleEndian<quint32>((const uchar*)buf.constData());
  if (global_opts.debug_level > 1) {
    if ((res & 0xFFFF0000) == 0) 
      qDebug("ovl: %-15s %5u (0x%08x)", descr, res, res);
    else
      qDebug("ovl: %-15s       (0x%08x)", descr, res);
  }
  return res;
}

static void
ggv_bin_read_text16(QFile& file, QByteArray& buf, const char* descr = NULL)
{
  quint16 len;
  
  len = ggv_bin_read16(file, descr);
  ggv_bin_read_bytes(file, buf, len, descr);
  buf[len] = 0;
  if (global_opts.debug_level > 1)
    qDebug() << "ovl: text =" << QString::fromLatin1(buf.constData()).simplified();
}

static void
ggv_bin_read_text32(QFile& file, QByteArray& buf, const char* descr = NULL)
{
  quint32 len;
  
  len = ggv_bin_read32(file, descr);
  ggv_bin_read_bytes(file, buf, len, descr);
  buf[len] = 0;
  if (global_opts.debug_level > 1)
    qDebug() << "ovl: text =" << QString::fromLatin1(buf.constData()).simplified();
}

static double
ggv_bin_read_double(QFile& file, const char* descr = NULL)
{
  QByteArray buf;
  quint64 tmp;
  const double *res;

  ggv_bin_read_bytes(file, buf, sizeof(double), descr);
  tmp = qFromLittleEndian<quint64>((const uchar*)buf.constData());
  res = reinterpret_cast<const double*>(&tmp);
  return *res;
}

/***************************************************************************
 *            OVL Version 2.0                                              *
 ***************************************************************************/

static void
ggv_bin_read_v2(QFile& file)
{
  QByteArray buf;
  QString track_name;
  QString waypt_name;
  route_head* ggv_bin_track;
  Waypoint* wpt;
  double lon, lat;
  quint16 header_len;
  quint16 entry_type;
  quint16 entry_subtype;
  quint16 line_points;
  quint64 entry_pos;

  // header length is usually either 0x90 or 0x00
  header_len = ggv_bin_read16(file, "map name len");
  if (header_len > 0) {
    ggv_bin_read_bytes(file, buf, header_len, "map name");
    buf.remove(0,4);
    buf.append('\0');
    if (global_opts.debug_level > 1)
      qDebug() << "ovl: name =" << buf.constData();
  }

  while (!file.atEnd()) {
    track_name.clear();

    if (global_opts.debug_level > 1)
      qDebug("------------------------------------ 0x%llx", file.pos());

    entry_pos = file.pos();
    entry_type = ggv_bin_read16(file, "entry type");
    ggv_bin_read16(file, "entry group");
    ggv_bin_read16(file, "entry zoom");
    entry_subtype = ggv_bin_read16(file, "entry subtype");

    switch (entry_subtype) {
    case 0x01:
      // no data following
      break;
    case 0x10:
      // text with 32 bit length field follows
      ggv_bin_read_text32(file, buf, "text len");
      track_name = QString::fromLatin1(buf.constData()).simplified();
      break;
    default:
      fatal(MYNAME ": Unknown subtype (%hu)\n", entry_subtype);
    }

    switch (entry_type) {
    case 0x02:
      // text
      ggv_bin_read16(file, "text color");
      ggv_bin_read16(file, "text size");
      ggv_bin_read16(file, "text trans");
      ggv_bin_read16(file, "text font");
      ggv_bin_read16(file, "text angle");
      lon = ggv_bin_read_double(file, "text lon");
      lat = ggv_bin_read_double(file, "text lat");
      ggv_bin_read_text16(file, buf, "text label");
      waypt_name = QString::fromLatin1(buf.constData()).simplified();
      wpt = new Waypoint;
      wpt->longitude = lon;
      wpt->latitude = lat;
      wpt->description = waypt_name;
      waypt_add(wpt);
      break;
    case 0x03:
      // line
    case 0x04:
      // area
      ggv_bin_read16(file, "line color");
      ggv_bin_read16(file, "line width");
      ggv_bin_read16(file, "line type");
      line_points = ggv_bin_read16(file, "line points");
      ggv_bin_track = route_head_alloc();
      track_add_head(ggv_bin_track);
      if (! track_name.isEmpty())
        ggv_bin_track->rte_name = track_name;

      for (int i = 1; i <= line_points; i++) {
        lon = ggv_bin_read_double(file, "line lon");
        lat = ggv_bin_read_double(file, "line lat");
        wpt = new Waypoint;
        wpt->longitude = lon;
        wpt->latitude = lat;
        track_add_wpt(ggv_bin_track, wpt);
      }
      break;
    case 0x05:
      // rectangle
    case 0x06:
      // circle
    case 0x07:
      // triangle
      ggv_bin_read16(file, "geom color");
      ggv_bin_read16(file, "geom prop1");
      ggv_bin_read16(file, "geom prop2");
      ggv_bin_read16(file, "geom angle");
      ggv_bin_read16(file, "geom stroke");
      ggv_bin_read16(file, "geom area");
      ggv_bin_read_double(file, "geom lon");
      ggv_bin_read_double(file, "geom lat");
      break;
    case 0x09:
      ggv_bin_read16(file, "bmp color");
      ggv_bin_read16(file, "bmp prop1");
      ggv_bin_read16(file, "bmp prop2");
      ggv_bin_read16(file, "bmp prop3");
      ggv_bin_read_double(file, "bmp lon");
      ggv_bin_read_double(file, "bmp lat");
      ggv_bin_read_text32(file, buf, "bmp data");
      break;
    default:
      fatal(MYNAME ": Unknown entry type (0x%hx, pos=0x%llx) \n", entry_type, entry_pos);
    }
  }
}

/***************************************************************************
 *           OVL Version 3.0 and 4.0                                       *
 ***************************************************************************/

static void
ggv_bin_read_v34_header(QFile& file, quint32& number_labels, quint32 &number_records)
{
  QByteArray buf;
  quint16 header_len;
    
  ggv_bin_read_bytes(file, buf, 8, "unknown");
  number_labels = ggv_bin_read32(file, "num labels");
  number_records = ggv_bin_read32(file, "num records");
  ggv_bin_read_text16(file, buf, "text label");
  ggv_bin_read16(file, "unknown");
  ggv_bin_read16(file, "unknown");
  // 8 bytes ending with 1E 00, contains len of header block
  ggv_bin_read16(file, "unknown");
  header_len = ggv_bin_read16(file, "header len");
  ggv_bin_read16(file, "unknown");
  ggv_bin_read16(file, "unknown");
  if (header_len > 0) {
    ggv_bin_read_bytes(file, buf, header_len, "map name");
    buf.remove(0,4);
    buf.append('\0');
    if (global_opts.debug_level > 1)
      qDebug() << "ovl: name =" << buf.constData();
  }
}

static void
ggv_bin_read_v34_label(QFile& file)
{
  QByteArray buf;

  if (global_opts.debug_level > 1)
    qDebug("------------------------------------ 0x%llx", file.pos());
  ggv_bin_read_bytes(file, buf, 0x08, "label header");
  ggv_bin_read_bytes(file, buf, 0x14, "label number");
  ggv_bin_read_text16(file, buf, "label text");
  ggv_bin_read16(file, "label flag1");
  ggv_bin_read16(file, "label flag2");
}

static QString
ggv_bin_read_v34_common(QFile& file)
{
  QByteArray buf;
  QString res;
  quint16 type1;
  quint16 type2;
  
  ggv_bin_read16(file, "entry group");
  ggv_bin_read16(file, "entry prop2");
  ggv_bin_read16(file, "entry prop3");
  ggv_bin_read16(file, "entry prop4");
  ggv_bin_read16(file, "entry prop5");
  ggv_bin_read16(file, "entry prop6");
  ggv_bin_read16(file, "entry prop7");
  ggv_bin_read16(file, "entry prop8");
  ggv_bin_read16(file, "entry zoom");
  ggv_bin_read16(file, "entry prop10");
  ggv_bin_read_text16(file, buf, "entry txt");
  res = QString::fromLatin1(buf.constData()).simplified();
  type1 = ggv_bin_read16(file, "entry type1");
  if (type1 != 1) {
    ggv_bin_read_text32(file, buf, "entry object");
  }
  type2 = ggv_bin_read16(file, "entry type2");
  if (type2 != 1) {
    ggv_bin_read_text32(file, buf, "entry object");
  }
  return res;
}

static void
ggv_bin_read_v34_record(QFile& file)
{
  QByteArray buf;
  QString label;
  Waypoint *wpt;
  route_head* ggv_bin_track;
  quint16 entry_type;
  quint32 bmp_len;
  quint16 line_points;
  double lon, lat;

  if (global_opts.debug_level > 1)
    qDebug("------------------------------------ 0x%llx", file.pos());

  entry_type = ggv_bin_read16(file, "entry type");
  label = ggv_bin_read_v34_common(file);

  switch (entry_type) {
  case 0x02:
    // text
    ggv_bin_read16(file, "text prop1");
    ggv_bin_read32(file, "text prop2");
    ggv_bin_read16(file, "text prop3");
    ggv_bin_read32(file, "text prop4");
    ggv_bin_read16(file, "text ltype");
    ggv_bin_read16(file, "text angle");
    ggv_bin_read16(file, "text size");
    ggv_bin_read16(file, "text area");
    lon = ggv_bin_read_double(file, "text lon");
    lat = ggv_bin_read_double(file, "text lat");
    ggv_bin_read_double(file, "text unk");
    ggv_bin_read_text16(file, buf, "text label");
    wpt = new Waypoint;
    wpt->longitude = lon;
    wpt->latitude = lat;
    wpt->description = QString::fromLatin1(buf.constData()).simplified();
    waypt_add(wpt);
    break;
  case 0x03:
  case 0x04:
    // area
  case 0x17:
    // line
    ggv_bin_track = route_head_alloc();
    track_add_head(ggv_bin_track);
      
    if (! label.isEmpty()) 
      ggv_bin_track->rte_name = label;

    ggv_bin_read16(file, "line prop1");
    ggv_bin_read32(file, "line prop2");
    ggv_bin_read16(file, "line prop3");
    ggv_bin_read32(file, "line color");
    ggv_bin_read16(file, "line size");
    ggv_bin_read16(file, "line stroke");
    line_points = ggv_bin_read16(file, "line points");
    if (entry_type == 0x04) {
      // found in example.ovl generated by Geogrid-Viewer 1.0
      ggv_bin_read16(file, "line pad");
    }

    for (int i=1; i <= line_points; i++) {
      lon = ggv_bin_read_double(file, "line lon");
      lat = ggv_bin_read_double(file, "line lat");
      ggv_bin_read_double(file, "line unk");
      wpt = new Waypoint;
      wpt->longitude = lon;
      wpt->latitude = lat;
      track_add_wpt(ggv_bin_track, wpt);
    }
    break;
  case 0x05:
  case 0x06:
  case 0x07:
    // circle
    ggv_bin_read16(file, "circle prop1");
    ggv_bin_read32(file, "circle prop2");
    ggv_bin_read16(file, "circle prop3");
    ggv_bin_read32(file, "circle color");
    ggv_bin_read32(file, "circle prop5");
    ggv_bin_read32(file, "circle prop6");
    ggv_bin_read16(file, "circle ltype");
    ggv_bin_read16(file, "circle angle");
    ggv_bin_read16(file, "circle size");
    ggv_bin_read16(file, "circle area");
    ggv_bin_read_double(file, "circle lon");
    ggv_bin_read_double(file, "circle lat");
    ggv_bin_read_double(file, "circle unk");
    break;
  case 0x09:
    // bmp
    ggv_bin_read16(file, "bmp prop1");
    ggv_bin_read32(file, "bmp prop2");
    ggv_bin_read16(file, "bmp prop3");
    ggv_bin_read32(file, "bmp prop4");
    ggv_bin_read32(file, "bmp prop5");
    ggv_bin_read32(file, "bmp prop6");
    ggv_bin_read_double(file, "bmp lon");
    ggv_bin_read_double(file, "bmp lat");
    ggv_bin_read_double(file, "bmp unk");
    bmp_len = ggv_bin_read32(file, "bmp len");
    ggv_bin_read16(file, "bmp prop");
    ggv_bin_read_bytes(file, buf, bmp_len, "bmp data");
    break;
  default:
    fatal(MYNAME ": Unsupported type: %x\n", entry_type);
  }
}

static void
ggv_bin_read_v34(QFile& file)
{
  QByteArray buf;
  QString track_name;
  quint32 label_count;
  quint32 record_count;

  while (!file.atEnd()) {
    ggv_bin_read_v34_header(file, label_count, record_count);

    if (label_count && !file.atEnd()) {
      if (global_opts.debug_level > 1)
        qDebug("-----labels------------------------- 0x%llx", file.pos());
      for (unsigned int i = 0; i < label_count; i++)
        ggv_bin_read_v34_label(file);
    }

    if (record_count && !file.atEnd()) {
      if (global_opts.debug_level > 1)
        qDebug("-----records------------------------ 0x%llx", file.pos());
      for (unsigned int i = 0; i < record_count; i++)
        ggv_bin_read_v34_record(file);
    }

    if (!file.atEnd()) {
      if (global_opts.debug_level > 1)
        qDebug("------------------------------------ 0x%llx", file.pos());
      // we just skip over the next magic bytes without checking they
      // contain the correct string. This is consistent with what I
      // believe GGV does
      ggv_bin_read_bytes(file, buf, 23, "magicbytes");
      if (global_opts.debug_level > 1)
        qDebug() << "ovl: header = " << buf.constData();
    }
  }
    
  if (global_opts.debug_level > 1) {
    qDebug("fpos: 0x%llx", file.pos());
    qDebug("size: 0x%llx", file.size());
  }
}

/***************************************************************************
 *           global callbacks called by gpsbabel main process              *
 ***************************************************************************/

static void
ggv_bin_read_file(QFile& file)
{
  QByteArray buf;

  ggv_bin_read_bytes(file, buf, 0x17, "magic");
  buf[23] = 0;
  if (global_opts.debug_level > 1) {
    qDebug() << "ovl: header =" << buf.constData();
  }

  if (buf.startsWith("DOMGVCRD Ovlfile V2.0")) {
    ggv_bin_read_v2(file);
  } else if (buf.startsWith("DOMGVCRD Ovlfile V3.0")) {
    ggv_bin_read_v34(file);
  } else if (buf.startsWith("DOMGVCRD Ovlfile V4.0")) {
    ggv_bin_read_v34(file);
  } else {
    fatal(MYNAME ": Unsupported file format\n");
  }
}

static void
ggv_bin_read_init(const char* fname)
{
  read_fname = QString::fromUtf8(fname);
}

static void
ggv_bin_read_deinit(void)
{
}

static void
ggv_bin_read(void)
{
  QFile file(read_fname);

  if (!file.open(QIODevice::ReadOnly)) {
    fatal(MYNAME ": Error opening file %s\n", read_fname.toStdString().c_str());
  }

  ggv_bin_read_file(file);
  file.close();
}

ff_vecs_t ggv_bin_vecs = {
  ff_type_file,
  {
    ff_cap_none,  // waypoints
    ff_cap_read,  // tracks
    ff_cap_none   // routes
  },
  ggv_bin_read_init,    // rd_init
  NULL,                 // wr_init
  ggv_bin_read_deinit,  // rd_deinit
  NULL,                 // wr_deinit
  ggv_bin_read,         // read
  NULL,                 // write
  NULL,                 // exit
  NULL,                 //args
  CET_CHARSET_ASCII, 0  //encode,fixed_encode
  //NULL                //name dynamic/internal?
};
