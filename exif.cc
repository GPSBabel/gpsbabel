/*

    Support for embedded (JPEG) Exif-GPS information.

    Copyright (C) 2008 Olaf Klein, o.b.klein@gpsbabel.org

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

/*
 * Exif specifications can be found at
 * 2016, version 2.31: http://www.cipa.jp/std/documents/e/DC-008-Translation-2016-E.pdf
 * 2012, version 2.3: http://www.cipa.jp/std/documents/e/DC-008-2012_E.pdf
 * 2010, version 2.3: http://www.cipa.jp/std/documents/e/DC-008-2010_E.pdf
 * 2002, version 2.2: http://www.exif.org/Exif2-2.PDF
 * 1998, version 2.1: http://www.exif.org/Exif2-1.PDF
 *
 * TIFF specifications can be found at
 * version 6.0: https://www.itu.int/itudoc/itu-t/com16/tiff-fx/docs/tiff6.pdf
 * version 6.0: http://www.npes.org/pdf/TIFF-v6.pdf
 * version 6.0: http://www.alternatiff.com/resources/TIFF6.pdf
 */

#include "exif.h"

#include <QByteArray>           // for QByteArray
#include <QDate>                // for QDate
#include <QDateTime>            // for QDateTime
#include <QFile>                // for QFile
#include <QFileInfo>            // for QFileInfo
#include <QList>                // for QList
#include <QPair>                // for QPair
#include <QRegularExpression>   // for QRegularExpressionMatch, QRegularExpression
#include <QString>              // for QString
#include <QTextCodec>           // for QTextCodec
#include <QTime>                // for QTime
#include <QVariant>             // for QVariant
#include <QVector>              // for QVector
#include <Qt>                   // for UTC, ISODate
#include <QtGlobal>             // for qAsConst, qPrintable, qint64

#include <algorithm>            // for sort, min
#include <cassert>              // for assert
#include <cctype>               // for isprint, isspace
#include <cfloat>               // for DBL_EPSILON
#include <cmath>                // for fabs, modf, copysign, round, fmax
#include <cstdint>              // for uint32_t, int32_t, uint16_t, int16_t, uint8_t, INT32_MAX
#include <cstdio>               // for printf, SEEK_SET, snprintf, SEEK_CUR
#include <cstdlib>              // for labs
#include <cstring>              // for memcmp, strlen
#include <type_traits>          // for add_const<>::type

#include "defs.h"               // for Waypoint, fatal, warning, global_options, global_opts, unknown_alt, xfree, route_disp_all, track_disp_all, waypt_disp_all, wp_flags, KNOTS_TO_MPS, KPH_TO_MPS, MPH_TO_MPS, MPS_TO_KPH, WAYPT_HAS, case_ignore_strcmp, rtrim, waypt_add, xstrdup, xstrndup, fix_2d
#include "garmin_tables.h"      // for gt_lookup_datum_index
#include "gbfile.h"             // for gbfputuint32, gbfputuint16, gbfgetuint16, gbfgetuint32, gbfseek, gbftell, gbfile, gbfclose, gbfcopyfrom, gbfwrite, gbfopen_be, gbfread, gbfrewind, gbfgetflt, gbfgetint16, gbfopen, gbfputc, gbfputflt, gbsize_t, gbfeof, gbfgetdbl, gbfputdbl, gbfile::(anonymous)
#include "jeeps/gpsmath.h"      // for GPS_Math_WGS84_To_Known_Datum_M
#include "src/core/datetime.h"  // for DateTime


#define MYNAME "exif"

#define IFD0    0
#define IFD1    1
#define EXIF_IFD  2   /* dummy index */
#define GPS_IFD   3   /* dummy index */
#define INTER_IFD 4   /* dummy index */

#define EXIF_TYPE_BYTE    1
#define EXIF_TYPE_ASCII   2
#define EXIF_TYPE_SHORT   3
#define EXIF_TYPE_LONG    4
#define EXIF_TYPE_RAT     5
#define EXIF_TYPE_SBYTE   6   /* TIFF 6.0 */
#define EXIF_TYPE_UNK     7   /* TIFF 6.0 */
#define EXIF_TYPE_SSHORT  8   /* TIFF 6.0 */
#define EXIF_TYPE_SLONG   9   /* TIFF 6.0 */
#define EXIF_TYPE_SRAT    10  /* TIFF 6.0 */
#define EXIF_TYPE_FLOAT   11  /* TIFF 6.0 */
#define EXIF_TYPE_DOUBLE  12  /* TIFF 6.0 */
#define EXIF_TYPE_IFD     13
#define EXIF_TYPE_UNICODE 14
#define EXIF_TYPE_COMPLEX 15
#define EXIF_TYPE_LONG8   16  /* BigTIFF */
#define EXIF_TYPE_SLONG8  17  /* BigTIFF */
#define EXIF_TYPE_IFD8    18  /* BigTIFF */

#define BYTE_TYPE(a) ( (a==EXIF_TYPE_BYTE) || (a==EXIF_TYPE_ASCII) || (a==EXIF_TYPE_SBYTE) || (a==EXIF_TYPE_UNK) )
#define WORD_TYPE(a) ( (a==EXIF_TYPE_SHORT) || (a==EXIF_TYPE_SSHORT) )
#define LONG_TYPE(a) ( (a==EXIF_TYPE_LONG) || (a==EXIF_TYPE_SLONG) || (a==EXIF_TYPE_IFD) )

#define IFD0_TAG_EXIF_IFD_OFFS  0x8769
#define IFD0_TAG_GPS_IFD_OFFS   0x8825

#define IFD1_TAG_COMPRESSION       0x0103  // Compression, 1 => uncompressed, 6 => JPEG compression
#define IFD1_TAG_STRIP_OFFS        0x0111  // StripOffsets
#define IFD1_TAG_STRIP_BYTE_COUNTS 0x0117  // StripByteCounts
#define IFD1_TAG_JPEG_OFFS         0x0201  // JPEGInterchangeFormat
#define IFD1_TAG_JPEG_SIZE         0x0202  // JPEGInterchangeFormatLength

#define EXIF_IFD_TAG_USER_CMT       0x9286
#define EXIF_IFD_TAG_INTER_IFD_OFFS 0xA005

#define GPS_IFD_TAG_VERSION   0x0000
#define GPS_IFD_TAG_LATREF    0x0001
#define GPS_IFD_TAG_LAT       0x0002
#define GPS_IFD_TAG_LONREF    0x0003
#define GPS_IFD_TAG_LON       0x0004
#define GPS_IFD_TAG_ALTREF    0x0005
#define GPS_IFD_TAG_ALT       0x0006
#define GPS_IFD_TAG_TIMESTAMP 0x0007
#define GPS_IFD_TAG_SAT       0x0008
#define GPS_IFD_TAG_MODE      0x000A
#define GPS_IFD_TAG_DOP       0x000B
#define GPS_IFD_TAG_SPEEDREF  0x000C
#define GPS_IFD_TAG_SPEED     0x000D
#define GPS_IFD_TAG_DATUM     0x0012
#define GPS_IFD_TAG_DATESTAMP 0x001D

// for debug only
void
ExifFormat::print_buff(const char* buf, int sz, const char* cmt)
{
  int i;

  printf("%s: ", cmt);
  for (i = 0; i < sz; i++) {
    printf("%02x ", buf[i] & 0xFF);
  }
  for (i = 0; i < sz; i++) {
    char c = buf[i];
    if (isspace(c)) {
      c = ' ';
    } else if (! isprint(c)) {
      c = '.';
    }
    printf("%c", c);
  }
}

uint16_t
ExifFormat::exif_type_size(const uint16_t type)
{
  uint16_t size;

  switch (type) {
  case EXIF_TYPE_BYTE:
  case EXIF_TYPE_ASCII:
  case EXIF_TYPE_SBYTE:
  case EXIF_TYPE_UNK:
    size = 1;
    break;

  case EXIF_TYPE_SHORT:
  case EXIF_TYPE_SSHORT:
  case EXIF_TYPE_UNICODE:
    size = 2;
    break;
  case EXIF_TYPE_IFD:
  case EXIF_TYPE_LONG:
  case EXIF_TYPE_SLONG:
  case EXIF_TYPE_FLOAT:
    size = 4;
    break;

  case EXIF_TYPE_RAT:
  case EXIF_TYPE_SRAT:
  case EXIF_TYPE_DOUBLE:
  case EXIF_TYPE_COMPLEX:
  case EXIF_TYPE_LONG8:
  case EXIF_TYPE_SLONG8:
  case EXIF_TYPE_IFD8:
    size = 8;
    break;

  default:
    fatal(MYNAME ": Unknown data type %d! Please report.\n", type);
  }
  return size;
}

QString
ExifFormat::exif_time_str(const QDateTime& time)
{
  QString str = time.toString(u"yyyy/MM/dd hh:mm:ss t");
  if (time.timeSpec() != Qt::UTC) {
    str.append(" (");
    str.append(time.toUTC().toString(u"yyyy/MM/dd hh:mm:ss t"));
    str.append(")");
  }
  return str;
}

char*
ExifFormat::exif_read_str(ExifTag* tag)
{
  // Panasonic DMC-TZ10 stores datum with trailing spaces.
  // Kodak stores zero count ASCII tags.
  char* buf = (tag->count == 0) ? xstrdup("") : xstrndup(tag->data.at(0).toByteArray().constData(), tag->size);
  rtrim(buf);
  return buf;
}

double
ExifFormat::exif_read_double(const ExifTag* tag, const int index)
{
  if (tag->type == EXIF_TYPE_RAT) {
    auto num = tag->data.at(index * 2).value<uint32_t>();
    auto den = tag->data.at((index * 2) + 1).value<uint32_t>();
    return (double)num / (double)den;
  } else { // EXIF_TYPE_SRAT
    auto num = tag->data.at(index * 2).value<int32_t>();
    auto den = tag->data.at((index * 2) + 1).value<int32_t>();
    return (double)num / (double)den;
  }
}

double
ExifFormat::exif_read_coord(const ExifTag* tag)
{
  double res = exif_read_double(tag, 0);
  if (tag->count == 1) {
    return res;
  }

  double min = exif_read_double(tag, 1);
  res += (min / 60);
  if (tag->count == 2) {
    return res;
  }

  double sec = exif_read_double(tag, 2);
  res += (sec / 3600);

  return res;
}

QTime
ExifFormat::exif_read_timestamp(const ExifTag* tag)
{
  double hour = exif_read_double(tag, 0);
  double min = exif_read_double(tag, 1);
  double sec = exif_read_double(tag, 2);

  return QTime(0, 0).addMSecs(lround((((hour * 60.0) + min) * 60.0 + sec) * 1000.0));
}

QDate
ExifFormat::exif_read_datestamp(const ExifTag* tag)
{
  return QDate::fromString(tag->data.at(0).toByteArray().constData(), "yyyy:MM:dd");
}

void
ExifFormat::exif_release_apps()
{
  for (auto* app : qAsConst(*exif_apps)) {
    if (app->fcache) {
      gbfclose(app->fcache);
    }
    if (app->fexif) {
      gbfclose(app->fexif);
    }
    delete app;
  }
  delete exif_apps;
  exif_apps = nullptr;
}

uint32_t
ExifFormat::exif_ifd_size(ExifIfd* ifd)
{
  uint32_t res = 6;   /* nr of tags + next_ifd */

  res += (ifd->count * 12);
  for (auto& tag_instance : ifd->tags) {
    ExifTag* tag = &tag_instance;
    if (tag->size > 4) {
      uint32_t size = tag->size;
      if (size & 1u) {
        size++;
      }
      res += size;
    }
  }

  return res;
}

ExifFormat::ExifApp*
ExifFormat::exif_load_apps()
{
  exif_app_ = nullptr;

  while (! gbfeof(fin_)) {
    exif_apps->append(new ExifApp);
    ExifApp* app = exif_apps->last();
    app->fcache = gbfopen(nullptr, "wb", MYNAME);

    app->marker = gbfgetuint16(fin_);
    app->len = gbfgetuint16(fin_);
    if (global_opts.debug_level >= 3) {
      printf(MYNAME ": api = %02X, len = %u (0x%04x), offs = 0x%08X\n", app->marker & 0xFF, app->len, app->len, gbftell(fin_));
    }
    if (exif_app_ || (app->marker == 0xFFDA)) { /* compressed data */
      gbfcopyfrom(app->fcache, fin_, 0x7FFFFFFF);
      if (global_opts.debug_level >= 3) {
        printf(MYNAME ": compressed data size = %u\n", gbftell(app->fcache));
      }
    } else {
      gbfcopyfrom(app->fcache, fin_, app->len - 2);
      if (app->marker == 0xFFE1) {
        exif_app_ = app;
      }
    }
  }

  return exif_app_;
}

#ifndef NDEBUG
void
ExifFormat::exif_validate_tag_structure(const ExifTag* tag)
{
  // The count times the element size should match the saved size.
  assert((tag->count * exif_type_size(tag->type)) == tag->size);

  // for BYTE_TYPE we store a QByteArray as the only component of the QVector,
  // and the count should match the size of the byte array.
  assert((!BYTE_TYPE(tag->type)) ||
         ((tag->data.size() == 0) && (tag->count == 0)) ||
         ((tag->data.size() == 1) && (static_cast<unsigned>(tag->data.at(0).toByteArray().size()) == tag->count)));

  // EXIF_TYPE_RAT and EXIF_TYPE_SRAT are stored as two values per item.
  assert(((tag->type != EXIF_TYPE_RAT) && (tag->type != EXIF_TYPE_SRAT)) ||
         (static_cast<unsigned>(tag->data.size()) == (2 * tag->count)));

  // types other that BYTE_TYPE, RAT, SRAT are stored as one value per item.
  assert(BYTE_TYPE(tag->type) || (tag->type == EXIF_TYPE_RAT) || (tag->type == EXIF_TYPE_SRAT) ||
         (static_cast<unsigned>(tag->data.size()) == tag->count));

  // For EXIF_TYPE_ASCII the last byte of the value must be NUL (binary 0).
  assert((tag->type != EXIF_TYPE_ASCII) ||
         ((tag->data.size() == 0) && (tag->count == 0)) ||
         ((tag->data.size() == 1) && (tag->data.at(0).toByteArray().endsWith('\0'))));
}
#endif

ExifFormat::ExifIfd*
ExifFormat::exif_read_ifd(ExifApp* app, const uint16_t ifd_nr, const gbsize_t offs,
              uint32_t* exif_ifd_ofs, uint32_t* gps_ifd_ofs, uint32_t* inter_ifd_ofs)
{
  gbfile* fin = app->fexif;

  app->ifds.append(ExifIfd());
  ExifIfd* ifd = &app->ifds.last();
  ifd->nr = ifd_nr;

  gbfseek(fin, offs, SEEK_SET);
  ifd->count = gbfgetuint16(fin);

  if (global_opts.debug_level >= 3) {
    const char* name;
    switch (ifd_nr) {
    case IFD0:
      name = "IFD0";
      break;
    case IFD1:
      name = "IFD1";
      break;
    case GPS_IFD:
      name = "GPS";
      break;
    case EXIF_IFD:
      name = "EXIF";
      break;
    case INTER_IFD:
      name = "INTER";
      break;
    default:
      name = "private";
      break;
    }
    printf(MYNAME "-offs 0x%08X: Number of items in IFD%d \"%s\" = %d (0x%04x)\n",
           offs, ifd_nr, name, ifd->count, ifd->count);
  }
  if (ifd->count == 0) {
    return ifd;
  }

  for (uint16_t i = 0; i < ifd->count; i++) {
    ifd->tags.append(ExifTag());
    ExifTag* tag = &ifd->tags.last();
    if (global_opts.debug_level >= 3) {
      tag->tag_offset = gbftell(fin);
    }

    tag->id = gbfgetuint16(fin);
    tag->type = gbfgetuint16(fin);
    tag->count = gbfgetuint32(fin);
    tag->size = exif_type_size(tag->type) * tag->count;

    if (tag->size <= 4) { // data is in value offset field
      if (BYTE_TYPE(tag->type)) {
        assert(tag->count <= 4);
        if (tag->count > 0) {
          QByteArray qba(tag->count, 0);
          gbfread(qba.data(), tag->count, 1, fin);
          tag->data.append(qba);
        }
      } else if (WORD_TYPE(tag->type)) {
        assert(tag->count <= 2);
        for (unsigned idx=0; idx < tag->count; ++idx) {
          tag->data.append(gbfgetuint16(fin));
        }
      } else if (LONG_TYPE(tag->type)) {
        assert(tag->count <= 1);
        if (tag->count == 1) {
          tag->data.append(gbfgetuint32(fin));
        }
      } else if (tag->type == EXIF_TYPE_FLOAT) {
        assert(tag->count <= 1);
        if (tag->count == 1) {
          tag->data.append(gbfgetflt(fin));
        }
      } else {
        fatal(MYNAME "Unknown type %d has size <= 4! Please report.\n", tag->type);
      }
      int skip_bytes = 4 - tag->size;
      if (skip_bytes > 0) {
        gbfseek(fin, skip_bytes, SEEK_CUR);
      }
      if (global_opts.debug_level >= 3) {
        gbfseek(fin, -4, SEEK_CUR);
        gbfread(tag->raw, 4, 1, fin);
      }
    } else { // offset is in value offset field
      tag->offset = gbfgetuint32(fin);
    }

    if (ifd_nr == IFD0) {
      if (tag->id == IFD0_TAG_EXIF_IFD_OFFS) {
        *exif_ifd_ofs = tag->toLong();
      } else if (tag->id == IFD0_TAG_GPS_IFD_OFFS) {
        *gps_ifd_ofs = tag->toLong();
      }
    } else if (ifd_nr == EXIF_IFD) {
      if (tag->id == EXIF_IFD_TAG_INTER_IFD_OFFS) {
        *inter_ifd_ofs = tag->toLong();
      }
    }
  }

  gbsize_t next_ifd_offs;
  if (global_opts.debug_level >= 3) {
    next_ifd_offs = gbftell(fin);
  }
  ifd->next_ifd = gbfgetuint32(fin);

  for (auto& tag_instance : ifd->tags) {
    ExifTag* tag = &tag_instance;
    if ((tag->size > 4) && (tag->offset)) {
      gbfseek(fin, tag->offset, SEEK_SET);

      if (BYTE_TYPE(tag->type)) {
        QByteArray qba(tag->count, 0);
        gbfread(qba.data(), tag->count, 1, fin);
        tag->data.append(qba);
      } else for (unsigned i = 0; i < tag->count; i++) {
          switch (tag->type) {
          case EXIF_TYPE_SHORT:
          case EXIF_TYPE_SSHORT:
            tag->data.append(gbfgetuint16(fin));
            break;
          case EXIF_TYPE_IFD:
          case EXIF_TYPE_LONG:
          case EXIF_TYPE_SLONG:
            tag->data.append(gbfgetuint32(fin));
            break;
          case EXIF_TYPE_RAT:
          case EXIF_TYPE_SRAT:
            tag->data.append(gbfgetuint32(fin));
            tag->data.append(gbfgetuint32(fin));
            break;
          case EXIF_TYPE_FLOAT:
            tag->data.append(gbfgetflt(fin));
            break;
          case EXIF_TYPE_DOUBLE:
            tag->data.append(gbfgetdbl(fin));
            break;
          default: {
            // We know the size for this tag type, but not the layout.
            // Save it is a byte array we can echo on write.
            QByteArray qba(tag->count, 0);
            gbfread(qba.data(), exif_type_size(tag->type), 1, fin);
            tag->data.append(qba);
          }
          break;
          }
        }
    }
    if (global_opts.debug_level >= 3) {
      printf(MYNAME "-offs 0x%08X: ifd=%d id=0x%04X t=0x%04X c=%4u s=%4u",
             tag->tag_offset, ifd->nr, tag->id, tag->type, tag->count, tag->size);
      if (tag->size > 4) {
        printf(" o=0x%08X", tag->offset);
      } else {
        printf(" v=0x%02X%02X%02X%02X", tag->raw[0], tag->raw[1], tag->raw[2], tag->raw[3]);
      }
      if (tag->type == EXIF_TYPE_ASCII) {
        char* str = exif_read_str(tag);
        printf(" \"%s\"", str);
        xfree(str);
      } else {
        for (unsigned idx = 0; idx < std::min(tag->count, 4u); ++idx) {
          if (tag->type == EXIF_TYPE_BYTE) {
            printf(" %u", tag->data.at(0).toByteArray().at(idx));
          } else if (tag->type == EXIF_TYPE_SBYTE) {
            printf(" %d", tag->data.at(0).toByteArray().at(idx));
          } else if (tag->type == EXIF_TYPE_UNK) {
            printf(" 0x%02X", tag->data.at(0).toByteArray().at(idx));
          } else if (tag->type == EXIF_TYPE_RAT) {
            printf(" %+#g(%u/%u)", exif_read_double(tag, idx), tag->data.at(idx * 2).value<uint32_t>(), tag->data.at((idx * 2) + 1).value<uint32_t>());
          } else if (tag->type == EXIF_TYPE_SRAT) {
            printf(" %+#g(%d/%d)", exif_read_double(tag, idx), tag->data.at(idx * 2).value<int32_t>(), tag->data.at((idx * 2) + 1).value<int32_t>());
          } else if (tag->type == EXIF_TYPE_SHORT) {
            printf(" %u", tag->data.at(idx).value<uint16_t>());
          } else if (tag->type == EXIF_TYPE_SSHORT) {
            printf(" %d", tag->data.at(idx).value<int16_t>());
          } else if (tag->type == EXIF_TYPE_LONG) {
            printf(" %u", tag->data.at(idx).value<uint32_t>());
          } else if (tag->type == EXIF_TYPE_SLONG) {
            printf(" %d", tag->data.at(idx).value<int32_t>());
          } else if (tag->type == EXIF_TYPE_FLOAT) {
            printf(" %+#g", tag->data.at(idx).value<float>());
          } else if (tag->type == EXIF_TYPE_DOUBLE) {
            printf(" %+#g", tag->data.at(idx).value<double>());
          } else {
            printf(" 0x%0*X", 2 * exif_type_size(tag->type), tag->data.at(idx).value<uint32_t>());
          }
        }
        if (tag->count > 4) {
          printf(" ...");
        }
      }
      printf("\n");
    }
#ifndef NDEBUG
    exif_validate_tag_structure(tag);
#endif
  }

  if (global_opts.debug_level >= 3) {
    printf(MYNAME "-offs 0x%08X: Next IFD=0x%08X\n", next_ifd_offs,  ifd->next_ifd);
  }

  return ifd;
}

void
ExifFormat::exif_read_app(ExifApp* app)
{
  gbsize_t offs;
  uint32_t exif_ifd_ofs, gps_ifd_ofs, inter_ifd_ofs;
  ExifIfd* ifd;
  gbfile* fin = app->fexif;

  if (global_opts.debug_level >= 3) {
    printf(MYNAME ": read_app...\n");
    print_buff((const char*)fin->handle.mem, 8, MYNAME "-offs 0x00000000: Image File Header");
    printf("\n");
  }
  exif_ifd_ofs = gps_ifd_ofs = inter_ifd_ofs = 0;

  gbfseek(fin, 4, SEEK_SET);
  offs = gbfgetuint32(fin); // Image File Header Bytes 4-7, the offset (in bytes) of the first IFD.

  ifd = exif_read_ifd(app, IFD0, offs, &exif_ifd_ofs, &gps_ifd_ofs, &inter_ifd_ofs);
  if (ifd == nullptr) {
    return;
  }
  if (ifd->next_ifd) {
    ifd = exif_read_ifd(app, IFD1, ifd->next_ifd, &exif_ifd_ofs, &gps_ifd_ofs, &inter_ifd_ofs);
  }
  if (exif_ifd_ofs) {
    ifd = exif_read_ifd(app, EXIF_IFD, exif_ifd_ofs, nullptr, nullptr, &inter_ifd_ofs);
  }
  if (gps_ifd_ofs) {
    ifd = exif_read_ifd(app, 3, gps_ifd_ofs, nullptr, nullptr, nullptr);
  }
  if (inter_ifd_ofs) {
    ifd = exif_read_ifd(app, 4, inter_ifd_ofs, nullptr, nullptr, nullptr);
  }
  // The return values of exif_read_ifd above aren't actually used.
  // Warning hush.
  (void) ifd;
}

void
ExifFormat::exif_examine_app(ExifApp* app)
{
  gbfile* ftmp = app->fcache;

  gbfrewind(ftmp);
  uint32_t ident = gbfgetuint32(ftmp);
  if (ident != 0x66697845) {
    fatal(MYNAME ": Invalid EXIF header magic.");
  }
  if (gbfgetint16(ftmp) != 0) {
    fatal(MYNAME ": Error in EXIF header.");
  }
  uint16_t endianness = gbfgetint16(ftmp);

  if (global_opts.debug_level >= 3) {
    printf(MYNAME ": endianness = 0x%04X\n", endianness);
  }
  if (endianness == 0x4949) {
    ftmp->big_endian = 0;
  } else if (endianness == 0x4D4D) {
    ftmp->big_endian = 1;
  } else {
    fatal(MYNAME ": Invalid endianness identifier 0x%04X!\n", endianness);
  }

  gbfseek(ftmp, 6, SEEK_SET);
  app->fexif = gbfopen(nullptr, "wb", MYNAME);
  app->fexif->big_endian = ftmp->big_endian;
  gbfcopyfrom(app->fexif, ftmp, 0x7FFFFFFF);

  exif_read_app(app);
}

ExifFormat::ExifIfd*
ExifFormat::exif_find_ifd(ExifApp* app, const uint16_t ifd_nr)
{
  for (auto& ifd_instance : app->ifds) {
    ExifIfd* ifd = &ifd_instance;

    if (ifd->nr == ifd_nr) {
      return ifd;
    }
  }
  return nullptr;
}

ExifFormat::ExifTag*
ExifFormat::exif_find_tag(ExifApp* app, const uint16_t ifd_nr, const uint16_t tag_id)
{
  ExifIfd* ifd = exif_find_ifd(app, ifd_nr);
  if (ifd != nullptr) {
    for (auto& tag_instance : ifd->tags) {
      ExifTag* tag = &tag_instance;
      if (tag->id == tag_id) {
        return tag;
      }
    }
  }
  return nullptr;
}

QDateTime
ExifFormat::exif_get_exif_time(ExifApp* app)
{
  QDateTime res;

  ExifTag* tag = exif_find_tag(app, EXIF_IFD, 0x9003);      /* DateTimeOriginal from EXIF */
  if (! tag) {
    tag = exif_find_tag(app, IFD0, 0x0132);  /* DateTime from IFD0 */
  }
  if (! tag) {
    tag = exif_find_tag(app, EXIF_IFD, 0x9004);  /* DateTimeDigitized from EXIF */
  }

  if (tag) {
    char* str = exif_read_str(tag);
    // This assumes the Qt::TimeSpec is Qt::LocalTime, i.e. the current system time zone.
    // Note the assumption of local time can be problematic if the data
    // is processed in a different time zone than was used in recording
    // the time in the image.
    res = QDateTime::fromString(str, "yyyy:MM:dd hh:mm:ss");
    xfree(str);

    // Exif 2.31 added offset tags to record the offset to UTC.
    // If these are present use them, otherwise assume local time.
    ExifTag* offset_tag = nullptr;
    switch (tag->id) {
    case 0x9003:
      offset_tag = exif_find_tag(app, EXIF_IFD, 0x9011);  /* OffsetTimeOriginal from EXIF */
      break;
    case 0x0132:
      offset_tag = exif_find_tag(app, EXIF_IFD, 0x9010);  /* OffsetTime from EXIF */
      break;
    case 0x9004:
      offset_tag = exif_find_tag(app, EXIF_IFD, 0x9012);  /* OffsetTimeDigitized from EXIF */
      break;
    }

    if (offset_tag) {
      char* time_tag = exif_read_str(offset_tag);
      // string should be +HH:MM or -HH:MM
      static const QRegularExpression re(R"(^([+-])(\d{2}):(\d{2})$)");
      assert(re.isValid());
      QRegularExpressionMatch match = re.match(time_tag);
      if (match.hasMatch()) {
        // Correct the date time by supplying the offset from UTC.
        int offset_hours = match.captured(1).append(match.captured(2)).toInt();
        int offset_mins = match.captured(1).append(match.captured(3)).toInt();
        res.setOffsetFromUtc(((offset_hours * 60) + offset_mins) * 60);
      }
    }
  }
  return res;
}

Waypoint*
ExifFormat::exif_waypt_from_exif_app(ExifApp* app) const
{
  ExifTag* tag;
  char lat_ref = '\0';
  char lon_ref = '\0';
  char alt_ref = 0;
  char speed_ref = 'K';
  char* datum = nullptr;
  char mode = '\0';
  double gpsdop = unknown_alt;
  double alt = unknown_alt;
  QTime timestamp;
  QDate datestamp;
  QDateTime gps_datetime;

  ExifIfd* ifd = exif_find_ifd(app, GPS_IFD);
  if (ifd == nullptr) {
    return nullptr;
  }

  auto* wpt = new Waypoint;

  wpt->latitude = unknown_alt;
  wpt->longitude = unknown_alt;

  for (auto& tag_instance : ifd->tags) {
    tag = &tag_instance;

    switch (tag->id) {
    case GPS_IFD_TAG_VERSION:
      break;
    case GPS_IFD_TAG_LATREF:
      lat_ref = tag->data.at(0).toByteArray().at(0);
      break;
    case GPS_IFD_TAG_LAT:
      wpt->latitude = exif_read_coord(tag);
      break;
    case GPS_IFD_TAG_LONREF:
      lon_ref = tag->data.at(0).toByteArray().at(0);
      break;
    case GPS_IFD_TAG_LON:
      wpt->longitude = exif_read_coord(tag);
      break;
    case GPS_IFD_TAG_ALTREF:
      alt_ref = tag->data.at(0).toByteArray().at(0);
      break;
    case GPS_IFD_TAG_ALT:
      alt = exif_read_double(tag, 0);
      break;
    case GPS_IFD_TAG_TIMESTAMP:
      timestamp = exif_read_timestamp(tag);
      break;
    case GPS_IFD_TAG_SAT:
      wpt->sat = tag->data.at(0).toByteArray().toInt();
      break;
    case GPS_IFD_TAG_MODE:
      mode = tag->data.at(0).toByteArray().at(0);
      break;
    case GPS_IFD_TAG_DOP:
      gpsdop = exif_read_double(tag, 0);
      break;
    case GPS_IFD_TAG_SPEEDREF:
      speed_ref = tag->data.at(0).toByteArray().at(0);
      break;
    case GPS_IFD_TAG_SPEED:
      WAYPT_SET(wpt, speed, exif_read_double(tag, 0));
      break;
    case GPS_IFD_TAG_DATUM:
      datum = exif_read_str(tag);
      break;
    case GPS_IFD_TAG_DATESTAMP:
      datestamp = exif_read_datestamp(tag);
      break;
    }
  }

  if ((wpt->latitude == unknown_alt) || (wpt->longitude == unknown_alt)) {
    fatal(MYNAME ": Missing GPSLatitude and/or GPSLongitude!\n");
  }

  if (lat_ref == 'S') {
    wpt->latitude *= -1;
  } else if (lat_ref != 'N') {
    warning(MYNAME ": GPSLatitudeRef not set! Using N(orth).\n");
  }

  if (lon_ref == 'W') {
    wpt->longitude *= -1;
  } else if (lon_ref != 'E') {
    warning(MYNAME ": GPSLongitudeRef not set! Using E(ast).\n");
  }

  if (global_opts.debug_level >= 3) {
    printf(MYNAME "-GPSLatitude =  %12.7f\n", wpt->latitude);
    printf(MYNAME "-GPSLongitude = %12.7f\n", wpt->longitude);
  }
  if (datum) {
    int idatum = gt_lookup_datum_index(datum, MYNAME);
    if (idatum < 0) {
      fatal(MYNAME ": Unknown GPSMapDatum \"%s\"!\n", datum);
    }
    if (idatum != kDautmWGS84) {
      GPS_Math_WGS84_To_Known_Datum_M(wpt->latitude, wpt->longitude, 0.0,
                                      &wpt->latitude, &wpt->longitude, &alt, idatum);
    }
    xfree(datum);
  }

  if (alt != unknown_alt) {
    double sign;
    switch (alt_ref) {
    case 0:
      sign = 1.0;
      break;

    case 1:
      sign = -1.0;
      break;

    default:
      warning(MYNAME ": Invalid GPSAltitudeRef (%d)! Using default value 0 (= Sea level).\n", alt_ref);
      sign = 1.0;
    }
    wpt->altitude = sign * alt;
    if (global_opts.debug_level >= 3) {
      printf(MYNAME "-GPSAltitude =  %12.7f m\n", wpt->altitude);
    }
  }

  if WAYPT_HAS(wpt, speed) {
    switch (speed_ref) {
    case 'K':
      wpt->speed = KPH_TO_MPS(wpt->speed);
      break;
    case 'M':
      wpt->speed = MPH_TO_MPS(wpt->speed);
      break;
    case 'N':
      wpt->speed = KNOTS_TO_MPS(wpt->speed);
      break;
    default:
      wpt->speed = 0;
      WAYPT_UNSET(wpt, speed);
      warning(MYNAME ": Unknown GPSSpeedRef unit %c (0x%02x)!\n", speed_ref, speed_ref);
    }
    if (global_opts.debug_level >= 3) {
      if WAYPT_HAS(wpt, speed) {
        printf(MYNAME "-GPSSpeed = %12.2f m/s\n", wpt->speed);
      }
    }
  }

  if (mode == '2') {
    wpt->fix = fix_2d;
    if (gpsdop != unknown_alt) {
      wpt->hdop = gpsdop;
    }
  } else if (mode == '3') {
    wpt->fix = fix_3d;
    if (gpsdop != unknown_alt) {
      wpt->pdop = gpsdop;
    }
  }

  gps_datetime = QDateTime(datestamp, timestamp, Qt::UTC);
  if (gps_datetime.isValid()) {
    if (global_opts.debug_level >= 3) {
      printf(MYNAME "-GPSTimeStamp =   %s\n", qPrintable(gps_datetime.toString(Qt::ISODate)));
    }
    wpt->SetCreationTime(gps_datetime);
  } else {
    wpt->SetCreationTime(exif_get_exif_time(app));
  }

  tag = exif_find_tag(app, EXIF_IFD, EXIF_IFD_TAG_USER_CMT); /* UserComment */
  if (tag && (tag->size > 8)) {
    // TODO: User comments with JIS and Undefined Code Designations are ignored.
    if (memcmp(tag->data.at(0).toByteArray().constData(), "ASCII\0\0\0", 8) == 0) {
      wpt->notes = QString::fromLatin1(tag->data.at(0).toByteArray().constData() + 8, tag->size - 8);
    } else if (memcmp(tag->data.at(0).toByteArray().constData(), "UNICODE\0", 8) == 0) {
      QTextCodec* utf16_codec;
      if (app->fcache->big_endian) {
        utf16_codec = QTextCodec::codecForName("UTF-16BE");
      } else {
        utf16_codec = QTextCodec::codecForName("UTF-16LE");
      }
      wpt->notes = utf16_codec->toUnicode(tag->data.at(0).toByteArray().constData() + 8, tag->size - 8);
    }
  }

  if (opt_filename) {
    QFileInfo fi(fin_->name);
    // No directory, no extension.
    wpt->shortname = fi.baseName();
  }

  return wpt;
}

// TODO: we could achieve an increased domain and accuracy for TIFF RATIONAL
// types if we handled them separately (int32_t -> uint32_t, INT32_MAX -> UINT32_MAX).
ExifFormat::Rational<int32_t> ExifFormat::exif_dec2frac(double val, double tolerance = DBL_EPSILON)
{
  constexpr double upper_limit = INT32_MAX;
  constexpr double lower_limit = 1.0/upper_limit;
  const double pval = fabs(val);
  const double tol = fmax(tolerance, DBL_EPSILON);

  if (pval < lower_limit) {
    return Rational<int32_t>(0, upper_limit);
  } else if (pval > upper_limit) {
    fatal(MYNAME ": Value (%f) to big for a rational representation!\n", val);
    return Rational<int32_t>(copysign(upper_limit, val), 1);
  }

  double b;
  double remainder = modf(pval, &b);
  Rational<double> prev_prev(1.0, 0.0);
  Rational<double> prev(b, 1.0);
  Rational<double> curr = prev;

  // phi = (1.0+sqrt(5.0))/2.0 is badly approximable and the slowest to converge.
  // This is a good test case to see the maximum number of iterations required.
  for (int idx = 0; idx < 64; ++idx) {
    // Calculate the next simple continued fraction coefficient (b), and remainder (remainder).
    if (remainder < lower_limit) {
      break; // remainder is nearly zero, use current estimate.
    }
    remainder = modf(1.0/remainder, &b);

    // Convert the truncated simple continued fraction, a.k.a. a convergent, to an ordinary fraction (curr.num/curr.den).
    Rational<double> candidate((b * prev.num) + prev_prev.num, (b * prev.den) + prev_prev.den);
    if (candidate.num > upper_limit) {
      break; // numerator too big, use current estimate.
    }
    if (candidate.den > upper_limit) {
      break; // denominator too big, use current estimate.
    }
    curr = candidate;

    if (fabs(pval- (curr.num/curr.den)) < (pval * tol)) {
      break; // close enough, use current estimate.
    }

    prev_prev = prev;
    prev = curr;
  }

  return Rational<int32_t>(round(copysign(curr.num, val)), round(curr.den));
}

ExifFormat::ExifTag*
ExifFormat::exif_put_value(const int ifd_nr, const uint16_t tag_id, const uint16_t type, const int count, const int index, const void* data) const
{
  ExifTag* tag = nullptr;
  uint16_t size;

  ExifIfd* ifd = exif_find_ifd(exif_app_, ifd_nr);
  if (ifd == nullptr) {
    exif_app_->ifds.append(ExifIfd());
    ifd = &exif_app_->ifds.last();
    ifd->nr = ifd_nr;
  } else {
    tag = exif_find_tag(exif_app_, ifd_nr, tag_id);
  }

  uint16_t item_size = exif_type_size(type);

  if ((data == nullptr) || (count < 1) || (index < 0)) {
    size = 0;
  } else {
    size = (index + count) * item_size;
  }

  if (tag == nullptr) { /* create new tag */
    if (size == 0) {
      return nullptr;
    }

    ifd->tags.append(ExifTag());
    tag = &ifd->tags.last();

    tag->id = tag_id;
    tag->type = type;
    tag->count = index + count;
    tag->size = size;
    ifd->count++;

  } else if (size == 0) {	/* remove this tag */
    ifd->count--;
    ifd->tags.removeOne(*tag);
    return nullptr;
  } else { /* modify existing tag */
    tag->count = index + count;
    tag->size = size;
  }

  if (BYTE_TYPE(type)) {
    assert(item_size == 1);
    if (tag->data.empty()) {
      tag->data.append(QByteArray());
    }
    QByteArray qba = tag->data.at(0).toByteArray();
    int size_increase = (index + count) - qba.size();
    if (size_increase > 0) {
      qba.append(size_increase, 0);
    }
    qba.replace(index, count, static_cast<const char*>(data), count);
    tag->data[0] = qba;
  } else {
    // we haven't coded for insertion of multiple values (except for BYTE_TYPE above).
    assert(count == 1);
    switch (type) {
    case EXIF_TYPE_SHORT:
    case EXIF_TYPE_SSHORT:
      tag->grow<uint16_t>(index + count);
      tag->data[index] = *static_cast<const uint16_t*>(data);
      break;
    case EXIF_TYPE_LONG:
    case EXIF_TYPE_SLONG:
    case EXIF_TYPE_IFD:
      tag->grow<uint32_t>(index + count);
      tag->data[index] = *static_cast<const uint32_t*>(data);
      break;
    case EXIF_TYPE_RAT:
    case EXIF_TYPE_SRAT: {
      double val = *static_cast<const double*>(data);

      if ((val < 0.0) && (type == EXIF_TYPE_RAT)) {
        fatal(MYNAME ": A negative value cannot be stored as type RATIONAL.");
      }

      Rational<int32_t> rat = exif_dec2frac(val, 1e-11);
      tag->grow<uint32_t>((index + count) * 2);
      tag->data[index * 2] = rat.num;
      tag->data[(index * 2) + 1] = rat.den;
    }
    break;
    case EXIF_TYPE_FLOAT:
      tag->grow<float>(index + count);
      tag->data[index] = *static_cast<const float*>(data);
      break;
    case EXIF_TYPE_DOUBLE:
      tag->grow<double>(index + count);
      tag->data[index] = *static_cast<const double*>(data);
      break;
    default:
      fatal(MYNAME ": Unknown data type %u!\n", type);
    }
  }
  return tag;
}


void
ExifFormat::exif_put_double(const int ifd_nr, const int tag_id, const int index, const double val) const
{
  // TODO: It seems wrong to throw away the sign.
  double d = fabs(val);
  exif_put_value(ifd_nr, tag_id, EXIF_TYPE_RAT, 1, index, &d);
}


void
ExifFormat::exif_put_str(const int ifd_nr, const int tag_id, const char* val) const
{
  int len = (val) ? strlen(val) + 1 : 0;
  exif_put_value(ifd_nr, tag_id, EXIF_TYPE_ASCII, len, 0, val);
}

void
ExifFormat::exif_put_coord(const int ifd_nr, const int tag_id, const double val) const
{
  double vdeg;
  double vmin;
  double fractional_part;

  fractional_part = modf(val, &vdeg);
  fractional_part = modf(60.0 * fractional_part, &vmin);
  double vsec = 60.0 * fractional_part;

  exif_put_double(ifd_nr, tag_id, 0, vdeg);
  exif_put_double(ifd_nr, tag_id, 1, vmin);
  exif_put_double(ifd_nr, tag_id, 2, vsec);
}

void
ExifFormat::exif_put_long(const int ifd_nr, const int tag_id, const int index, const int32_t val) const
{
  exif_put_value(ifd_nr, tag_id, EXIF_TYPE_LONG, 1, index, &val);
}

void
ExifFormat::exif_put_short(const int ifd_nr, const int tag_id, const int index, const int16_t val) const
{
  exif_put_value(ifd_nr, tag_id, EXIF_TYPE_SHORT, 1, index, &val);
}

void
ExifFormat::exif_remove_tag(const int ifd_nr, const int tag_id) const
{
  exif_put_value(ifd_nr, tag_id, EXIF_TYPE_BYTE, 0, 0, nullptr);
}

void
ExifFormat::exif_find_wpt_by_time(const Waypoint* wpt)
{
  if (!wpt->creation_time.isValid()) {
    return;
  }

  if (exif_wpt_ref == nullptr) {
    exif_wpt_ref = wpt;
  } else if (labs(exif_time_ref.msecsTo(wpt->creation_time)) < labs(exif_time_ref.msecsTo(exif_wpt_ref->creation_time))) {
    exif_wpt_ref = wpt;
  }
}

void
ExifFormat::exif_find_wpt_by_name(const Waypoint* wpt)
{
  if (exif_wpt_ref != nullptr) {
    return;
  } else if ((wpt->shortname != nullptr) && (case_ignore_strcmp(wpt->shortname, opt_name) == 0)) {
    exif_wpt_ref = wpt;
  }
}


bool
ExifFormat::exif_sort_tags_cb(const ExifTag& A, const ExifTag& B)
{
  return A.id < B.id;
}

bool
ExifFormat::exif_sort_ifds_cb(const ExifIfd& A, const ExifIfd& B)
{
  return A.nr < B.nr;
}

void
ExifFormat::exif_write_value(ExifTag* tag, gbfile* fout)
{
  if (tag->size > 4) {
    gbfputuint32(tag->offset, fout);  /* offset to data */
  } else {
    if BYTE_TYPE(tag->type) {
      assert(tag->count <= 4);
      if (tag->count > 0) {
        gbfwrite(tag->data.at(0).toByteArray().constData(), tag->count, 1, fout);
      }
    } else if WORD_TYPE(tag->type) {
      assert(tag->count <= 2);
      for (unsigned idx = 0; idx < tag->count; ++idx) {
        gbfputuint16(tag->data.at(idx).value<uint16_t>(), fout);
      }
    } else if LONG_TYPE(tag->type) {
      assert(tag->count <= 1);
      if (tag->count > 0) {
        gbfputuint32(tag->data.at(0).value<uint32_t>(), fout);
      }
    } else if (tag->type == EXIF_TYPE_FLOAT) {
      assert(tag->count <= 1);
      if (tag->count > 0) {
        gbfputflt(tag->data.at(0).value<float>(), fout);
      }
    } else {
      fatal(MYNAME ": Unknown data type %d or wrong tag size %d!\n", tag->type, tag->size);
    }
    int fill_bytes = 4 - tag->size;
    for (int idx = 0; idx < fill_bytes; ++idx) {
      gbfputc(0, fout);
    }
  }
}

void
ExifFormat::exif_write_ifd(ExifIfd* ifd, const char next, gbfile* fout)
{
  gbfputuint16(ifd->count, fout);
  gbsize_t offs = gbftell(fout) + (ifd->count * 12) + 4;

  for (auto& tag_instance : ifd->tags) {
    ExifTag* tag = &tag_instance;

#ifndef NDEBUG
    exif_validate_tag_structure(tag);
#endif

    gbfputuint16(tag->id, fout);
    gbfputuint16(tag->type, fout);
    gbfputuint32(tag->count, fout);
    if (tag->size > 4) {
      tag->offset = offs;
      offs += tag->size;
      if (offs & 1u) {
        offs++;
      }
      gbfputuint32(tag->offset, fout);
    } else {
      exif_write_value(tag, fout);
    }
  }

  if (next) {
    gbfputuint32(offs, fout);
  } else {
    gbfputuint32(0, fout);
  }

  for (auto& tag_instance : ifd->tags) {
    ExifTag* tag = &tag_instance;

    if (tag->size > 4) {
      if BYTE_TYPE(tag->type) {
        gbfwrite(tag->data.at(0).toByteArray().constData(), tag->size, 1, fout);
      } else for (unsigned i = 0; i < tag->count; i++) {
          switch (tag->type) {
          case EXIF_TYPE_SHORT:
          case EXIF_TYPE_SSHORT:
            gbfputuint16(tag->data.at(i).value<uint16_t>(), fout);
            break;
          case EXIF_TYPE_LONG:
          case EXIF_TYPE_SLONG:
          case EXIF_TYPE_IFD:
            gbfputuint32(tag->data.at(i).value<uint32_t>(), fout);
            break;
          case EXIF_TYPE_RAT:
          case EXIF_TYPE_SRAT:
            gbfputuint32(tag->data.at(2 * i).value<uint32_t>(), fout);
            gbfputuint32(tag->data.at((2 * i) + 1).value<uint32_t>(), fout);
            break;
          case EXIF_TYPE_FLOAT:
            gbfputflt(tag->data.at(i).value<float>(), fout);
            break;
          case EXIF_TYPE_DOUBLE:
            gbfputdbl(tag->data.at(i).value<double>(), fout);
            break;
          default:
            // We know the size for this tag type, but not the layout.
            // Echo saved data from read.
            gbfwrite(tag->data.at(i).toByteArray().constData(), tag->size, 1, fout);
            break;
          }
        }
      if (gbftell(fout) & 1u) {
        gbfputc(0, fout);
      }
    }
  }
}

void
ExifFormat::exif_write_apps() const
{
  gbfputuint16(0xFFD8, fout_);

  for (auto* app : qAsConst(*exif_apps)) {

    gbfputuint16(app->marker, fout_);

    if (app == exif_app_) {
      assert(app->marker == 0xFFE1);
      uint32_t len = 8;

      exif_put_long(IFD0, IFD0_TAG_GPS_IFD_OFFS, 0, 0);
      exif_put_value(GPS_IFD, GPS_IFD_TAG_VERSION, EXIF_TYPE_BYTE, 4, 0, writer_gps_tag_version);

      std::sort(app->ifds.begin(), app->ifds.end(), exif_sort_ifds_cb);

      for (auto& ifd_instance : app->ifds) {
        ExifIfd* ifd = &ifd_instance;

        if (ifd->nr == GPS_IFD) {
          exif_put_long(IFD0, IFD0_TAG_GPS_IFD_OFFS, 0, len);
        } else if (ifd->nr == EXIF_IFD) {
          exif_put_long(IFD0, IFD0_TAG_EXIF_IFD_OFFS, 0, len);
        } else if (ifd->nr == INTER_IFD) {
          exif_put_long(EXIF_IFD, EXIF_IFD_TAG_INTER_IFD_OFFS, 0, len);
        }

        len += exif_ifd_size(ifd);
      }

      len += 4; /* DWORD(0) after last ifd */

      // gather offsets and sizes of thumbnail image data to relocate.
      // update offsets to point to relocated data.
      ExifTag* tag_offset;
      ExifTag* tag_size;
      QVector<QPair<uint32_t, uint32_t>> image_data;
      if ((tag_offset = exif_find_tag(app, IFD1, IFD1_TAG_JPEG_OFFS))) {
        // IFD1_TAG_COMPRESSION should be 6 indicating JPEG compressed image data.
        tag_size = exif_find_tag(app, IFD1, IFD1_TAG_JPEG_SIZE);
        if (tag_size == nullptr) {
          fatal(MYNAME ": Invalid image file, in IFD1 both JPEGInterchangeFormat and JPEGInterchangeFormatLength must exist for compressed thumbnails.");
        }
        auto offset = tag_offset->data.at(0).value<uint32_t>();
        auto size = tag_size->data.at(0).value<uint32_t>();
        image_data.append(QPair<uint32_t, uint32_t>(offset, size));
        exif_put_long(IFD1, IFD1_TAG_JPEG_OFFS, 0, len);
        len += size;
      } else if ((tag_offset = exif_find_tag(app, IFD1, IFD1_TAG_STRIP_OFFS))) {
        // IFD1_TAG_COMPRESSION should be 1 indicating uncompressed image data.
        tag_size = exif_find_tag(app, IFD1, IFD1_TAG_STRIP_BYTE_COUNTS);
        if ((tag_size == nullptr) || (tag_size->count != tag_offset->count)) {
          fatal(MYNAME ": Invalid image file, in IFD1 both StripOffsets and StripByteCounts must exist and have equal counts for uncompressed thumbnails.");
        }
        for (unsigned idx = 0; idx < tag_offset->count; idx++) {
          auto offset = tag_offset->data.at(idx).value<uint32_t>();
          auto size = tag_size->data.at(idx).value<uint32_t>();
          image_data.append(QPair<uint32_t, uint32_t>(offset, size));
          if (tag_offset->type == EXIF_TYPE_SHORT) {
            exif_put_short(IFD1, IFD1_TAG_STRIP_OFFS, idx, len);
          } else {
            exif_put_long(IFD1, IFD1_TAG_STRIP_OFFS, idx, len);
          }
          len += size;
        }
      }

      for (auto& ifd_instance : app->ifds) {
        ExifIfd* ifd = &ifd_instance;
        std::sort(ifd->tags.begin(), ifd->tags.end(), exif_sort_tags_cb);
      }

      gbfile* ftmp = gbfopen_be(nullptr, "wb", MYNAME);
      ftmp->big_endian = app->fcache->big_endian;

      gbfwrite((ftmp->big_endian) ? "MM" : "II", 2, 1, ftmp);
      gbfputuint16(0x2A, ftmp);
      gbfputuint32(0x08, ftmp); /* offset to first IFD */

      for (int i = 0; i < app->ifds.size(); ++i) {
        ExifIfd* ifd = &app->ifds[i];

        char next = ((ifd->nr == IFD0) && ((i + 1) < app->ifds.size()) && (app->ifds[i+1].nr == IFD1));

        exif_write_ifd(ifd, next, ftmp);
      }

      gbfputuint32(0, ftmp); /* DWORD(0) after last ifd */

      // relocate thumbnail image data.
      for (const auto& segment : image_data) {
        gbfseek(app->fexif, segment.first, SEEK_SET);
        gbfcopyfrom(ftmp, app->fexif, segment.second);
      }

      len = gbftell(ftmp);
      gbfrewind(ftmp);
      gbfputuint16(len + 8, fout_);
      gbfwrite("Exif\0\0", 6, 1, fout_);
      gbfcopyfrom(fout_, ftmp, len);

      gbfclose(ftmp);
    } else {
      gbfputuint16(app->len, fout_);
      gbfrewind(app->fcache);
      gbfcopyfrom(fout_, app->fcache, 0x7FFFFFFF);
    }
  }
}

/*******************************************************************************
* %%%        global callbacks called by gpsbabel main process              %%% *
*******************************************************************************/

void
ExifFormat::rd_init(const QString& fname)
{
  fin_ = gbfopen_be(fname, "rb", MYNAME);
  exif_apps = new QList<ExifApp*>;
}

void
ExifFormat::rd_deinit()
{
  exif_release_apps();
  gbfclose(fin_);
}

void
ExifFormat::read()
{
  uint16_t soi = gbfgetuint16(fin_);
  /* only jpeg for now */
  if (soi != 0xFFD8) {
    fatal(MYNAME ": Unknown image file.");
  }

  exif_app_ = exif_load_apps();
  if (exif_app_ == nullptr) {
    fatal(MYNAME ": No EXIF header in source file \"%s\".", fin_->name);
  }

  exif_examine_app(exif_app_);
  Waypoint* wpt = exif_waypt_from_exif_app(exif_app_);
  if (wpt) {
    waypt_add(wpt);
  }
}

void
ExifFormat::wr_init(const QString& fname)
{
  exif_success = 0;
  exif_fout_name = fname;

  exif_apps = new QList<ExifApp*>;

  fin_ = gbfopen_be(fname, "rb", MYNAME);
  if (fin_->is_pipe) {
    fatal(MYNAME ": Sorry, this format cannot be used with pipes!");
  }

  uint16_t soi = gbfgetuint16(fin_);
  if (soi != 0xFFD8) {
    fatal(MYNAME ": Unknown image file.");
  }
  exif_app_ = exif_load_apps();
  if (exif_app_ == nullptr) {
    fatal(MYNAME ": No EXIF header found in source file \"%s\".", fin_->name);
  }
  exif_examine_app(exif_app_);
  gbfclose(fin_);

  exif_time_ref = exif_get_exif_time(exif_app_);
  if (!exif_time_ref.isValid()) {
    fatal(MYNAME ": No valid timestamp found in picture!\n");
  }

  QString filename(fname);
  filename += ".jpg";
  fout_ = gbfopen_be(filename, "wb", MYNAME);
}

void
ExifFormat::wr_deinit()
{

  exif_release_apps();
  QString tmpname = QString(fout_->name);
  gbfclose(fout_);

  if (exif_success) {
    if (*opt_overwrite == '1') {
      QFile::remove(exif_fout_name);
      QFile::rename(tmpname, exif_fout_name);
    }
  } else {
    QFile::remove(tmpname);
  }

  exif_fout_name.clear();
}

void
ExifFormat::write()
{
  exif_wpt_ref = nullptr;

  if (opt_name) {
    auto exif_find_wpt_by_name_lambda = [this](const Waypoint* waypointp)->void {
      exif_find_wpt_by_name(waypointp);
    };
    waypt_disp_all(exif_find_wpt_by_name_lambda);
    if (exif_wpt_ref == nullptr) {
      route_disp_all(nullptr, nullptr, exif_find_wpt_by_name_lambda);
    }
    if (exif_wpt_ref == nullptr) {
      track_disp_all(nullptr, nullptr, exif_find_wpt_by_name_lambda);
    }
    if (exif_wpt_ref == nullptr) {
      warning(MYNAME ": No matching point with name \"%s\" found.\n", opt_name);
    }
  } else {
    auto exif_find_wpt_by_time_lambda = [this](const Waypoint* waypointp)->void {
      exif_find_wpt_by_time(waypointp);
    };
    track_disp_all(nullptr, nullptr, exif_find_wpt_by_time_lambda);
    route_disp_all(nullptr, nullptr, exif_find_wpt_by_time_lambda);
    waypt_disp_all(exif_find_wpt_by_time_lambda);

    qint64 frame = xstrtoi(opt_frame, nullptr, 10);

    if (exif_wpt_ref == nullptr) {
      warning(MYNAME ": No point with a valid timestamp found.\n");
    } else if (labs(exif_time_ref.secsTo(exif_wpt_ref->creation_time)) > frame) {
      QString time_str = exif_time_str(exif_time_ref);
      warning(MYNAME ": No matching point found for image date %s!\n", qPrintable(time_str));
      if (exif_wpt_ref != nullptr) {
        QString str = exif_time_str(exif_wpt_ref->creation_time);
        warning(MYNAME ": Best is from %s, %ld second(s) away.\n",
                qPrintable(str), labs(exif_time_ref.secsTo(exif_wpt_ref->creation_time)));
      }
      exif_wpt_ref = nullptr;
    }
  }

  if (exif_wpt_ref != nullptr) {
    const Waypoint* wpt = exif_wpt_ref;

    exif_put_long(IFD0, IFD0_TAG_GPS_IFD_OFFS, 0, 0);
    exif_put_value(GPS_IFD, GPS_IFD_TAG_VERSION, EXIF_TYPE_BYTE, 4, 0, writer_gps_tag_version);
    exif_put_str(GPS_IFD, GPS_IFD_TAG_DATUM, "WGS-84");

    exif_put_str(GPS_IFD, GPS_IFD_TAG_LATREF, wpt->latitude < 0 ? "S" : "N");
    exif_put_coord(GPS_IFD, GPS_IFD_TAG_LAT, fabs(wpt->latitude));
    exif_put_str(GPS_IFD, GPS_IFD_TAG_LONREF, wpt->longitude < 0 ? "W" : "E");
    exif_put_coord(GPS_IFD, GPS_IFD_TAG_LON, fabs(wpt->longitude));

    if (wpt->altitude == unknown_alt) {
      exif_remove_tag(GPS_IFD, GPS_IFD_TAG_ALT);
      exif_remove_tag(GPS_IFD, GPS_IFD_TAG_ALTREF);
    } else {
      uint8_t alt_ref = (wpt->altitude >= 0.0) ? 0 : 1;
      exif_put_value(GPS_IFD, GPS_IFD_TAG_ALTREF, EXIF_TYPE_BYTE, 1, 0, &alt_ref);
      exif_put_double(GPS_IFD, GPS_IFD_TAG_ALT, 0, fabs(wpt->altitude));
    }

    if (wpt->creation_time.isValid()) {
      const QDateTime dt = wpt->GetCreationTime().toUTC();

      exif_put_double(GPS_IFD, GPS_IFD_TAG_TIMESTAMP, 0, dt.time().hour());
      exif_put_double(GPS_IFD, GPS_IFD_TAG_TIMESTAMP, 1, dt.time().minute());
      exif_put_double(GPS_IFD, GPS_IFD_TAG_TIMESTAMP, 2, dt.time().second());

      exif_put_str(GPS_IFD, GPS_IFD_TAG_DATESTAMP, CSTR(dt.toString(u"yyyy:MM:dd")));
    } else {
      exif_remove_tag(GPS_IFD, GPS_IFD_TAG_TIMESTAMP);
      exif_remove_tag(GPS_IFD, GPS_IFD_TAG_DATESTAMP);
    }

    if (wpt->sat > 0) {
      char buf[16];
      snprintf(buf, sizeof(buf), "%d", wpt->sat);
      exif_put_str(GPS_IFD, GPS_IFD_TAG_SAT, buf);
    } else {
      exif_remove_tag(GPS_IFD, GPS_IFD_TAG_SAT);
    }

    if (wpt->fix == fix_2d) {
      exif_put_str(GPS_IFD, GPS_IFD_TAG_MODE, "2");
    } else if (wpt->fix == fix_3d) {
      exif_put_str(GPS_IFD, GPS_IFD_TAG_MODE, "3");
    } else {
      exif_remove_tag(GPS_IFD, GPS_IFD_TAG_MODE);
    }

    if (wpt->hdop > 0) {
      exif_put_double(GPS_IFD, GPS_IFD_TAG_DOP, 0, wpt->hdop);
    } else {
      exif_remove_tag(GPS_IFD, GPS_IFD_TAG_DOP);
    }

    if WAYPT_HAS(wpt, speed) {
      exif_put_str(GPS_IFD, GPS_IFD_TAG_SPEEDREF, "K");
      exif_put_double(GPS_IFD, GPS_IFD_TAG_SPEED, 0, MPS_TO_KPH(wpt->speed));
    } else {
      exif_remove_tag(GPS_IFD, GPS_IFD_TAG_SPEEDREF);
      exif_remove_tag(GPS_IFD, GPS_IFD_TAG_SPEED);
    }

    exif_write_apps();  /* Success, write the new file */

    exif_success = 1;
  }

}
