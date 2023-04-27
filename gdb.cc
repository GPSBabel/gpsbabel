/*
	Garmin GPS Database Reader/Writer

	Copyright (C) 2005-2008 Olaf Klein, o.b.klein@gpsbabel.org
	Mainly based on mapsource.c,
	Copyright (C) 2005 Robert Lipe, robertlipe+source@gpsbabel.org


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

  A format description obtained from reverse-engineering is at
  https://www.memotech.franken.de/FileFormats/Garmin_MPS_GDB_and_GFI_Format.pdf
*/

#include "gdb.h"

#include <QByteArray>               // for QByteArray, operator==
#include <QDate>                    // for QDate
#include <QDateTime>                // for QDateTime
#include <QList>                    // for QList<>::const_iterator, QList
#include <QString>                  // for QString, operator!=, operator==
#include <QTime>                    // for QTime
#include <Qt>                       // for CaseInsensitive
#include <QtGlobal>                 // for Q_UNUSED, qPrintable, foreach

#include <cmath>                    // for fabs
#include <cstdio>                   // for printf, SEEK_SET
#include <cstdlib>                  // for strtol
#include <cstring>                  // for memset, strstr, strcmp
#include <iterator>                 // for next

#include "defs.h"                   // for Waypoint, warning, route_head, fatal, UrlLink, bounds, mkshort, UrlList, unknown_alt, xfree, waypt_add_to_bounds, waypt_init_bounds, xstrtoi, mkshort_del_handle, route_add_wpt, route_disp_all, waypt_bounds_valid, xmalloc, gb_color, WaypointList, find_wa...
#include "formspec.h"               // for FormatSpecificDataList
#include "garmin_fs.h"              // for garmin_fs_t, garmin_ilink_t, garmin_fs_alloc
#include "garmin_tables.h"          // for gt_waypt_class_map_point, gt_color_index_by_rgb, gt_color_value, gt_waypt_classes_e, gt_find_desc_from_icon_number, gt_find_icon_number_from_desc, gt_gdb_display_mode_symbol, gt_get_icao_country, gt_waypt_class_user_waypoint, GDB, gt_display_mode_symbol
#include "gbfile.h"                 // for gbfgetint32, gbfputint32, gbfgetc, gbfread, gbfwrite, gbfgetdbl, gbfputc, gbfgetcstr, gbfclose, gbfgetnativecstr, gbfopen_le, gbfputint16, gbfile, gbfcopyfrom, gbfputcstr, gbfrewind, gbfseek, gbftell, gbfgetcstr_old, gbfgetint16, gbfgetuint32, gbfputdbl
#include "grtcirc.h"                // for RAD, gcdist, radtometers
#include "jeeps/gpsmath.h"          // for GPS_Math_Deg_To_Semi, GPS_Math_Semi_To_Deg
#include "src/core/datetime.h"      // for DateTime


#define MYNAME "gdb"

#define GDB_DEF_CLASS		gt_waypt_class_user_waypoint
#define GDB_DEF_HIDDEN_CLASS	gt_waypt_class_map_point

#define GDB_DBG_WPT		1
#define GDB_DBG_RTE		2
#define GDB_DBG_TRK		4

#define GDB_DBG_WPTe		8
#define GDB_DBG_RTEe		16
#define GDB_DBG_TRKe		32

#define GDB_DEBUG		(GDB_DBG_WPTe) /* | GDB_DBG_RTE) */
#undef GDB_DEBUG
// #define GDB_DEBUG 0xff

#define DBG(a, b)		if ((GDB_DEBUG & (a)) && (b))

void
GdbFormat::gdb_flush_waypt_queue(QList<Waypoint*>* Q)
{

  while (!Q->isEmpty()) {
    const Waypoint* wpt = Q->takeFirst();
    if (wpt->extra_data) {
      // wpt->extra_data may be holding a pointer to a QString, courtesy
      // the grossness at the end of write_waypoint_cb().
      delete static_cast<QString*>(wpt->extra_data);
    }
    delete wpt;
  }
}

#if GDB_DEBUG
void
GdbFormat::disp_summary(const gbfile* f) const
{
  int i, len;

  len = strlen(f->name);

  warning(MYNAME ": =====================");
  for (i = 0; i < len; i++) {
    warning("=");
  }
  warning("\n" MYNAME ": %s summary for \"%s\"\n",
          (f->mode == 'r') ? "Reader" : "Writer", f->name);

  warning(MYNAME ": ---------------------");
  for (i = 0; i < len; i++) {
    warning("-");
  }

  warning("\n" MYNAME ": %d waypoint(s)\n", waypt_ct - waypth_ct);
  warning(MYNAME ": %d hidden waypoint(s)\n", waypth_ct);
  warning(MYNAME ": %d route(s) with total %d point(s)\n", rte_ct, rtept_ct);
  warning(MYNAME ": %d track(s) with total %d point(s)\n", trk_ct, trkpt_ct);
  warning(MYNAME ": ---------------------");

  for (i = 0; i < len; i++) {
    warning("-");
  }
  warning("\n");
}
#else
void
GdbFormat::disp_summary(const gbfile* /* f */) const
{
}
#endif

/*******************************************************************************/
/* TOOLS AND MACROS FOR THE READER */
/*-----------------------------------------------------------------------------*/

#define FREAD_C gbfgetc(fin)
#define FREAD(a, b) gbfread(a, (b), 1, fin)
#define FREAD_i32 gbfgetint32(fin)
#define FREAD_i16 gbfgetint16(fin)
#define FREAD_DBL gbfgetdbl(fin)
#define FREAD_LATLON GPS_Math_Semi_To_Deg(gbfgetint32(fin))

#define FREAD_STR() gbfgetnativecstr(fin)

// This is all very messy.  Some versions of GDB store strings as
// 8859-1 strings and others as UTF8.  This wrapper tries to hide
// all that while (while keeping the character sets correct) and
// not pushing that decision  down into gbfread.  This module is
// still pretty messy and the points as to which fields are encode
// which ways in which versions are not at all clear, leading to
// encoding issues on read and leaks because of the differences
// in calling conventions on who owns/destroys the result.

#define FREAD_CSTR_AS_QSTR gbfgetcstr(fin)

QString GdbFormat::fread_cstr() const
{
  QString rv;
  char* s = gdb_fread_cstr(fin);
  if (gdb_ver >= kGDBVerUTF8) {
    rv = QString::fromUtf8(s);
  } else {
    rv = QString::fromLatin1(s);
  }

  xfree(s);

  return rv;
}

char*
GdbFormat::gdb_fread_cstr(gbfile* file_in)
{
  char* result = gbfgetcstr_old(file_in);

  if (result && (*result == '\0')) {
    xfree(result);
    result = nullptr;
  }

  return result;
}

QString
GdbFormat::gdb_fread_strlist() const
{
  QString res;

  int count = FREAD_i32;

  while (count > 0) {
    QString str = fread_cstr();
    if (!str.isEmpty()) {
      res = str;
    }
    count--;
  }

  QString qres = res;
  return qres;
}

Waypoint*
GdbFormat::gdb_find_wayptq(const QList<Waypoint*>* Q, const Waypoint* wpt, const char exact)
{
  QString name = wpt->shortname;
  foreach (Waypoint* tmp, *Q) {
    if (name.compare(tmp->shortname,Qt::CaseInsensitive) == 0) {
      if (! exact) {
        return tmp;
      }

      if ((tmp->latitude == wpt->latitude) &&
          (tmp->longitude == wpt->longitude)) {
        return tmp;
      }
    }
  }
  return nullptr;
}

Waypoint*
GdbFormat::gdb_reader_find_waypt(const Waypoint* wpt, const char exact) const
{
  Waypoint* res = gdb_find_wayptq(&wayptq_in, wpt, exact);
  if (res == nullptr) {
    res = gdb_find_wayptq(&wayptq_in_hidden, wpt, exact);
  }
  return res;
}

Waypoint*
GdbFormat::gdb_add_route_waypt(route_head* rte, Waypoint* ref, const int wpt_class) const
{
  Waypoint* tmp = gdb_reader_find_waypt(ref, 1);
  if (tmp == nullptr) {
    tmp = find_waypt_by_name(ref->shortname);
    if (tmp == nullptr) {
      route_add_wpt(rte, ref);
      return ref;
    }

    /* At this point we have found a waypoint with same name,
       but probably from another data stream. Check coordinates!
    */
    double dist = radtometers(gcdist(
                                RAD(ref->latitude), RAD(ref->longitude),
                                RAD(tmp->latitude), RAD(tmp->longitude)));

    if (fabs(dist) > 100) {
      fatal(MYNAME ": Route point mismatch!\n" \
            "  \"%s\" from waypoints differs to \"%s\"\n" \
            "  from route table by more than %0.1f meters!\n", \
            qPrintable(tmp->shortname), qPrintable(ref->shortname), dist);
    }
  }
  Waypoint* res = nullptr;
  int turn_point = (gdb_roadbook && (wpt_class > gt_waypt_class_map_point) && !tmp->description.isEmpty());
  if (turn_point || !gdb_hide_rpt || (wpt_class < gt_waypt_class_map_point)) {
    res = new Waypoint(*tmp);
    route_add_wpt(rte, res);
  }
  delete ref;
  return res;
}

QString GdbFormat::gdb_to_ISO8601_duration(unsigned int seconds)
{
  if (seconds == 0u) {
    return QString("PT0S");
  }
  unsigned int days = seconds / 86400u;
  QString out = "P";
  if (days != 0) {
    out.append(QStringLiteral("D%1").arg(days));
    seconds -= 86400u * days;
  }
  out.append(QString("T"));
  unsigned int hours = seconds / 3600u;
  if (hours != 0) {
    out.append(QStringLiteral("%1H").arg(hours));
    seconds -= 3600u * hours;
  }
  unsigned int minutes = seconds / 60u;
  if (minutes != 0) {
    out.append(QStringLiteral("%1M").arg(minutes));
    seconds -= 60u * minutes;
  }
  if (seconds != 0) {
    out.append(QStringLiteral("%1S").arg(seconds));
  }
  return out;
}

/*******************************************************************************/
/* TOOLS AND MACROS FOR THE WRITER */
/*-----------------------------------------------------------------------------*/
void GdbFormat::gdb_write_cstr(QStringView a) const
{
  if (a.isEmpty()) {
    gbfputc(0, fout);
    return;
  }
  if (gdb_ver >= kGDBVerUTF8) {
    gbfputcstr(a.toUtf8().constData(), fout);
  } else {
    gbfputcstr(a.toLatin1().constData(), fout);
  }
}

#define FWRITE_i16(a) gbfputint16((a), fout)
#define FWRITE_i32(a) gbfputint32((a), fout)
#define FWRITE(a, b) gbfwrite(a, (b), 1, fout)
#define FWRITE_C(a) gbfputc((a), fout)
#define FWRITE_DBL(a, b) gdb_write_dbl((a), (b))
#define FWRITE_TIME(a) gdb_write_time((a))
#define FWRITE_LATLON(a) gbfputint32(GPS_Math_Deg_To_Semi((a)), fout)

void
GdbFormat::gdb_write_cstr_list(QStringView str) const
{
  if (!str.isEmpty()) {
    gbfputint32(1, fout);
    gdb_write_cstr(str);
  } else {
    gbfputint32(0, fout);
  }
}

void
GdbFormat::gdb_write_dbl(const double value, const double def) const
{
  if (value == def) {
    gbfputc(0, fout);
  } else {
    gbfputc(1, fout);
    gbfputdbl(value, fout);
  }
}

void
GdbFormat::gdb_write_time(const int time) const
{
  if (time > 0) {
    gbfputc(1, fout);
    gbfputint32(time, fout);
  } else {
    gbfputc(0, fout);
  }
}

/*******************************************************************************/
/* GDB "Garmin Database" READER CODE */
/*-----------------------------------------------------------------------------*/

void
GdbFormat::read_file_header()
{
  char buf[128];

  /*
  	We are beginning with a simple binary read.
  */
  FREAD(buf, 6);
  /*
  	A "gbfgetcstr" (FREAD_CSTR) works too, but if we get a wrong file as input,
  	the file validation my be comes too late. For example a XML base file normally
  	has no binary zeros inside and produce, if big enough, a buffer overflow.
  	The following message "local buffer overflow detected..." could be
  	misinterpreted.
  */
  if (strcmp(buf, "MsRcf") != 0) {
    fatal(MYNAME ": Invalid file \"%s\"!", fin->name);
  }

  int reclen = FREAD_i32;
  Q_UNUSED(reclen);
  QByteArray drec = FREAD_STR();
  if (drec.at(0) != 'D') {
    fatal(MYNAME ": Invalid file \"%s\"!", fin->name);
  }

  gdb_ver = drec.at(1) - 'k' + 1;
  if ((gdb_ver < kGDBVerMin) || (gdb_ver > kGDBVerMax)) {
    fatal(MYNAME ": Unknown or/and unsupported GDB version (%d.0)!", gdb_ver);
  }

  if (global_opts.verbose_status > 0) {
    printf(MYNAME ": Reading Garmin GPS Database version %d.0\n", gdb_ver);
  }

  reclen = FREAD_i32;
  if (reclen + 1 > int(sizeof(buf))) {
    fatal(MYNAME ": Invalid record length\n");
  }
  (void) FREAD(buf, reclen + 1);
  if (global_opts.verbose_status > 0) {
    const char* name = buf+2;
    if (strstr(name, "SQA") == nullptr) {
      name = "MapSource";
    } else if (strstr(name, "neaderhi") == nullptr) {
      name = "MapSource BETA";
    }
    warning(MYNAME ": File created with \"%s\"\n", name);
  }

  QByteArray applicationField = FREAD_STR();
  if (!((applicationField == "MapSource") || (applicationField == "BaseCamp"))) {
    fatal(MYNAME ": Not a recognized signature in header");
  }
}

/*-----------------------------------------------------------------------------*/

Waypoint*
GdbFormat::read_waypoint(gt_waypt_classes_e* waypt_class_out)
{
  char buf[128];		/* used for temporary stuff */
  gt_waypt_classes_e wpt_class;
  Waypoint* res;
  garmin_fs_t* gmsd;
#ifdef GMSD_EXPERIMENTAL
  char subclass[22];
#endif
  waypt_ct++;
  res = new Waypoint;

  gmsd = garmin_fs_alloc(-1);
  res->fs.FsChainAdd(gmsd);
  res->shortname = fread_cstr();
  wpt_class = (gt_waypt_classes_e) FREAD_i32;
  garmin_fs_t::set_wpt_class(gmsd, wpt_class);
  if (wpt_class != 0) {
    waypth_ct++;
  }

  garmin_fs_t::set_cc(gmsd, fread_cstr());

#ifdef GMSD_EXPERIMENTAL
  FREAD(subclass, sizeof(subclass));
  if (gmsd && (wpt_class >= gt_waypt_class_map_point)) {
    memcpy(gmsd->subclass, subclass, sizeof(gmsd->subclass));
    gmsd->flags.subclass = 1;
  }
#else
  FREAD(buf, 22);
#endif
  res->latitude = FREAD_LATLON;
  res->longitude = FREAD_LATLON;

  if (FREAD_C == 1) {
    double alt = FREAD_DBL;
    if (alt < 1.0e24) {
      res->altitude = alt;
#if GDB_DEBUG
      DBG(GDB_DBG_WPTe, 1)
      printf(MYNAME "-wpt \"%s\" (%d): Altitude = %.1f\n",
             qPrintable(res->shortname), wpt_class, alt);
#endif
    }
  }
#if GDB_DEBUG
  DBG(GDB_DBG_WPT, 1)
  printf(MYNAME "-wpt \"%s\": coordinates = %c%0.6f %c%0.6f\n",
         qPrintable(res->shortname),
         res->latitude < 0 ? 'S' : 'N', res->latitude,
         res->longitude < 0 ? 'W' : 'E', res->longitude);
#endif
  res->notes = fread_cstr();
#if GDB_DEBUG
  DBG(GDB_DBG_WPTe, !res->notes.isNull())
  printf(MYNAME "-wpt \"%s\" (%d): notes = %s\n",
         qPrintable(res->shortname), wpt_class,
         qPrintable(QString(res->notes).replace("\r\n", ", ")));
#endif
  if (FREAD_C == 1) {
    res->set_proximity(FREAD_DBL);
#if GDB_DEBUG
    DBG(GDB_DBG_WPTe, 1)
    printf(MYNAME "-wpt \"%s\" (%d): Proximity = %.1f\n",
           qPrintable(res->shortname), wpt_class, res->proximity / 1000);
#endif
  }
  int display = FREAD_i32;
#if GDB_DEBUG
  DBG(GDB_DBG_WPTe, 1)
  printf(MYNAME "-wpt \"%s\" (%d): display = %d\n",
         qPrintable(res->shortname), wpt_class, display);
#endif
  switch (display) {			/* display value */
  case gt_gdb_display_mode_symbol:
    display = gt_display_mode_symbol;
    break;
  case gt_gdb_display_mode_symbol_and_comment:
    display = gt_display_mode_symbol_and_comment;
    break;
  default:
    display = gt_display_mode_symbol_and_name;
    break;
  }
  garmin_fs_t::set_display(gmsd, display);

  FREAD_i32;				/* color !not implemented! */
  int icon = FREAD_i32;
  garmin_fs_t::set_icon(gmsd, icon);
  garmin_fs_t::set_city(gmsd, fread_cstr());
  garmin_fs_t::set_state(gmsd, fread_cstr());
  garmin_fs_t::set_facility(gmsd, fread_cstr());

  FREAD(buf, 1);

  if (FREAD_C == 1) {
    res->set_depth(FREAD_DBL);
#if GDB_DEBUG
    DBG(GDB_DBG_WPTe, 1)
    printf(MYNAME "-wpt \"%s\" (%d): Depth = %.1f\n",
           qPrintable(res->shortname), wpt_class, res->depth);
#endif
  }

  /* VERSION DEPENDENT CODE */

  if (gdb_ver <= kGDBVer2) {

    FREAD(buf, 2);				/* ?????????????????????????????????? */
    waypt_flag = FREAD_C;
    if (waypt_flag == 0) {
      FREAD(buf, 3);
    } else {
      FREAD(buf, 2);
    }

#if GDB_DEBUG
    QString temp = FREAD_CSTR_AS_QSTR;				/* undocumented & unused string */
    DBG(GDB_DBG_WPTe, !temp.isEmpty())
    printf(MYNAME "-wpt \"%s\" (%d): Unknown string = %s\n",
           qPrintable(res->shortname), wpt_class, qPrintable(temp));
#else
    (void) FREAD_CSTR_AS_QSTR;				/* undocumented & unused string */
#endif

    QString linky = FREAD_CSTR_AS_QSTR;
    UrlLink l(linky);
    if (!linky.isEmpty()) {
      res->AddUrlLink(l);
    }
    if (wpt_class != 0) {
      res->description = l.url_;
    }
  } else { // if (gdb_ver >= kGDBVer3)

    waypt_flag = 0;

    garmin_fs_t::set_addr(gmsd, fread_cstr());

    FREAD(buf, 1);
    unsigned int duration = gbfgetuint32(fin);

    res->description = FREAD_CSTR_AS_QSTR;	/* instruction */
    if (wpt_class == gt_waypt_class_map_intersection || wpt_class == gt_waypt_class_map_line) {
      garmin_fs_t::set_duration(gmsd, duration);
      res->notes = QStringLiteral("[%1]").arg(gdb_to_ISO8601_duration(duration));
#if GDB_DEBUG
      DBG(GDB_DBG_WPTe, 1)
      printf(MYNAME "-wpt \"%s\" (%d): duration = %u\n",
             qPrintable(res->shortname), wpt_class, duration);
#endif
    }
    int url_ct = FREAD_i32;
    for (int i = url_ct; (i); i--) {
      QString str = FREAD_CSTR_AS_QSTR;
      if (!str.isEmpty()) {
        waypt_add_url(res, str, nullptr);
#if GDB_DEBUG
        DBG(GDB_DBG_WPTe, 1)
        printf(MYNAME "-wpt \"%s\" (%d): url(%d) = %s\n",
               qPrintable(res->shortname), wpt_class, url_ct - i, qPrintable(str));
#endif
      }
    }
  }

#if GDB_DEBUG
  DBG(GDB_DBG_WPTe, !res->description.isNull())
  printf(MYNAME "-wpt \"%s\" (%d): description = %s\n",
         qPrintable(res->shortname), wpt_class, qPrintable(res->description));
  DBG(GDB_DBG_WPTe, res->urls.HasUrlLink())
  printf(MYNAME "-wpt \"%s\" (%d): url = %s\n",
         qPrintable(res->shortname), wpt_class, qPrintable(res->urls.GetUrlLink().url_));
#endif
  int category = FREAD_i16;
  if (category != 0) {
    garmin_fs_t::set_category(gmsd, category);
  }
#if GDB_DEBUG
  DBG(GDB_DBG_WPTe, category)
  printf(MYNAME "-wpt \"%s\" (%d): category = %d\n",
         qPrintable(res->shortname), wpt_class, category);
#endif

  if (FREAD_C == 1) {
    res->set_temperature(FREAD_DBL);
#if GDB_DEBUG
    DBG(GDB_DBG_WPTe, 1)
    printf(MYNAME "-wpt \"%s\" (%d): temperature = %.1f\n",
           qPrintable(res->shortname), wpt_class, res->temperature);
#endif
  }

  /* VERSION DEPENDENT CODE */
  if (gdb_ver <= kGDBVer2) {
    if (waypt_flag != 0) {
      FREAD(buf, 1);
    }
  }
  if (FREAD_C == 1) {
    res->SetCreationTime(FREAD_i32);
  }

  /* VERSION DEPENDENT CODE */
  if (gdb_ver >= kGDBVer3) {
    if (FREAD_i32 == 1) {
      garmin_fs_t::set_phone_nr(gmsd, fread_cstr());
      (void) FREAD_STR();		/* ?? fax / mobile ?? */
    }
    garmin_fs_t::set_country(gmsd, fread_cstr());
    garmin_fs_t::set_postal_code(gmsd, fread_cstr());
  }

  res->icon_descr = gt_find_desc_from_icon_number(icon, GDB);

#if GDB_DEBUG
  DBG(GDB_DBG_WPTe, icon != kGDBDefIcon)
  printf(MYNAME "-wpt \"%s\" (%d): icon = \"%s\" (MapSource symbol %d)\n",
         qPrintable(res->shortname), wpt_class, qPrintable(res->icon_descr), icon);
#endif
  QString str;
  if (!(str = garmin_fs_t::get_cc(gmsd, nullptr)).isEmpty()) {
    if (!(garmin_fs_t::has_country(gmsd))) {
      garmin_fs_t::set_country(gmsd, gt_get_icao_country(str));
    }
  }
  if (gdb_roadbook && (wpt_class > gt_waypt_class_map_point) && !res->description.isEmpty()) {
    wpt_class = gt_waypt_class_user_waypoint;
    garmin_fs_t::set_wpt_class(gmsd, wpt_class);
#ifdef GMSD_EXPERIMENTAL
    garmin_fs_t::unset_subclass(gmsd);
#endif
  }
  *waypt_class_out = wpt_class;
  return res;
}

/*-----------------------------------------------------------------------------*/

route_head*
GdbFormat::read_route()
{
  rte_ct++;
  int warnings = 0;

  auto* rte = new route_head;
  rte->rte_name = fread_cstr();

  char tbuf[128];
  FREAD(tbuf, 1);			/* display/autoname - 1 byte */

  if (FREAD_C == 0) {		/* max. data flag */
    /* maxlat = */ (void) FREAD_i32;
    /* maxlon = */
    (void) FREAD_i32;
    if (FREAD_C == 1) { /* maxalt = */
      FREAD_DBL;
    }
    /* minlat = */ (void) FREAD_i32;
    /* minlon = */
    (void) FREAD_i32;
    if (FREAD_C == 1) { /* minalt = */
      FREAD_DBL;
    }
  }

  int points = FREAD_i32;

#if GDB_DEBUG
  DBG(GDB_DBG_RTE, 1)
  printf(MYNAME "-rte \"%s\": loading route with %d point(s)...\n",
         qPrintable(rte->rte_name), points);
#endif

  for (int i = 0; i < points; i++) {
    char buf[128];

    auto* wpt = new Waypoint;
    rtept_ct++;

    wpt->shortname = fread_cstr();	/* shortname */
    int wpt_class = FREAD_i32;		/* waypoint class */
    (void) FREAD_STR();			/* country code */
    FREAD(buf, 18 + 4);		/* subclass part 1-3 / unknown */

    if (FREAD_C != 0) {
      FREAD(buf, 8);		/* aviation data (?); only seen with class "1" (Airport) */
      /* VERSION DEPENDENT CODE */
      if (gdb_ver >= kGDBVer3) {
        FREAD(buf, 8);  /* a second block since V3 */
      }
    }

    FREAD(buf, 18);			/* unknown 18 bytes; but first should be 0x01 or 0x03 */
    /* seen also 0 with VER3 */
    if ((buf[0] != 0x00) && (buf[0] != 0x01) && (buf[0] != 0x03)) {
      warnings++;
      if (warnings > 3) {
        fatal(MYNAME "-rte_pt \"%s\": too many warnings!\n", qPrintable(wpt->shortname));
      }
      warning(MYNAME "-rte_pt \"%s\" (class %d): possible error in route.\n", qPrintable(wpt->shortname), wpt_class);
      warning(MYNAME "-rte_pt (dump):");
      for (int idx = 0; idx < 18; idx++) {
        warning(" %02x", (unsigned char)buf[idx]);
      }
      warning("\n");
    }

    int links = FREAD_i32;
    garmin_ilink_t* il_anchor = nullptr;
    garmin_ilink_t* il_root = nullptr;
#if GDB_DEBUG
    DBG(GDB_DBG_RTE, links)
    printf(MYNAME "-rte_pt \"%s\" (%d): %d interlink step(s)\n",
           qPrintable(wpt->shortname), wpt_class, links);
#endif
    for (int j = 0; j < links; j++) {
      auto* il_step = (garmin_ilink_t*) xmalloc(sizeof(garmin_ilink_t));

      il_step->ref_count = 1;

      il_step->lat = FREAD_LATLON;
      il_step->lon = FREAD_LATLON;
      if (FREAD_C == 1) {
        il_step->alt = FREAD_DBL;
      } else {
        il_step->alt = unknown_alt;
      }

      if (j == 0) {
        wpt->latitude = il_step->lat;
        wpt->longitude = il_step->lon;
        wpt->altitude = il_step->alt;
      }

      il_step->next = nullptr;
      if (il_anchor == nullptr) {
        il_root = il_step;
      } else {
        il_anchor->next = il_step;
      }
      il_anchor = il_step;

#if GDB_DEBUG
      DBG(GDB_DBG_RTEe, 1) {
        printf(MYNAME "-rte_il \"%s\" (%d of %d): %c%0.6f %c%0.6f\n",
               qPrintable(wpt->shortname), j + 1, links,
               il_step->lat < 0 ? 'S' : 'N', il_step->lat,
               il_step->lon < 0 ? 'W' : 'E', il_step->lon);
      }
#endif
    }

    bounds bounds;
    waypt_init_bounds(&bounds);

    if (FREAD_C == 0) {		/* interlink bounds */
      bounds.max_lat = FREAD_LATLON;
      bounds.max_lon = FREAD_LATLON;
      if (FREAD_C == 1) {
        bounds.max_alt = FREAD_DBL;
      }
      bounds.min_lat = FREAD_LATLON;
      bounds.min_lon = FREAD_LATLON;
      if (FREAD_C == 1) {
        bounds.min_alt = FREAD_DBL;
      }
    }

    if (links == 0) {
      /* Without links we need all information from wpt */
      Waypoint* tmp = gdb_reader_find_waypt(wpt, 0);
      if (tmp != nullptr) {
        delete wpt;
        wpt = new Waypoint(*tmp);
      } else {
        if (waypt_bounds_valid(&bounds)) {
          warning(MYNAME ": (has bounds)\n");
        }

        warning(MYNAME ": Data corruption detected!\n");
        fatal(MYNAME ": Sleeping route point without coordinates!\n");
      }
    }

    /* VERSION DEPENDENT CODE */
    if (gdb_ver >= kGDBVer2) {
      FREAD(buf, 8);
      if (gdb_ver >= kGDBVer3) {
        FREAD(buf, 2);
      }
    }
#if GDB_DEBUG
    DBG(GDB_DBG_RTE, 1)
    printf(MYNAME "-rte_pt \"%s\": coordinates = %c%0.6f, %c%0.6f\n",
           qPrintable(wpt->shortname),
           wpt->latitude < 0 ? 'S' : 'N', wpt->latitude,
           wpt->longitude < 0 ? 'W' : 'E', wpt->longitude);
#endif
    wpt = gdb_add_route_waypt(rte, wpt, wpt_class);
    if (wpt != nullptr) {
      garmin_fs_t* gmsd = garmin_fs_t::find(wpt);
      if (gmsd == nullptr) {
        gmsd = garmin_fs_alloc(-1);
        wpt->fs.FsChainAdd(gmsd);
      }
      garmin_fs_t::set_wpt_class(gmsd, wpt_class);
      gmsd->ilinks = il_root;
      il_root = nullptr;
    }

    while (il_root) {
      garmin_ilink_t* il = il_root;
      il_root = il_root->next;
      xfree(il);
    }
  } /* ENDFOR: for (i = 0; i < points; i++) */

  /* VERSION DEPENDENT CODE */
  if (gdb_ver <= kGDBVer2) {
    rte->rte_urls.AddUrlLink(UrlLink(fread_cstr()));
  } else {
    rte->rte_urls.AddUrlLink(UrlLink(gdb_fread_strlist()));

    int color_idx = FREAD_i32;
    rte->line_color.bbggrr = gt_color_value(color_idx);
    int autoroute = FREAD_C;
    if (autoroute == 1) {
      FREAD(tbuf, 6); /* unknown bytes */
      int route_style = FREAD_C;
      int calc_type = FREAD_i32;
      int vehicle_type = FREAD_C;
      int road_selection = FREAD_i32;
      double driving_speed[5];
      driving_speed[0] = FREAD_DBL;
      driving_speed[1] = FREAD_DBL;
      driving_speed[2] = FREAD_DBL;
      driving_speed[3] = FREAD_DBL;
      driving_speed[4] = FREAD_DBL;
      FREAD(tbuf, 8); /* unknown bytes */
#if GDB_DEBUG
      DBG(GDB_DBG_RTE, 1)
      printf(MYNAME "-rte_pt: autoroute info: route style %d, calculation type %d, vehicle type %d, road selection %d\n"
             "                            driving speeds (kph) %.0f, %.0f, %.0f, %.0f, %.0f\n",
             route_style, calc_type, vehicle_type, road_selection,
             driving_speed[0], driving_speed[1], driving_speed[2], driving_speed[3], driving_speed[4]);
#else
      Q_UNUSED(route_style);
      Q_UNUSED(calc_type);
      Q_UNUSED(vehicle_type);
      Q_UNUSED(road_selection);
      Q_UNUSED(driving_speed);
#endif
    }

    rte->rte_desc = fread_cstr();
#if 0
    /* replace CRLF's with ", " */
    if (rte->rte_desc) {
      char* c = rte->rte_desc;
      while ((c = strstr(c, "\r\n"))) {
        *c++ = ',';
        *c++ = ' ';
      }
    }
#endif
  }
  return rte;
}

/*-----------------------------------------------------------------------------*/

route_head*
GdbFormat::read_track()
{
  char dummy;

  trk_ct++;

  auto* res = new route_head;
  res->rte_name = fread_cstr();
//	res->rte_num = trk_ct;

  FREAD(&dummy, 1);		/* display - 1 byte */
  int color_idx = FREAD_i32;		/* color -  1 dword */
  res->line_color.bbggrr = gt_color_value(color_idx);

  int points = FREAD_i32;

  for (int index = 0; index < points; index++) {
    auto* wpt = new Waypoint;

    trkpt_ct++;

    wpt->latitude = FREAD_LATLON;
    wpt->longitude = FREAD_LATLON;
    if (FREAD_C == 1) {
      double alt = FREAD_DBL;
      if (alt < 1.0e24) {
        wpt->altitude = alt;
      }
    }
    if (FREAD_C == 1) {
      wpt->SetCreationTime(FREAD_i32);
    }
    if (FREAD_C == 1) {
      wpt->set_depth(FREAD_DBL);
    }
    if (FREAD_C == 1) {
      wpt->set_temperature(FREAD_DBL);
    }

    track_add_wpt(res, wpt);
  }

  /* VERSION DEPENDENT CODE */
  if (gdb_ver >= kGDBVer3) {
    res->rte_urls.AddUrlLink(UrlLink(gdb_fread_strlist()));
  } else { /* if (gdb_ver <= kGDBVer2) */
    res->rte_urls.AddUrlLink(UrlLink(FREAD_CSTR_AS_QSTR));
  }
#if GDB_DEBUG
  DBG(GDB_DBG_TRK, res->rte_urls.HasUrlLink())
  printf(MYNAME "-trk \"%s\": url = %s\n",
         qPrintable(res->rte_name), qPrintable(res->rte_urls.GetUrlLink().url_));
#endif
  return res;
}

/*******************************************************************************/

void
GdbFormat::rd_init(const QString& fname)
{
  fin = gbfopen_le(fname, "rb", MYNAME);
  ftmp = gbfopen_le(nullptr, "wb", MYNAME);
  read_file_header();

  wayptq_in.clear();
  wayptq_in_hidden.clear();

  bool via = gdb_opt_via;
  bool drop_wpt = gdb_opt_drop_hidden_wpt;
  gdb_roadbook = gdb_opt_roadbook;
  gdb_hide_wpt = via || drop_wpt || gdb_roadbook;
  gdb_hide_rpt = via || gdb_roadbook;

  waypt_ct = 0;
  waypth_ct = 0;
  rtept_ct = 0;
  trkpt_ct = 0;
  rte_ct = 0;
  trk_ct = 0;
}

void
GdbFormat::rd_deinit()
{
  disp_summary(fin);
  gdb_flush_waypt_queue(&wayptq_in);
  gdb_flush_waypt_queue(&wayptq_in_hidden);
  gbfclose(ftmp);
  gbfclose(fin);
}

void
GdbFormat::read()
{
  int incomplete = 0;	/* number of incomplete reads */

  for (;;) {
    char typ;
    gt_waypt_classes_e wpt_class;
    Waypoint* wpt;
    route_head* trk, *rte;

    int len = FREAD_i32;
    if (FREAD(&typ, 1) < 1) {
      fatal(MYNAME ": Attempt to read past EOF.");
    }
    if (typ == 'V') {
      break;  /* break the loop */
    }

    gbfrewind(ftmp);
    gbfwrite(nullptr, 0, 0, ftmp);	/* truncate */
    gbfcopyfrom(ftmp, fin, len);
    gbfrewind(ftmp);

    gbfile* fsave = fin;			/* swap standard 'fin' with cached input */
    fin = ftmp;

    char dump = 1;
    wpt_class = GDB_DEF_CLASS;

    switch (typ) {
    case 'W':
      wpt = read_waypoint(&wpt_class);
      if (!gdb_hide_wpt || (wpt_class == 0)) {
        waypt_add(wpt);
        auto* dupe = new Waypoint(*wpt);
        wayptq_in.append(dupe);
      } else {
        wayptq_in_hidden.append(wpt);
      }
      break;
    case 'R':
      rte = read_route();
      if (rte) {
        route_add_head(rte);
      }
      break;
    case 'T':
      trk = read_track();
      if (trk) {
        track_add_head(trk);
      }
      break;
    default:
      dump = 0;	/* make a dump only for main types */
      break;
    }

    int delta = len - gbftell(ftmp);
    if (delta > 1000000) {
      fatal("Internal consistency error.  Delta too big");
    }

    // Avoid finite loop on bogus beta files from '06.
    // THe 100000 is totally pulled from my hat.
    // is_fatal((delta > 1000000) || (delta < 0), "Internal GDB error; invalid delta.");

    if (dump && delta) {
      if (! incomplete++) {
        warning(MYNAME ":==========================================\n");
        warning(MYNAME ":===          W A R N I N G             ===\n");
      }
      if (typ == 'W')
        warning(MYNAME ":(%d%c-%02d): delta = %d (flag=%3d/%02x)-",
                gdb_ver, typ, wpt_class, delta, waypt_flag, waypt_flag);
      else {
        warning(MYNAME ":(%d%c): delta = %d -", gdb_ver, typ, delta);
      }
      if (delta > 0) {
        char* buf = (char*) xmalloc(delta);
        if (FREAD(buf, delta) < 1) {
          fatal(MYNAME ": Attempt to read past EOF.\n");
        }
        for (int i = 0; i < delta; i++) {
          warning(" %02x", (unsigned char)buf[i]);
        }
        xfree(buf);
      }
      warning("\n");
    }

    fin = fsave;
  }


  if (incomplete) {
    warning(MYNAME ":------------------------------------------\n");
    warning(MYNAME ": \"%s\"\n", fin->name);
    warning(MYNAME ":------------------------------------------\n");
    warning(MYNAME ":       Please mail this information\n");
    warning(MYNAME "     and, if you can, the used GDB file\n");
    warning(MYNAME ":  to gpsbabel-misc@lists.sourceforge.net\n");
    warning(MYNAME ":==========================================\n");
  }
}

/*******************************************************************************/

/*
 * reset_short_handle: used for waypoint, route and track names
 */
void
GdbFormat::reset_short_handle(const char* defname)
{
  if (short_h != nullptr) {
    mkshort_del_handle(&short_h);
  }

  short_h = mkshort_new_handle();

  setshort_length(short_h, kGDBNameBufferLen);
  setshort_badchars(short_h, "\r\n\t");
  setshort_mustupper(short_h, 0);
  setshort_mustuniq(short_h, 1);
  setshort_whitespace_ok(short_h, 1);
  setshort_repeating_whitespace_ok(short_h, 1);
  setshort_defname(short_h, defname);
}

/* ----------------------------------------------------------------------------*/

void
GdbFormat::write_header()
{
   /* This is our "Watermark" to show this file was created by GPSbabel */
   /* This used to be from CVS, and may be from git in the future */
  static const QDateTime gdb_release_dt = QDateTime(QDate(2011, 4, 14), QTime(1, 30, 1), Qt::UTC);

  FWRITE("MsRc", 4); // Signature
  FWRITE_i16(0x66);  // Primary File Format

  FWRITE_i32(2);     // Record Length in Bytes
  FWRITE_C('D');     // Record Type
  FWRITE_i16(gdb_ver + 0x6a); // File Format

  gbfile* fsave = fout;
  fout = ftmp;
  FWRITE_i16(605); // program version 6.5 -> 6*100 + 5
  gdb_write_cstr(QStringLiteral("GPSBabel-%1").arg(gpsbabel_version));
  gdb_write_cstr(gdb_release_dt.toString("MMM dd yyyy"));
  gdb_write_cstr(gdb_release_dt.toString("HH:mm:ss"));
  finalize_item(fsave, 'A');

  gdb_write_cstr(u"MapSource");		/* MapSource magic */
}

/*-----------------------------------------------------------------------------*/

/*
 * gdb_check_waypt: As implemented in waypt_add, but we have some leaks where
 *                  waypoints are modified after waypt_add. Maybe we need a data check
 *                  after each input module.
 */

void
GdbFormat::gdb_check_waypt(Waypoint* wpt)
{
  double lat_orig = wpt->latitude;
  double lon_orig = wpt->longitude;

  if (wpt->latitude < -90) {
    wpt->latitude += 180;
  } else if (wpt->latitude > +90) {
    wpt->latitude -= 180;
  }
  if (wpt->longitude < -180) {
    wpt->longitude += 360;
  } else if (wpt->longitude > +180) {
    wpt->longitude -= 360;
  }

  if ((wpt->latitude < -90) || (wpt->latitude > 90.0))
    fatal("Invalid latitude %f in waypoint %s.\n",
          lat_orig, !wpt->shortname.isEmpty() ? qPrintable(wpt->shortname) : "<no name>");
  if ((wpt->longitude < -180) || (wpt->longitude > 180.0))
    fatal("Invalid longitude %f in waypoint %s.\n",
          lon_orig, !wpt->shortname.isEmpty() ? qPrintable(wpt->shortname) : "<no name>");
}

/*-----------------------------------------------------------------------------*/

void
GdbFormat::write_waypoint(
  const Waypoint* wpt, const QString& shortname, garmin_fs_t* gmsd,
  const int icon, const int display)
{
  char zbuf[32], ffbuf[32];

  waypt_ct++;	/* increase informational number of written waypoints */

  memset(zbuf, 0, sizeof(zbuf));
  memset(ffbuf, 0xFF, sizeof(ffbuf));

  int wpt_class = wpt->wpt_flags.fmt_use;		/* trick */

  gdb_write_cstr(shortname);			/* unique (!!!) shortname */
  FWRITE_i32(wpt_class);			/* waypoint class */
  gdb_write_cstr(garmin_fs_t::get_cc(gmsd, ""));		/* country code */

  if (wpt_class != 0) {
    waypth_ct++;
  }

#ifdef GMSD_EXPERIMENTAL
  if (gmsd && gmsd->flags.subclass && (wpt_class >= gt_waypt_class_map_point)) {
    FWRITE(gmsd->subclass, sizeof(gmsd->subclass));
  } else
#endif
  {
    FWRITE(zbuf, 4);		/* subclass part 1 */
    FWRITE(ffbuf, 12);		/* subclass part 2 */
    FWRITE(zbuf, 2);		/* subclass part 3 */
    FWRITE(ffbuf, 4);		/* unknown */
  }

  FWRITE_LATLON(wpt->latitude);		/* latitude */
  FWRITE_LATLON(wpt->longitude);		/* longitude */
  FWRITE_DBL(wpt->altitude, unknown_alt);	/* altitude */
  if (!wpt->notes.isEmpty()) {
    gdb_write_cstr(wpt->notes);
  } else {
    gdb_write_cstr(wpt->description);
  }
  FWRITE_DBL(wpt->proximity_value_or(unknown_alt), unknown_alt);	/* proximity */
  FWRITE_i32(display);			/* display */
  FWRITE_i32(0);				/* color */
  FWRITE_i32(icon);			/* icon */
  gdb_write_cstr(garmin_fs_t::get_city(gmsd, ""));	/* city */
  gdb_write_cstr(garmin_fs_t::get_state(gmsd, ""));	/* state */
  gdb_write_cstr(garmin_fs_t::get_facility(gmsd, ""));	/* facility */
  FWRITE_C(0);				/* unknown */
  FWRITE_DBL(wpt->depth_value_or(unknown_alt), unknown_alt);	/* depth */

  /* VERSION DEPENDENT CODE */
  if (gdb_ver <= kGDBVer2) {
    FWRITE(zbuf, 3);
    FWRITE(zbuf, 4);
    QString ld;
    if (wpt->HasUrlLink()) {
      UrlLink l = wpt->GetUrlLink();
      ld = l.url_;
    }
    QString descr = (wpt_class < gt_waypt_class_map_point) ?
                    ld : wpt->description;
    if ((descr != nullptr) && (wpt_class >= gt_waypt_class_map_point) && \
        descr == CSTRc(wpt->shortname)) {
      descr.clear();
    }
    gdb_write_cstr(descr);
  } else { /* if (gdb_ver > kGDBVer3) */
//    url_link* url_next;
//    const char* str;
    QString str;

    if (wpt_class < gt_waypt_class_map_point) {	/* street address */
      str  = garmin_fs_t::get_addr(gmsd, "");
    } else {
      str = "";
    }
    gdb_write_cstr(str);
    FWRITE(zbuf, 5);				/* instruction dependent */

    /* GBD doesn't have a native description field */
    /* here we misuse the instruction field */
#if 1
    QString d = wpt->description;
    if (wpt->description == wpt->shortname) {
      d.clear();
    }
    if (str == wpt->notes) {
      d.clear();
    }
    gdb_write_cstr(d);				/* instruction */
#else
    str = wpt->description;
    if (str && (strcmp(str, wpt->shortname) == 0)) {
      str = NULL;
    }
    if (str && wpt->notes && (strcmp(str, wpt->notes) == 0)) {
      str = NULL;
    }
    gdb_write_cstr(str);				/* instruction */
#endif

    FWRITE_i32(wpt->urls.size());
    foreach (UrlLink l, wpt->urls) {
      gdb_write_cstr(l.url_);
    }
  }

  FWRITE_i16(garmin_fs_t::get_category(gmsd, gdb_category));
  FWRITE_DBL(wpt->temperature_value_or(0), 0);
  FWRITE_TIME(wpt->GetCreationTime().toTime_t());

  /* VERSION DEPENDENT CODE */
  if (gdb_ver >= kGDBVer3) {
    QString str = garmin_fs_t::get_phone_nr(gmsd, "");
    if (!str.isEmpty()) {
      FWRITE_i32(1);
      gdb_write_cstr(str);
      gdb_write_cstr();
    } else {
      FWRITE_i32(0);
    }
    gdb_write_cstr(garmin_fs_t::get_country(gmsd, ""));
    gdb_write_cstr(garmin_fs_t::get_postal_code(gmsd, ""));
  }
}

void
GdbFormat::route_compute_bounds(const route_head* rte, bounds* bounds)
{
  waypt_init_bounds(bounds);
  foreach (Waypoint* wpt, rte->waypoint_list) {
    gdb_check_waypt(wpt);
    waypt_add_to_bounds(bounds, wpt);
  }
}

void
GdbFormat::route_write_bounds(bounds* bounds) const
{
  if (waypt_bounds_valid(bounds)) {
    FWRITE_C(0);
    FWRITE_LATLON(bounds->max_lat);
    FWRITE_LATLON(bounds->max_lon);
    FWRITE_DBL(bounds->max_alt, unknown_alt);
    FWRITE_LATLON(bounds->min_lat);
    FWRITE_LATLON(bounds->min_lon);
    FWRITE_DBL(bounds->min_alt, -(unknown_alt));
  } else {
    FWRITE_C(1);
  }
}

void
GdbFormat::write_route(const route_head* rte, const QString& rte_name)
{
  bounds bounds;
  char zbuf[32], ffbuf[32];

  memset(zbuf, 0, sizeof(zbuf));
  memset(ffbuf, 0xFF, sizeof(ffbuf));

  gdb_write_cstr(rte_name);
  FWRITE_C(0);				/* display/autoname - 1 byte */

  route_compute_bounds(rte, &bounds);
  route_write_bounds(&bounds);

  int points = rte->rte_waypt_ct();
  FWRITE_i32(points);

  int index = 0;

  for (auto it =rte->waypoint_list.cbegin(); it != rte->waypoint_list.cend(); ++it) {

    Waypoint* wpt = *it;
    Waypoint* next = nullptr;
    if (index < points) {
      next = *std::next(it);
    }

    index++;
    rtept_ct++;	/* increase informational number of written route points */

    if (index == 1) {
      gdb_check_waypt(wpt);
    }
    if (index < points) {
      gdb_check_waypt(next);
    }

    Waypoint* test = gdb_find_wayptq(&wayptq_out, wpt, 1);
    if (test != nullptr) {
      wpt = test;
    } else {
      fatal(MYNAME ": Sorry, that should never happen!!!\n");
    }

    garmin_fs_t* gmsd = garmin_fs_t::find(wpt);

    /* extra_data may contain a modified shortname */
    gdb_write_cstr((wpt->extra_data) ? *static_cast<QString*>(wpt->extra_data) : wpt->shortname);

    int wpt_class = wpt->wpt_flags.fmt_use;			/* trick */

    FWRITE_i32(wpt_class);				/* waypoint class */
    gdb_write_cstr(garmin_fs_t::get_cc(gmsd, ""));			/* country */
#ifdef GMSD_EXPERIMENTAL
    if (gmsd && gmsd->flags.subclass && (wpt_class >= gt_waypt_class_map_point)) {
      FWRITE(gmsd->subclass, sizeof(gmsd->subclass));
    } else
#endif
    {
      FWRITE(zbuf, 4);				/* subclass part 1 */
      FWRITE(ffbuf, 12);				/* subclass part 2 */
      FWRITE(zbuf, 2);				/* subclass part 3 */
      FWRITE(ffbuf, 4);				/* unknown */
    }

    FWRITE_C(0);					/* unknown value or string */
    FWRITE_C(3);					/* unknown 18 bytes starting with 0x03 */
    FWRITE(zbuf, 3);
    FWRITE(ffbuf, 4);
    FWRITE(zbuf, 10);

    if (index == points) {
      FWRITE_i32(0);		/* no more steps */
      FWRITE_C(1);		/* skip bounds */
    } else { /* if (index < points) */
      FWRITE_i32(2);		/* two interstep links */

      FWRITE_LATLON(wpt->latitude);
      FWRITE_LATLON(wpt->longitude);
      FWRITE_DBL(wpt->altitude, unknown_alt);
      FWRITE_LATLON(next->latitude);
      FWRITE_LATLON(next->longitude);
      FWRITE_DBL(next->altitude, unknown_alt);

      waypt_init_bounds(&bounds);
      waypt_add_to_bounds(&bounds, wpt);
      waypt_add_to_bounds(&bounds, next);
      route_write_bounds(&bounds);

    }

    /* VERSION DEPENDENT CODE */
    if (gdb_ver >= kGDBVer2) {
      FWRITE(ffbuf, 8);
      if (gdb_ver >= kGDBVer3) {
        FWRITE(zbuf, 2);
      }
    }
  }

  /* VERSION DEPENDENT CODE */
  if (gdb_ver <= kGDBVer2) {
    if (rte->rte_urls.HasUrlLink()) {
      gdb_write_cstr(rte->rte_urls.GetUrlLink().url_);
    } else {
      gdb_write_cstr();
    }
  } else { /* if (gdb_ver >= kGDBVer3) */
    if (rte->rte_urls.HasUrlLink()) {
      gdb_write_cstr_list(rte->rte_urls.GetUrlLink().url_);
    } else {
      gdb_write_cstr_list();
    }
    /* "Magenta" (14) is MapSource default */
    FWRITE_i32((rte->line_color.bbggrr < 0) ? 14 : gt_color_index_by_rgb(rte->line_color.bbggrr));
    FWRITE_C(0);
    gdb_write_cstr(rte->rte_desc);
  }
}

void
GdbFormat::write_track(const route_head* trk, const QString& trk_name)
{
  int points = trk->rte_waypt_ct();

  gdb_write_cstr(trk_name);
  FWRITE_C(0);
  /* "Unknown" (0) is MapSource default */
  FWRITE_i32(gt_color_index_by_rgb(trk->line_color.bbggrr));

  FWRITE_i32(points);	/* total number of waypoints in waypoint list */

  foreach (const Waypoint* wpt, trk->waypoint_list) {

    trkpt_ct++;	/* increase informational number of written route points */

    FWRITE_LATLON(wpt->latitude);
    FWRITE_LATLON(wpt->longitude);
    FWRITE_DBL(wpt->altitude, unknown_alt);
    FWRITE_TIME(wpt->GetCreationTime().toTime_t());
    double d = wpt->depth_value_or(unknown_alt);
    FWRITE_DBL(d, unknown_alt);
    d = wpt->temperature_value_or(-99999);
    FWRITE_DBL(d, -99999);
  }

  /* finalize track */

  /* VERSION DEPENDENT CODE */
  if (gdb_ver <= kGDBVer2) {
    if (trk->rte_urls.HasUrlLink()) {
      gdb_write_cstr(trk->rte_urls.GetUrlLink().url_);
    } else {
      gdb_write_cstr();
    }
  } else { /* if (gdb_ver >= kGDBVer3 */
    if (trk->rte_urls.HasUrlLink()) {
      gdb_write_cstr_list(trk->rte_urls.GetUrlLink().url_);
    } else {
      gdb_write_cstr_list();
    }
  }
}

/*-----------------------------------------------------------------------------*/

void
GdbFormat::finalize_item(gbfile* origin, const char identifier)
{
  int len = gbftell(fout);

  fout = origin;
  gbfseek(ftmp, 0, SEEK_SET);

  FWRITE_i32(len);
  FWRITE_C(identifier);
  gbfcopyfrom(fout, ftmp, len);

  gbfseek(ftmp, 0, SEEK_SET);	/* Truncate memory stream */
  gbfwrite(nullptr, 0, 0, ftmp);
}

/*-----------------------------------------------------------------------------*/

void
GdbFormat::write_waypoint_cb(const Waypoint* refpt)
{
  /* do this when backup always happens in main */
// but, but, casting away the const here is wrong...
  (const_cast<Waypoint*>(refpt))->shortname = refpt->shortname.trimmed();
  Waypoint* test = gdb_find_wayptq(&wayptq_out, refpt, 1);

  if (refpt->HasUrlLink() && test && test->HasUrlLink() && route_flag == 0) {
    UrlLink orig_link = refpt->GetUrlLink();
    UrlLink test_link = test->GetUrlLink();
    if (orig_link.url_ != test_link.url_) {
      test = nullptr;
    }
  }

  if ((test != nullptr) && (route_flag == 0)) {
    if (test->notes != refpt->notes) {
      test = nullptr;
    }
  }

  if (test == nullptr) {
    int display;
    auto* wpt = new Waypoint(*refpt);

    gdb_check_waypt(wpt);
    wayptq_out.append(wpt);

    gbfile* fsave = fout;
    fout = ftmp;

    /* prepare the waypoint */
    garmin_fs_t* gmsd = garmin_fs_t::find(wpt);

    int wpt_class = garmin_fs_t::get_wpt_class(gmsd, -1);
    if (wpt_class == -1) {
      wpt_class = (route_flag) ? GDB_DEF_HIDDEN_CLASS : GDB_DEF_CLASS;
    }
    wpt->wpt_flags.fmt_use = wpt_class; 	/* trick, we need this for the route(s) */

    int icon = garmin_fs_t::get_icon(gmsd, -1);
    if (icon < 0) {
      if (wpt->icon_descr.isNull()) {
        icon = kGDBDefIcon;
      } else {
        icon = gt_find_icon_number_from_desc(wpt->icon_descr, GDB);
      }
    }

    switch (garmin_fs_t::get_display(gmsd, -1)) {		/* display */
    case -1:
      if (wpt_class < 8) {
        display = gt_gdb_display_mode_symbol_and_name;
      } else {
        display = gt_gdb_display_mode_symbol;
      }
      break;
    case gt_display_mode_symbol:
      display = gt_gdb_display_mode_symbol;
      break;
    case gt_display_mode_symbol_and_comment:
      display = gt_gdb_display_mode_symbol_and_comment;
      break;
    default:
      display = gt_gdb_display_mode_symbol_and_name;
      break;
    }

    QString name = wpt->shortname;

    if (global_opts.synthesize_shortnames || name.isEmpty()) {
      name = wpt->notes;
      if (name.isEmpty()) {
        name = wpt->description;
      }
      if (name.isEmpty()) {
        name = wpt->shortname;
      }
    }

    name = mkshort(short_h, name);
    wpt->extra_data = new QString(name);
    write_waypoint(wpt, name, gmsd, icon, display);

    finalize_item(fsave, 'W');
  }
}

void
GdbFormat::write_route_cb(const route_head* rte)
{
  if (rte->rte_waypt_empty()) {
    return;
  }

  QString name;
  if (rte->rte_name.isNull()) {
    name = mkshort(short_h, QString::asprintf("Route%04d", rte->rte_num));
  } else {
    name = mkshort(short_h, rte->rte_name);
  }

  rte_ct++;	/* increase informational number of written routes */

  gbfile* fsave = fout;
  fout = ftmp;
  write_route(rte, name);
  finalize_item(fsave, 'R');
}

void
GdbFormat::write_track_cb(const route_head* trk)
{
  if (trk->rte_waypt_empty()) {
    return;
  }

  QString name;
  if (trk->rte_name.isNull()) {
    name = mkshort(short_h, QString::asprintf("Track%04d", trk->rte_num));
  } else {
    name = mkshort(short_h, trk->rte_name);
  }

  trk_ct++;	/* increase informational number of written tracks */

  gbfile* fsave = fout;
  fout = ftmp;
  write_track(trk, name);

  finalize_item(fsave, 'T');
}

/*-----------------------------------------------------------------------------*/

void
GdbFormat::wr_init(const QString& fname)
{
  fout = gbfopen_le(fname, "wb", MYNAME);
  ftmp = gbfopen_le(nullptr, "wb", MYNAME);

  gdb_category = (gdb_opt_category) ? xstrtoi(gdb_opt_category, nullptr, 10) : 0;
  gdb_ver = (gdb_opt_ver && *gdb_opt_ver) ? xstrtoi(gdb_opt_ver, nullptr, 10) : 0;

  if (gdb_category) {
    if ((gdb_category < 1) || (gdb_category > 16)) {
      fatal(MYNAME ": cat must be between 1 and 16!");
    }
    gdb_category = 1 << (gdb_category - 1);
  }

  if (gdb_opt_bitcategory) {
    gdb_category = strtol(gdb_opt_bitcategory, nullptr, 0);
  }

  wayptq_out.clear();
  short_h = nullptr;

  waypt_ct = 0;
  waypth_ct = 0;
  rtept_ct = 0;
  trkpt_ct = 0;
  rte_ct = 0;
  trk_ct = 0;
}

void
GdbFormat::wr_deinit()
{
  disp_summary(fout);
  gdb_flush_waypt_queue(&wayptq_out);
  mkshort_del_handle(&short_h);
  gbfclose(fout);
  gbfclose(ftmp);
}

void
GdbFormat::write()
{
  if (gdb_opt_ver) {
    gdb_ver = xstrtoi(gdb_opt_ver, nullptr, 10);
  }
  write_header();

  reset_short_handle("WPT");
  route_flag = 0;
  auto write_waypoint_cb_lambda = [this](const Waypoint* waypointp)->void {
    write_waypoint_cb(waypointp);
  };
  waypt_disp_all(write_waypoint_cb_lambda);
  route_flag = 1;
  route_disp_all(nullptr, nullptr, write_waypoint_cb_lambda);

  reset_short_handle("Route");
  auto write_route_cb_lambda = [this](const route_head* rte)->void {
    write_route_cb(rte);
  };
  route_disp_all(write_route_cb_lambda, nullptr, nullptr);

  reset_short_handle("Track");
  auto write_track_cb_lambda = [this](const route_head* rte)->void {
    write_track_cb(rte);
  };
  track_disp_all(write_track_cb_lambda, nullptr, nullptr);

  FWRITE_i32(2);			/* finalize gdb with empty map segment */
  gdb_write_cstr(u"V");
  FWRITE_C(1);
}
