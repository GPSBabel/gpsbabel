/*

    Support for Garmin Points of Interest (.gpi files)

    Copyright (C) 2007 Olaf Klein, o.b.klein@gpsbabel.org
    Copyright (C) 2007-2012 Robert Lipe, robertlipe+source@gpsbabel.org

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

#include "garmin_gpi.h"

#include <QByteArray>              // for QByteArray, operator==
#include <QDateTime>               // for QDateTime
#include <QList>                   // for QList
#include <QString>                 // for QString, operator+, operator<
#include <QThread>                 // for QThread
#include <Qt>                      // for CaseInsensitive
#include <QtGlobal>                // for foreach, Q_UNUSED

#include <algorithm>               // for stable_sort
#include <cctype>                  // for tolower
#include <cstdint>                 // for uint32_t, int32_t
#include <cstdio>                  // for SEEK_CUR, SEEK_SET
#include <cstring>                 // for strlen, strncmp
#include <ctime>                   // for time, time_t
#include <memory>                  // for unique_ptr

#include "defs.h"
#include "formspec.h"              // for FormatSpecificDataList
#include "garmin_fs.h"             // for garmin_fs_t, garmin_fs_alloc
#include "gbfile.h"                // for gbfputint32, gbfgetint32, gbfgetint16, gbfputint16, gbfgetc, gbfputc, gbfread, gbftell, gbfwrite, gbfseek, gbfclose, gbfopen_le, gbfgetuint16, gbsize_t, gbfile
#include "jeeps/gpsmath.h"         // for GPS_Math_Deg_To_Semi, GPS_Math_Semi_To_Deg


#define MYNAME "garmin_gpi"

#define DEFAULT_ICON  "Waypoint"
#define WAYPOINTS_PER_BLOCK  128

/* flags used in the gpi address mask */
#define GPI_ADDR_CITY       1
#define GPI_ADDR_COUNTRY     2
#define GPI_ADDR_STATE       4
#define GPI_ADDR_POSTAL_CODE 8
#define GPI_ADDR_ADDR      16

#define GPI_BITMAP_SIZE sizeof(gpi_bitmap)

#define GPI_DBG global_opts.debug_level >= 3
#define PP if (GPI_DBG) warning("@%1$6x (%1$8d): ", gbftell(fin))

/*******************************************************************************
* %%%                             gpi reader                               %%% *
*******************************************************************************/

/* look for or initialize GMSD */
garmin_fs_t*
GarminGPIFormat::gpi_gmsd_init(Waypoint* wpt)
{
  if (wpt == nullptr) {
    fatal(MYNAME ": Error in file structure.\n");
  }
  garmin_fs_t* gmsd = garmin_fs_t::find(wpt);
  if (gmsd == nullptr) {
    gmsd = garmin_fs_alloc(-1);
    wpt->fs.FsChainAdd(gmsd);
  }
  return gmsd;
}

GarminGPIFormat::lc_string
GarminGPIFormat::gpi_read_lc_string() const
{
  lc_string result;

  result.lc.resize(2);
  gbfread(result.lc.data(), 1, 2, fin);
  if ((result.lc.at(0) < 'A') || (result.lc.at(0) > 'Z') ||
      (result.lc.at(1) < 'A') || (result.lc.at(1) > 'Z')) {
    fatal(MYNAME ": Invalid language code %s!\n", result.lc.constData());
  }

  result.strlen = gbfgetint16(fin);

  PP;
  if (result.strlen > 0) {
    result.str.resize(result.strlen);
    gbfread(result.str.data(), 1, result.strlen, fin);
  }

  return result;
}

/* read a standard string with or without 'EN' (or whatever) header */
QString
GarminGPIFormat::gpi_read_string(const char* field) const
{
  QByteArray string;

  int l0 = gbfgetint16(fin);
  if (l0 > 0) {
    char first = gbfgetc(fin);
    if (first == 0) {

      if (gbfgetc(fin) != 0) {
        fatal(MYNAME ": Error reading field '%s'!", field);
      }

      lc_string res1 = gpi_read_lc_string();
      if ((res1.strlen + 4) < l0) { // dual language?
        lc_string res2 = gpi_read_lc_string();
        if (res1.strlen + 4 + res2.strlen + 4 != l0) {
          fatal(MYNAME ": Error out of sync (wrong size %d/%d/%d) on field '%s'!", l0, res1.strlen, res2.strlen, field);
        }
        if (opt_lang && (opt_lang  == res1.lc)) {
          string = res1.str;
        } else if (opt_lang && (opt_lang == res2.lc)) {
          string = res2.str;
        } else {
          fatal(MYNAME ": Must select language code, %s and %s found.\n", res1.lc.constData(), res2.lc.constData());
        }
      } else { // normal case, single language
        if (res1.strlen + 4 != l0) {
          fatal(MYNAME ": Error out of sync (wrong size %d/%d) on field '%s'!", l0, res1.strlen, field);
        }
        string = res1.str;
      }
    } else {
      string.resize(l0);
      string[0] = first;
      PP;
      if (l0 > 1) {
        gbfread(string.data() + 1, 1, l0 - 1, fin);
      }
    }
  }

  QString result = str_to_unicode(string).trimmed();
  if (GPI_DBG) {
    warning("%s: \"%s\"\n", field, result.isNull() ? "<NULL>" : qPrintable(result));
  }
  return result;
}

void
GarminGPIFormat::read_header()
{
  int len, i;

  i = gbfgetint32(fin);
  if (i != 0) {
    i = gbfgetint32(fin);
  }
  rdata->D2 = gbfgetint32(fin);

  gbfread(&rdata->S3, 1, sizeof(rdata->S3) - 1, fin);  /* GRMRECnn */
  if (strncmp(rdata->S3, "GRMREC", 6) != 0) {
    fatal(MYNAME ": No GPI file!\n");
  }

  PP;
  rdata->crdate = gbfgetint32(fin);
  if (GPI_DBG) {
    time_t crdate = GPS_Math_Gtime_To_Utime(rdata->crdate);
    warning("crdate = %lld (%s)\n", (long long) rdata->crdate,
            CSTR(QDateTime::fromSecsSinceEpoch(crdate, Qt::UTC).toString(Qt::ISODate)));
  }

  (void) gbfgetint16(fin);  /* 0 */

  len = gbfgetint16(fin);
  gbfseek(fin, len, SEEK_CUR);  /* "my.gpi" */

  i =  gbfgetint32(fin);  /* 1 */
  (void) gbfgetint32(fin);  /* 12 */
  /* There are two dwords next.  On most typical files, they're
  * "1" and "12".  On files from garminoneline.de/extras/poi, the
  * next two words are "15" and "5" and there's 17 additional bytes
  * that I can't identify.   So hardcode a seek here for now.
  */
  if (i == 15) {
    gbfseek(fin, 17, SEEK_CUR);
  }

  gbfread(&rdata->POI, 1, sizeof(rdata->POI) - 1, fin);

  if (strncmp(rdata->POI, "POI", 3) != 0) {
    fatal(MYNAME ": Wrong or unsupported GPI file!\n");
  }

  for (i = 0; i < 3; i++) {
    (void)gbfgetc(fin);
  }
  gbfread(&rdata->S8, 1, sizeof(rdata->S8) - 1, fin);

  codepage = gbfgetuint16(fin);
  if (GPI_DBG) {
    PP;
    warning("Code Page: %d\n",codepage);
  }
  (void) gbfgetint16(fin);     /* typically 0, but  0x11 in
            Garminonline.de files.  */

  if (GPI_DBG) {
    PP;
    warning("< leaving header\n");
  }
}

/* read a single poi with all options */
void
GarminGPIFormat::read_poi(const int sz, const int tag)
{
  if (GPI_DBG) {
    PP;
    warning("> reading poi (size %d)\n", sz);
  }
  PP;
  int len = 0;
  if (tag == 0x80002) {
    len = gbfgetint32(fin);  /* sub-header size */
  }
  if (GPI_DBG) {
  warning("poi sublen = %d (0x%x)\n", len, len);
  }
  (void) len;
  int pos = gbftell(fin);

  auto* wpt = new Waypoint;
  wpt->icon_descr = DEFAULT_ICON;

  wpt->latitude = GPS_Math_Semi_To_Deg(gbfgetint32(fin));
  wpt->longitude = GPS_Math_Semi_To_Deg(gbfgetint32(fin));

  (void) gbfgetint16(fin);  /* ? always 1 ? */
  (void) gbfgetc(fin);    /* seems to 1 when extra options present */
  wpt->shortname = gpi_read_string("Shortname");

  while (gbftell(fin) < (gbsize_t)(pos + sz - 4)) {
    int skip_tag = gbfgetint32(fin);
    if (! read_tag("read_poi", skip_tag, wpt)) {
      break;
    }
  }

  if (wpt->description.isEmpty() && !wpt->notes.isEmpty()) {
    wpt->description = wpt->notes;
  }
  if (wpt->notes.isEmpty() && !wpt->description.isEmpty()) {
    wpt->notes = wpt->description;
  }

  waypt_add(wpt);

  if (GPI_DBG) {
    PP;
    warning("< leaving poi\n");
  }
}

/* read poi's following a group header */
void
GarminGPIFormat::read_poi_list(const int sz)
{
  int pos = gbftell(fin);
  if (GPI_DBG) {
    PP;
    warning("> reading poi list (-> %x / %d )\n", pos + sz, pos + sz);
  }
  PP;
  int i = gbfgetint32(fin);  /* mostly 23 (0x17) */
  if (GPI_DBG) {
    warning("list sublen = %d (0x%x)\n", i, i);
  }
  (void) i;

  (void) gbfgetint32(fin);  /* max-lat */
  (void) gbfgetint32(fin);  /* max-lon */
  (void) gbfgetint32(fin);  /* min-lat */
  (void) gbfgetint32(fin);  /* min-lon */

  (void) gbfgetc(fin);    /* three unknown bytes */
  (void) gbfgetc(fin);    /* ? should be zero ? */
  (void) gbfgetc(fin);

  (void) gbfgetint32(fin);  /* ? const 0x1000100 ? */

  while (gbftell(fin) < (gbsize_t)(pos + sz - 4)) {
    int tag = gbfgetint32(fin);
    if (! read_tag("read_poi_list", tag, nullptr)) {
      return;
    }
  }
  if (GPI_DBG) {
    PP;
    warning("< leaving poi list\n");
  }
}

void
GarminGPIFormat::read_poi_group(const int sz, const int tag)
{
  int pos = gbftell(fin);
  if (GPI_DBG) {
    PP;
    warning("> reading poi group (-> %x / %d)\n", pos + sz, pos + sz);
  }
  if (tag == 0x80009) {
    PP;
    int subsz = gbfgetint32(fin);  /* ? offset to category data ? */
    if (GPI_DBG) {
      warning("group sublen = %d (-> %x / %d)\n", subsz, pos + subsz + 4, pos + subsz + 4);
    }
    (void)subsz;
  }
  rdata->group = gpi_read_string("Group");

  while (gbftell(fin) < (gbsize_t)(pos + sz)) {
    int subtag = gbfgetint32(fin);
    if (! read_tag("read_poi_group", subtag, nullptr)) {
      break;
    }
  }

  if (GPI_DBG) {
    PP;
    warning("< leaving poi group\n");
  }
}

// TODO: 'tag' is probably not a 32 bit value.
// most likely it's a pair of 16's: the first pair is the tag number.
// if the second 16 is "eight", then it's an
// extended thingy and it has a 4-byte extended record length (total number
// of bytes for all record fields and all nested records, starting after the
// length field)
/* gpi tag handler */
int
GarminGPIFormat::read_tag(const char* caller, const int tag, Waypoint* wpt)
{
  Q_UNUSED(caller);
  int dist;
  double speed;
  short mask;
  QString str;
  garmin_fs_t* gmsd;

  int sz = gbfgetint32(fin);
  int pos = gbftell(fin);

  if (GPI_DBG) {
    PP;
    warning("%s: tag = 0x%x (size %d)\n", caller, tag, sz);
  }
  if ((tag >= 0x80000) && (tag <= 0x800ff)) {
    sz += 4;
  }

  switch (tag) {
  case 0x3:  /* size = 12 */
  case 0x80003:  /* size = 12 */

    dist = gbfgetint16(fin);    /* proximity distance in meters */
    speed = (double)gbfgetint16(fin) / 100;  /* speed in meters per second */

    if (dist > 0) {
      wpt->set_proximity(dist);
    }
    if (speed > 0) {
      /* speed isn't part of a normal waypoint
      wpt->set_speed(speed);
      */
      if ((wpt->shortname.isEmpty()  || ((wpt->shortname).indexOf('@'))==-1)) {
        if (units == 's') {
          speed = MPS_TO_MPH(speed);
        } else {
          speed = MPS_TO_KPH(speed);
        }
        QString base = wpt->shortname.isEmpty() ? "WPT" : wpt->shortname;
        wpt->shortname = base + QStringLiteral("@%1").arg(speed,0,'f',0);
      }
    }

    (void) gbfgetint32(fin);
    (void) gbfgetint32(fin);
    break;

  case 0x4:  /* size = 2  ? */
  case 0x6:  /* size = 2  ? */
    break;

  case 0x5:  /* group bitmap */
    break;

  case 0x7:
    (void) gbfgetint16(fin);  /* category number */
    rdata->category = gpi_read_string("Category");
    break;

  case 0xa:
    wpt->description = gpi_read_string("Description");
    break;

  case 0xe:  /* ? notes or description / or both ? */
    mask = gbfgetc(fin);
    // Olaf's code called this a mask, but the bits below have nothing
    // in common.  I'm wondering if that first byte is something else and
    // a type e is always a note.
    switch (mask) {
    case 0x01:
    case 0x05:
    case 0x32:
      str = gpi_read_string("Notes");
    default:
      break;
    }

    if (!wpt->description.isEmpty()) {
      wpt->notes = str;
    } else {
      wpt->description = str;
    }
    break;

  case 0x2:
  case 0x80002:
    read_poi(sz, tag);
    break;

  case 0x80008:
    read_poi_list(sz);
    break;

  case 0x9:  /* ? older versions / no category data ? */
  case 0x80009:  /* current POI loader */
    read_poi_group(sz, tag);
    break;

  case 0x8000b:  /* address (street/city...) */
    (void) gbfgetint32(fin);
  // FALLTHROUGH
  case 0xb:  /* as seen in German POI files. */
    PP;
    mask = gbfgetint16(fin); /* address fields mask */
    if (GPI_DBG) {
      warning("GPI Address field mask: %d (0x%02x)\n", mask, mask);
    }
    if ((mask & GPI_ADDR_CITY) && !(str = gpi_read_string("City")).isEmpty()) {
      gmsd = gpi_gmsd_init(wpt);
      garmin_fs_t::set_city(gmsd, str);
    }
    if ((mask & GPI_ADDR_COUNTRY) && !(str = gpi_read_string("Country")).isEmpty()) {
      gmsd = gpi_gmsd_init(wpt);
      garmin_fs_t::set_country(gmsd, str);
    }
    if ((mask & GPI_ADDR_STATE) && !(str = gpi_read_string("State")).isEmpty()) {
      gmsd = gpi_gmsd_init(wpt);
      garmin_fs_t::set_state(gmsd, str);
    }
    if ((mask & GPI_ADDR_POSTAL_CODE) && !(str = gpi_read_string("Postal code")).isEmpty()) {
      gmsd = gpi_gmsd_init(wpt);
      garmin_fs_t::set_postal_code(gmsd, str);
    }
    if ((mask & GPI_ADDR_ADDR) && !(str = gpi_read_string("Street address")).isEmpty()) {
      gmsd = gpi_gmsd_init(wpt);
      garmin_fs_t::set_addr(gmsd, str);
    }
    break;

  case 0xc:
    mask = gbfgetint16(fin);
    if ((mask & 1) && !(str = gpi_read_string("Phone")).isEmpty()) {
      gmsd = gpi_gmsd_init(wpt);
      garmin_fs_t::set_phone_nr(gmsd, str);
    }
    if ((mask & 2) && !(str = gpi_read_string("Phone2")).isEmpty()) {
      gmsd = gpi_gmsd_init(wpt);
      garmin_fs_t::set_phone_nr2(gmsd, str);
    }
    if ((mask & 4) && !(str = gpi_read_string("Fax")).isEmpty()) {
      gmsd = gpi_gmsd_init(wpt);
      garmin_fs_t::set_fax_nr(gmsd, str);
    }
    if ((mask & 8) && !(str = gpi_read_string("Email")).isEmpty()) {
      gmsd = gpi_gmsd_init(wpt);
      garmin_fs_t::set_email(gmsd, str);
    }
    if ((mask & 0x10) && (str = gpi_read_string("Link"), !str.isEmpty())) {
      waypt_add_url(wpt, str, str);
    }
    break;

  case 0x8000c:  /* phone-number */
    (void) gbfgetint32(fin);
    PP;

    mask = gbfgetint16(fin); /* phone fields mask */
    if (GPI_DBG) {
      warning("GPI Phone field mask: %d (0x%02x)\n", mask, mask);
    }
    if ((mask & 1) && !(str = gpi_read_string("Phone")).isEmpty()) {
      gmsd = gpi_gmsd_init(wpt);
      garmin_fs_t::set_phone_nr(gmsd, str);
    }
    break;

  case 0x80012:  /* ? sounds / images ? */
    break;

  /* Images? Seen in http://geepeeex.com/Stonepages.gpi */
  case 0xd:
    break;

  case 0x11:
  case 0x80007:
    /* Looks like some kind of calendar information. */
    if (GPI_DBG) {
      int x;
      std::unique_ptr<unsigned char[]> b(new unsigned char[sz]);
      fprintf(stderr, "Tag: %x\n", tag);
      gbfread(b.get(), 1, sz, fin);
      fprintf(stderr, "\n");
      for (x = 0; x < sz; x++) {
        fprintf(stderr, "%02x ", b[x]);
      }
      fprintf(stderr, "\n");
      for (x = 0; x < sz; x++) {
        fprintf(stderr, "%c", isalnum(b[x]) ? b[x] : '.');
      }
      fprintf(stderr, "\n");
    }
  break;
  default:
    warning(MYNAME ": Unknown tag (0x%x). Please report!\n", tag);
    return 0;
  }
  gbfseek(fin, pos + sz, SEEK_SET);
  return 1;
}

/*******************************************************************************
* %%%                             gpi writer                               %%% *
*******************************************************************************/

void
GarminGPIFormat::write_string(const QByteArray& str, const char long_format) const
{
  int len = str.size();
  if (long_format) {
    gbfputint32(len + 4, fout);
    gbfwrite("EN", 1, 2, fout);
  }
  gbfputint16(len, fout);
  gbfwrite(str.constData(), 1, len, fout);
}

bool
GarminGPIFormat::compare_wpt_cb(const Waypoint* a, const Waypoint* b)
{
  return a->shortname < b->shortname;
}

char
GarminGPIFormat::compare_strings(const QString& s1, const QString& s2)
{
  return s1.compare(s2);
}

GarminGPIFormat::writer_data_t*
GarminGPIFormat::wdata_alloc()
{
  auto* res = new writer_data_t;
  waypt_init_bounds(&res->bds);

  return res;
}

void
GarminGPIFormat::wdata_free(writer_data_t* data)
{
  foreach (Waypoint* wpt, data->waypt_list) {

    if (wpt->extra_data) {
      auto* dt = (gpi_waypt_t*) wpt->extra_data;
      delete dt;
    }
    delete wpt;
  }

  if (data->top_left) {
    wdata_free(data->top_left);
  }
  if (data->top_right) {
    wdata_free(data->top_right);
  }
  if (data->bottom_left) {
    wdata_free(data->bottom_left);
  }
  if (data->bottom_right) {
    wdata_free(data->bottom_right);
  }

  delete data;
}

void
GarminGPIFormat::wdata_add_wpt(writer_data_t* data, Waypoint* wpt)
{
  data->waypt_list.append(wpt);
  waypt_add_to_bounds(&data->bds, wpt);
}

void
GarminGPIFormat::wdata_check(writer_data_t* data) const
{
  double center_lon;

  if ((data->waypt_list.size() <= WAYPOINTS_PER_BLOCK) ||
      /* avoid endless loop for points (more than WAYPOINTS_PER_BLOCK)
         at same coordinates */
      ((data->bds.min_lat >= data->bds.max_lat) && (data->bds.min_lon >= data->bds.max_lon))) {
    if (data->waypt_list.size() > 1) {
      std::stable_sort(data->waypt_list.begin(), data->waypt_list.end(), compare_wpt_cb);
    }
    return;
  }

  /* compute the (mean) center of current bounds */

  double center_lat = center_lon = 0;
  foreach (const Waypoint* wpt, data->waypt_list) {
    center_lat += wpt->latitude;
    center_lon += wpt->longitude;
  }
  center_lat /= data->waypt_list.size();
  center_lon /= data->waypt_list.size();

  while (!data->waypt_list.isEmpty()) {
    Waypoint* wpt = data->waypt_list.takeFirst();
    writer_data_t** ref;

    if (wpt->latitude < center_lat) {
      if (wpt->longitude < center_lon) {
        ref = &data->bottom_left;
      } else {
        ref = &data->bottom_right;
      }
    } else {
      if (wpt->longitude < center_lon) {
        ref = &data->top_left;
      } else {
        ref = &data->top_right;
      }
    }

    if (*ref == nullptr) {
      *ref = wdata_alloc();
    }

    wdata_add_wpt(*ref, wpt);
  }

  if (data->top_left) {
    wdata_check(data->top_left);
  }
  if (data->top_right) {
    wdata_check(data->top_right);
  }
  if (data->bottom_left) {
    wdata_check(data->bottom_left);
  }
  if (data->bottom_right) {
    wdata_check(data->bottom_right);
  }
}

int
GarminGPIFormat::wdata_compute_size(writer_data_t* data) const
{
  int res = 0;

  if (data->waypt_list.isEmpty()) {
    goto skip_empty_block;  /* do not issue an empty block */
  }

  res = 23;  /* bounds, ... of tag 0x80008 */

  foreach (Waypoint* wpt, data->waypt_list) {
    garmin_fs_t* gmsd;

    res += 12;    /* tag/sz/sub-sz */
    res += 19;    /* poi fixed size */
    res += str_from_unicode(wpt->shortname).size();
    if (! opt_hide_bitmap) {
      res += 10;  /* tag(4) */
    }

    auto* dt = new gpi_waypt_t;
    wpt->extra_data = dt;

    if (alerts) {
      if (int pidx = wpt->shortname.indexOf('@'); pidx != -1) {
        double scale;
        if (units == 's') {
          scale = MPH_TO_MPS(1);
        } else {
          scale = KPH_TO_MPS(1);
        }
        double speed = 0;
        parse_speed(wpt->shortname.mid(pidx + 1), &speed, scale, MYNAME);
        if (speed > 0) {
          wpt->set_speed(speed);
        }
#if 0
        wpt->shortname.truncate(pidx);
#endif
      } else if ((opt_speed) && (!wpt->speed_has_value())) {
        wpt->set_speed(defspeed);
      }

      if ((opt_proximity) && (!wpt->proximity_has_value())) {
        wpt->set_proximity(defproximity);
      }

      if ((wpt->speed_has_value() && (wpt->speed_value() > 0)) ||
          (wpt->proximity_has_value() && (wpt->proximity_value() > 0))) {
        data->alert = 1;
        dt->alerts++;
        res += 20;    /* tag(3) */
      }
    }

    QString str;
    if (opt_descr) {
      if (!wpt->description.isEmpty()) {
        str = wpt->description;
      }
    } else if (opt_notes) {
      if (!wpt->notes.isEmpty()) {
        str = wpt->notes;
      }
    } else if (opt_pos) {
      str = pretty_deg_format(wpt->latitude, wpt->longitude, 's', " ", false);
    }


    if (!str.isEmpty()) {
      dt->addr = str;
      dt->mask |= GPI_ADDR_ADDR;
      dt->sz += (8 + str_from_unicode(dt->addr).size());
    }

    if ((gmsd = garmin_fs_t::find(wpt))) {
      if ((dt->mask == 0) && !(dt->addr = garmin_fs_t::get_addr(gmsd, nullptr)).isEmpty()) {
        dt->mask |= GPI_ADDR_ADDR;
        dt->sz += (8 + str_from_unicode(dt->addr).size());
      }
      if (!(dt->city = garmin_fs_t::get_city(gmsd, nullptr)).isEmpty()) {
        dt->mask |= GPI_ADDR_CITY;
        dt->sz += (8 + str_from_unicode(dt->city).size());
      }
      if (!(dt->country = garmin_fs_t::get_country(gmsd, nullptr)).isEmpty()) {
        dt->mask |= GPI_ADDR_COUNTRY;
        dt->sz += (8 + str_from_unicode(dt->country).size());
      }
      if (!(dt->state = garmin_fs_t::get_state(gmsd, nullptr)).isEmpty()) {
        dt->mask |= GPI_ADDR_STATE;
        dt->sz += (8 + str_from_unicode(dt->state).size());
      }
      if (!(dt->postal_code = garmin_fs_t::get_postal_code(gmsd, nullptr)).isEmpty()) {
        dt->mask |= GPI_ADDR_POSTAL_CODE;
        dt->sz += (2 + str_from_unicode(dt->postal_code).size());  /* short form */
      }

      if (!(dt->phone_nr = garmin_fs_t::get_phone_nr(gmsd, nullptr)).isEmpty()) {
        res += (12 + 4 +  str_from_unicode(dt->phone_nr).size());
      }
    }
    if (dt->mask) {
      dt->sz += 2;  /* + mask (two bytes) */
    }
    if (dt->sz) {
      res += (dt->sz + 12);  /* + header size */
    }

    str = wpt->description;
    if (str.isEmpty()) {
      str = wpt->notes;
    }
//    if (str && (strcmp(str, wpt->shortname) == 0)) str = NULL;
    if (!str.isEmpty()) {
      res += (12 + 4 + str_from_unicode(str).size());
    }
  }

skip_empty_block:

  if (data->top_left) {
    res += wdata_compute_size(data->top_left);
  }
  if (data->top_right) {
    res += wdata_compute_size(data->top_right);
  }
  if (data->bottom_left) {
    res += wdata_compute_size(data->bottom_left);
  }
  if (data->bottom_right) {
    res += wdata_compute_size(data->bottom_right);
  }

  data->sz = res;

  if (data->waypt_list.isEmpty()) {
    return res;
  }

  return res + 12;  /* + 12 = caller needs info about tag header size */
}

void
GarminGPIFormat::wdata_write(const writer_data_t* data) const
{
  if (data->waypt_list.isEmpty()) {
    goto skip_empty_block;  /* do not issue an empty block */
  }

  gbfputint32(0x80008, fout);
  gbfputint32(data->sz, fout);
  gbfputint32(23, fout);  /* bounds + three bytes */

  gbfputint32(GPS_Math_Deg_To_Semi(data->bds.max_lat), fout);
  gbfputint32(GPS_Math_Deg_To_Semi(data->bds.max_lon), fout);
  gbfputint32(GPS_Math_Deg_To_Semi(data->bds.min_lat), fout);
  gbfputint32(GPS_Math_Deg_To_Semi(data->bds.min_lon), fout);

  gbfputint32(0, fout);
  gbfputint16(1, fout);
  gbfputc(data->alert, fout);

  foreach (const Waypoint* wpt, data->waypt_list) {
    int s1;
    auto* dt = (gpi_waypt_t*) wpt->extra_data;

    QString str = wpt->description;
    if (str.isEmpty()) {
      str = wpt->notes;
    }

    gbfputint32(0x80002, fout);
    int s0 = s1 = 19 + str_from_unicode(wpt->shortname).size();
    if (! opt_hide_bitmap) {
      s0 += 10;  /* tag(4) */
    }
    if (!str.isEmpty()) {
      s0 += (12 + 4 + str_from_unicode(str).size());  /* descr */
    }
    if (dt->sz) {
      s0 += (12 + dt->sz);  /* address part */
    }
    if (!dt->phone_nr.isEmpty()) {
      s0 += (12 + 4 + str_from_unicode(dt->phone_nr).size());
    }
    if (dt->alerts) {
      s0 += 20;  /* tag(3) */
    }

    gbfputint32(s0, fout);  /* size of following data (tag) */
    gbfputint32(s1, fout);  /* basic size (without options) */

    gbfputint32(GPS_Math_Deg_To_Semi(wpt->latitude), fout);
    gbfputint32(GPS_Math_Deg_To_Semi(wpt->longitude), fout);

    gbfputint16(1, fout);  /* ? always 1 ? */
    gbfputc(alerts, fout);  /* seems to be 1 when extra options present */

    write_string(str_from_unicode(wpt->shortname), 1);

    if (dt->alerts) {
      char flag = 0;

      gbfputint32(3, fout);  /* tag(3) */
      gbfputint32(12, fout);  /* always 12 */

      if (wpt->proximity_has_value() && (wpt->proximity_value() > 0)) {
        gbfputint16((int) wpt->proximity_value(), fout);
        flag = 4;
      } else {
        gbfputint16(0, fout);
      }
      if (wpt->speed_has_value() && (wpt->speed_value() > 0)) {
        gbfputint16((int)(wpt->speed_value() * 100), fout);
        flag = 5;
      } else {
        gbfputint16(0, fout);
      }

      gbfputint32(0x100100, fout);  /* ??? */
      gbfputc(1, fout);    /* ??? */
      gbfputc(1, fout);    /* ??? */
      gbfputc(flag, fout);
      gbfputc(0x10, fout);    /* ??? */
    }

    if (! opt_hide_bitmap) {
      gbfputint32(4, fout);  /* tag(4) */
      gbfputint32(2, fout);  /* ? always 2 == version ??? */
      gbfputint16(0, fout);
    }

    if (!str.isEmpty()) {
      gbfputint32(0xa, fout);
      gbfputint32(str_from_unicode(str).size() + 8, fout);  /* string + string header */
      write_string(str_from_unicode(str), 1);
    }

    if (dt->sz) {          /* gpi address */
      gbfputint32(0x8000b, fout);
      gbfputint32(dt->sz, fout);
      gbfputint32(0x2, fout);      /* ? always 2 ? */
      gbfputint16(dt->mask, fout);
      if (dt->mask & GPI_ADDR_CITY) {
        write_string(str_from_unicode(dt->city), 1);
      }
      if (dt->mask & GPI_ADDR_COUNTRY) {
        write_string(str_from_unicode(dt->country), 1);
      }
      if (dt->mask & GPI_ADDR_STATE) {
        write_string(str_from_unicode(dt->state), 1);
      }
      if (dt->mask & GPI_ADDR_POSTAL_CODE) {
        write_string(str_from_unicode(dt->postal_code), 0);
      }
      if (dt->mask & GPI_ADDR_ADDR) {
        write_string(str_from_unicode(dt->addr), 1);
      }
    }

    if (!dt->phone_nr.isEmpty()) {
      gbfputint32(0x8000c, fout);
      gbfputint32(str_from_unicode(dt->phone_nr).size() + 2 + 2, fout);
      gbfputint32(0x2, fout);      /* ? always 2 ? */
      gbfputint16(1, fout);      /* mask */
      write_string(str_from_unicode(dt->phone_nr), 0);
    }
  }

skip_empty_block:

  if (data->top_left) {
    wdata_write(data->top_left);
  }
  if (data->top_right) {
    wdata_write(data->top_right);
  }
  if (data->bottom_left) {
    wdata_write(data->bottom_left);
  }
  if (data->bottom_right) {
    wdata_write(data->bottom_right);
  }
}

void
GarminGPIFormat::write_category(const char* /*unused*/, const unsigned char* image, const int image_sz) const
{
  int sz = wdata_compute_size(wdata);
  sz += 8;  /* string header */
  sz += str_from_unicode(QString::fromUtf8(opt_cat)).size();

  gbfputint32(0x80009, fout);
  if ((! opt_hide_bitmap) && image_sz) {
    gbfputint32(sz + image_sz + 8, fout);
  } else {
    gbfputint32(sz, fout);
  }
  gbfputint32(sz, fout);
  write_string(str_from_unicode(QString::fromUtf8(opt_cat)), 1);

  wdata_write(wdata);

  if ((! opt_hide_bitmap) && image_sz) {
    gbfputint32(5, fout);
    gbfputint32(image_sz, fout);
    gbfwrite(image, 1, image_sz, fout);
  }
}

void
GarminGPIFormat::write_header() const
{
  time_t time = gpi_timestamp;

  if (time != 0) {
    time = GPS_Math_Utime_To_Gtime(gpi_timestamp);
  }

  gbfputint32(0, fout);
  gbfputint32(0x16, fout);
  gbfwrite("GRMREC00", 1, 8, fout);
  gbfputint32(time, fout);
  gbfputint16(0, fout);
  gbfputint16(6, fout);
  gbfwrite("my.gpi", 1, 6, fout);
  gbfputint32(1, fout);
  gbfputint32(0xc, fout);
  gbfwrite("POI", 1, 3, fout);
  gbfputc(0, fout);
  gbfputc(0, fout);
  gbfputc(0, fout);
  gbfwrite("00", 1, 2, fout);
  gbfputint16(codepage, fout);
  gbfputint16(0, fout);
}

void
GarminGPIFormat::enum_waypt_cb(const Waypoint* ref) const
{
  foreach (const Waypoint* cmp, wdata->waypt_list) {

    /* sort out nearly equal waypoints */
    if ((compare_strings(cmp->shortname, ref->shortname) == 0) &&
        (cmp->latitude == ref->latitude) &&
        (cmp->longitude == ref->longitude) &&
        (compare_strings(cmp->description, ref->description) == 0) &&
        (compare_strings(cmp->notes, ref->notes) == 0)) {
      return;
    }
  }

  auto* wpt = new Waypoint(*ref);

  if (*opt_unique == '1') {
    wpt->shortname = short_h->mkshort(wpt->shortname);
  }

  wdata_add_wpt(wdata, wpt);
}

void
GarminGPIFormat::load_bitmap_from_file(const char* fname, const unsigned char** data, int* data_sz)
{
  int i, sz;
  int dest_bpp;
  int src_line_sz, dest_line_sz;
  bmp_header_t src_h;
  gpi_bitmap_header_t* dest_h;
  unsigned char* ptr;

  gbfile* f = gbfopen_le(fname, "rb", MYNAME);
  if (gbfgetint16(f) != 0x4d42) {
    fatal(MYNAME ": No BMP image.");
  }

  /* read a standard bmp file header */
  src_h.size = gbfgetint32(f);
  src_h.res1 = gbfgetint16(f);
  src_h.res2 = gbfgetint16(f);
  src_h.image_offset = gbfgetint32(f);
  src_h.header_size = gbfgetint32(f);
  src_h.width = gbfgetint32(f);
  src_h.height = gbfgetint32(f);
  src_h.planes = gbfgetint16(f);
  src_h.bpp = gbfgetint16(f);
  src_h.compression_type = gbfgetint32(f);
  src_h.image_data_size = gbfgetint32(f);
  src_h.resolution_h = gbfgetint32(f);
  src_h.resolution_v = gbfgetint32(f);
  src_h.used_colors = gbfgetint32(f);
  src_h.important_colors = gbfgetint32(f);

  /* Workaround for indexed BMP's with used_colors = 0 */
  if ((src_h.bpp == 8) && (src_h.used_colors == 0)) {
    src_h.used_colors = (src_h.image_offset - gbftell(f)) / 4;
  }

  if (GPI_DBG) {
    printf("data size:             0x%x (%d)\n", src_h.size, src_h.size);
    printf("image data offset:     0x%x (%d)\n", src_h.image_offset, src_h.image_offset);
    printf("header size:           0x%x (%d)\n", src_h.header_size, src_h.header_size);
    printf("image width:           0x%x (%d)\n", src_h.width, src_h.width);
    printf("image height:          0x%x (%d)\n", src_h.height, src_h.height);
    printf("number of planes:      0x%x (%d)\n", src_h.planes, src_h.planes);
    printf("bits per pixel:        0x%x (%d)\n", src_h.bpp, src_h.bpp);
    printf("compression type:      0x%x (%d)\n", src_h.compression_type, src_h.compression_type);
    printf("image size:            0x%x (%d)\n", src_h.image_data_size, src_h.image_data_size);
    printf("horizontal resolution: 0x%x (%d)\n", src_h.resolution_h, src_h.resolution_h);
    printf("vertical resolution:   0x%x (%d)\n", src_h.resolution_v, src_h.resolution_v);
    printf("number of colors:      0x%x (%d)\n", src_h.used_colors, src_h.used_colors);
    printf("important colors:      0x%x (%d)\n", src_h.important_colors, src_h.important_colors);
  }

  /* sort out unsupported files */
  if (!((src_h.width <= 24) && (src_h.height <= 24) &&
        (src_h.width > 0) && (src_h.height > 0))) {
    fatal(MYNAME ": Unsupported format (%dx%d)!\n", src_h.width, src_h.height);
  }
  if (!((src_h.bpp == 8) || (src_h.bpp == 24) || (src_h.bpp == 32))) {
    fatal(MYNAME ": Unsupported color depth (%d)!\n", src_h.bpp);
  }
  if (!(src_h.compression_type == 0)) {
    fatal(MYNAME ": Sorry, we don't support compressed bitmaps.\n");
  }

  std::unique_ptr<uint32_t[]> color_table;
  if (src_h.used_colors > 0) {
    color_table.reset(new uint32_t[src_h.used_colors]);
    gbfread(color_table.get(), 1, 4 * src_h.used_colors, f);
    for (i = 0; i < src_h.used_colors; i++) {
      uint32_t color = color_table[i];
      /* swap blue and red value */
      color_table[i] = ((color & 0x00ff0000) >> 16) | ((color & 0x000000ff) << 16) | (color & 0x0000ff00);
    }
  }

  /* calculate line-size for source and destination */
  src_line_sz = (src_h.width * src_h.bpp) / 8;
  src_line_sz = (((src_line_sz + 3) / 4)) * 4;

  if (src_h.bpp == 24) {
    dest_bpp = 32;
  } else {
    dest_bpp = src_h.bpp;
  }

  dest_line_sz = (src_h.width * dest_bpp) / 8;
  dest_line_sz = (((dest_line_sz + 3) / 4)) * 4;

  sz = sizeof(*dest_h) + (src_h.height * dest_line_sz);
  if (src_h.used_colors) {
    sz += (src_h.used_colors * 4);
  }

  ptr = (unsigned char*) xmalloc(sz);
  const unsigned char* const startptr = ptr;
  dest_h = (gpi_bitmap_header_t*)ptr;
  *data = ptr;
  *data_sz = sz;

  le_write16(&dest_h->index, 0);
  le_write16(&dest_h->height, src_h.height);
  le_write16(&dest_h->width, src_h.width);
  le_write16(&dest_h->line_sz, dest_line_sz);
  le_write16(&dest_h->bpp, dest_bpp);
  le_write16(&dest_h->fixed_0, 0);    /* seems to be fixed */
  le_write32(&dest_h->image_size, dest_line_sz * src_h.height);
  le_write32(&dest_h->fixed_2c, 0x2c);    /* seems to be fixed */
  le_write32(&dest_h->palette_size, src_h.used_colors);
  le_write32(&dest_h->tr_color, 0xff00ff);  /* magenta = transparent color */
  le_write32(&dest_h->flag2, 0x1);    /* ? enable transparent mode ? */
  le_write32(&dest_h->size_2c, (dest_line_sz * src_h.height) + 0x2c);

  /* copy and revert order of BMP lines */
  ptr = (unsigned char*)dest_h;
  ptr += (sizeof(*dest_h) + (dest_line_sz * (src_h.height - 1)));

  gbfseek(f, src_h.image_offset, SEEK_SET);

  if (src_h.bpp == 24) {
    /* 24 bpp seems to be not supported, convert to 32 bpp */
    for (i = 0; i < src_h.height; i++) {
      int j;
      unsigned char* p = ptr;

      for (j = 0; j < src_h.width; j++) {
        int color = (int32_t)gbfgetint16(f) | (gbfgetc(f) << 16);
        le_write32(p, color);
        p += 4;
      }
      for (j = (src_h.width * src_h.bpp) / 8; j < src_line_sz; j++) {
        gbfgetc(f);  /* drop fill-in bytes */
      }
      ptr -= dest_line_sz;
    }
  } else {
    for (i = 0; i < src_h.height; i++) {
      gbfread(ptr, 1, src_line_sz, f);
      ptr -= dest_line_sz;
    }
  }

  if (src_h.used_colors > 0) {
    ptr = (unsigned char*)dest_h;
    ptr += (sizeof(*dest_h) + (src_h.height * src_line_sz));

    for (i = 0; i < src_h.used_colors; i++) {
      le_write32(ptr, color_table[i]);
      ptr += 4;
    }
  }

  auto bytesout = ptr - startptr;
  if (bytesout != *data_sz) {
    warning(MYNAME ": Code error in load_bitmap_from_file, expected output size %d, actual output %td.", *data_sz, bytesout);
  }
  gbfclose(f);
}

/*******************************************************************************
* %%%        global callbacks called by gpsbabel main process              %%% *
*******************************************************************************/

void
GarminGPIFormat::rd_init(const QString& fname)
{
  fin = gbfopen_le(fname, "rb", MYNAME);
  rdata = new reader_data_t;

  read_header();

  if ((codepage >= 1250) && (codepage <= 1257)) {
    QByteArray qCodecName = "windows-" + QByteArray::number(codepage);
    codec = get_codec(qCodecName);
  } else if (codepage == 65001) {
    codec = get_codec("utf8");
  } else {
    fatal(MYNAME ": Unsupported code page (%d). File is likely encrypted.\n", codepage);
  }

  units = tolower(opt_units[0]);
  if ((units != 'm') && (units != 's')) {
    fatal(MYNAME ": Unknown units parameter (%c).\n", opt_units[0]);
  }
}

void
GarminGPIFormat::wr_init(const QString& fname)
{
  if ((gpi_timestamp != 0) && !gpsbabel_testmode()) {      /* not the first gpi output session */
    time_t t = time(nullptr);
    if (t <= gpi_timestamp) {
      gpi_timestamp++;  /* don't create files with same timestamp */
    } else {
      gpi_timestamp = t;
    }
  } else {
    gpi_timestamp = gpsbabel_time;
  }

  fout = gbfopen_le(fname, "wb", MYNAME);

  short_h = new MakeShort;

  short_h->set_length(1024);
  short_h->set_badchars("\r\n");
  short_h->set_mustupper(false);
  short_h->set_mustuniq(true);
  short_h->set_whitespace_ok(true);
  short_h->set_repeating_whitespace_ok(false);
  short_h->set_defname("POI");

  codepage = 0;

  for (int i = 1250; i <= 1257; i++) {
    if (QStringLiteral("windows-%1").arg(i).compare(QString(opt_writecodec), Qt::CaseInsensitive) == 0) {
      codepage = i;
      break;
    }
  }
  if (! codepage) {
    if (QString("utf8").compare(QString(opt_writecodec), Qt::CaseInsensitive) == 0) {
      codepage = 65001;
    }
  }

  if (! codepage) {
    warning(MYNAME ": Unsupported character set (%s)!\n", opt_writecodec);
    fatal(MYNAME ": Valid values are windows-1250 to windows-1257 and utf8.\n");
  }

  codec = get_codec(opt_writecodec);

  units = tolower(opt_units[0]);
  if ((units != 'm') && (units != 's')) {
    fatal(MYNAME ": Unknown units parameter (%c).\n", opt_units[0]);
  }

  alerts = (opt_alerts) ? 1 : 0;

  if (opt_speed) {
    double scale;
    alerts = 1;          /* Force alerts to be enabled */
    if (units == 's') {
      scale = MPH_TO_MPS(1);  /* We need speed in meters per second */
    } else {
      scale = KPH_TO_MPS(1);
    }
    parse_speed(opt_speed, &defspeed, scale, MYNAME);
  }

  if (opt_proximity) {
    double scale;
    alerts = 1;           /* Force alerts to be enabled */
    if (units == 's') {
      scale = MILES_TO_METERS(1);  /* We need proximity in meters */
    } else {
      scale = 1000.0;  /* one kilometer in meters */
    }
    parse_distance(opt_proximity, &defproximity, scale, MYNAME);
  }
  wdata = wdata_alloc();
}

void
GarminGPIFormat::rd_deinit()
{
  delete rdata;
  gbfclose(fin);
}

void
GarminGPIFormat::wr_deinit()
{
  wdata_free(wdata);
  delete short_h;
  short_h = nullptr;
  gbfclose(fout);

  if ((opt_sleep) && !gpsbabel_testmode()) {  /* don't sleep during 'testo' */
    int sleep = xstrtoi(opt_sleep, nullptr, 10);
    if (sleep < 1) {
      sleep = 1;
    }
    gpi_timestamp += sleep;
    while (gpi_timestamp > time(nullptr)) {
      QThread::usleep(100);
    }
  }
}

void
GarminGPIFormat::read()
{
  while (true) {
    int tag = gbfgetint32(fin);
    if (tag == 0xffff) {
      return;
    }
    if (! read_tag("garmin_gpi_read", tag, nullptr)) {
      return;
    }
  }
}

void
GarminGPIFormat::write()
{
  const unsigned char* image;
  int image_sz;

  if (strlen(opt_cat) == 0) {
    fatal(MYNAME ": Can't write empty category!\n");
  }

  if (opt_hide_bitmap) {
    image = nullptr;
    image_sz = 0;
  } else if (opt_bitmap && *opt_bitmap) {
    load_bitmap_from_file(opt_bitmap, &image, &image_sz);
  } else {
    image = gpi_bitmap;  /* embedded GPSBabel icon in gpi format */
    image_sz = GPI_BITMAP_SIZE;
  }
  auto enum_waypt_cb_lambda = [this](const Waypoint* waypointp)->void {
    enum_waypt_cb(waypointp);
  };
  waypt_disp_all(enum_waypt_cb_lambda);

  wdata_check(wdata);
  write_header();
  write_category(opt_cat, image, image_sz);

  gbfputint32(0xffff, fout);  /* final tag */
  gbfputint32(0, fout);    /* ? dummy size ? */

  if (image != gpi_bitmap) {
    xfree(image);
  }
}
