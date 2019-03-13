/*

    Support for Memory-Map Navigator Overlay Files (.mmo)

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
    Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111 USA

 */

#include <cctype>                // for isspace
#include <cerrno>                // for errno
#include <cstdio>                // for SEEK_CUR, fprintf, size_t, stdout
#include <cstdlib>               // for abort, strtol
#include <cstdint>
#include <cstring>               // for strcmp, strlen, memset, strchr, strncmp
#include <ctime>

#include <QtCore/QByteArray>     // for QByteArray
#include <QtCore/QChar>          // for operator==, QChar
#include <QtCore/QCharRef>       // for QCharRef
#include <QtCore/QDateTime>      // for QDateTime
#include <QtCore/QHash>          // for QHash, QHash<>::const_iterator
#include <QtCore/QLatin1String>  // for QLatin1String
#include <QtCore/QString>        // for QString, operator==
#include <QtCore/Qt>             // for CaseInsensitive
#include <QtCore/QtGlobal>       // for qAsConst, QAddConst<>::Type, foreach, Q_UNUSED

#include "defs.h"
#include "cet.h"                 // for cet_ucs4_to_utf8, cet_utf8_to_ucs4
#include "gbfile.h"              // for gbfputc, gbfgetuint16, gbfgetc, gbfgetdbl, gbfgetuint32, gbfputflt, gbfputuint32, gbfgetint16, gbfputdbl, gbfputuint16, gbfclose, gbfread, gbfseek, gbfputint16, gbfwrite, gbfcopyfrom, gbfeof, gbfgetflt, gbfgetint32, gbfile, gbfopen, gbfrewind, gbsize_t
#include "session.h"             // for curr_session, session_t
#include "src/core/datetime.h"   // for DateTime


#define MYNAME "mmo"

// #define MMO_DBG

static char* opt_locked, *opt_visible, *opt_version;

static
arglist_t mmo_args[] = {
  {
    "locked", &opt_locked, "Write items 'locked' [default no]", "0",
    ARGTYPE_BOOL, ARG_NOMINMAX, nullptr
  },
  {
    "visible", &opt_visible, "Write items 'visible' [default yes]", "1",
    ARGTYPE_BOOL, ARG_NOMINMAX, nullptr
  },
  {
    "ver", &opt_version, "Write files with internal version [n]", nullptr,
    ARGTYPE_INT, "17", "18", nullptr
  },
  ARG_TERMINATOR
};

typedef struct mmo_data_s {
  int objid;		/* internal object id */
  char* name;
  const char* category;	/* currently not handled */
  gpsdata_type type;	/* type of "data" */
  time_t ctime;
  time_t mtime;
  int left;		/* number of un-readed route points */
  void* data;		/* can be a waypoint, a route or a track */
  int refct;
  struct mmo_data_s** members;
  unsigned char visible:1;
  unsigned char locked:1;
  unsigned char loaded:1;
} mmo_data_t;

static gbfile* fin, *fout;
static int mmo_version;
static int mmo_obj_ct;
static int mmo_object_id;
static uint32_t mmo_filemark;
static uint16_t wpt_object_id;
static uint16_t rte_object_id;
static uint16_t trk_object_id;
static uint16_t cat_object_id;
static uint16_t ico_object_id;
static uint16_t pos_object_id;
static uint16_t txt_object_id;
static gpsdata_type mmo_datatype;
static const route_head* mmo_rte;

static QHash<QString, int> category_names;
static QHash<int, QString> icons;
static QHash<int, mmo_data_t*> objects;
static QHash<QString, unsigned> mmobjects;

typedef struct mmo_icon_mapping_s {
  const int	value;
  const char*	icon;
} mmo_icon_mapping_t;

/* standard icons; no bitmaps in file */

static const mmo_icon_mapping_t mmo_icon_value_table[] = {
  { 0x00, "Dot" },
  { 0x01, "House" },
  { 0x02, "Fuel" },
  { 0x03, "Car" },
  { 0x04, "Fish" },
  { 0x05, "Boat" },
  { 0x06, "Anchor" },
  { 0x07, "Wreck" },
  { 0x08, "Exit" },
  { 0x09, "Skull" },
  { 0x0A, "Flag" },
  { 0x0B, "Camp" },
  { 0x0C, "Man Overboard" },
  { 0x0D, "Deer" },
  { 0x0E, "First Aid" },
  { 0x0F, "Trackback" },
  { 0x10, "Tiny dot" },
  { 0x11, "Triangle" },
  { 0x12, "Square" },
  { 0x13, "Circle" },
  { 0x14, "Green bouy" },
  { 0x15, "Red bouy" },
  { 0x16, "Yellow bouy" },
  { 0x17, "Geocache" },

  { -1, nullptr }
};

static const uint32_t obj_type_ico = 0x00;
static const uint32_t obj_type_rte = 0x14;
static const uint32_t obj_type_trk = 0x1E;
#ifdef MMO_DBG
static const uint32_t obj_type_txt = 0x32;
#endif
static const uint32_t obj_type_wpt = 0x3C;

/* helpers */

#ifdef MMO_DBG
static void
dbgprintf(const char* sobj, const char* fmt, ...)
{
  va_list args;
  va_start(args, fmt);

  printf(MYNAME "-%s: ", sobj);
  vprintf(fmt, args);
  va_end(args);
}

# define DBG(args) dbgprintf args
#else
# define DBG(args) do {} while (0) ;
#endif

static char*
mmo_readstr()
{
  char* res;

  signed int len = (unsigned)gbfgetc(fin);
  if (len == 0xFF) {
    // Next two bytes are either the length (strings longer than 254 chars)
    // or FE then FF (which is -2) meaning a UTF-16 string
    len = gbfgetint16(fin);
    if (len == -2) {
      // read the new length (single byte)
      // length is number of "characters" not number of bytes
      len = (unsigned)gbfgetc(fin);
      if (len > 0) {
        unsigned int resbytes=0;
        res = (char*) xmalloc(len*2 + 1);  // bigger to allow for utf-8 expansion
        for (signed int ii = 0; ii<len; ii++) {
          char utf8buf[8];
          unsigned int ch = gbfgetint16(fin);
          // convert to utf-8, possibly multiple bytes
          int utf8len = cet_ucs4_to_utf8(utf8buf, sizeof(utf8buf), ch);
          for (signed int jj = 0; jj < utf8len; jj++) {
            res[resbytes++] = utf8buf[jj];
          }
        }
        res[resbytes] = '\0';
        return res;
      }
      // length zero is handled below: returns an empty string
    } else if (len < 0) {
      fatal(MYNAME ": Invalid string length (%d)!\n", len);
    }
    // positive values of len are for strings longer than 254, handled below:
  }
  // length zero returns an empty string
  res = (char*) xmalloc(len + 1);
  res[len] = '\0';
  if (len) {
    gbfread(res, len, 1, fin);
    if (static_cast<size_t>(len) != strlen(res)) {
      // strlen requires a size_t, but Microsoft's stupid compiler doesn't
      // do C99 %zd.  Thanx, Microsoft.
      fprintf(stdout, "got len %d but str is '%s' (strlen %d)\n", len, res, (int) strlen(res));
      fatal(MYNAME ": Error in file structure!\n");
    }
  }

  return res;
}


static int
mmo_fillbuf2(void* buf, const gbsize_t bufsz, const gbsize_t count, const int need_all)
{
  if (count > (unsigned int)bufsz) {
    fatal(MYNAME ": Internal error (bufsz too small)!\n");
  }

  memset(buf, 0xFF, count);
  gbsize_t res = gbfread(buf, 1, count, fin);
  if (need_all && (res < count)) {
    fatal(MYNAME ": Unexpected end of file!\n");
  }

  return res;
}
#define mmo_fillbuf(a,b,c) mmo_fillbuf2((a),sizeof((a)),(b),(c))

#ifdef MMO_DBG
static void
mmo_printbuf(const char* buf, int count, const char* comment)
{
  int i;
  printf("%s", comment);
  for (i = 0; i < count; i++) {
    printf("%02X ", buf[i] & 0xFF);
  }
  printf("- ");
  for (i = 0; i < count; i++)
    if (isprint(buf[i])) {
      printf("%c", buf[i] & 0xFF);
    } else {
      printf(".");
    }
  printf("\n");
  fflush(stdout);
}
#endif

/******************************************************************************/

static mmo_data_t*
mmo_register_object(const int objid, const void* ptr, const gpsdata_type type)
{
  mmo_data_t* data = (mmo_data_t*) xcalloc(1, sizeof(*data));
  data->data = const_cast<void*>(ptr);
  data->visible = 1;
  data->locked = 0;
  data->type = type;
  data->objid = objid;

  objects.insert(objid, data);

  return data;
}


static int
mmo_get_objid(const void* ptr)
{
  for (auto o = objects.constBegin(); o != objects.constEnd(); ++o) {
    if (o.value()->data == ptr) {
      return o.key();
    }
  }
  return 0;
}


static mmo_data_t*
mmo_get_object(const uint16_t objid)
{
  int key = objid | 0x8000;
  if (!objects.contains(key)) {
#ifdef MMO_DBG
    gbfseek(fin, -2, SEEK_CUR);
    int ni, n;
    for (ni = 0; (n = gbfgetc(fin)) != EOF; ni++) {
      DBG(("mmo_get_object", "%04X %02X %c (%d)\n",
           ni, n, n >= 32 && n <= 126 ? (char)n : '.', n));
    }
#endif
    fatal(MYNAME ": Unregistered object id 0x%04X!\n", objid | 0x8000);
  }

  return objects.value(key);
}

static Waypoint*
mmo_get_waypt(mmo_data_t* data)
{
  data->refct++;
  if (data->refct == 1) {
    return static_cast<Waypoint*>(data->data);
  } else {
    return new Waypoint(*(Waypoint*)data->data);
  }
}

static void
mmo_free_object(mmo_data_t* data)
{
  if (data->name) {
    xfree(data->name);
  }
  if ((data->type == wptdata) && (data->refct == 0)) {
    delete (Waypoint*)data->data;
  }
  xfree(data);
}


static void
mmo_register_icon(const int id, const char* name)
{
  icons.insert(id, QString::fromUtf8(name));
}


static mmo_data_t* mmo_read_object();


static void
mmo_end_of_route(mmo_data_t* data)
{
#ifdef MMO_DBG
  const char* sobj = "CObjRoute";
#endif
  route_head* rte = (route_head*) data->data;
  char buf[7];

  if (mmo_version >= 0x12) {
    mmo_fillbuf(buf, 7, 1);
    DBG((sobj, "route data (since 0x12): "));
#ifdef MMO_DBG
    mmo_printbuf(buf, 7, "");
#endif
    rte->line_color.bbggrr = le_read32(&buf[0]);
    rte->line_color.opacity = 255 - (buf[6] * 51);
    DBG((sobj, "color = 0x%06X\n", rte->line_color.bbggrr));
    DBG((sobj, "transparency = %d (-> %d)\n", buf[6], rte->line_color.opacity));
    DBG((sobj, "for \"%s\" \n", data->name));
  }

  if (rte->rte_waypt_ct == 0) {	/* don't keep empty routes */
    route_del_head(rte);
    data->data = nullptr;
  }
}


static void
mmo_read_category(mmo_data_t* data)
{
  int marker = gbfgetuint16(fin);

  if (marker & 0x8000) {
    DBG(("mmo_read_category", "reading category object\n"));
    gbfseek(fin, -2, SEEK_CUR);
    mmo_data_t* tmp = mmo_read_object();
    if (data) {
      data->category = tmp->name;
    }
  }
}


static void
mmo_read_CObjIcons(mmo_data_t* data)
{
  Q_UNUSED(data);
#ifdef MMO_DBG
  const char* sobj = "CObjIcons";
#endif
  int icon_id;
  uint16_t u16;

  DBG((sobj, ":-----------------------------------------------------\n"));
  DBG((sobj, "name = \"%s\" [ visible=%s, id=0x%04X ]\n",
       data->name, data->visible ? "yes" : "NO", data->objid));

  if (mmo_version >= 0x18) {
    u16 = gbfgetuint16(fin);
    DBG((sobj, "unknown value = 0x%04X (since 0x18)\n", u16));
    u16 = gbfgetuint16(fin);
    DBG((sobj, "unknown value = 0x%04X (since 0x18)\n", u16));
    u16 = gbfgetuint16(fin);
    DBG((sobj, "unknown value = 0x%04X (since 0x18)\n", u16));
    u16 = gbfgetuint16(fin);
    DBG((sobj, "unknown value = 0x%04X (since 0x18)\n", u16));
  }
  u16 = gbfgetuint16(fin);
  (void) u16;
  DBG((sobj, "unknown value = 0x%04X\n", u16));
  u16 = gbfgetuint16(fin);
  DBG((sobj, "unknown value = 0x%04X\n", u16));
  u16 = gbfgetuint16(fin);
  DBG((sobj, "unknown value = 0x%04X\n", u16));

  while ((icon_id = gbfgetuint32(fin))) {
    (void) gbfgetuint32(fin);
    (void) gbfgetuint32(fin);
    char* name = mmo_readstr();
    DBG((sobj, "bitmap(0x%08X) = \"%s\"\n", icon_id, name));
    mmo_register_icon(icon_id, name);
    xfree(name);
    // The next four bytes hold the length of the image,
    // read them and then skip the image data.
    gbfseek(fin, gbfgetuint32(fin), SEEK_CUR);
  }
}


static void
mmo_read_CObjWaypoint(mmo_data_t* data)
{
#ifdef MMO_DBG
  const char* sobj = "CObjWaypoint";
#endif
  Waypoint* wpt;
  mmo_data_t** rtelink = nullptr;
  char buf[16];
  int i;

  DBG((sobj, ":-----------------------------------------------------\n"));
  DBG((sobj, "name = \"%s\" [ visible=%s, id=0x%04X ]\n",
       data->name, data->visible ? "yes" : "NO", data->objid));

  data->data = wpt = new Waypoint;
  wpt->shortname = QString::fromLatin1(data->name);

  time_t time = data->mtime;
  if (! time) {
    time = data->ctime;
  }
  if (time > 0) {
    wpt->SetCreationTime(time);
  }

  if (mmo_version >= 0x18) {
    uint16_t u16 = gbfgetuint16(fin);
    DBG((sobj, "unknown value = 0x%04X (since 0x18)\n", u16));
    u16 = gbfgetuint16(fin);
    DBG((sobj, "unknown value = 0x%04X (since 0x18)\n", u16));
    u16 = gbfgetuint16(fin);
    DBG((sobj, "unknown value = 0x%04X (since 0x18)\n", u16));
    u16 = gbfgetuint16(fin);
    DBG((sobj, "unknown value = 0x%04X (since 0x18)\n", u16));
    (void) u16;
  }

  wpt->latitude = gbfgetdbl(fin);
  wpt->longitude = gbfgetdbl(fin);

  DBG((sobj, "trackpoint %d/%d coordinates = %f / %f\n", ctp+1,tp, wpt->latitude, wpt->longitude));

  int rtelinks = gbfgetuint16(fin);
  if (rtelinks > 0) {

    rtelink = (mmo_data_t**) xcalloc(sizeof(*rtelink), rtelinks);
    DBG((sobj, "rtelinks = %d\n", rtelinks));

    for (i = 0; i < rtelinks; i++) {
      DBG((sobj, "read rtelink number %d\n", i + 1));
      rtelink[i] = mmo_read_object();
    }

  }

  char* str = mmo_readstr();	/* descr + url */
  if (strncmp(str, "_FILE_ ", 7) == 0) {
    char* cx = lrtrim(str + 7);
    char* cend = strchr(cx, '\n');
    if (cend == nullptr) {
      cend = cx + strlen(cx);
    }

    {
      QString url = QString::fromUtf8(cx, cend-cx).trimmed();
      if (!url.isEmpty()) {
        wpt->AddUrlLink(url);
      }
    }

    if (*cend++) {
      wpt->notes = QString::fromLatin1(cend);
    }

    if (wpt->HasUrlLink()) {
      DBG((sobj, "url = \"%s\"\n", wpt->url));
    }
  } else if (*str) {
    wpt->notes = QString::fromLatin1(str);
  }
  xfree(str);
  if (!wpt->notes.isEmpty()) {
    DBG((sobj, "notes = \"%s\"\n", wpt->notes));
  }

  mmo_fillbuf(buf, 12, 1);
  i = le_read32(&buf[8]);		/* icon */
  if (i != -1) {
    if (icons.contains(i)) {
      wpt->icon_descr = icons.value(i);
      DBG((sobj, "icon = \"%s\"\n", qPrintable(wpt->icon_descr)));
    }
#ifdef MMO_DBG
    else {
      DBG((sobj, "icon not found for 0x%08X\n", i));
    }
#endif
  }

  wpt->proximity = le_read_float(&buf[4]);
  if (wpt->proximity) {
    wpt->wpt_flags.proximity = 1;
    DBG((sobj, "proximity = %f\n", wpt->proximity));
  }

  str = mmo_readstr();	/* name on gps ??? option ??? */
  if (*str) {
    wpt->description = wpt->shortname;
    wpt->shortname = str;
    DBG((sobj, "name on gps = %s\n", str));
  } else {
    xfree(str);
  }

  int ux = gbfgetuint32(fin);
  DBG((sobj, "proximity type = %d\n", ux));
  (void) ux;

  data->loaded = 1;

  if (rtelink) {
    xfree(rtelink);
  } else {
    waypt_add(mmo_get_waypt(data));
  }
}


static void
mmo_read_CObjRoute(mmo_data_t* data)
{
#ifdef MMO_DBG
  const char* sobj = "CObjRoute";
#endif
  route_head* rte;

  DBG((sobj, ":-----------------------------------------------------\n"));
  DBG((sobj, "name = \"%s\" [ visible=%s, id=0x%04X ]\n",
       data->name, data->visible ? "yes" : "NO", data->objid));

  data->data = rte = route_head_alloc();
  rte->rte_name = data->name;
  route_add_head(rte);

  if (mmo_version >= 0x18) {
    uint16_t u16 = gbfgetuint16(fin);
    DBG((sobj, "unknown value = 0x%04X (since 0x18)\n", u16));
    u16 = gbfgetuint16(fin);
    DBG((sobj, "unknown value = 0x%04X (since 0x18)\n", u16));
    u16 = gbfgetuint16(fin);
    DBG((sobj, "unknown value = 0x%04X (since 0x18)\n", u16));
    u16 = gbfgetuint16(fin);
    DBG((sobj, "unknown value = 0x%04X (since 0x18)\n", u16));
    (void) u16;
  }

  int ux = gbfgetc(fin);		/* line label */
  DBG((sobj, "line label = %d\n", ux));
  (void) ux;

  data->left = gbfgetint16(fin);

  if (data->left <= 0) {
    if (mmo_version >= 0x12) {
      char buf[16];
      mmo_fillbuf(buf, 7, 1);
    }
    route_del_head(rte);
    data->data = nullptr;

    return;
  }

  while (data->left > 0) {
    DBG((sobj, "read next waypoint\n"));
    mmo_data_t* tmp = mmo_read_object();
    if (tmp && tmp->data && (tmp->type == wptdata)) {
      Waypoint* wpt;

      /* FIXME: At this point this waypoint maybe not fully loaded (initialized) !!!
      	  We need a final procedure to handle this !!! */
      if (! tmp->loaded) {
        wpt = new Waypoint;
        wpt->latitude = 0;
        wpt->longitude = 0;
        xasprintf(&wpt->shortname, "\01%p", tmp);
      } else {
        wpt = mmo_get_waypt(tmp);
      }

      route_add_wpt(rte, wpt);
      data->left--;
    }
  }

  if (mmo_version > 0x11) {
    mmo_end_of_route(data);
  }
}


static void
mmo_read_CObjTrack(mmo_data_t* data)
{
#ifdef MMO_DBG
  const char* sobj = "CObjTrack";
#endif

  DBG((sobj, ":-----------------------------------------------------\n"));
  DBG((sobj, "name = \"%s\" [ visible=%s, id=0x%04X ]\n",
       data->name, data->visible ? "yes" : "NO", data->objid));

  route_head* trk = route_head_alloc();
  trk->rte_name = data->name;
  track_add_head(trk);

  if (mmo_version >= 0x18) {
    uint16_t u16 = gbfgetuint16(fin);
    DBG((sobj, "unknown value = 0x%04X (since 0x18)\n", u16));
    u16 = gbfgetuint16(fin);
    DBG((sobj, "unknown value = 0x%04X (since 0x18)\n", u16));
    u16 = gbfgetuint16(fin);
    DBG((sobj, "unknown value = 0x%04X (since 0x18)\n", u16));
    u16 = gbfgetuint16(fin);
    DBG((sobj, "unknown value = 0x%04X (since 0x18)\n", u16));
    (void) u16;
  }

  int tp = gbfgetint16(fin);
  DBG((sobj, "track has %d point(s)\n", tp));

  for (int ctp = 0; ctp < tp; ctp++) {
    Waypoint* wpt = new Waypoint;

    wpt->latitude = gbfgetdbl(fin);
    wpt->longitude = gbfgetdbl(fin);
    DBG((sobj, "coordinates = %f / %f\n", wpt->latitude, wpt->longitude));
    char unk = gbfgetc(fin);
    DBG((sobj, "Unknown = 0x%02X (%d)\n", unk, unk));

    wpt->SetCreationTime(gbfgetint32(fin));
    wpt->altitude = gbfgetflt(fin);

    if (unk != 0) {
      uint16_t ux = gbfgetuint16(fin);
      DBG((sobj, "unknown value = 0x%04X (%d)\n", ux, ux));
      (void) ux;
      if (unk > 1) {
        uint16_t ux;
        ux = gbfgetuint16(fin);
        DBG((sobj, "unknown value = 0x%04X (%d)\n", ux, ux));
        (void) ux;
      }
    }
    track_add_wpt(trk, wpt);
  }

  if (mmo_version > 0) {
    uint32_t u32 = gbfgetuint32(fin); 	/* Min. update interval */
    DBG((sobj, "min. update interval = %d\n", u32));
    u32 = gbfgetuint32(fin); 	/* unknown */
    DBG((sobj, "unknown value = 0x%08X (%d)\n", u32, u32));
    u32 = gbfgetuint32(fin); 	/* unknown */
    DBG((sobj, "unknown value = 0x%08X (%d)\n", u32, u32));
    u32 = gbfgetuint32(fin); 	/* unknown */
    DBG((sobj, "min. update distance = %d\n", u32));
    u32 = gbfgetuint32(fin); 	/* unknown */
    DBG((sobj, "track partition interval = %d\n", u32 / 60));
    u32 = gbfgetuint32(fin); 	/* unknown */
    DBG((sobj, "unknown value = 0x%08X (%d)\n", u32, u32));
    u32 = gbfgetuint32(fin); 	/* unknown */
    DBG((sobj, "tick interval = %d\n", u32 / 60));
    trk->line_color.bbggrr = gbfgetuint32(fin); 	/* rgb color */
    trk->line_color.opacity = 255;
    DBG((sobj, "color = 0x%06X\n", trk->line_color.bbggrr));
    (void) u32;
  }

  if (mmo_version >= 0x12) {
    char u8 = gbfgetc(fin);
    DBG((sobj, "line width = %d - (since 0x12)\n", u8));
    u8 = gbfgetc(fin);
    DBG((sobj, "line style = %d - (since 0x12)\n", u8));
    u8 = gbfgetc(fin);
    DBG((sobj, "transparency = %d - (since 0x12)\n", u8));
    trk->line_color.opacity = 255 - (u8 * 51);

    if (mmo_version >= 0x16) {
      // XXX ARB was u8 = gbfgetc(fin); but actually a string
      char* text = mmo_readstr();
      DBG((sobj, "text = \"%s\"\n", text));
      xfree(text);
      uint16_t u16 = gbfgetuint16(fin);
      DBG((sobj, "unknown value = 0x%04X (since 0x16)\n", u16));
      u16 = gbfgetuint16(fin);
      DBG((sobj, "unknown value = 0x%04X (since 0x16)\n", u16));
      (void) u16;
    }
  }

  if (trk->rte_waypt_ct == 0) {
    track_del_head(trk);
    data->data = nullptr;
  }
}


static void
mmo_read_CObjText(mmo_data_t*)
{
#ifdef MMO_DBG
  const char* sobj = "CObjText";
#endif
  char buf[28];

  DBG((sobj, ":-----------------------------------------------------\n"));
  DBG((sobj, "name = \"%s\" [ visible=%s, id=0x%04X ]\n",
       data->name, data->visible ? "yes" : "NO", data->objid));

  double lat = gbfgetdbl(fin);
  double lon = gbfgetdbl(fin);
  DBG((sobj, "coordinates = %f / %f\n", lat, lon));
  (void) lat;
  (void) lon;

  char* text = mmo_readstr();
  DBG((sobj, "text = \"%s\"\n", text));
  xfree(text);

  mmo_fillbuf(buf, 28, 1);

  char* font = mmo_readstr();
  DBG((sobj, "font = \"%s\"\n", font));
  xfree(font);

  mmo_fillbuf(buf, 25, 1);
}


static void
mmo_read_CObjCurrentPosition(mmo_data_t*)
{
#ifdef MMO_DBG
  const char* sobj = "CObjCurrentPosition";
#endif
  char buf[24];

  DBG((sobj, ":-----------------------------------------------------\n"));
  DBG((sobj, "name = \"%s\" [ visible=%s, id=0x%04X ]\n",
       data->name, data->visible ? "yes" : "NO", data->objid));

  double lat = gbfgetdbl(fin);
  double lon = gbfgetdbl(fin);
  DBG((sobj, "coordinates = %f / %f\n", lat, lon));
  (void) lat;
  (void) lon;

  mmo_fillbuf(buf, 24, 1);
  if (mmo_version >= 0x18) {
    mmo_fillbuf(buf, 8, 1); // XXX ARB read an extra 8
  }

  if (mmo_version >= 0x14) {
    char* name = mmo_readstr();
    DBG((sobj, "name = \"%s\"\n", name));
    xfree(name);
    // XXX ARB was just: mmo_fillbuf(buf, 13, 1);
    // but actually it's string/long/string/long/long
    (void) gbfgetuint32(fin);
    name = mmo_readstr();
    DBG((sobj, "name = \"%s\"\n", name));
    xfree(name);
    (void) gbfgetuint32(fin);
    (void) gbfgetuint32(fin);
  }
}


static mmo_data_t*
mmo_read_object()
{
  mmo_data_t* data = nullptr;

  // There are three cases:
  // a new object of a type that has not occurred previously in this file;
  // a new object; or
  // a back reference to an object that appears earlier in the file.

  int objid = gbfgetuint16(fin);
  if (objid == 0xFFFF) {
    DBG(("mmo_read_object", "Registering new object type\n"));

    objid = mmo_object_id++;

    uint16_t version = gbfgetuint16(fin);
    is_fatal(version != mmo_version, MYNAME ": Invalid version identifier!\n");

    int len = gbfgetint16(fin);

    char* sobj = (char*) xmalloc(len + 1);
    sobj[len] = '\0';
    gbfread(sobj, len, 1, fin);
    DBG(("mmo_read_object", "%s\n", sobj));

    if (strcmp(sobj, "CObjIcons") == 0) {
      ico_object_id = objid;
    } else if (strcmp(sobj, "CCategory") == 0) {
      cat_object_id = objid;
    } else if (strcmp(sobj, "CObjWaypoint") == 0) {
      wpt_object_id = objid;
    } else if (strcmp(sobj, "CObjRoute") == 0) {
      rte_object_id = objid;
    } else if (strcmp(sobj, "CObjTrack") == 0) {
      trk_object_id = objid;
    } else if (strcmp(sobj, "CObjCurrentPosition") == 0) {
      pos_object_id = objid;
    } else if (strcmp(sobj, "CObjText") == 0) {
      txt_object_id = objid;
    } else {
      fatal(MYNAME ": Unknown Object \"%s\"!\n", sobj);
    }
    xfree(sobj);
  }

  DBG(("mmo_read_object", "objid = 0x%04X\n", objid));

  if (objid & 0x8000) {
    data = mmo_register_object(mmo_object_id++, nullptr, (gpsdata_type)0);
    data->name = mmo_readstr();

    if (objid != cat_object_id) {
      data->ctime = gbfgetuint32(fin);
      data->mtime = gbfgetuint32(fin);
      data->locked = gbfgetc(fin);
      data->visible = gbfgetc(fin);

      uint32_t obj_type = gbfgetuint32(fin);
      (void) obj_type;
#ifdef MMO_DBG
      uint32_t expected_type = 0xFFFFFFFF;
      if (objid == ico_object_id) {
        expected_type = obj_type_ico;
      } else if (objid == trk_object_id) {
        expected_type = obj_type_trk;
      } else if (objid == wpt_object_id) {
        expected_type = obj_type_wpt;
      } else if (objid == rte_object_id) {
        expected_type = obj_type_rte;
      } else if (objid == txt_object_id) {
        expected_type = obj_type_txt;
      }
      if (mmo_version >= 0x18) {
        expected_type <<= 24;
      }
      DBG(("mmo_read_object", "object type = 0x%08X\n", obj_type));
      if (obj_type != expected_type) {
        DBG(("mmo_read_object", "   expected   0x%08X\n", expected_type));
      }
#endif

      if (objid != ico_object_id) {
        mmo_read_category(data);
      }
      DBG(("mmo_read_object", "Category : %s\n",
           data->category ? data->category : "[No category]"));
    }

    if (objid == cat_object_id) ; 	/* do nothing */
    else if (objid == ico_object_id) {
      mmo_read_CObjIcons(data);
    } else if (objid == trk_object_id) {
      data->type = trkdata;
      mmo_read_CObjTrack(data);
    } else if (objid == wpt_object_id) {
      data->type = wptdata;
      mmo_read_CObjWaypoint(data);
    } else if (objid == rte_object_id) {
      data->type = rtedata;
      mmo_read_CObjRoute(data);
    } else if (objid == pos_object_id) {
      mmo_read_CObjCurrentPosition(data);
    } else if (objid == txt_object_id) {
      mmo_read_CObjText(data);
    } else {
      fatal(MYNAME ": Unregistered Object-ID 0x%04X\n", objid);
    }
  } else {
    data = mmo_get_object(objid);
  }

  return data;
}

static void
mmo_finalize_rtept_cb(const Waypoint* wptref)
{
  Waypoint* wpt = const_cast<Waypoint*>(wptref);

  if ((wpt->shortname[0] == 1) && (wpt->latitude == 0) && (wpt->longitude == 0)) {
    mmo_data_t* data;
    Waypoint* wpt2;

// This code path isn't tested in anything we have and I have  No Idea
// what it was trying to do.  Throw a hard error to force the hand of
// getting a sample file.
    abort();
#if OLD
    sscanf(wpt->shortname + 1, "%p", &data);
#endif
    wpt2 = (Waypoint*)data->data;

    wpt->latitude = wpt2->latitude;
    wpt->longitude = wpt2->longitude;
    wpt->shortname = wpt2->shortname;

    wpt->description = wpt2->description;
    wpt->notes = (wpt2->notes);
    if (wpt2->HasUrlLink()) {
      UrlLink l = wpt2->GetUrlLink();
      wpt->notes = l.url_;
    }

    wpt->proximity = wpt2->proximity;
    wpt->wpt_flags.proximity = wpt2->wpt_flags.proximity;

    if (!wpt2->icon_descr.isNull()) {
      wpt->icon_descr = wpt2->icon_descr;
    }
  }
}

/*******************************************************************************
* %%%        global callbacks called by gpsbabel main process              %%% *
*******************************************************************************/

static void
mmo_rd_init(const QString& fname)
{
  fin = gbfopen_le(fname, "rb", MYNAME);

  ico_object_id = pos_object_id = txt_object_id = cat_object_id = 0;
  wpt_object_id = rte_object_id = trk_object_id = 0;

  mmo_object_id = 0x8001;

  int i = 0;
  while (mmo_icon_value_table[i].icon) {
    mmo_register_icon(mmo_icon_value_table[i].value, mmo_icon_value_table[i].icon);
    i++;
  }
}


static void
mmo_rd_deinit()
{
  route_disp_session(curr_session(), nullptr, nullptr, mmo_finalize_rtept_cb);

  icons.clear();

  for (auto value : qAsConst(objects)) {
    mmo_free_object(value);
  }
  objects.clear();

  gbfclose(fin);
}


static void
mmo_read()
{
#ifdef MMO_DBG
  const char* sobj = "main";
#endif

  /* copy file to memory stream (needed for seek-ops and piped commands) */

  DBG(("main", "loading file \"%s\".\n", fin->name));

  gbfile* fx = gbfopen(nullptr, "wb", MYNAME);
  gbfcopyfrom(fx, fin, 0x7FFFFFFF);
  gbfrewind(fx);
  gbfclose(fin);
  fin = fx;

  mmo_obj_ct = gbfgetuint16(fin);
  DBG((sobj, "number of objects = %d\n", mmo_obj_ct));

  int i = gbfgetuint16(fin);
  if (i != 0xFFFF) {
    fatal(MYNAME ": Marker not equal to 0xFFFF!\n");
  }

  mmo_version = gbfgetuint16(fin);
  DBG((sobj, "version = 0x%02X\n", mmo_version));

  mmo_filemark = 0xFFFF0000UL | be_read16(&mmo_version);
  DBG((sobj, "filemark = 0x%08X\n", mmo_filemark));

  gbfseek(fin, -4, SEEK_CUR);

  while (! gbfeof(fin)) {		/* main read loop */

    (void) mmo_read_object();

  }

#ifdef MMO_DBG
  printf("\n" MYNAME ":---------------------------------------\n");
  printf(MYNAME ": EOF reached, nice!!!\n");
  printf(MYNAME ": =======================================\n\n");
#endif
}

/**************************************************************************/

static void
mmo_register_category_names(const QString& name)
{
  category_names.insert(name, mmo_object_id);
}


static void
mmo_writestr(const char* str)
{
  int ii, topbitset = 0;
  int len = strlen(str);

  // see if there's any utf-8 multi-byte chars
  for (ii = 0; ii < len; ii++) {
    if (str[ii] & 0x80) {
      topbitset = 1;
      break;
    }
  }
  // Old version can't handle utf-16
  // XXX ARB check which version number can, just guessed at 0x12
  if (mmo_version < 0x12) {
    topbitset = 0;
  }

  // XXX ARB need to convert UTF-8 into UTF-16
  if (topbitset) {
    gbfputc(0xFF, fout); // means two-byte length follows
    gbfputc(0xFE, fout); // means utf-16 little-endian string follows
    gbfputc(0xFF, fout); // ditto
    gbfputc(len, fout);
  } else if (len > 254) {
    len = len & 0x7FFF;
    gbfputc(0xFF, fout); // means two-byte length follows
    gbfputint16(len, fout);
  } else {
    gbfputc(len, fout);
  }
  if (len) {
    if (topbitset) {
      int utf16val;
      int utf16len;
      for (ii=0; ii<len; ii++) {
        cet_utf8_to_ucs4(str+ii, &utf16len, &utf16val);
        // this format only handles two-byte encoding
        // so only write the lower two bytes
        gbfputint16(utf16val & 0xffff, fout);
        // if utf8 char was multi-byte then skip them
        ii += (utf16len - 1);
      }
    } else {
      gbfwrite(str, len, 1, fout);
    }
  }
}

static void
mmo_writestr(const QString& str)
{
  // If UTF-8 is used instgead of Latin1, we fail in weird ways.
  mmo_writestr(str.toLatin1().constData());
}


static void
mmo_enum_waypt_cb(const Waypoint*)
{
  mmo_obj_ct++;
}


static void
mmo_enum_route_cb(const route_head* rte)
{
  if (rte->rte_waypt_ct > 0) {
    mmo_obj_ct++;
  }
}


static int
mmo_write_obj_mark(const char* sobj, const char* name)
{
  QString key = QString::fromUtf8(sobj);

  if (mmobjects.contains(key)) {
    uint16_t nr = mmobjects.value(key);
    gbfputuint16(nr, fout);
  } else {
    mmo_object_id++;

    DBG(("write", "object \"%s\", registered type \"%s\" (id = 0x%04X)\n",
         name, sobj, mmo_object_id));

    mmobjects.insert(key, mmo_object_id);

    gbfputuint32(mmo_filemark, fout);
    gbfputuint16(strlen(sobj), fout);
    gbfwrite(sobj, strlen(sobj), 1, fout);
  }

  mmo_object_id++;
  int res = mmo_object_id;
  mmo_writestr(name);

  return res;
}


static void
mmo_write_category(const char* sobj, const char* name)
{
  QString key = QString::fromUtf8(name);

  if (category_names.contains(key)) {
    uint16_t nr = category_names.value(key);
    gbfputuint16(nr & 0x7FFF, fout);
  } else {
    mmo_write_obj_mark(sobj, name);
    mmo_register_category_names(key);
  }
}


static int
mmo_write_obj_head(const char* sobj, const char* name, const time_t ctime,
                   const uint32_t obj_type)
{
  int res = mmo_write_obj_mark(sobj, name);

  gbfputuint32(ctime, fout);
  gbfputuint32(ctime, fout);

  gbfputc(*opt_locked, fout);
  gbfputc(*opt_visible, fout);

  gbfputuint32(obj_type, fout);

  return res;
}


static void
mmo_write_wpt_cb(const Waypoint* wpt)
{
  QString str;
  int icon = 0;

  time_t time = wpt->GetCreationTime().toTime_t();
  if (time < 0) {
    time = 0;
  }

  if (mmo_datatype == trkdata) {
    gbfputdbl(wpt->latitude, fout);
    gbfputdbl(wpt->longitude, fout);
    gbfputc(0, fout);
    gbfputuint32(time, fout);
    if (wpt->altitude != unknown_alt) {
      gbfputflt(wpt->altitude, fout);
    } else {
      gbfputflt(0, fout);
    }

    return;
  }

  DBG(("write", "waypoint \"%s\"\n", wpt->shortname ? wpt->shortname : "Mark"));
  int objid = mmo_write_obj_head("CObjWaypoint",
                                 wpt->shortname.isEmpty() ? "Mark" : CSTRc(wpt->shortname), time, obj_type_wpt);
  mmo_data_t* data = mmo_register_object(objid, wpt, wptdata);
  data->refct = 1;
  mmo_write_category("CCategory", (mmo_datatype == rtedata) ? "Waypoints" : "Marks");

  gbfputdbl(wpt->latitude, fout);
  gbfputdbl(wpt->longitude, fout);

  if (mmo_datatype == rtedata) {
    int i = mmo_get_objid(mmo_rte);
    gbfputuint16(1, fout); /* two extra bytes */
    gbfputuint16(i & 0x7FFF, fout);
  } else {
    gbfputuint16(0, fout);  /* extra bytes */
  }

  if (wpt->HasUrlLink()) {
    str = "_FILE_ ";
    UrlLink l = wpt->GetUrlLink();
    str += l.url_;
    str += "\n";
  }

  QString cx = wpt->notes;
  if (cx == nullptr) {
    cx = wpt->description;
  }
  if (cx != nullptr) {
    char* kml = nullptr;

    if (wpt->session->name == QLatin1String("kml")) {
      utf_string tmp(true, cx);
      cx = kml = strip_html(&tmp);
    }
    str += cx;
    if (kml) {
      xfree(kml);
    }
  }
  mmo_writestr(str);

  gbfputuint32(0x01, fout);
  if WAYPT_HAS(wpt, proximity) {
    gbfputflt((int) wpt->proximity, fout);
  } else {
    gbfputflt(0, fout);
  }

  if (!wpt->icon_descr.isNull()) {
    int i = 0;

    while (mmo_icon_value_table[i].icon) {
      if (wpt->icon_descr.compare(mmo_icon_value_table[i].icon, Qt::CaseInsensitive) == 0) {
        icon = mmo_icon_value_table[i].value;
        break;
      }
      i++;
    }
  }
  gbfputuint32(icon, fout);

  mmo_writestr("");		/* name on gps */
  gbfputuint32(0x00, fout);
}


static void
mmo_write_rte_head_cb(const route_head* rte)
{
  time_t time = 0x7FFFFFFF;

  if (rte->rte_waypt_ct <= 0) {
    return;
  }

  mmo_rte = rte;

  foreach (const Waypoint* wpt, rte->waypoint_list) {
    QDateTime t = wpt->GetCreationTime();
    if ((t.isValid()) && (t.toTime_t() < time)) {
      time = t.toTime_t();
    }
  }
  if (time == 0x7FFFFFFF) {
    time = gpsbabel_time;
  }
  int objid = mmo_write_obj_head("CObjRoute",
                                 rte->rte_name.isEmpty() ? "Route" : CSTRc(rte->rte_name), time, obj_type_rte);
  mmo_register_object(objid, rte, rtedata);
  mmo_write_category("CCategory", "Route");
  gbfputc(0, fout); /* unknown */
  gbfputuint16(rte->rte_waypt_ct, fout);
}


static void
mmo_write_rte_tail_cb(const route_head* rte)
{
  if (rte->rte_waypt_ct <= 0) {
    return;
  }

  DBG(("write", "route with %d point(s).\n", rte->rte_waypt_ct));

  if (mmo_version >= 0x12) {
    if (rte->line_color.bbggrr < 0) {
      gbfputuint32(0xFF, fout);	/* color; default red */
      gbfputc(0x01, fout);		/* Line width "normal" */
      gbfputc(0x00, fout);		/* Line style "solid"*/
      gbfputc(0x00, fout);		/* Transparency "Opaque" */
    } else {
      gbfputuint32(rte->line_color.bbggrr, fout);	/* color */
      gbfputc(0x01, fout);		/* Line width "normal" */
      gbfputc(0x00, fout);		/* Line style "solid"*/
      gbfputc((255 - rte->line_color.opacity) / 51, fout);	/* Transparency "Opaque" */
    }
  }

  foreach (const Waypoint* wpt, rte->waypoint_list) {
    int objid = mmo_get_objid(wpt);
    gbfputuint16(objid & 0x7FFF, fout);
  }
}


static void
mmo_write_trk_head_cb(const route_head* trk)
{
  if (trk->rte_waypt_ct <= 0) {
    return;
  }
  int objid = mmo_write_obj_head("CObjTrack",
                                 trk->rte_name.isEmpty() ? "Track" : CSTRc(trk->rte_name), gpsbabel_time, obj_type_trk);

  mmo_write_category("CCategory", "Track");
  gbfputuint16(trk->rte_waypt_ct, fout);

  mmo_register_object(objid, trk, trkdata);
}


static void
mmo_write_trk_tail_cb(const route_head* trk)
{
  if (trk->rte_waypt_ct <= 0) {
    return;
  }

  gbfputuint32(0x0A, fout);	/* Min. update interval */
  gbfputflt(0, fout);
  gbfputflt(0, fout);
  gbfputuint32(0x0F, fout);	/* Min. update distance */
  gbfputuint32(0xE10, fout);	/* Track partition interval */
  gbfputuint32(0x00, fout);	/* ??? */
  gbfputuint32(0x12C, fout);

  if (trk->line_color.bbggrr < 0) {
    gbfputuint32(0xFF0000, fout);	/* color; default blue */
    if (mmo_version >= 0x12) {
      gbfputc(0x01, fout);		/* Line width "normal" */
      gbfputc(0x00, fout);		/* Line style "solid"*/
      gbfputc(0x00, fout);		/* Transparency "Opaque" */
    }
  } else {
    gbfputuint32(trk->line_color.bbggrr, fout);	/* color */
    if (mmo_version >= 0x12) {
      gbfputc(0x01, fout);		/* Line width "normal" */
      gbfputc(0x00, fout);		/* Line style "solid"*/
      gbfputc((255 - trk->line_color.opacity) / 51, fout);	/* Transparency "Opaque" */
    }
  }
}

/**************************************************************************/

static void
mmo_wr_init(const QString& fname)
{
  fout = gbfopen_le(fname, "wb", MYNAME);

  mmo_object_id = 0x8000;
  mmo_obj_ct = 1;			/* ObjIcons always present */
  mmo_version = 0x12;		/* by default we write as version 0x12 */
  if (opt_version) {
    while (isspace(*opt_version)) {
      opt_version++;
    }
    errno = 0;
    mmo_version = strtol(opt_version, nullptr, 0);
    if (errno || ((mmo_version != 0x11) && (mmo_version != 0x12))) {
      fatal(MYNAME ": Unsupported version identifier (%s)!\n", opt_version);
    }
  }
  DBG(("write", "version = 0x%02X\n", mmo_version));
  mmo_filemark = 0xFFFFUL | (mmo_version << 16);
}


static void
mmo_wr_deinit()
{
  mmobjects.clear();
  category_names.clear();

  for (auto value : qAsConst(objects)) {
    mmo_free_object(value);
  }
  objects.clear();

  gbfclose(fout);
}


static void
mmo_write()
{
  /* find out number of objects we have to write */
  waypt_disp_all(mmo_enum_waypt_cb);
  route_disp_all(mmo_enum_route_cb, nullptr, mmo_enum_waypt_cb);
  track_disp_all(mmo_enum_route_cb, nullptr, nullptr);

  gbfputuint16(mmo_obj_ct, fout);

  mmo_write_obj_head("CObjIcons", "Unnamed object", gpsbabel_time, obj_type_ico);
  for (int i = 0; i < 5; i++) {
    gbfputuint16(0, fout);
  }

  mmo_datatype = wptdata;
  waypt_disp_all(mmo_write_wpt_cb);
  mmo_datatype = rtedata;
  route_disp_all(mmo_write_rte_head_cb, mmo_write_rte_tail_cb, mmo_write_wpt_cb);
  mmo_datatype = trkdata;
  track_disp_all(mmo_write_trk_head_cb, mmo_write_trk_tail_cb, mmo_write_wpt_cb);
}

/**************************************************************************/

ff_vecs_t mmo_vecs = {
  ff_type_file,
  FF_CAP_RW_ALL,	/* read and write waypoints, tracks and routes*/
  mmo_rd_init,
  mmo_wr_init,
  mmo_rd_deinit,
  mmo_wr_deinit,
  mmo_read,
  mmo_write,
  nullptr,
  mmo_args,
  CET_CHARSET_MS_ANSI, 0
  , NULL_POS_OPS,
  nullptr

};

/**************************************************************************/
