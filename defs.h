/*
    Copyright (C) 2002-2014 Robert Lipe, robertlipe+source@gpsbabel.org

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
#ifndef DEFS_H_INCLUDED_
#define DEFS_H_INCLUDED_

#include <cmath>                // for M_PI
#include <cstdarg>              // for va_list
#include <cstddef>              // for NULL, nullptr_t, size_t
#include <cstdint>              // for int32_t, uint32_t
#include <cstdio>               // for NULL, fprintf, FILE, stdout
#include <ctime>                // for time_t
#include <utility>              // for move

#if HAVE_CONFIG_H
#include "config.h"
#endif
#if HAVE_LIBZ
#include <zlib.h>               // doesn't really belong here, but is missing elsewhere.
#elif !ZLIB_INHIBITED
#include "zlib.h"               // doesn't really belong here, but is missing elsewhere.
#endif

#include <QtCore/QByteArray>    // for QByteArray
#include <QtCore/QChar>         // for QChar
#include <QtCore/QList>         // for QList, QList<>::const_reverse_iterator, QList<>::reverse_iterator
#include <QtCore/QString>       // for QString
#include <QtCore/QStringRef>    // for QStringRef
#include <QtCore/QTextCodec>    // for QTextCodec
#include <QtCore/Qt>            // for CaseInsensitive
#include <QtCore/QtGlobal>      // for foreach

#include "cet.h"                // for cet_cs_vec_t
#include "inifile.h"            // for inifile_t
#include "gbfile.h"             // doesn't really belong here, but is missing elsewhere.
#include "session.h"            // for session_t
#include "src/core/datetime.h"  // for DateTime
#include "src/core/optional.h"  // for optional


#define CSTR(qstr) ((qstr).toUtf8().constData())
#define CSTRc(qstr) ((qstr).toLatin1().constData())
#define STRFROMUNICODE(qstr) (global_opts.codec->fromUnicode(qstr).constData())
#define STRTOUNICODE(cstr) (global_opts.codec->toUnicode(cstr))

/*
 * Amazingly, this constant is not specified in the standard...
 */
#ifndef M_PI
#  define M_PI 3.14159265358979323846
#endif

#ifndef FALSE
#  define FALSE false
#endif

#ifndef TRUE
#  define TRUE true
#endif

#define FEET_TO_METERS(feetsies) ((feetsies) * 0.3048)
#define METERS_TO_FEET(meetsies) ((meetsies) * 3.2808399)

#define NMILES_TO_METERS(a) ((a) * 1852.0)	/* nautical miles */
#define METERS_TO_NMILES(a) ((a) / 1852.0)

#define MILES_TO_METERS(a) ((a) * 1609.344)
#define METERS_TO_MILES(a) ((a) / 1609.344)
#define FATHOMS_TO_METERS(a) ((a) * 1.8288)

#define CELSIUS_TO_FAHRENHEIT(a) (((a) * 1.8) + 32)
#define FAHRENHEIT_TO_CELSIUS(a) (((a) - 32) / 1.8)

#define SECONDS_PER_HOUR (60L*60)
#define SECONDS_PER_DAY (24L*60*60)

/* meters/second to kilometers/hour */
#define MPS_TO_KPH(a) ((double)(a)*SECONDS_PER_HOUR/1000)

/* meters/second to miles/hour */
#define MPS_TO_MPH(a) (METERS_TO_MILES(a) * SECONDS_PER_HOUR)

/* meters/second to knots */
#define MPS_TO_KNOTS(a) (MPS_TO_KPH((a)/1.852))

/* kilometers/hour to meters/second */
#define KPH_TO_MPS(a) ((double)(a)*1000/SECONDS_PER_HOUR)

/* miles/hour to meters/second */
#define MPH_TO_MPS(a) (MILES_TO_METERS(a) / SECONDS_PER_HOUR)

/* knots to meters/second */
#define KNOTS_TO_MPS(a) (KPH_TO_MPS((a)*1.852))

#define MILLI_TO_MICRO(t) ((t) * 1000)  /* Milliseconds to Microseconds */
#define MICRO_TO_MILLI(t) ((t) / 1000)  /* Microseconds to Milliseconds*/
#define CENTI_TO_MICRO(t) ((t) * 10000) /* Centiseconds to Microseconds */
#define MICRO_TO_CENTI(t) ((t) / 10000) /* Centiseconds to Microseconds */

/*
 * Snprintf is in SUS (so it's in most UNIX-like substance) and it's in
 * C99 (albeit with slightly different semantics) but it isn't in C89.
 * This tweaks allows us to use snprintf on the holdout.
 */
#if __WIN32__
#  define snprintf _snprintf
#  define vsnprintf _vsnprintf
#  ifndef fileno
#    define fileno _fileno
#  endif
#  define strdup _strdup
#endif

/* Turn off numeric conversion warning */
#if __WIN32__
#  if _MSC_VER
#    pragma warning(disable:4244)
#  endif
#if !defined _CRT_SECURE_NO_DEPRECATE
#  define _CRT_SECURE_NO_DEPRECATE 1
#endif
#endif

/* Pathname separator character */
#if __WIN32__
#  define GB_PATHSEP '\\'
#else
#  define GB_PATHSEP '/'
#endif

/*
 *  Toss in some GNU C-specific voodoo for checking.
 */
#if __GNUC__
#  define PRINTFLIKE(x,y) __attribute__ ((__format__ (__printf__, (x), (y))))
#else
#  define PRINTFLIKE(x,y)
#endif


/*
 * Common definitions.   There should be no protocol or file-specific
 * data in this file.
 */


/*
 * Define globally on which kind of data gpsbabel is working.
 * Important for "file types" that are essentially a communication
 * protocol for a receiver, like the Magellan serial data.
 */
typedef enum {
  unknown_gpsdata = 0,
  trkdata = 1,
  wptdata,
  rtedata,
  posndata
} gpsdata_type;

#define NOTHINGMASK		0U
#define WPTDATAMASK		1U
#define TRKDATAMASK		2U
#define	RTEDATAMASK		4U
#define	POSNDATAMASK		8U

/* mask objective testing */
#define	doing_nothing (global_opts.masked_objective == NOTHINGMASK)
#define	doing_wpts ((global_opts.masked_objective & WPTDATAMASK) == WPTDATAMASK)
#define	doing_trks ((global_opts.masked_objective & TRKDATAMASK) == TRKDATAMASK)
#define	doing_rtes ((global_opts.masked_objective & RTEDATAMASK) == RTEDATAMASK)
#define	doing_posn ((global_opts.masked_objective & POSNDATAMASK) == POSNDATAMASK)

typedef struct {
  int synthesize_shortnames;
  int debug_level;
  gpsdata_type objective;
  unsigned int	masked_objective;
  int verbose_status;	/* set by GUI wrappers for status */
  int smart_icons;
  int smart_names;
  cet_cs_vec_t* charset;
  QString charset_name;
  inifile_t* inifile;
  QTextCodec* codec;
} global_options;

extern global_options global_opts;
extern const char gpsbabel_version[];
extern time_t gpsbabel_now;	/* gpsbabel startup-time; initialized in main.c with time() */
extern time_t gpsbabel_time;	/* gpsbabel startup-time; initialized in main.c with current_time(), ! ZERO within testo ! */
extern int geocaches_present;

typedef enum {
  fix_unknown=-1,
  fix_none=0,
  fix_2d=1,
  fix_3d,
  fix_dgps,
  fix_pps
} fix_type;

typedef enum {
  status_unknown=0,
  status_true,
  status_false
} status_type;

/*
 * Extended data if waypoint happens to represent a geocache.  This is
 * totally voluntary data...
 */

typedef enum {
  gt_unknown = 0,
  gt_traditional,
  gt_multi,
  gt_virtual,
  gt_letterbox,
  gt_event,
  gt_suprise,
  gt_webcam,
  gt_earth,
  gt_locationless,
  gt_benchmark, /* Extension to Groundspeak for GSAK */
  gt_cito,
  gt_ape,
  gt_mega,
  gt_wherigo
} geocache_type;

typedef enum {
  gc_unknown = 0,
  gc_micro,
  gc_other,
  gc_regular,
  gc_large,
  gc_virtual,
  gc_small
} geocache_container;

class utf_string
{
public:
  utf_string() = default;
  utf_string(bool html, QString str) :
    is_html{html},
    utfstring{std::move(str)}
  {}
  bool is_html{false};
  QString utfstring;
};

class geocache_data
{
public:
  geocache_data() :
    id(0),
    type(gt_unknown),
    container(gc_unknown),
    diff(0),
    terr(0),
    is_archived(status_unknown),
    is_available(status_unknown),
    is_memberonly(status_unknown),
    has_customcoords(status_unknown),
    placer_id(0),
    favorite_points(0)
  {}
  int id; /* The decimal cache number */
  geocache_type type:5;
  geocache_container container:4;
  unsigned int diff:6; /* (multiplied by ten internally) */
  unsigned int terr:6; /* (likewise) */
  status_type is_archived:2;
  status_type is_available:2;
  status_type is_memberonly:2;
  status_type has_customcoords:2;
  gpsbabel::DateTime exported;
  gpsbabel::DateTime last_found;
  QString placer; /* Placer name */
  int placer_id; /* Placer id */
  QString hint; /* all these UTF8, XML entities removed, May be not HTML. */
  utf_string desc_short;
  utf_string desc_long;
  int favorite_points;
  QString personal_note;
};

typedef void (*fs_destroy)(void*);
typedef void (*fs_copy)(void**, void*);
typedef void (*fs_convert)(void*);

typedef struct format_specific_data {
  long type;
  struct format_specific_data* next;

  fs_destroy destroy;
  fs_copy copy;
  fs_convert convert;
} format_specific_data;

class gb_color
{
public:
  int bbggrr{-1};   // 32 bit color: Blue/Green/Red.  < 0 == unknown.
  unsigned char opacity{255};  // 0 == transparent.  255 == opaque.
};


format_specific_data* fs_chain_copy(format_specific_data* source);
void fs_chain_destroy(format_specific_data* chain);
format_specific_data* fs_chain_find(format_specific_data* chain, long type);
void fs_chain_add(format_specific_data** chain, format_specific_data* data);

#define FS_GPX 0x67707800L
#define FS_AN1W 0x616e3177L
#define FS_AN1L 0x616e316cL
#define FS_AN1V 0x616e3176L
#define FS_OZI 0x6f7a6900L
#define FS_GMSD 0x474d5344L	/* GMSD = Garmin specific data */
#define FS_LOWRANCEUSR4 0x615f234cL

/*
 * Structures and functions for multiple URLs per waypoint.
 */

class UrlLink
{
public:
  UrlLink() = default;
  UrlLink(QString url) :
    url_(std::move(url))
  { }
  UrlLink(const char* url) :
    url_(url)
  { }
  UrlLink(QString url, QString url_link_text) :
    url_(std::move(url)),
    url_link_text_(std::move(url_link_text))
  { }
  UrlLink(QString url, QString url_link_text, QString url_link_type) :
    url_(std::move(url)),
    url_link_text_(std::move(url_link_text)),
    url_link_type_(std::move(url_link_type))
  { }
  QString url_;
  QString url_link_text_;
  QString url_link_type_;
};

class UrlList : public QList<UrlLink>
{
public:
  void AddUrlLink(const UrlLink& l)
  {
    push_back(l);
  }

  bool HasUrlLink() const
  {
    return !isEmpty();
  }

  const UrlLink& GetUrlLink() const
  {
    return first();
  }
};

/*
 * Misc bitfields inside struct waypoint;
 */
class wp_flags
{
public:
  wp_flags() :
    shortname_is_synthetic(0),
    cet_converted(0),
    fmt_use(0),
    temperature(0),
    proximity(0),
    course(0),
    speed(0),
    geoidheight(0),
    depth(0),
    is_split(0),
    new_trkseg(0) {}
  unsigned int shortname_is_synthetic:1;
  unsigned int cet_converted:1;		/* strings are converted to UTF8; interesting only for input */
  unsigned int fmt_use:2;			/* lightweight "extra data" */
  /* "flagged fields" */
  unsigned int temperature:1;		/* temperature field is set */
  unsigned int proximity:1;		/* proximity field is set */
  unsigned int course:1;			/* course field is set */
  unsigned int speed:1;			/* speed field is set */
  unsigned int geoidheight:1;	/* geoidheight field is set */
  unsigned int depth:1;			/* depth field is set */
  /* !ToDo!
  unsigned int altitude:1;		/+ altitude field is set +/
  ... and others
  */
  unsigned int is_split:1;		/* the waypoint represents a split */
  unsigned int new_trkseg:1;		/* True if first in new trkseg. */

};

// These are dicey as they're collected on read. Subsequent filters may change
// things, though it's u nlikely to matter in practical terms.  Don't use these
// if a false positive would be deleterious.
#
class global_trait
{
public:
  global_trait() :
    trait_geocaches(0),
    trait_heartrate(0),
    trait_cadence(0),
    trait_power(0),
    trait_depth(0),
    trait_temperature(0) {}
  unsigned int trait_geocaches:1;
  unsigned int trait_heartrate:1;
  unsigned int trait_cadence:1;
  unsigned int trait_power:1;
  unsigned int trait_depth:1;
  unsigned int trait_temperature:1;
};

/*
 *  Bounding box information.
 */
typedef struct {
  double max_lat;
  double max_lon;
  double max_alt;	/*  unknown_alt => invalid */
  double min_lat;
  double min_lon;
  double min_alt;	/* -unknown_alt => invalid */
} bounds;

#define WAYPT_SET(wpt,member,val) { (wpt)->member = (val); wpt->wpt_flags.member = 1; }
#define WAYPT_GET(wpt,member,def) ((wpt->wpt_flags.member) ? (wpt->member) : (def))
#define WAYPT_UNSET(wpt,member) wpt->wpt_flags.member = 0
#define WAYPT_HAS(wpt,member) (wpt->wpt_flags.member)

/*
 * This is a waypoint, as stored in the GPSR.   It tries to not
 * cater to any specific model or protocol.  Anything that needs to
 * be truncated, edited, or otherwise trimmed should be done on the
 * way to the target.
 */
class Waypoint
{
private:
  static geocache_data empty_gc_data;

public:

  double latitude;		/* Degrees */
  double longitude; 		/* Degrees */
  double altitude; 		/* Meters. */
  double geoidheight;	/* Height (in meters) of geoid (mean sea level) above WGS84 earth ellipsoid. */

  /*
   * The "thickness" of a waypoint; adds an element of 3D.  Can be
   * used to construct rudimentary polygons for, say, airspace
   * definitions.   The units are meters.
   */
  double depth;

  /*
   * An alarm trigger value that can be considered to be a circle
   * surrounding a waypoint (or cylinder if depth is also defined).
   * The units are meters.
   */
  double proximity;

  /* shortname is a waypoint name as stored in receiver.  It should
   * strive to be, well, short, and unique.   Enforcing length and
   * character restrictions is the job of the output.   A typical
   * minimum length for shortname is 6 characters for NMEA units,
   * 8 for Magellan and 10 for Vista.   These are only guidelines.
   */
  QString shortname;
  /*
   * description is typically a human readable description of the
   * waypoint.   It may be used as a comment field in some receivers.
   * These are probably under 40 bytes, but that's only a guideline.
   */
  QString description;
  /*
   * notes are relatively long - over 100 characters - prose associated
   * with the above.   Unlike shortname and description, these are never
   * used to compute anything else and are strictly "passed through".
   * Few formats support this.
   */
  QString notes;

  UrlList urls;

  wp_flags wpt_flags;
  QString icon_descr;

  gpsbabel::DateTime creation_time;

  /*
   * route priority is for use by the simplify filter.  If we have
   * some reason to believe that the route point is more important,
   * we can give it a higher (numerically; 0 is the lowest) priority.
   * This causes it to be removed last.
   * This is currently used by the saroute input filter to give named
   * waypoints (representing turns) a higher priority.
   * This is also used by the google input filter because they were
   * nice enough to use exactly the same priority scheme.
   */
  int route_priority;

  /* Optional dilution of precision:  positional, horizontal, veritcal.
   * 1 <= dop <= 50
   */
  float hdop;
  float vdop;
  float pdop;
  float course;	/* Optional: degrees true */
  float speed;   	/* Optional: meters per second. */
  fix_type fix;	/* Optional: 3d, 2d, etc. */
  int  sat;	/* Optional: number of sats used for fix */

  unsigned char heartrate; /* Beats/min. likely to get moved to fs. */
  unsigned char cadence;	 /* revolutions per minute */
  float power; /* watts, as measured by cyclists */
  float temperature; /* Degrees celsius */
  float odometer_distance; /* Meters? */
  geocache_data* gc_data;
  format_specific_data* fs;
  const session_t* session;	/* pointer to a session struct */
  void* extra_data;	/* Extra data added by, say, a filter. */

public:
  Waypoint();
  ~Waypoint();
  Waypoint(const Waypoint& other);
  Waypoint& operator=(const Waypoint& other);

  bool HasUrlLink() const;
  const UrlLink& GetUrlLink() const;
  [[deprecated]] const QList<UrlLink> GetUrlLinks() const;
  void AddUrlLink(const UrlLink& l);
  QString CreationTimeXML() const;
  gpsbabel::DateTime GetCreationTime() const;
  void SetCreationTime(const gpsbabel::DateTime& t);
  void SetCreationTime(time_t t);
  void SetCreationTime(time_t t, int ms);
  geocache_data* AllocGCData();
  int EmptyGCData() const;
};

typedef void (*waypt_cb)(const Waypoint*);

// TODO: Consider using composition instead of private inheritance.
class WaypointList : private QList<Waypoint*>
{
public:
  typedef bool (*Compare)(const Waypoint* a, const Waypoint* b);

  void waypt_add(Waypoint* wpt); // a.k.a. append(), push_back()
  void add_rte_waypt(int waypt_ct, Waypoint* wpt, bool synth, const QString& namepart, int number_digits);
  // FIXME: Generally it is inefficient to use an element pointer or reference to define the element to be deleted, use iterator instead,
  //        and/or implement pop_back() a.k.a. removeLast(), and/or pop_front() a.k.a. removeFirst().
  void waypt_del(Waypoint* wpt); // a.k.a. erase()
  // FIXME: Generally it is inefficient to use an element pointer or reference to define the element to be deleted, use iterator instead,
  //        and/or implement pop_back() a.k.a. removeLast(), and/or pop_front() a.k.a. removeFirst().
  void del_rte_waypt(Waypoint* wpt);
  void waypt_compute_bounds(bounds* bounds) const;
  Waypoint* find_waypt_by_name(const QString& name) const;
  void flush(); // a.k.a. clear()
  void copy(WaypointList** dst) const;
  void restore(WaypointList* src);
  void swap(WaypointList& other);
  void sort(Compare cmp);
  template <typename T>
  void waypt_disp_session(const session_t* se, T cb);

  // Expose limited methods for portability.
  // public types
  using QList<Waypoint*>::const_iterator;
  using QList<Waypoint*>::const_reverse_iterator;
  using QList<Waypoint*>::iterator;
  using QList<Waypoint*>::reverse_iterator;
  // public functions
  using QList<Waypoint*>::back; // a.k.a. last()
  using QList<Waypoint*>::begin;
  using QList<Waypoint*>::cbegin;
  using QList<Waypoint*>::cend;
  using QList<Waypoint*>::count; // a.k.a. size()
  using QList<Waypoint*>::crbegin;
  using QList<Waypoint*>::crend;
  using QList<Waypoint*>::empty; // a.k.a. isEmpty()
  using QList<Waypoint*>::end;
  using QList<Waypoint*>::front; // a.k.a. first()
  using QList<Waypoint*>::rbegin;
  using QList<Waypoint*>::rend;
};

const global_trait* get_traits();
void waypt_init();
//void update_common_traits(const Waypoint* wpt);
void waypt_add(Waypoint* wpt);
void waypt_del(Waypoint* wpt);
unsigned int waypt_count();
void waypt_disp(const Waypoint* wpt);
void waypt_status_disp(int total_ct, int myct);
//void waypt_disp_all(waypt_cb); /* template */
//void waypt_disp_session(const session_t* se, waypt_cb cb); /* template */
void waypt_init_bounds(bounds* bounds);
int waypt_bounds_valid(bounds* bounds);
void waypt_add_to_bounds(bounds* bounds, const Waypoint* waypointp);
void waypt_compute_bounds(bounds* bounds);
Waypoint* find_waypt_by_name(const QString& name);
void waypt_flush_all();
void waypt_append(WaypointList* src);
void waypt_backup(WaypointList** head_bak);
void waypt_restore(WaypointList* head_bak);
void waypt_swap(WaypointList& other);
void waypt_sort(WaypointList::Compare cmp);
void waypt_add_url(Waypoint* wpt, const QString& link,
                   const QString& url_link_text);
void waypt_add_url(Waypoint* wpt, const QString& link,
                   const QString& url_link_text,
                   const QString& url_link_type);
double gcgeodist(double lat1, double lon1, double lat2, double lon2);
double waypt_time(const Waypoint* wpt);
double waypt_distance_ex(const Waypoint* A, const Waypoint* B);
double waypt_distance(const Waypoint* A, const Waypoint* B);
double waypt_speed_ex(const Waypoint* A, const Waypoint* B);
double waypt_speed(const Waypoint* A, const Waypoint* B);
double waypt_vertical_speed(const Waypoint* A, const Waypoint* B);
double waypt_gradient(const Waypoint* A, const Waypoint* B);
double waypt_course(const Waypoint* A, const Waypoint* B);

template <typename T>
void
WaypointList::waypt_disp_session(const session_t* se, T cb)
{
  int i = 0;
  foreach (Waypoint* waypointp, *this) {
    if ((se == nullptr) || (waypointp->session == se)) {
      if (global_opts.verbose_status) {
        i++;
        waypt_status_disp(waypt_count(), i);
      }
      cb(waypointp);
    }
  }
  if (global_opts.verbose_status) {
    fprintf(stdout, "\r\n");
  }
}

template <typename T>
void
waypt_disp_session(const session_t* se, T cb)
{
  extern WaypointList* global_waypoint_list;

  global_waypoint_list->waypt_disp_session(se, cb);
}

template <typename T>
void
waypt_disp_all(T cb)
{
  extern WaypointList* global_waypoint_list;

  global_waypoint_list->waypt_disp_session(nullptr, cb);
}

/*
 *  Structure of recomputed track/route data.
 */
struct computed_trkdata {
  double distance_meters{0.0};
  gpsbabel_optional::optional<double> max_alt;	/* Meters */
  gpsbabel_optional::optional<double> min_alt;	/* Meters */
  gpsbabel_optional::optional<double> max_spd;	/* Meters/sec */
  gpsbabel_optional::optional<double> min_spd;	/* Meters/sec */
  gpsbabel_optional::optional<double> avg_hrt;	/* Avg Heartrate */
  gpsbabel_optional::optional<double> avg_cad;	/* Avg Cadence */
  gpsbabel::DateTime start;		/* Min time */
  gpsbabel::DateTime end;		/* Max time */
  gpsbabel_optional::optional<int> min_hrt;			/* Min Heartrate */
  gpsbabel_optional::optional<int> max_hrt;			/* Max Heartrate */
  gpsbabel_optional::optional<int> max_cad;			/* Max Cadence */
};

class route_head
{
public:
  WaypointList waypoint_list;	/* List of child waypoints */
  QString rte_name;
  QString rte_desc;
  UrlList rte_urls;
  int rte_num;
  int rte_waypt_ct;		/* # waypoints in waypoint list */
  format_specific_data* fs;
  unsigned short cet_converted;	/* strings are converted to UTF8; interesting only for input */
  gb_color line_color;         /* Optional line color for rendering */
  int line_width;         /* in pixels (sigh).  < 0 is unknown. */
  const session_t* session;	/* pointer to a session struct */

public:
  route_head();
  // the default copy constructor and assignment operator are not appropriate as we do deep copy of some members,
  // and we haven't bothered to write an appropriate one.
  // Catch attempts to use the default copy constructor and assignment operator.
  route_head(const route_head& other) = delete;
  route_head& operator=(const route_head& rhs) = delete;
  ~route_head();
};

typedef void (*route_hdr)(const route_head*);
typedef void (*route_trl)(const route_head*);

// TODO: Consider using composition instead of private inheritance.
class RouteList : private QList<route_head*>
{
public:
  typedef bool (*Compare)(const route_head* a, const route_head* b);

  int waypt_count() const;
  void add_head(route_head* rte); // a.k.a. append(), push_back()
  // FIXME: Generally it is inefficient to use an element pointer or reference to define the element to be deleted, use iterator instead,
  //        and/or implement pop_back() a.k.a. removeLast(), and/or pop_front() a.k.a. removeFirst().
  void del_head(route_head* rte); // a.k.a. erase()
  // FIXME: Generally it is inefficent to use an element pointer or reference to define the insertion point, use iterator instead.
  void insert_head(route_head* rte, route_head* predecessor); // a.k.a. insert
  void add_wpt(route_head* rte, Waypoint* wpt, bool synth, const QString& namepart, int number_digits);
  // FIXME: Generally it is inefficent to use an element pointer or reference to define the insertion point, use iterator instead.
  void del_wpt(route_head* rte, Waypoint* wpt);
  void common_disp_session(const session_t* se, route_hdr rh, route_trl rt, waypt_cb wc);
  void flush(); // a.k.a. clear()
  void copy(RouteList** dst) const;
  void restore(RouteList* src);
  void swap(RouteList& other);
  void sort(Compare cmp);
  template <typename T1, typename T2, typename T3>
  void disp_all(T1 rh, T2 rt, T3 wc);
  template <typename T2, typename T3>
  void disp_all(std::nullptr_t /* rh */, T2 rt, T3 wc);
  template <typename T1, typename T3>
  void disp_all(T1 rh, std::nullptr_t /* rt */, T3 wc);
  template <typename T3>
  void disp_all(std::nullptr_t /* rh */, std::nullptr_t /* rt */, T3 wc);

  // Only expose methods from our underlying container that won't corrupt our private data.
  // Our contained element (route_head) also contains a container (waypoint_list), 
  // and we maintain a total count the elements in these contained containers, i.e.
  // the total number of waypoints in all the routes in the RouteList.
  // public types
  using QList<route_head*>::const_iterator;
  using QList<route_head*>::const_reverse_iterator;
  using QList<route_head*>::iterator;
  using QList<route_head*>::reverse_iterator;
  // public functions
  using QList<route_head*>::back; // a.k.a. last()
  using QList<route_head*>::begin;
  using QList<route_head*>::cbegin;
  using QList<route_head*>::cend;
  using QList<route_head*>::count; // a.k.a. size()
  using QList<route_head*>::crbegin;
  using QList<route_head*>::crend;
  using QList<route_head*>::empty; // a.k.a. isEmpty()
  using QList<route_head*>::end;
  using QList<route_head*>::front; // a.k.a. first()
  using QList<route_head*>::rbegin;
  using QList<route_head*>::rend;

private:
  int waypt_ct{0};
};

void route_init();
unsigned int route_waypt_count();
unsigned int route_count();
unsigned int track_waypt_count();
unsigned int track_count();
route_head* route_head_alloc();
void route_add_head(route_head* rte);
void route_del_head(route_head* rte);
void track_add_head(route_head* rte);
void track_del_head(route_head* rte);
void track_insert_head(route_head* rte, route_head* predecessor);
void route_add_wpt(route_head* rte, Waypoint* wpt, const QString& namepart = "RPT", int number_digits = 3);
void track_add_wpt(route_head* rte, Waypoint* wpt, const QString& namepart = "RPT", int number_digits = 3);
void route_del_wpt(route_head* rte, Waypoint* wpt);
void track_del_wpt(route_head* rte, Waypoint* wpt);
//void route_disp(const route_head* rte, waypt_cb); /* template */
void route_disp(const route_head* rte, std::nullptr_t /* waypt_cb */); /* override to catch nullptr */
//void route_disp_all(route_hdr, route_trl, waypt_cb); /* template */
//void track_disp_all(route_hdr, route_trl, waypt_cb); /* template */
void route_disp_session(const session_t* se, route_hdr rh, route_trl rt, waypt_cb wc);
void track_disp_session(const session_t* se, route_hdr rh, route_trl rt, waypt_cb wc);
void route_flush_all_routes();
void route_flush_all_tracks();
void route_deinit();
void route_append(RouteList* src);
void track_append(RouteList* src);
void route_backup(RouteList** head_bak);
void route_restore(RouteList* head_bak);
void route_swap(RouteList& other);
void route_sort(RouteList::Compare cmp);
void track_backup(RouteList** head_bak);
void track_restore(RouteList* head_bak);
void track_swap(RouteList& other);
void track_sort(RouteList::Compare cmp);
computed_trkdata track_recompute(const route_head* trk);

template <typename T>
void
route_disp(const route_head* rh, T cb)
{
// cb != nullptr, caught with an overload of route_disp
  foreach (const Waypoint* waypointp, rh->waypoint_list) {
    cb(waypointp);
  }
}

template <typename T1, typename T2, typename T3>
void
RouteList::disp_all(T1 rh, T2 rt, T3 wc)
{
  foreach (const route_head* rhp, *this) {
// rh != nullptr, caught with an overload of common_disp_all
    rh(rhp);
    route_disp(rhp, wc);
// rt != nullptr, caught with an overload of common_disp_all
    rt(rhp);
  }
}

template <typename T2, typename T3>
void
RouteList::disp_all(std::nullptr_t /* rh */, T2 rt, T3 wc)
{
  foreach (const route_head* rhp, *this) {
// rh == nullptr
    route_disp(rhp, wc);
// rt != nullptr, caught with an overload of common_disp_all
    rt(rhp);
  }
}

template <typename T1, typename T3>
void
RouteList::disp_all(T1 rh, std::nullptr_t /* rt */, T3 wc)
{
  foreach (const route_head* rhp, *this) {
// rh != nullptr, caught with an overload of common_disp_all
    rh(rhp);
    route_disp(rhp, wc);
// rt == nullptr
  }
}

template <typename T3>
void
RouteList::disp_all(std::nullptr_t /* rh */, std::nullptr_t /* rt */, T3 wc)
{
  foreach (const route_head* rhp, *this) {
// rh == nullptr
    route_disp(rhp, wc);
// rt == nullptr
  }
}

template <typename T1, typename T2, typename T3>
void
route_disp_all(T1 rh, T2 rt, T3 wc)
{
  extern RouteList* global_route_list;

  global_route_list->disp_all(rh, rt, wc);
}

template <typename T1, typename T2, typename T3>
void
track_disp_all(T1 rh, T2 rt, T3 wc)
{
  extern RouteList* global_track_list;

  global_track_list->disp_all(rh, rt, wc);
}

typedef struct {
  volatile int request_terminate;
} posn_status;

extern posn_status tracking_status;

typedef void (*ff_init)(const QString&);
typedef void (*ff_deinit)();
typedef void (*ff_read)();
typedef void (*ff_write)();
typedef void (*ff_exit)();
typedef void (*ff_writeposn)(Waypoint*);
typedef Waypoint* (*ff_readposn)(posn_status*);

char* get_option(const char* iarglist, const char* argname);

geocache_type gs_mktype(const QString& t);
geocache_container gs_mkcont(const QString& t);

/*
 * All shortname functions take a shortname handle as the first arg.
 * This is an opaque pointer.  Callers must not fondle the contents of it.
 */
// This is a crutch until the new C++ shorthandle goes in.

struct mkshort_handle_imp; // forward declare, definition in mkshort.cc
typedef mkshort_handle_imp* short_handle;

char* mkshort(short_handle,  const char*);
QString mkshort(short_handle,  const QString&);
short_handle mkshort_new_handle();
QString mkshort_from_wpt(short_handle h, const Waypoint* wpt);
void mkshort_del_handle(short_handle* h);
void setshort_length(short_handle, int n);
void setshort_badchars(short_handle,  const char*);
void setshort_goodchars(short_handle,  const char*);
void setshort_mustupper(short_handle,  int n);
void setshort_mustuniq(short_handle,  int n);
void setshort_whitespace_ok(short_handle,  int n);
void setshort_repeating_whitespace_ok(short_handle,  int n);
void setshort_defname(short_handle, const char* s);
void setshort_is_utf8(short_handle h, int is_utf8);

#define ARGTYPE_UNKNOWN    0x00000000
#define ARGTYPE_INT        0x00000001
#define ARGTYPE_FLOAT      0x00000002
#define ARGTYPE_STRING     0x00000003
#define ARGTYPE_BOOL       0x00000004
#define ARGTYPE_FILE       0x00000005
#define ARGTYPE_OUTFILE    0x00000006

/* REQUIRED means that the option is required to be set.
 * See also BEGIN/END_REQ */
#define ARGTYPE_REQUIRED   0x40000000U

/* HIDDEN means that the option does not appear in help texts.  Useful
 * for debugging or testing options */
#define ARGTYPE_HIDDEN     0x20000000U

/* BEGIN/END_EXCL mark the beginning and end of an exclusive range of
 * options. No more than one of the options in the range may be selected
 * or set. If exactly one must be set, use with BEGIN/END_REQ
 * Both of these flags set is just like neither set, so avoid doing that. */
#define ARGTYPE_BEGIN_EXCL 0x10000000U
#define ARGTYPE_END_EXCL   0x08000000U

/* BEGIN/END_REQ mark the beginning and end of a required range of
 * options.  One or more of the options in the range MUST be selected or set.
 * If exactly one must be set, use with BEGIN/END_EXCL
 * Both of these flags set is synonymous with REQUIRED, so use that instead
 * for "groups" of exactly one option. */
#define ARGTYPE_BEGIN_REQ  0x04000000U
#define ARGTYPE_END_REQ    0x02000000U

#define ARGTYPE_TYPEMASK 0x00000fffU
#define ARGTYPE_FLAGMASK 0xfffff000U

#define ARG_NOMINMAX NULL, NULL
#define ARG_TERMINATOR {0, 0, 0, 0, 0, ARG_NOMINMAX, NULL}

typedef struct arglist {
  const char* argstring;
  char** argval;
  const char* helpstring;
  const char* defaultvalue;
  const uint32_t argtype;
  const char* minvalue;		/* minimum value for numeric options */
  const char* maxvalue;		/* maximum value for numeric options */
  char* argvalptr;	/* !!! internal helper. Not used in definitions !!! */
} arglist_t;

typedef enum {
  ff_type_file = 1,	/* normal format: useful to a GUI. */
  ff_type_internal,	/* fmt not useful with default options */
  ff_type_serial		/* format describes a serial protocol (GUI can display port names) */
} ff_type;

typedef enum {
  ff_cap_rw_wpt,
  ff_cap_rw_trk,
  ff_cap_rw_rte
} ff_cap_array;

typedef enum {
  ff_cap_none,
  ff_cap_read = 1,
  ff_cap_write = 2
} ff_cap;

#define FF_CAP_RW_ALL \
	{ (ff_cap) (ff_cap_read | ff_cap_write), (ff_cap) (ff_cap_read | ff_cap_write), (ff_cap) (ff_cap_read | ff_cap_write) }

#define FF_CAP_RW_WPT \
	{ (ff_cap) (ff_cap_read | ff_cap_write), ff_cap_none, ff_cap_none}

/*
 * Format capabilities for realtime positioning.
 */
typedef struct position_ops {
  ff_init rd_init;
  ff_readposn rd_position;
  ff_deinit rd_deinit;

  ff_init wr_init;
  ff_writeposn wr_position;
  ff_deinit wr_deinit;
} position_ops_t;

#define NULL_POS_OPS { 0, 0, 0, 0, 0, 0, }

/*
 *  Describe the file format to the caller.
 */
typedef struct ff_vecs {
  ff_type type;
  ff_cap cap[3];
  ff_init rd_init;
  ff_init wr_init;
  ff_deinit rd_deinit;
  ff_deinit wr_deinit;
  ff_read read;
  ff_write write;
  ff_exit exit;
  arglist_t* args;
  const char* encode;
  int fixed_encode;
  position_ops_t position_ops;
  const char* name;		/* dyn. initialized by find_vec */
} ff_vecs_t;

typedef struct style_vecs {
  const char* name;
  const char* style_buf;
} style_vecs_t;
extern style_vecs_t style_list[];

[[noreturn]] void fatal(const char*, ...) PRINTFLIKE(1, 2);
void is_fatal(int condition, const char*, ...) PRINTFLIKE(2, 3);
void warning(const char*, ...) PRINTFLIKE(1, 2);
void debug_print(int level, const char* fmt, ...) PRINTFLIKE(2,3);

ff_vecs_t* find_vec(const char*, const char**);
void assign_option(const char* vecname, arglist_t* ap, const char* val);
void disp_vec_options(const char* vecname, arglist_t* ap);
void disp_vecs();
void disp_vec(const char* vecname);
void init_vecs();
void exit_vecs();
void disp_formats(int version);
const char* name_option(uint32_t type);
void printposn(double c, int is_lat);

void* xcalloc(size_t nmemb, size_t size);
void* xmalloc(size_t size);
void* xrealloc(void* p, size_t s);
void xfree(const void* mem);
char* xstrdup(const QString& s);
char* xstrndup(const char* str, size_t sz);
char* xstrappend(char* src, const char* newd);
char* xstrdup(const char* s);

FILE* xfopen(const char* fname, const char* type, const char* errtxt);

// Thin wrapper around fopen() that supports Unicode fname on all platforms.
FILE* ufopen(const QString& fname, const char* mode);

// OS-abstracting wrapper for getting Unicode environment variables.
QString ugetenv(const char* env_var);

// FIXME: case_ignore_strcmp() and case_ignore_strncmp() should probably
// just be replaced at the call sites.  These shims are just here to make
// them more accomidating of QString input.
inline int
case_ignore_strcmp(const QString& s1, const QString& s2)
{
  return QString::compare(s1, s2, Qt::CaseInsensitive);
}
// In 95% of the callers, this could be s1.startsWith(s2)...
inline int case_ignore_strncmp(const QString& s1, const QString& s2, int n)
{
  return s1.leftRef(n).compare(s2.left(n), Qt::CaseInsensitive);
}

int str_match(const char* str, const char* match);

char* strsub(const char* s, const char* search, const char* replace);
char* gstrsub(const char* s, const char* search, const char* replace);

void rtrim(char* s);
char* lrtrim(char* buff);
int xasprintf(char** strp, const char* fmt, ...) PRINTFLIKE(2, 3);
int xasprintf(QString* strp, const char* fmt, ...) PRINTFLIKE(2, 3);
int xvasprintf(char** strp, const char* fmt, va_list ap);
char* strupper(char* src);
char* strlower(char* src);
signed int get_tz_offset();
time_t mklocaltime(struct tm* t);
time_t mkgmtime(struct tm* t);
gpsbabel::DateTime current_time();
void dotnet_time_to_time_t(double dotnet, time_t* t, int* millisecs);
signed int month_lookup(const char* m);
const char* get_cache_icon(const Waypoint* waypointp);
const char* gs_get_cachetype(geocache_type t);
const char* gs_get_container(geocache_container t);
char* xml_entitize(const char* str);
char* html_entitize(const QString& str);
char* strip_html(const utf_string*);
char* strip_nastyhtml(const QString& in);
char* convert_human_date_format(const char* human_datef);	/* "MM,YYYY,DD" -> "%m,%Y,%d" */
char* convert_human_time_format(const char* human_timef);	/* "HH+mm+ss"   -> "%H+%M+%S" */
char* pretty_deg_format(double lat, double lon, char fmt, const char* sep, int html);    /* decimal ->  dd.dddd or dd mm.mmm or dd mm ss */

const QString get_filename(const QString& fname);			/* extract the filename portion */

/*
 * Character encoding transformations.
 */

#define CET_NOT_CONVERTABLE_DEFAULT '$'
#define CET_CHARSET_ASCII	"US-ASCII"
#define CET_CHARSET_UTF8	"UTF-8"
#define CET_CHARSET_HEBREW  "ISO-8859-8"
#define CET_CHARSET_MS_ANSI	"windows-1252"
#define CET_CHARSET_LATIN1	"ISO-8859-1"

#define str_utf8_to_cp1252(str) cet_str_utf8_to_cp1252((str))
#define str_cp1252_to_utf8(str) cet_str_cp1252_to_utf8((str))

#define str_utf8_to_iso8859_1(str) cet_str_utf8_to_iso8859_1((str))
#define str_iso8859_1_to_utf8(str) cet_str_iso8859_1_to_utf8((str))

/* this lives in gpx.c */
gpsbabel::DateTime xml_parse_time(const QString& cdatastr);

QString rot13(const QString& s);

/*
 * PalmOS records like fixed-point numbers, which should be rounded
 * to deal with possible floating-point representation errors.
 */

signed int si_round(double d);

/*
 * Protypes for Endianness helpers.
 */

signed int be_read16(const void* ptr);
unsigned int be_readu16(const void* ptr);
signed int be_read32(const void* ptr);
signed int le_read16(const void* ptr);
unsigned int le_readu16(const void* ptr);
signed int le_read32(const void* ptr);
unsigned int le_readu32(const void* ptr);
void le_read64(void* dest, const void* src);
void be_write16(void* ptr, unsigned value);
void be_write32(void* ptr, unsigned value);
void le_write16(void* ptr, unsigned value);
void le_write32(void* ptr, unsigned value);

double endian_read_double(const void* ptr, int read_le);
float  endian_read_float(const void* ptr, int read_le);
void   endian_write_double(void* ptr, double value, int write_le);
void   endian_write_float(void* ptr, float value, int write_le);

float  be_read_float(void* ptr);
double be_read_double(void* ptr);
void   be_write_float(void* ptr, float value);
void   be_write_double(void* ptr, double value);

float  le_read_float(const void* ptr);
double le_read_double(const void* ptr);
void   le_write_float(void* ptr, float value);
void   le_write_double(void* ptr, double value);

/*
 * Prototypes for generic conversion routines (util.c).
 */

double ddmm2degrees(double pcx_val);
double degrees2ddmm(double deg_val);

typedef enum {
  grid_unknown = -1,
  grid_lat_lon_ddd = 0,
  grid_lat_lon_dmm = 1,
  grid_lat_lon_dms = 2,
  grid_bng = 3,
  grid_utm = 4,
  grid_swiss = 5
} grid_type;

#define GRID_INDEX_MIN	grid_lat_lon_ddd
#define GRID_INDEX_MAX	grid_swiss

#define DATUM_OSGB36	86
#define DATUM_WGS84	118

/* bit manipulation functions (util.c) */

char gb_getbit(const void* buf, uint32_t nr);
void gb_setbit(void* buf, uint32_t nr);

void* gb_int2ptr(int i);
int gb_ptr2int(const void* p);

void list_codecs();

/*
 *  From parse.c
 */
int parse_coordinates(const char* str, int datum, grid_type grid,
                      double* latitude, double* longitude, const char* module);
int parse_coordinates(const QString& str, int datum, grid_type grid,
                      double* latitude, double* longitude, const char* module);
int parse_distance(const char* str, double* val, double scale, const char* module);
int parse_distance(const QString& str, double* val, double scale, const char* module);
int parse_speed(const char* str, double* val, double scale, const char* module);
int parse_speed(const QString& str, double* val, double scale, const char* module);

/*
 *  From util_crc.c
 */
unsigned long get_crc32(const void* data, int datalen);

/*
 *  From units.c
 */
typedef enum {
  units_unknown = 0,
  units_statute = 1,
  units_metric = 2,
  units_nautical =3,
  units_aviation =4
} fmt_units;

int    fmt_setunits(fmt_units);
double fmt_distance(double, const char** tag);
double fmt_altitude(double, const char** tag);
double fmt_speed(double, const char** tag);

/*
 * From nmea.c
 */
int nmea_cksum(const char* buf);

/*
 * Color helpers.
 */
int color_to_bbggrr(const char* cname);

/*
 * A constant for unknown altitude.   It's tempting to just use zero
 * but that's not very nice for the folks near sea level.
 */
#define unknown_alt 	-99999999.0
#define unknown_color	-1

// TODO: this is a (probably temporary) shim for the C->QString conversion.
// It's here instead of gps to avoid C/C++ linkage issues.
int32_t GPS_Lookup_Datum_Index(const QString& n);

#endif // DEFS_H_INCLUDED_
