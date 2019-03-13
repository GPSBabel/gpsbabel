/*
	Support for Google Earth & Keyhole "kml" format.

	Copyright (C) 2005-2013 Robert Lipe, robertlipe+source@gpsbabel.org
	Updates by Andrew Kirmse, akirmse at google.com

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

#ifdef __WIN32__
# include <windows.h>
#endif

#include <cctype>                      // for tolower, toupper
#include <cmath>                       // for fabs
#include <cstdio>                      // for sscanf, printf
#include <cstdlib>                     // for atoi, atol, atof
#include <cstring>                     // for strcmp
#include <tuple>                       // for tuple, make_tuple, tie

#include <QtCore/QByteArray>           // for QByteArray
#include <QtCore/QChar>                // for QChar
#include <QtCore/QDate>                // for QDate
#include <QtCore/QDateTime>            // for QDateTime
#include <QtCore/QFile>                // for QFile
#include <QtCore/QIODevice>            // for operator|, QIODevice, QIODevice::Text, QIODevice::WriteOnly
#include <QtCore/QList>                // for QList
#include <QtCore/QStaticStringData>    // for QStaticStringData
#include <QtCore/QString>              // for QString, QStringLiteral, operator+, operator!=
#include <QtCore/QXmlStreamAttributes> // for QXmlStreamAttributes
#include <QtCore/Qt>                   // for ISODate
#include <QtCore/QtGlobal>             // for foreach, qint64, qPrintable

#include "defs.h"
#include "grtcirc.h"                    // for RAD, gcdist, radtometers
#include "src/core/datetime.h"          // for DateTime
#include "src/core/file.h"              // for File
#include "src/core/optional.h"          // for optional
#include "src/core/xmlstreamwriter.h"   // for XmlStreamWriter
#include "src/core/xmltag.h"            // for xml_findfirst, xml_tag, fs_xml, xml_attribute, xml_findnext
#include "xmlgeneric.h"                 // for cb_cdata, cb_end, cb_start, xg_callback, xg_string, xg_cb_type, xml_deinit, xml_ignore_tags, xml_init, xml_read, xg_tag_mapping


// options
static char* opt_deficon = nullptr;
static char* opt_export_lines = nullptr;
static char* opt_export_points = nullptr;
static char* opt_export_track = nullptr;
static char* opt_line_width = nullptr;
static char* opt_line_color = nullptr;
static char* opt_floating = nullptr;
static char* opt_extrude = nullptr;
static char* opt_trackdata = nullptr;
static char* opt_trackdirection = nullptr;
static char* opt_units = nullptr;
static char* opt_labels = nullptr;
static char* opt_max_position_points = nullptr;
static char* opt_rotate_colors = nullptr;
static char* opt_precision = nullptr;

static int export_lines;
static int export_points;
static int export_track;
static int floating;
static int extrude;
static int trackdata;
static int trackdirection;
static int max_position_points;
static int rotate_colors;
static int line_width;
static int html_encrypt;
static int precision;

static Waypoint* wpt_tmp;
static int wpt_tmp_queued;
static QString posnfilename;
static QString posnfilenametmp;

static route_head* gx_trk_head;
static QList<gpsbabel::DateTime>* gx_trk_times;
static QList<std::tuple<int, double, double, double>>* gx_trk_coords;

static gpsbabel::File* oqfile;
static gpsbabel::XmlStreamWriter* writer;

typedef enum  {
  kmlpt_unknown,
  kmlpt_waypoint,
  kmlpt_track,
  kmlpt_route,
  kmlpt_multitrack,
  kmlpt_other
} kml_point_type;

static int realtime_positioning;
static bounds kml_bounds;
static gpsbabel::DateTime kml_time_min;
static gpsbabel::DateTime kml_time_max;

#define DEFAULT_PRECISION "6"

//  Icons provided and hosted by Google.  Used with permission.
#define ICON_BASE "https://earth.google.com/images/kml-icons/"

// Multitrack ids to correlate Schema to SchemaData
static const char kmt_heartrate[] = "heartrate";
static const char kmt_cadence[] = "cadence";
static const char kmt_temperature[] = "temperature";
static const char kmt_depth[] = "depth";
static const char kmt_power[] = "power";


static
arglist_t kml_args[] = {
  {"deficon", &opt_deficon, "Default icon name", nullptr, ARGTYPE_STRING, ARG_NOMINMAX, nullptr },
  {
    "lines", &opt_export_lines,
    "Export linestrings for tracks and routes",
    "1", ARGTYPE_BOOL, ARG_NOMINMAX, nullptr,
  },
  {
    "points", &opt_export_points,
    "Export placemarks for tracks and routes",
    "1", ARGTYPE_BOOL, ARG_NOMINMAX, nullptr
  },
  {
    "line_width", &opt_line_width,
    "Width of lines, in pixels",
    "6", ARGTYPE_INT, ARG_NOMINMAX, nullptr
  },
  {
    "line_color", &opt_line_color,
    "Line color, specified in hex AABBGGRR",
    "99ffac59", ARGTYPE_STRING, ARG_NOMINMAX, nullptr
  },
  {
    "floating", &opt_floating,
    "Altitudes are absolute and not clamped to ground",
    "0", ARGTYPE_BOOL, ARG_NOMINMAX, nullptr
  },
  {
    "extrude", &opt_extrude,
    "Draw extrusion line from trackpoint to ground",
    "0", ARGTYPE_BOOL, ARG_NOMINMAX, nullptr
  },
  {
    "track", &opt_export_track,
    "Write KML track (default = 0)",
    "0", ARGTYPE_BOOL, ARG_NOMINMAX, nullptr
  },
  {
    "trackdata", &opt_trackdata,
    "Include extended data for trackpoints (default = 1)",
    "1", ARGTYPE_BOOL, ARG_NOMINMAX, nullptr
  },
  {
    "trackdirection", &opt_trackdirection,
    "Indicate direction of travel in track icons (default = 0)",
    "0", ARGTYPE_BOOL, ARG_NOMINMAX, nullptr
  },
  {
    "units", &opt_units,
    "Units used when writing comments ('s'tatute, 'm'etric,' 'n'autical, 'a'viation)",
    "s", ARGTYPE_STRING, ARG_NOMINMAX, nullptr
  },
  {
    "labels", &opt_labels,
    "Display labels on track and routepoints  (default = 1)",
    "1", ARGTYPE_BOOL, ARG_NOMINMAX, nullptr
  },
  {
    "max_position_points", &opt_max_position_points,
    "Retain at most this number of position points  (0 = unlimited)",
    "0", ARGTYPE_INT, ARG_NOMINMAX, nullptr
  },
  {
    "rotate_colors", &opt_rotate_colors,
    "Rotate colors for tracks and routes (default automatic)",
    nullptr, ARGTYPE_FLOAT, "0", "360", nullptr
  },
  {
    "prec", &opt_precision,
    "Precision of coordinates, number of decimals",
    DEFAULT_PRECISION, ARGTYPE_INT, ARG_NOMINMAX, nullptr
  },
  ARG_TERMINATOR
};

static
struct {
  int freshness;
  const char* icon;
} kml_tracking_icons[] = {
  { 60, ICON_BASE "youarehere-60.png" }, // Red
  { 30, ICON_BASE "youarehere-30.png" }, // Yellow
  { 0,  ICON_BASE "youarehere-0.png" }, // Green
};

#define ICON_NOSAT ICON_BASE "youarehere-warning.png";
#define ICON_WPT "https://maps.google.com/mapfiles/kml/pal4/icon61.png"
#define ICON_TRK ICON_BASE "track-directional/track-none.png"
#define ICON_RTE ICON_BASE "track-directional/track-none.png"
#define ICON_MULTI_TRK ICON_BASE "track-directional/track-0.png"
#define ICON_DIR ICON_BASE "track-directional/track-%1.png" // format string where next arg is rotational degrees.

static struct {
  float seq{0.0f};
  float step{0.0f};
  gb_color color;
} kml_color_sequencer;
#define KML_COLOR_LIMIT 204	/* allowed range [0,255] */

#define MYNAME "kml"

static void kml_init_color_sequencer(unsigned int steps_per_rev)
{
  if (rotate_colors) {
    float color_step = atof(opt_rotate_colors);
    if (color_step > 0.0f) {
      // step around circle by given number of degrees for each track(route)
      kml_color_sequencer.step = ((float)KML_COLOR_LIMIT) * 6.0f * color_step / 360.0f;
    } else {
      // one cycle around circle for all the tracks(routes)
      kml_color_sequencer.step = ((float)KML_COLOR_LIMIT) * 6.0f / ((float)steps_per_rev);
    }
    kml_color_sequencer.color.opacity=255;
    kml_color_sequencer.seq = 0.0f;
  }
}

static void kml_step_color()
{
  // Map kml_color_sequencer.seq to an integer in the range [0, KML_COLOR_LIMIT*6).
  // Note that color_seq may be outside this range if the cast from float to int fails.
  int color_seq = ((int) kml_color_sequencer.seq) % (KML_COLOR_LIMIT * 6);
  if (global_opts.debug_level >= 1) {
    printf(MYNAME ": kml_color_sequencer seq %f %d, step %f\n",kml_color_sequencer.seq, color_seq, kml_color_sequencer.step);
  }
  if ((color_seq >= (0*KML_COLOR_LIMIT)) && (color_seq < (1*KML_COLOR_LIMIT))) {
    kml_color_sequencer.color.bbggrr = (0)<<16 | (color_seq)<<8 | (KML_COLOR_LIMIT);
  } else if ((color_seq >= (1*KML_COLOR_LIMIT)) && (color_seq < (2*KML_COLOR_LIMIT))) {
    kml_color_sequencer.color.bbggrr = (0)<<16 | (KML_COLOR_LIMIT)<<8 | (2*KML_COLOR_LIMIT-color_seq);
  } else if ((color_seq >= (2*KML_COLOR_LIMIT)) && (color_seq < (3*KML_COLOR_LIMIT))) {
    kml_color_sequencer.color.bbggrr = (color_seq-2*KML_COLOR_LIMIT)<<16 | (KML_COLOR_LIMIT)<<8 | (0);
  } else if ((color_seq >= (3*KML_COLOR_LIMIT)) && (color_seq < (4*KML_COLOR_LIMIT))) {
    kml_color_sequencer.color.bbggrr = (KML_COLOR_LIMIT)<<16 | (4*KML_COLOR_LIMIT-color_seq)<<8 | (0);
  } else if ((color_seq >= (4*KML_COLOR_LIMIT)) && (color_seq < (5*KML_COLOR_LIMIT))) {
    kml_color_sequencer.color.bbggrr = (KML_COLOR_LIMIT)<<16 | (0)<<8 | (color_seq-4*KML_COLOR_LIMIT);
  } else if ((color_seq >= (5*KML_COLOR_LIMIT)) && (color_seq < (6*KML_COLOR_LIMIT))) {
    kml_color_sequencer.color.bbggrr = (6*KML_COLOR_LIMIT-color_seq)<<16 | (0)<<8 | (KML_COLOR_LIMIT);
  } else { // should not occur, but to be safe generate a legal color.
    warning(MYNAME ": Error in color conversion - using default color.\n");
    kml_color_sequencer.color.bbggrr = (102)<<16 | (102)<<8 | (102);
  }
  // compute next color.
  kml_color_sequencer.seq = kml_color_sequencer.seq + kml_color_sequencer.step;
}

static xg_callback wpt_s, wpt_e;
static xg_callback wpt_name, wpt_desc, wpt_coord, wpt_icon, trk_coord, wpt_time, wpt_ts_begin, wpt_ts_end;
static xg_callback gx_trk_s, gx_trk_e;
static xg_callback gx_trk_when, gx_trk_coord;

static
xg_tag_mapping kml_map[] = {
  { wpt_s, 	cb_start, 	"/Placemark" },
  { wpt_e, 	cb_end, 	"/Placemark" },
  { wpt_name, 	cb_cdata, 	"/Placemark/name" },
  { wpt_desc, 	cb_cdata, 	"/Placemark/description" },
  { wpt_ts_begin, cb_cdata,	"/Placemark/TimeSpan/begin" },
  { wpt_ts_end, cb_cdata, 	"/Placemark/TimeSpan/end" },
  { wpt_time, 	cb_cdata, 	"/Placemark/TimeStamp/when" },
  // Alias for above used in KML 2.0
  { wpt_time, 	cb_cdata, 	"/Placemark/TimeInstant/timePosition" },
  { wpt_coord, 	cb_cdata, 	"/Placemark/Point/coordinates" },
  { wpt_icon, 	cb_cdata, 	"/Placemark/Style/Icon/href" },
  { trk_coord, 	cb_cdata, 	"/Placemark/MultiGeometry/LineString/coordinates" },
  { trk_coord, 	cb_cdata, 	"/Placemark/GeometryCollection/LineString/coordinates" },
  { trk_coord, 	cb_cdata, 	"/Placemark/Polygon/outerBoundaryIs/LinearRing/coordinates" },
  { trk_coord, 	cb_cdata, 	"/Placemark/LineString/coordinates" },
  { gx_trk_s,  	cb_start, 	"/Placemark/*gx:Track" },
  { gx_trk_e,  	cb_end, 	"/Placemark/*gx:Track" },
  { gx_trk_when,  cb_cdata, "/Placemark/*gx:Track/when" },
  { gx_trk_coord, cb_cdata, "/Placemark/*gx:Track/gx:coord" },
  { gx_trk_s,  	cb_start, 	"/Placemark/Track" }, // KML 2.3
  { gx_trk_e,  	cb_end, 	"/Placemark/Track" }, // KML 2.3
  { gx_trk_when,  cb_cdata, "/Placemark/Track/when" }, // KML 2.3
  { gx_trk_coord, cb_cdata, "/Placemark/Track/coord" }, // KML 2.3
  { gx_trk_s,  	cb_start, 	"/Placemark/MultiTrack/Track" }, // KML 2.3
  { gx_trk_e,  	cb_end, 	"/Placemark/MultiTrack/Track" }, // KML 2.3
  { gx_trk_when,  cb_cdata, "/Placemark/MultiTrack/Track/when" }, // KML 2.3
  { gx_trk_coord, cb_cdata, "/Placemark/MultiTrack/Track/coord" }, // KML 2.3
  { nullptr,	(xg_cb_type) 0, 		nullptr }
};

static
const char* kml_tags_to_ignore[] = {
  "kml",
  "Document",
  "Folder",
  nullptr,
};

// The TimeSpan/begin and TimeSpan/end DateTimes:
static gpsbabel::DateTime wpt_timespan_begin, wpt_timespan_end;

void wpt_s(xg_string, const QXmlStreamAttributes*)
{
  if (wpt_tmp) {
    fatal(MYNAME ": wpt_s: invalid kml file\n");
  }
  wpt_tmp = new Waypoint;
  wpt_tmp_queued = 0;

  /* Invalidate timespan elements for a beginning Placemark,
   * so that each Placemark has its own (or no) TimeSpan. */
  wpt_timespan_begin = gpsbabel::DateTime();
  wpt_timespan_end = gpsbabel::DateTime();
}

void wpt_e(xg_string, const QXmlStreamAttributes*)
{
  if (!wpt_tmp) {
    fatal(MYNAME ": wpt_e: invalid kml file\n");
  }
  if (wpt_tmp_queued) {
    waypt_add(wpt_tmp);
    wpt_tmp = nullptr;
  } else {
    delete wpt_tmp;
    wpt_tmp = nullptr;
  }
  wpt_tmp_queued = 0;
}

void wpt_name(xg_string args, const QXmlStreamAttributes*)
{
  if (!wpt_tmp) {
    fatal(MYNAME ": wpt_name: invalid kml file\n");
  }
  wpt_tmp->shortname = args;
}

void wpt_desc(const QString& args, const QXmlStreamAttributes*)
{
  if (!wpt_tmp) {
    fatal(MYNAME ": wpt_desc: invalid kml file\n");
  }
  wpt_tmp->description += args.trimmed();
}

void wpt_time(xg_string args, const QXmlStreamAttributes*)
{
  if (!wpt_tmp) {
    fatal(MYNAME ": wpt_time: invalid kml file\n");
  }
  wpt_tmp->SetCreationTime(xml_parse_time(args));
}

void wpt_ts_begin(xg_string args, const QXmlStreamAttributes*)
{
  wpt_timespan_begin = xml_parse_time(args);
}

void wpt_ts_end(xg_string args, const QXmlStreamAttributes*)
{
  wpt_timespan_end = xml_parse_time(args);
}

void wpt_coord(const QString& args, const QXmlStreamAttributes*)
{
  double lat, lon, alt;
  if (! wpt_tmp) {
    return;
  }
  // Alt is actually optional.
  int n = sscanf(CSTRc(args), "%lf,%lf,%lf", &lon, &lat, &alt);
  if (n >= 2) {
    wpt_tmp->latitude = lat;
    wpt_tmp->longitude = lon;
  }
  if (n == 3) {
    wpt_tmp->altitude = alt;
  }
  wpt_tmp_queued = 1;
}

void wpt_icon(xg_string args, const QXmlStreamAttributes*)
{
  if (wpt_tmp)  {
    wpt_tmp->icon_descr = args;
  }
}

void trk_coord(xg_string args, const QXmlStreamAttributes*)
{
  int consumed = 0;
  double lat, lon, alt;
  Waypoint* trkpt;
  int n = 0;

  route_head* trk_head = route_head_alloc();

  QString iargs = args;
  if (wpt_tmp && !wpt_tmp->shortname.isEmpty()) {
    trk_head->rte_name  = wpt_tmp->shortname;
  }
  track_add_head(trk_head);
  while ((n = sscanf(CSTRc(iargs), "%lf,%lf,%lf%n", &lon, &lat, &alt, &consumed)) > 0) {
    trkpt = new Waypoint;
    trkpt->latitude = lat;
    trkpt->longitude = lon;

    // Line malformed or two-arg format without alt .  Rescan.
    if (2 == n) {
      sscanf(CSTRc(iargs), "%lf,%lf%n", &lon, &lat, &consumed);
    }

    if (3 == n) {
      trkpt->altitude = alt;
    }

    track_add_wpt(trk_head, trkpt);
    iargs = iargs.mid(consumed);
  }

  /* The track coordinates do not have a time associated with them. This is specified by using:
   *
   * <TimeSpan>
   *   <begin>2017-08-21T17:00:05Z</begin>
   *   <end>2017-08-21T17:22:32Z</end>
   * </TimeSpan>
   *
   * If this is specified, then SetCreationDate
   */
  if (wpt_timespan_begin.isValid() && wpt_timespan_end.isValid()) {

    // If there are some Waypoints, then distribute the TimeSpan to all Waypoints
    if (trk_head->rte_waypt_ct > 0) {
      qint64 timespan_ms = wpt_timespan_begin.msecsTo(wpt_timespan_end);
      qint64 ms_per_waypoint = timespan_ms / trk_head->rte_waypt_ct;
      foreach (Waypoint* trkpt, trk_head->waypoint_list) {
        trkpt->SetCreationTime(wpt_timespan_begin);
        wpt_timespan_begin = wpt_timespan_begin.addMSecs(ms_per_waypoint);
      }
    }
  }
}

void gx_trk_s(xg_string, const QXmlStreamAttributes*)
{
  gx_trk_head = route_head_alloc();
  if (wpt_tmp && !wpt_tmp->shortname.isEmpty()) {
    gx_trk_head->rte_name  = wpt_tmp->shortname;
  }
  if (wpt_tmp && !wpt_tmp->description.isEmpty()) {
    gx_trk_head->rte_desc  = wpt_tmp->description;
  }
  track_add_head(gx_trk_head);
  delete gx_trk_times;
  gx_trk_times = new QList<gpsbabel::DateTime>;
  delete gx_trk_coords;
  gx_trk_coords = new QList<std::tuple<int, double, double, double>>;
}

void gx_trk_e(xg_string, const QXmlStreamAttributes*)
{
  // Check that for every temporal value (kml:when) in a kml:Track there is a position (kml:coord) value.
  // Check that for every temporal value (kml:when) in a gx:Track there is a position (gx:coord) value.
  if (gx_trk_times->size() != gx_trk_coords->size()) {
    fatal(MYNAME ": There were more coord elements than the number of when elements.\n");
  }

  // In KML 2.3 kml:Track elements kml:coord and kml:when elements are not required to be in any order.
  // In gx:Track elements all kml:when elements are required to precede all gx:coord elements.
  // For both we allow any order.  Many writers using gx:Track elements don't adhere to the schema.
  while (!gx_trk_times->isEmpty()) {
    Waypoint* trkpt = new Waypoint;
    trkpt->SetCreationTime(gx_trk_times->takeFirst());
    double lat, lon, alt;
    int n;
    std::tie(n, lat, lon, alt) = gx_trk_coords->takeFirst();
    // An empty kml:coord element is permitted to indicate missing position data;
    // the estimated position may be determined using some interpolation method.
    // However if we get one we will throw away the time as we don't have a location.
    // It is not clear that coord elements without altitude are allowed, but our
    // writer produces them.
    if (n >= 2) {
      trkpt->latitude = lat;
      trkpt->longitude = lon;
      if (n >= 3) {
        trkpt->altitude = alt;
      }
      track_add_wpt(gx_trk_head, trkpt);
    } else {
      delete trkpt;
    }
  }

  if (!gx_trk_head->rte_waypt_ct) {
    track_del_head(gx_trk_head);
  }
  delete gx_trk_times;
  gx_trk_times = nullptr;
  delete gx_trk_coords;
  gx_trk_coords = nullptr;
}

void gx_trk_when(xg_string args, const QXmlStreamAttributes*)
{
  if (! gx_trk_times) {
    fatal(MYNAME ": gx_trk_when: invalid kml file\n");
  }
  gx_trk_times->append(xml_parse_time(args));
}

void gx_trk_coord(xg_string args, const QXmlStreamAttributes*)
{
  if (! gx_trk_coords) {
    fatal(MYNAME ": gx_trk_coord: invalid kml file\n");
  }

  double lat, lon, alt;
  int n = sscanf(CSTR(args), "%lf %lf %lf", &lon, &lat, &alt);
  if (0 != n && 2 != n && 3 != n) {
    fatal(MYNAME ": coord field decode failure on \"%s\".\n", qPrintable(args));
  }
  gx_trk_coords->append(std::make_tuple(n, lat, lon, alt));
}

static
void
kml_rd_init(const QString& fname)
{
  xml_init(fname, kml_map, nullptr);
  xml_ignore_tags(kml_tags_to_ignore);
}

static
void
kml_read()
{
  xml_read();
}

static void
kml_rd_deinit()
{
  xml_deinit();
}

static void
kml_wr_init(const QString& fname)
{
  char u = 's';
  waypt_init_bounds(&kml_bounds);
  kml_time_min = QDateTime();
  kml_time_max = QDateTime();

  if (opt_units) {
    u = tolower(opt_units[0]);
  }

  switch (u) {
  case 's':
    fmt_setunits(units_statute);
    break;
  case 'm':
    fmt_setunits(units_metric);
    break;
  case 'n':
    fmt_setunits(units_nautical);
    break;
  case 'a':
    fmt_setunits(units_aviation);
    break;
  default:
    fatal("Units argument '%s' should be 's' for statute units, 'm' for metric, 'n' for nautical or 'a' for aviation.\n", opt_units);
    break;
  }
  /*
   * Reduce race conditions with network read link.
   */
  oqfile = new gpsbabel::File(fname);
  oqfile->open(QIODevice::WriteOnly | QIODevice::Text);

  writer = new gpsbabel::XmlStreamWriter(oqfile);
  writer->setAutoFormattingIndent(2);
}

/*
 * The magic here is to try to ensure that posnfilename is atomically
 * updated.
 */
static void
kml_wr_position_init(const QString& fname)
{
  posnfilename = fname;
  posnfilenametmp = QString("%1-").arg(fname);
  realtime_positioning = 1;
  max_position_points = atoi(opt_max_position_points);
}

static void
kml_wr_deinit()
{
  writer->writeEndDocument();
  delete writer;
  writer = nullptr;
  oqfile->close();
  delete oqfile;
  oqfile = nullptr;

  if (!posnfilenametmp.isEmpty()) {
    // QFile::rename() can't replace an existing file, so do a QFile::remove()
    // first (which can fail silently if posnfilename doesn't exist). A race
    // condition can theoretically still cause rename to fail... oh well.
    QFile::remove(posnfilename);
    QFile::rename(posnfilenametmp, posnfilename);
  }
}

static void
kml_wr_position_deinit()
{
//	kml_wr_deinit();
  posnfilename.clear();
  posnfilenametmp.clear();
}


static void
kml_output_linestyle(char* /*color*/, int width)
{
  // Style settings for line strings
  writer->writeStartElement(QStringLiteral("LineStyle"));
  writer->writeTextElement(QStringLiteral("color"), opt_line_color);
  writer->writeTextElement(QStringLiteral("width"), QString::number(width));
  writer->writeEndElement(); // Close LineStyle tag
}


static void kml_write_bitmap_style_(const QString& style, const QString& bitmap,
                                    int highlighted, int force_heading)
{
  int is_track = style.startsWith("track");
  int is_multitrack = style.startsWith("multiTrack");

  writer->writeComment((highlighted ? QStringLiteral(" Highlighted ") : QStringLiteral(" Normal ")) + style + QStringLiteral(" style "));
  writer->writeStartElement(QStringLiteral("Style"));
  writer->writeAttribute(QStringLiteral("id"), style + (highlighted? QStringLiteral("_h") : QStringLiteral("_n")));

  writer->writeStartElement(QStringLiteral("IconStyle"));
  if (highlighted) {
    writer->writeTextElement(QStringLiteral("scale"), QStringLiteral("1.2"));
  } else {
    if (is_track) {
      writer->writeTextElement(QStringLiteral("scale"), QStringLiteral(".5"));
    }
  }
  /* Our icons are pre-rotated, so nail them to the maps. */
  if (force_heading) {
    writer->writeTextElement(QStringLiteral("heading"), QStringLiteral("0"));
  }
  writer->writeStartElement(QStringLiteral("Icon"));
  writer->writeTextElement(QStringLiteral("href"), bitmap);
  writer->writeEndElement(); // Close Icon tag
  writer->writeEndElement(); // Close IconStyle tag

  if (is_track && !highlighted) {
    writer->writeStartElement(QStringLiteral("LabelStyle"));
    writer->writeTextElement(QStringLiteral("scale"), QStringLiteral("0"));
    writer->writeEndElement(); //Close LabelStyle tag
  }

  if (is_multitrack) {
    kml_output_linestyle(opt_line_color,
                         highlighted ? line_width + 2 :
                         line_width);
  }

  writer->writeEndElement(); // Close Style tag
}

/* A wrapper for the above function to emit both a highlighted
 * and non-highlighted version of the style to allow the icons
 * to magnify slightly on a rollover.
 */
static void kml_write_bitmap_style(kml_point_type pt_type, const QString& bitmap,
                                   const QString& customstyle)
{
  int force_heading = 0;
  QString style;
  switch (pt_type) {
  case kmlpt_track:
    style = "track";
    break;
  case kmlpt_route:
    style = "route";
    break;
  case kmlpt_waypoint:
    style = "waypoint";
    break;
  case kmlpt_multitrack:
    style = "multiTrack";
    break;
  case kmlpt_other:
    style = customstyle;
    force_heading = 1;
    break;
  default:
    fatal("kml_output_point: unknown point type");
    break;
  }

  kml_write_bitmap_style_(style, bitmap, 0, force_heading);
  kml_write_bitmap_style_(style, bitmap, 1, force_heading);

  writer->writeStartElement(QStringLiteral("StyleMap"));
  writer->writeAttribute(QStringLiteral("id"), style);
  writer->writeStartElement(QStringLiteral("Pair"));
  writer->writeTextElement(QStringLiteral("key"), QStringLiteral("normal"));
  writer->writeTextElement(QStringLiteral("styleUrl"), QStringLiteral("#") + style + QStringLiteral("_n"));
  writer->writeEndElement(); // Close Pair tag
  writer->writeStartElement(QStringLiteral("Pair"));
  writer->writeTextElement(QStringLiteral("key"), QStringLiteral("highlight"));
  writer->writeTextElement(QStringLiteral("styleUrl"), QStringLiteral("#") + style + QStringLiteral("_h"));
  writer->writeEndElement(); // Close Pair tag
  writer->writeEndElement(); // Close StyleMap tag
}

static void kml_output_timestamp(const Waypoint* waypointp)
{
  QString time_string = waypointp->CreationTimeXML();
  if (!time_string.isEmpty()) {
    writer->writeStartElement(QStringLiteral("TimeStamp"));
    writer->writeTextElement(QStringLiteral("when"), time_string);
    writer->writeEndElement(); // Close TimeStamp tag
  }
}

static
void kml_td(gpsbabel::XmlStreamWriter& hwriter, const QString& boldData, const QString& data)
{
  hwriter.writeCharacters(QStringLiteral("\n"));
  hwriter.writeStartElement(QStringLiteral("tr"));
  hwriter.writeStartElement(QStringLiteral("td"));
  hwriter.writeTextElement(QStringLiteral("b"), boldData);
  hwriter.writeCharacters(data);
  hwriter.writeEndElement(); // Close td tag
  hwriter.writeEndElement(); // Close tr tag
}

static
void kml_td(gpsbabel::XmlStreamWriter& hwriter, const QString& data)
{
  hwriter.writeCharacters(QStringLiteral("\n"));
  hwriter.writeStartElement(QStringLiteral("tr"));
  hwriter.writeStartElement(QStringLiteral("td"));
  hwriter.writeCharacters(data);
  hwriter.writeEndElement(); // Close td tag
  hwriter.writeEndElement(); // Close tr tag
}

/*
 * Output the track summary.
 */
static
void kml_output_trkdescription(const route_head* header, const computed_trkdata* td)
{
  if (!td || !trackdata) {
    return;
  }

  QString hstring;
  gpsbabel::XmlStreamWriter hwriter(&hstring);

  writer->writeEmptyElement(QStringLiteral("snippet"));

  writer->writeStartElement(QStringLiteral("description"));

  hwriter.writeStartElement(QStringLiteral("table"));
  if (!header->rte_desc.isEmpty()) {
    kml_td(hwriter, QStringLiteral("Description"), QStringLiteral(" %1 ").arg(header->rte_desc));
  }
  const char* distance_units;
  double distance = fmt_distance(td->distance_meters, &distance_units);
  kml_td(hwriter, QStringLiteral("Distance"), QStringLiteral(" %1 %2 ").arg(QString::number(distance, 'f', 1), distance_units));
  if (td->min_alt) {
    const char* min_alt_units;
    double min_alt = fmt_altitude(*td->min_alt, &min_alt_units);
    kml_td(hwriter, QStringLiteral("Min Alt"), QStringLiteral(" %1 %2 ").arg(QString::number(min_alt, 'f', 3), min_alt_units));
  }
  if (td->max_alt) {
    const char* max_alt_units;
    double max_alt = fmt_altitude(*td->max_alt, &max_alt_units);
    kml_td(hwriter, QStringLiteral("Max Alt"), QStringLiteral(" %1 %2 ").arg(QString::number(max_alt, 'f', 3), max_alt_units));
  }
  if (td->min_spd) {
    const char* spd_units;
    double spd = fmt_speed(*td->min_spd, &spd_units);
    kml_td(hwriter, QStringLiteral("Min Speed"), QStringLiteral(" %1 %2 ").arg(QString::number(spd, 'f', 1), spd_units));
  }
  if (td->max_spd) {
    const char* spd_units;
    double spd = fmt_speed(*td->max_spd, &spd_units);
    kml_td(hwriter, QStringLiteral("Max Speed"), QStringLiteral(" %1 %2 ").arg(QString::number(spd, 'f', 1), spd_units));
  }
  if (td->max_spd && td->start.isValid() && td->end.isValid()) {
    const char* spd_units;
    double elapsed = td->start.msecsTo(td->end)/1000.0;
    double spd = fmt_speed(td->distance_meters / elapsed, &spd_units);
    if (spd > 1.0)  {
      kml_td(hwriter, QStringLiteral("Avg Speed"), QStringLiteral(" %1 %2 ").arg(QString::number(spd, 'f', 1), spd_units));
    }
  }
  if (td->avg_hrt) {
    kml_td(hwriter, QStringLiteral("Avg Heart Rate"), QStringLiteral(" %1 bpm ").arg(QString::number(*td->avg_hrt, 'f', 1)));
  }
  if (td->min_hrt) {
    kml_td(hwriter, QStringLiteral("Min Heart Rate"), QStringLiteral(" %1 bpm ").arg(QString::number(*td->min_hrt)));
  }
  if (td->max_hrt) {
    kml_td(hwriter, QStringLiteral("Max Heart Rate"), QStringLiteral(" %1 bpm ").arg(QString::number(*td->max_hrt)));
  }
  if (td->avg_cad) {
    kml_td(hwriter, QStringLiteral("Avg Cadence"), QStringLiteral(" %1 rpm ").arg(QString::number(*td->avg_cad, 'f', 1)));
  }
  if (td->max_cad) {
    kml_td(hwriter, QStringLiteral("Max Cadence"), QStringLiteral(" %1 rpm ").arg(QString::number(*td->max_cad)));
  }
  if (td->start.isValid() && td->end.isValid()) {
    kml_td(hwriter, QStringLiteral("Start Time"), td->start.toPrettyString());
    kml_td(hwriter, QStringLiteral("End Time"), td->end.toPrettyString());
  }

  hwriter.writeCharacters(QStringLiteral("\n"));
  hwriter.writeEndElement(); // Close table tag
  //hwriter.writeEndDocument(); // FIXME: it seems like we should end the doc but it causes a reference mismatch by adding a final \n
  writer->writeCharacters(QStringLiteral("\n"));
  writer->writeCDATA(hstring);
  writer->writeCharacters(QStringLiteral("\n"));
  writer->writeEndElement(); // Close description tag

  /* We won't always have times. Garmin saved tracks, for example... */
  if (td->start.isValid() && td->end.isValid()) {
    writer->writeStartElement(QStringLiteral("TimeSpan"));
    writer->writeTextElement(QStringLiteral("begin"), td->start.toPrettyString());
    writer->writeTextElement(QStringLiteral("end"), td->end.toPrettyString());
    writer->writeEndElement(); // Close TimeSpan tag
  }
}


static
void kml_output_header(const route_head* header, const computed_trkdata* td)
{
  if (!realtime_positioning)  {
    writer->writeStartElement(QStringLiteral("Folder"));
  }
  writer->writeOptionalTextElement(QStringLiteral("name"), header->rte_name);
  kml_output_trkdescription(header, td);

  if (export_points && header->rte_waypt_ct > 0) {
    // Put the points in a subfolder
    writer->writeStartElement(QStringLiteral("Folder"));
    writer->writeTextElement(QStringLiteral("name"), QStringLiteral("Points"));
  }
}

static
int kml_altitude_known(const Waypoint* waypoint)
{
  if (waypoint->altitude == unknown_alt) {
    return 0;
  }
  // We see way more data that's sourceed at 'zero' than is actually
  // precisely at 0 MSL.
  if (fabs(waypoint->altitude) < 0.01) {
    return 0;
  }
  return 1;
}

static
void kml_write_coordinates(const Waypoint* waypointp)
{
  if (kml_altitude_known(waypointp)) {
    writer->writeTextElement(QStringLiteral("coordinates"),
                             QString::number(waypointp->longitude, 'f', precision) + QString(",") +
                             QString::number(waypointp->latitude, 'f', precision) + QString(",") +
                             QString::number(waypointp->altitude, 'f', 2)
                            );
  } else {
    writer->writeTextElement(QStringLiteral("coordinates"),
                             QString::number(waypointp->longitude, 'f', precision) + QString(",") +
                             QString::number(waypointp->latitude, 'f', precision)
                            );
  }
}

/* Rather than a default "top down" view, view from the side to highlight
 * topo features.
 */
static void kml_output_lookat(const Waypoint* waypointp)
{
  writer->writeStartElement(QStringLiteral("LookAt"));
  writer->writeTextElement(QStringLiteral("longitude"), QString::number(waypointp->longitude, 'f', precision));
  writer->writeTextElement(QStringLiteral("latitude"), QString::number(waypointp->latitude, 'f', precision));
  writer->writeTextElement(QStringLiteral("tilt"), QStringLiteral("66"));
  writer->writeEndElement(); // Close LookAt tag
}

static void kml_output_positioning(bool tessellate)
{
  // These elements must be output as a sequence, i.e. in order.
  if (extrude) {
    writer->writeTextElement(QStringLiteral("extrude"), QStringLiteral("1"));
  }

  if (tessellate) {
    writer->writeTextElement(QStringLiteral("tessellate"), QStringLiteral("1"));
  }

  if (floating) {
    writer->writeTextElement(QStringLiteral("altitudeMode"), QStringLiteral("absolute"));
  }

}

/* Output something interesing when we can for route and trackpoints */
static void kml_output_description(const Waypoint* pt)
{
  const char* alt_units;

  if (!trackdata) {
    return;
  }

  QString hstring;
  gpsbabel::XmlStreamWriter hwriter(&hstring);

  double alt = fmt_altitude(pt->altitude, &alt_units);

  writer->writeStartElement(QStringLiteral("description"));
  hwriter.writeCharacters(QStringLiteral("\n"));
  hwriter.writeStartElement(QStringLiteral("table"));

  kml_td(hwriter, QStringLiteral("Longitude: %1 ").arg(QString::number(pt->longitude, 'f', precision)));
  kml_td(hwriter, QStringLiteral("Latitude: %1 ").arg(QString::number(pt->latitude, 'f', precision)));

  if (kml_altitude_known(pt)) {
    kml_td(hwriter, QStringLiteral("Altitude: %1 %2 ").arg(QString::number(alt, 'f', 3), alt_units));
  }

  if (pt->heartrate) {
    kml_td(hwriter, QStringLiteral("Heart rate: %1 ").arg(QString::number(pt->heartrate)));
  }

  if (pt->cadence) {
    kml_td(hwriter, QStringLiteral("Cadence: %1 ").arg(QString::number(pt->cadence)));
  }

  /* Which unit is this temp in? C? F? K? */
  if WAYPT_HAS(pt, temperature) {
    kml_td(hwriter, QStringLiteral("Temperature: %1 ").arg(QString::number(pt->temperature, 'f', 1)));
  }

  if WAYPT_HAS(pt, depth) {
    const char* depth_units;
    double depth = fmt_distance(pt->depth, &depth_units);
    kml_td(hwriter, QStringLiteral("Depth: %1 %2 ").arg(QString::number(depth, 'f', 1), depth_units));
  }

  if WAYPT_HAS(pt, speed) {
    const char* spd_units;
    double spd = fmt_speed(pt->speed, &spd_units);
    kml_td(hwriter, QStringLiteral("Speed: %1 %2 ").arg(QString::number(spd, 'f', 1), spd_units));
  }

  if WAYPT_HAS(pt, course) {
    kml_td(hwriter, QStringLiteral("Heading: %1 ").arg(QString::number(pt->course, 'f', 1)));
  }

  /* This really shouldn't be here, but as of this writing,
   * Earth can't edit/display the TimeStamp.
   */
  if (pt->GetCreationTime().isValid()) {
    QString time_string = pt->CreationTimeXML();
    if (!time_string.isEmpty()) {
      kml_td(hwriter, QStringLiteral("Time: %1 ").arg(time_string));
    }
  }

  hwriter.writeCharacters(QStringLiteral("\n"));
  hwriter.writeEndElement(); // Close table tag
  hwriter.writeEndDocument();
  writer->writeCDATA(hstring);
  writer->writeEndElement(); // Close description tag
}

static void kml_recompute_time_bounds(const Waypoint* waypointp)
{
  if (waypointp->GetCreationTime().isValid()) {
    if (!(kml_time_min.isValid()) ||
        (waypointp->GetCreationTime() < kml_time_min)) {
      kml_time_min = waypointp->GetCreationTime();
    }
    if (!(kml_time_max.isValid()) ||
        (waypointp->GetCreationTime() > kml_time_max)) {
      kml_time_max = waypointp->GetCreationTime();
    }
  }
}

static void kml_add_to_bounds(const Waypoint* waypointp)
{
  waypt_add_to_bounds(&kml_bounds, waypointp);
  kml_recompute_time_bounds(waypointp);
}

static void kml_output_point(const Waypoint* waypointp, kml_point_type pt_type)
{
  QString style;

  switch (pt_type) {
  case kmlpt_track:
    style = "#track";
    break;
  case kmlpt_route:
    style = "#route";
    break;
  default:
    fatal("kml_output_point: unknown point type");
    break;
  }

  if (export_points) {
    writer->writeStartElement(QStringLiteral("Placemark"));
    if (atoi(opt_labels)) {
      writer->writeOptionalTextElement(QStringLiteral("name"), waypointp->shortname);
    }
    writer->writeEmptyElement(QStringLiteral("snippet"));
    kml_output_description(waypointp);
    kml_output_lookat(waypointp);
    kml_output_timestamp(waypointp);


    if (opt_deficon) {
      writer->writeStartElement(QStringLiteral("Style"));
      writer->writeStartElement(QStringLiteral("IconStyle"));
      writer->writeStartElement(QStringLiteral("Icon"));
      writer->writeTextElement(QStringLiteral("href"), opt_deficon);
      writer->writeEndElement(); // Close Icon tag
      writer->writeEndElement(); // Close IconStyle tag
      writer->writeEndElement(); // Close Style tag
    } else {
      if (trackdirection && (pt_type == kmlpt_track)) {
        QString value;
        if (waypointp->speed < 1) {
          value = QString("%1-none").arg(style);
        } else {
          value = QString("%1-%2").arg(style)
                  .arg((int)(waypointp->course / 22.5 + .5) % 16);
        }
        writer->writeTextElement(QStringLiteral("styleUrl"), value);
      } else {
        writer->writeTextElement(QStringLiteral("styleUrl"), style);
      }
    }

    writer->writeStartElement(QStringLiteral("Point"));
    kml_output_positioning(false);
    kml_write_coordinates(waypointp);
    writer->writeEndElement(); // Close Point tag

    writer->writeEndElement(); // Close Placemark tag
  }
}

static void kml_output_tailer(const route_head* header)
{

  if (export_points && header->rte_waypt_ct > 0) {
    writer->writeEndElement(); // Close Folder tag
  }

  // Add a linestring for this track?
  if (export_lines && header->rte_waypt_ct > 0) {
    int needs_multigeometry = 0;

    foreach (const Waypoint* tpt, header->waypoint_list) {
      int first_in_trk = tpt == header->waypoint_list.front();
      if (!first_in_trk && tpt->wpt_flags.new_trkseg) {
        needs_multigeometry = 1;
        break;
      }
    }
    writer->writeStartElement(QStringLiteral("Placemark"));
    writer->writeTextElement(QStringLiteral("name"), QStringLiteral("Path"));
    if (!rotate_colors) {
      writer->writeTextElement(QStringLiteral("styleUrl"), QStringLiteral("#lineStyle"));
    }
    if (header->line_color.bbggrr >= 0 || header->line_width >= 0 || rotate_colors) {
      writer->writeStartElement(QStringLiteral("Style"));
      writer->writeStartElement(QStringLiteral("LineStyle"));
      if (rotate_colors) {
        kml_step_color();
        writer->writeTextElement(QStringLiteral("color"), QStringLiteral("%1%2")
                                 .arg(kml_color_sequencer.color.opacity, 2, 16, QChar('0')).arg(kml_color_sequencer.color.bbggrr, 6, 16, QChar('0')));
        writer->writeTextElement(QStringLiteral("width"), opt_line_width);
      } else {
        if (header->line_color.bbggrr >= 0) {
          writer->writeTextElement(QStringLiteral("color"), QStringLiteral("%1%2")
                                   .arg(header->line_color.opacity, 2, 16, QChar('0')).arg(header->line_color.bbggrr, 6, 16, QChar('0')));
        }
        if (header->line_width >= 0) {
          writer->writeTextElement(QStringLiteral("width"), QString::number(header->line_width));
        }
      }
      writer->writeEndElement(); // Close LineStyle tag
      writer->writeEndElement(); // Close Style tag
    }
    if (needs_multigeometry) {
      writer->writeStartElement(QStringLiteral("MultiGeometry"));
    }

    foreach (const Waypoint* tpt, header->waypoint_list) {
      int first_in_trk = tpt == header->waypoint_list.front();
      if (tpt->wpt_flags.new_trkseg) {
        if (!first_in_trk) {
          writer->writeEndElement(); // Close coordinates tag
          writer->writeEndElement(); // Close LineString tag
        }
        writer->writeStartElement(QStringLiteral("LineString"));
        kml_output_positioning(true);
        writer->writeStartElement(QStringLiteral("coordinates"));
        writer->writeCharacters(QStringLiteral("\n"));
      }
      if (kml_altitude_known(tpt)) {
        writer->writeCharacters(QString::number(tpt->longitude, 'f', precision) + QStringLiteral(",") +
                                QString::number(tpt->latitude, 'f', precision) + QStringLiteral(",") +
                                QString::number(tpt->altitude, 'f', 2) + QStringLiteral("\n")
                               );
      } else {
        writer->writeCharacters(QString::number(tpt->longitude, 'f', precision) + QStringLiteral(",") +
                                QString::number(tpt->latitude, 'f', precision) + QStringLiteral("\n")
                               );
      }
    }
    writer->writeEndElement(); // Close coordinates tag
    writer->writeEndElement(); // Close LineString tag
    if (needs_multigeometry) {
      writer->writeEndElement(); // Close MultiGeometry tag
    }
    writer->writeEndElement(); // Close Placemark tag
  }

  if (!realtime_positioning)  {
    writer->writeEndElement();  // Close folder tag
  }
}

/*
 * Completely different writer for geocaches.
 */

// Text that's common to all tabs.
static
void kml_gc_all_tabs_text(QString& cdataStr)
{
  // cdataStr.append("<a href=\"http://www.geocaching.com\"><img style=\"float: left; padding: 10px\" src=\"http://www.geocaching.com/images/nav/logo_sub.gif\" /> </a>\n");
  cdataStr.append("<img align=\"right\" src=\"$[gc_icon]\" />\n");
  cdataStr.append("<a href=\"https://www.geocaching.com/seek/cache_details.aspx?wp=$[gc_num]\"><b>$[gc_num]</b></a> <b>$[gc_name]</b> \n");
  cdataStr.append("a $[gc_type],<br />on $[gc_placed] by <a href=\"https://www.geocaching.com/profile?id=$[gc_placer_id\">$[gc_placer]</a><br/>\n");
  cdataStr.append("Difficulty: <img src=\"https://www.geocaching.com/images/stars/$[gc_diff_stars].gif\" alt=\"$[gc_diff]\" width=\"61\" height=\"13\" />\n");
  cdataStr.append("&nbsp;Terrain: <img src=\"https://www.geocaching.com/images/stars/$[gc_terr_stars].gif\" alt=\"$[gc_terr]\" width=\"61\" height=\"13\" /><br />\n");
  cdataStr.append("Size: <img src=\"https://www.geocaching.com/images/icons/container/$[gc_cont_icon].gif\" width=\"45\" height=\"12\" alt=\"$[gc_cont_icon]\"/>&nbsp;($[gc_cont_icon])<br />\n");

}

static const QString map_templates[] = {
  "<a href=\"https://www.google.com/maps?q=$[gc_lat],$[gc_lon]\" target=\"_blank\">Google Maps</a>",
  "<a href=\"http://www.geocaching.com/map/default.aspx?lat=$[gc_lat]&lng=$[gc_lon]\" target=\"_blank\">Geocaching.com Google Map</a>",
  "<a href=\"http://www.mytopo.com/maps.cfm?lat=$[gc_lat]&lon=$[gc_lon]&pid=groundspeak\" target=\"_blank\">MyTopo Maps</a>",
  "<a href=\"http://www.mapquest.com/maps/map.adp?searchtype=address&formtype=latlong&latlongtype=decimal&latitude=$[gc_lat]&longitude=$[gc_lon]&zoom=10\" target=\"_blank\">MapQuest</a>",
  "<a href=\"http://www.bing.com/maps/default.aspx?v=2&sp=point.$[gc_lat]$[gc_lon]\" target=\"_blank\">Bing Maps</a>",
  "<a href=\"http://maps.randmcnally.com/#s=screen&lat=$[gc_lat]&lon=$[gc_lon]&zoom=13&loc1=$[gc_lat],$[gc_lon]\" target=\"_blank\">Rand McNally</a>",
  "<a href=\"http://www.opencyclemap.org/?zoom=12&lat=$[gc_lat]&lon=$[gc_lon]\" target=\"_blank\">Open Cycle Maps</a>",
  "<a href=\"http://www.openstreetmap.org/?mlat=$[gc_lat]&mlon=$[gc_lon]&zoom=12\" target=\"_blank\">Open Street Maps</a>",
  nullptr
};


static
void kml_gc_make_balloonstyletext()
{
  QString cdataStr;

  writer->writeStartElement(QStringLiteral("BalloonStyle"));
  writer->writeStartElement(QStringLiteral("text"));
  cdataStr.append("\n");

  cdataStr.append("<!DOCTYPE html>\n");
  cdataStr.append("<html>\n");
  cdataStr.append("<head>\n");
  cdataStr.append("<link href=\"https://ajax.googleapis.com/ajax/libs/jqueryui/1.12.1/themes/base/jquery-ui.css\" rel=\"stylesheet\" type=\"text/css\"/>\n");
  cdataStr.append("<script src=\"https://ajax.googleapis.com/ajax/libs/jquery/1.12.4/jquery.min.js\"></script>\n");
  cdataStr.append("<script src=\"https://ajax.googleapis.com/ajax/libs/jqueryui/1.12.1/jquery-ui.min.js\"></script>\n");
  cdataStr.append("<script>\n");
  cdataStr.append("$(document).ready(function() {\n");
  cdataStr.append("  $(\"#tabs\").tabs();\n");
  cdataStr.append("});\n");
  cdataStr.append("</script>\n");
  cdataStr.append("</head>\n");

  cdataStr.append("<body>\n");
  cdataStr.append("<div id=\"tabs\">\n");

  // The tabbed menu bar.  Oddly, it has to be on top.
  cdataStr.append("<ul>\n");
  cdataStr.append("  <li><a href=\"#fragment-1\"><span>Description</span></a></li>\n");
  cdataStr.append("  <li><a href=\"#fragment-2\"><span>Logs</span></a></li>\n");
  cdataStr.append("  <li><a href=\"#fragment-3\"><span>Extras</span></a></li>\n");
  cdataStr.append("</ul>\n");
  cdataStr.append("\n");

  cdataStr.append("<div id=\"fragment-1\">\n");
  kml_gc_all_tabs_text(cdataStr);
  cdataStr.append("  <p />$[gc_issues]\n");
  cdataStr.append("  $[gc_short_desc]\n");
  cdataStr.append("  $[gc_long_desc]\n");
  cdataStr.append("</div>\n");

  cdataStr.append("<div id=\"fragment-2\">\n");
  kml_gc_all_tabs_text(cdataStr);
  cdataStr.append("  $[gc_logs]\n");
  cdataStr.append("</div>\n");

  // "Extra" stuff tab.
  cdataStr.append("<div id=\"fragment-3\">\n");
  kml_gc_all_tabs_text(cdataStr);
  cdataStr.append("  <h1>Extra Maps</h1>\n");

  cdataStr.append("  <ul>\n");
  // Fortunately, all the mappy map URLs take lat/longs in the URLs, so
  // the substition is easy.
  for (int tp = 0; !map_templates[tp].isEmpty(); tp++) {
    cdataStr.append("    <li>\n");
    cdataStr.append("    ");
    cdataStr.append(map_templates[tp]);
    cdataStr.append("</li>\n");
  }
  cdataStr.append("  <ul>\n");

  cdataStr.append("</div>\n"); //  fragment-3.

  cdataStr.append("</div>\n"); // tabs.
  cdataStr.append("</body>\n");
  cdataStr.append("</html>\n");

  writer->writeCDATA(cdataStr);
  writer->writeEndElement(); // Close text tag
  writer->writeEndElement(); // Close BalloonStyle tag
}

static
void kml_gc_make_balloonstyle()
{
  // For Normal style of gecoaches, scale of label is set to zero
  // to make the label invisible.  On hover (highlight?) enlarge
  // the icon sightly and set the scale of the label to 1 so the
  // label pops.
  // It's unfortunate that we have to repeat so much of the template
  // but KML doesn't have a cascading style-like substance.
  //
  writer->writeStartElement(QStringLiteral("Style"));
  writer->writeAttribute(QStringLiteral("id"), QStringLiteral("geocache_n"));
  writer->writeStartElement(QStringLiteral("IconStyle"));
  writer->writeTextElement(QStringLiteral("scale"), QStringLiteral(".6"));
  writer->writeEndElement(); // Close IconStyle tag
  writer->writeStartElement(QStringLiteral("LabelStyle"));
  writer->writeTextElement(QStringLiteral("scale"), QStringLiteral("0"));
  writer->writeEndElement(); // Close LabelStyle tag
  kml_gc_make_balloonstyletext();
  writer->writeEndElement(); // Close Style tag

  writer->writeStartElement(QStringLiteral("Style"));
  writer->writeAttribute(QStringLiteral("id"), QStringLiteral("geocache_h"));
  writer->writeStartElement(QStringLiteral("IconStyle"));
  writer->writeTextElement(QStringLiteral("scale"), QStringLiteral(".8"));
  writer->writeEndElement(); // Close IconStyle tag
  writer->writeStartElement(QStringLiteral("LabelStyle"));
  writer->writeTextElement(QStringLiteral("scale"), QStringLiteral("1"));
  writer->writeEndElement(); // Close LabelStyle tag
  kml_gc_make_balloonstyletext();
  writer->writeEndElement(); // Close Style tag

  writer->writeStartElement(QStringLiteral("StyleMap"));
  writer->writeAttribute(QStringLiteral("id"), QStringLiteral("geocache"));

  writer->writeStartElement(QStringLiteral("Pair"));
  writer->writeTextElement(QStringLiteral("key"), QStringLiteral("normal"));
  writer->writeTextElement(QStringLiteral("styleUrl"), QStringLiteral("#geocache_n"));
  writer->writeEndElement(); // Close Pair tag

  writer->writeStartElement(QStringLiteral("Pair"));
  writer->writeTextElement(QStringLiteral("key"), QStringLiteral("highlight"));
  writer->writeTextElement(QStringLiteral("styleUrl"), QStringLiteral("#geocache_h"));
  writer->writeEndElement(); // Close Pair tag

  writer->writeEndElement(); // Close StyleMap tag
}

static
QString
kml_lookup_gc_icon(const Waypoint* waypointp)
{
  const char* icon;
  /* This could be done so much better in C99 with designated
   * initializers...
   */
  switch (waypointp->gc_data->type) {
  case gt_traditional:
    icon = "2.png";
    break;
  case gt_multi:
    icon = "3.png";
    break;
  case gt_virtual:
    icon = "4.png";
    break;
  case gt_letterbox:
    icon = "5.png";
    break;
  case gt_event:
    icon = "6.png";
    break;
  case gt_ape:
    icon = "7.png";
    break;
  case gt_locationless:
    icon = "8.png";
    break; // No unique icon.
  case gt_suprise:
    icon = "8.png";
    break;
  case gt_webcam:
    icon = "11.png";
    break;
  case gt_cito:
    icon = "13.png";
    break;
  case gt_earth:
    icon = "earthcache.png";
    break;
  case gt_mega:
    icon = "453.png";
    break;
  case gt_wherigo:
    icon = "1858.png";
    break;
  default:
    icon = "8.png";
    break;
  }

  return QString("https://www.geocaching.com/images/kml/%1").arg(icon);
}

static const
char*
kml_lookup_gc_container(const Waypoint* waypointp)
{
  const char* cont;

  switch (waypointp->gc_data->container) {
  case gc_micro:
    cont="micro";
    break;
  case gc_regular:
    cont="regular";
    break;
  case gc_large:
    cont="large";
    break;
  case gc_small:
    cont="small";
    break;
  case gc_virtual:
    cont="virtual";
    break;
  case gc_other:
    cont="other";
    break;
  default:
    cont="not_chosen";
    break;
  }

  return cont;
}

static QString kml_gc_mkstar(int rating)
{
  QString star_content;

  if (rating < 0 || rating > 50 || rating % 5 != 0) {
    fatal("Bogus difficulty or terrain rating.");
  }

  if (0 == rating % 10) {
    star_content = QString("stars%1").arg(rating / 10);
  } else {
    star_content = QString("stars%1_%2").arg(rating / 10).arg(rating % 10);
  }

  return star_content;

}

static QString kml_geocache_get_logs(const Waypoint* wpt)
{
  QString r;

  fs_xml* fs_gpx = (fs_xml*)fs_chain_find(wpt->fs, FS_GPX);

  if (!fs_gpx) {
    return r;
  }

  xml_tag* root = fs_gpx->tag;
  xml_tag* curlog = xml_findfirst(root, "groundspeak:log");
  while (curlog) {
    // Unless we have a broken GPX input, these logparts
    // branches will always be taken.
    xml_tag* logpart = xml_findfirst(curlog, "groundspeak:type");
    if (logpart) {
      r = r + "<p><b>" + logpart->cdata + "</b>";
    }

    logpart = xml_findfirst(curlog, "groundspeak:finder");
    if (logpart) {
      r = r + " by " + logpart->cdata;
    }

    logpart = xml_findfirst(curlog, "groundspeak:date");
    if (logpart) {
      gpsbabel::DateTime t = xml_parse_time(logpart->cdata);
      if (t.isValid()) {
        r += t.date().toString(Qt::ISODate);
      }
    }

    logpart = xml_findfirst(curlog, "groundspeak:text");
    if (logpart) {
      char* encstr = xml_attribute(logpart, "encoded");
      int encoded = (toupper(encstr[0]) != 'F');

      QString s;
      if (html_encrypt && encoded) {
        s = rot13(logpart->cdata);
      } else {
        s = logpart->cdata;
      }

      r = r + "<br />";
      char* t = html_entitize(s);
      r = r + t;
      xfree(t);
    }

    r += "</p>";
    curlog = xml_findnext(root, curlog, "groundspeak:log");
  }
  return r;
}

static void kml_write_data_element(const QString& name, const QString& value)
{
  writer->writeStartElement(QStringLiteral("Data"));
  writer->writeAttribute(QStringLiteral("name"), name);
  writer->writeTextElement(QStringLiteral("value"), value);
  writer->writeEndElement(); // Close Data tag
}

static void kml_write_data_element(const QString& name, const int value)
{
  writer->writeStartElement(QStringLiteral("Data"));
  writer->writeAttribute(QStringLiteral("name"), name);
  writer->writeTextElement(QStringLiteral("value"), QString::number(value));
  writer->writeEndElement(); // Close Data tag
}

static void kml_write_data_element(const QString& name, const double value)
{
  writer->writeStartElement(QStringLiteral("Data"));
  writer->writeAttribute(QStringLiteral("name"), name);
  writer->writeTextElement(QStringLiteral("value"), QString::number(value, 'f', 6));
  writer->writeEndElement(); // Close Data tag
}

static void kml_write_cdata_element(const QString& name, const QString& value)
{
  writer->writeStartElement(QStringLiteral("Data"));
  writer->writeAttribute(QStringLiteral("name"), name);
  writer->writeStartElement(QStringLiteral("value"));
  writer->writeCDATA(value);
  writer->writeEndElement(); // Close value tag
  writer->writeEndElement(); // Close Data tag
}

static void kml_geocache_pr(const Waypoint* waypointp)
{
  const char* issues = "";

  writer->writeStartElement(QStringLiteral("Placemark"));

  writer->writeStartElement(QStringLiteral("name"));
  if (waypointp->HasUrlLink()) {
    UrlLink link = waypointp->GetUrlLink();
    writer->writeCDATA(link.url_link_text_);
  }
  writer->writeEndElement(); // Close name tag

  // Timestamp
  kml_output_timestamp(waypointp);
  QString date_placed;
  if (waypointp->GetCreationTime().isValid()) {
    date_placed = waypointp->GetCreationTime().toString("dd-MMM-yyyy");
  }

  writer->writeTextElement(QStringLiteral("styleUrl"), QStringLiteral("#geocache"));
  writer->writeStartElement(QStringLiteral("Style"));
  writer->writeStartElement(QStringLiteral("IconStyle"));
  writer->writeStartElement(QStringLiteral("Icon"));
  QString is = kml_lookup_gc_icon(waypointp);
  writer->writeTextElement(QStringLiteral("href"), is);
  writer->writeEndElement(); // Close Icon tag
  writer->writeEndElement(); // Close IconStyle tag
  writer->writeEndElement(); // Close Style tag

  writer->writeStartElement(QStringLiteral("ExtendedData"));
  if (!waypointp->shortname.isEmpty()) {
    kml_write_data_element("gc_num", waypointp->shortname);
  }

  if (waypointp->HasUrlLink()) {
    UrlLink link = waypointp->GetUrlLink();
    kml_write_data_element("gc_name", link.url_link_text_);
  }

  if (!waypointp->gc_data->placer.isEmpty()) {
    kml_write_data_element("gc_placer", waypointp->gc_data->placer);
  }

  kml_write_data_element("gc_placer_id", waypointp->gc_data->placer_id);
  kml_write_data_element("gc_placed", date_placed);

  kml_write_data_element("gc_diff_stars", kml_gc_mkstar(waypointp->gc_data->diff));
  kml_write_data_element("gc_terr_stars", kml_gc_mkstar(waypointp->gc_data->terr));

  kml_write_data_element("gc_cont_icon", kml_lookup_gc_container(waypointp));

  // Highlight any issues with the cache, such as temp unavail
  // or archived.
  if (waypointp->gc_data->is_archived == status_true) {
    issues = "&lt;font color=\"red\"&gt;This cache has been archived.&lt;/font&gt;&lt;br/&gt;\n";
  } else if (waypointp->gc_data->is_available == status_false) {
    issues = "&lt;font color=\"red\"&gt;This cache is temporarily unavailable.&lt;/font&gt;&lt;br/&gt;\n";
  }
  kml_write_data_element("gc_issues", issues);

  kml_write_data_element("gc_lat", waypointp->latitude);
  kml_write_data_element("gc_lon", waypointp->longitude);

  kml_write_data_element("gc_type", gs_get_cachetype(waypointp->gc_data->type));
  kml_write_data_element("gc_icon", is);
  kml_write_cdata_element("gc_short_desc", waypointp->gc_data->desc_short.utfstring);
  kml_write_cdata_element("gc_long_desc", waypointp->gc_data->desc_long.utfstring);
  QString logs = kml_geocache_get_logs(waypointp);
  kml_write_cdata_element("gc_logs", logs);

  writer->writeEndElement(); // Close ExtendedData tag

  // Location
  writer->writeStartElement(QStringLiteral("Point"));
  kml_write_coordinates(waypointp);

  writer->writeEndElement(); // Close Point tag
  writer->writeEndElement(); // Close Placemark tag
}

/*
 * WAYPOINTS
 */

static void kml_waypt_pr(const Waypoint* waypointp)
{
  QString icon;

#if 0 // Experimental
  if (realtime_positioning) {
    writer->wrteStartTag("LookAt");
    writer->writeTextElement(QStringLiteral("longitude"), QString::number(waypointp->longitude, 'f', precision);
                             writer->writeTextElement(QStringLiteral("latitude"), QString::number(waypointp->latitude, 'f', precision);
                                 writer->writeTextElement(QStringLiteral("altitude"), QStringLiteral("1000"));
                                 writer->writeEndElement(); // Close LookAt tag
  }
#endif

  if (waypointp->gc_data->diff && waypointp->gc_data->terr) {
    kml_geocache_pr(waypointp);
    return;
  }

  writer->writeStartElement(QStringLiteral("Placemark"));

  writer->writeOptionalTextElement(QStringLiteral("name"), waypointp->shortname);

  // Description
  if (waypointp->HasUrlLink()) {
    writer->writeEmptyElement(QStringLiteral("snippet"));
    UrlLink link = waypointp->GetUrlLink();
    if (!link.url_link_text_.isEmpty()) {
      QString odesc = link.url_;
      QString olink = link.url_link_text_;
      writer->writeStartElement(QStringLiteral("description"));
      writer->writeCDATA(QStringLiteral("<a href=\"%1\">%2</a>").arg(odesc, olink));
      writer->writeEndElement(); // Close description tag
    } else {
      writer->writeTextElement(QStringLiteral("description"), link.url_);
    }
  } else {
    if (waypointp->shortname != waypointp->description) {
      writer->writeOptionalTextElement(QStringLiteral("description"), waypointp->description);
    }
  }

  // Timestamp
  kml_output_timestamp(waypointp);

  // Icon - but only if it looks like a URL.
  icon = opt_deficon ? opt_deficon : waypointp->icon_descr;
  if (icon.contains("://")) {
    writer->writeStartElement(QStringLiteral("Style"));
    writer->writeStartElement(QStringLiteral("IconStyle"));
    writer->writeStartElement(QStringLiteral("Icon"));
    writer->writeTextElement(QStringLiteral("href"), icon);
    writer->writeEndElement(); // Close Icon tag
    writer->writeEndElement(); // Close IconStyle tag
    writer->writeEndElement(); // Close Style tag
  } else {
    writer->writeTextElement(QStringLiteral("styleUrl"), QStringLiteral("#waypoint"));
  }

  // Location
  writer->writeStartElement(QStringLiteral("Point"));
  kml_output_positioning(false);
  kml_write_coordinates(waypointp);
  writer->writeEndElement(); // Close Point tag

  writer->writeEndElement(); // Close Placemark tag
}

/*
 * TRACKPOINTS
 */

static void kml_track_hdr(const route_head* header)
{
  computed_trkdata td = track_recompute(header);
  if (header->rte_waypt_ct > 0 && (export_lines || export_points)) {
    kml_output_header(header, &td);
  }
}

static void kml_track_disp(const Waypoint* waypointp)
{
  kml_output_point(waypointp, kmlpt_track);
}

static void kml_track_tlr(const route_head* header)
{
  if (header->rte_waypt_ct > 0 && (export_lines || export_points)) {
    kml_output_tailer(header);
  }
}

/*
 * New for 2010, Earth adds "MultiTrack" as an extension.
 * Unlike every other format, we do the bulk of the work in the header
 * callback as we have to make multiple passes over the track queues.
 */

// Helper to write gx:SimpleList, iterating over a route queue and writing out.

typedef enum {
  fld_cadence,
  fld_depth,
  fld_heartrate,
  fld_temperature,
  fld_power
} wp_field;

static void kml_mt_simple_array(const route_head* header,
                                const char* name,
                                wp_field member)
{
  writer->writeStartElement(QStringLiteral("gx:SimpleArrayData"));
  writer->writeAttribute(QStringLiteral("name"), name);

  foreach (const Waypoint* wpt, header->waypoint_list) {

    switch (member) {
    case fld_power:
      writer->writeTextElement(QStringLiteral("gx:value"), QString::number(wpt->power, 'f', 1));
      break;
    case fld_cadence:
      writer->writeTextElement(QStringLiteral("gx:value"), QString::number(wpt->cadence));
      break;
    case fld_depth:
      writer->writeTextElement(QStringLiteral("gx:value"), QString::number(wpt->depth, 'f', 1));
      break;
    case fld_heartrate:
      writer->writeTextElement(QStringLiteral("gx:value"), QString::number(wpt->heartrate));
      break;
    case fld_temperature:
      writer->writeTextElement(QStringLiteral("gx:value"), QString::number(wpt->temperature, 'f', 1));
      break;
    default:
      fatal("Bad member type");
    }
  }
  writer->writeEndElement(); // Close SimpleArrayData tag
}

// True if at least two points in the track have timestamps.
static int track_has_time(const route_head* header)
{
  int points_with_time = 0;
  foreach (const Waypoint* tpt, header->waypoint_list) {
    if (tpt->GetCreationTime().isValid()) {
      points_with_time++;
      if (points_with_time >= 2) {
        return 1;
      }
    }
  }
  return 0;
}

// Simulate a track_disp_all callback sequence for a single track.
static void write_as_linestring(const route_head* header)
{
  kml_track_hdr(header);
  foreach (const Waypoint* tpt, header->waypoint_list) {
    kml_track_disp(tpt);
  }
  kml_track_tlr(header);

}

static void kml_mt_hdr(const route_head* header)
{
  int has_cadence = 0;
  int has_depth = 0;
  int has_heartrate = 0;
  int has_temperature = 0;
  int has_power = 0;

  // This logic is kind of inside-out for GPSBabel.  If a track doesn't
  // have enough interesting timestamps, just write it as a LineString.
  if (!track_has_time(header)) {
    write_as_linestring(header);
    return;
  }

  writer->writeStartElement(QStringLiteral("Placemark"));
  writer->writeOptionalTextElement(QStringLiteral("name"), header->rte_name);
  writer->writeTextElement(QStringLiteral("styleUrl"), QStringLiteral("#multiTrack"));
  writer->writeStartElement(QStringLiteral("gx:Track"));
  kml_output_positioning(false);

  foreach (const Waypoint* tpt, header->waypoint_list) {
    if (tpt->GetCreationTime().isValid()) {
      QString time_string = tpt->CreationTimeXML();
      writer->writeOptionalTextElement(QStringLiteral("when"), time_string);
    } else {
      writer->writeStartElement(QStringLiteral("when"));
      writer->writeEndElement(); // Close when tag
    }
  }

  // TODO: How to handle clamped, floating, extruded, etc.?
  foreach (const Waypoint* tpt, header->waypoint_list) {
    if (kml_altitude_known(tpt)) {
      writer->writeTextElement(QStringLiteral("gx:coord"),
                               QString::number(tpt->longitude, 'f', precision) + QString(" ") +
                               QString::number(tpt->latitude, 'f', precision) + QString(" ") +
                               QString::number(tpt->altitude, 'f', 2)
                              );
    } else {
      writer->writeTextElement(QStringLiteral("gx:coord"),
                               QString::number(tpt->longitude, 'f', precision) + QString(" ") +
                               QString::number(tpt->latitude, 'f', precision)
                              );
    }

    // Capture interesting traits to see if we need to do an ExtendedData
    // section later.
    if (tpt->cadence) {
      has_cadence = 1;
    }
    if (WAYPT_HAS(tpt, depth)) {
      has_depth = 1;
    }
    if (tpt->heartrate) {
      has_heartrate = 1;
    }
    if (WAYPT_HAS(tpt, temperature)) {
      has_temperature = 1;
    }
    if (tpt->power) {
      has_power = 1;
    }
  }

  if (has_cadence || has_depth || has_heartrate || has_temperature ||
      has_power) {
    writer->writeStartElement(QStringLiteral("ExtendedData"));
    writer->writeStartElement(QStringLiteral("SchemaData"));
    writer->writeAttribute(QStringLiteral("schemaUrl"), QStringLiteral("#schema"));

    if (has_cadence) {
      kml_mt_simple_array(header, kmt_cadence, fld_cadence);
    }

    if (has_depth) {
      kml_mt_simple_array(header, kmt_depth, fld_depth);
    }

    if (has_heartrate) {
      kml_mt_simple_array(header, kmt_heartrate, fld_heartrate);
    }

    if (has_temperature) {
      kml_mt_simple_array(header, kmt_temperature, fld_temperature);
    }

    if (has_power) {
      kml_mt_simple_array(header, kmt_power, fld_power);
    }

    writer->writeEndElement(); // Close SchemaData tag
    writer->writeEndElement(); // Close ExtendedData tag
  }
}

static void kml_mt_tlr(const route_head* header)
{
  if (track_has_time(header)) {
    writer->writeEndElement(); // Close gx:Track tag
    writer->writeEndElement(); // Close Placemark tag
  }
}

/*
 * ROUTES
 */

static void kml_route_hdr(const route_head* header)
{
  kml_output_header(header, nullptr);
}

static void kml_route_disp(const Waypoint* waypointp)
{
  kml_output_point(waypointp, kmlpt_route);
}

static void kml_route_tlr(const route_head* header)
{
  kml_output_tailer(header);
}

// For Earth 5.0 and later, we write a LookAt that encompasses
// the bounding box of our entire data set and set the event times
// to include all our data.
static void kml_write_AbstractView()
{
  // Make a pass through all the points to find the bounds.
  if (waypt_count()) {
    waypt_disp_all(kml_add_to_bounds);
  }
  if (track_waypt_count())  {
    track_disp_all(nullptr, nullptr, kml_add_to_bounds);
  }
  if (route_waypt_count()) {
    route_disp_all(nullptr, nullptr, kml_add_to_bounds);
  }

  writer->writeStartElement(QStringLiteral("LookAt"));

  if (kml_time_min.isValid() || kml_time_max.isValid()) {
    writer->writeStartElement(QStringLiteral("gx:TimeSpan"));
    if (kml_time_min.isValid()) {
      writer->writeTextElement(QStringLiteral("begin"), kml_time_min.toPrettyString());
    }
    if (kml_time_max.isValid()) {
      // In realtime tracking mode, we fudge the end time by a few minutes
      // to ensure that the freshest data (our current location) is contained
      // within the timespan.   Earth's time may not match the GPS because
      // we may not be running NTP, plus it's polling a file (sigh) to read
      // the network position.  So we shove the end of the timespan out to
      // ensure the right edge of that time slider includes us.
      //
      gpsbabel::DateTime time_max = realtime_positioning ? kml_time_max.addSecs(600) : kml_time_max;
      writer->writeTextElement(QStringLiteral("end"), time_max.toPrettyString());
    }
    writer->writeEndElement(); // Close gx:TimeSpan tag
  }

// If our BB spans the antemeridian, flip sign on one.
// This doesn't make our BB optimal, but it at least prevents us from
// zooming to the wrong hemisphere.
  if (kml_bounds.min_lon * kml_bounds.max_lon < 0) {
    kml_bounds.min_lon = -kml_bounds.max_lon;
  }

  writer->writeTextElement(QStringLiteral("longitude"), QString::number((kml_bounds.min_lon + kml_bounds.max_lon) / 2, 'f', precision));
  writer->writeTextElement(QStringLiteral("latitude"), QString::number((kml_bounds.min_lat + kml_bounds.max_lat) / 2, 'f', precision));

  // It turns out the length of the diagonal of the bounding box gives us a
  // reasonable guess for setting the camera altitude.
  double bb_size = gcgeodist(kml_bounds.min_lat, kml_bounds.min_lon,
                             kml_bounds.max_lat, kml_bounds.max_lon);
  // Clamp bottom zoom level.  Otherwise, a single point zooms to grass.
  if (bb_size < 1000) {
    bb_size = 1000;
  }
  writer->writeTextElement(QStringLiteral("range"), QString::number(bb_size * 1.3, 'f', 6));

  writer->writeEndElement(); // Close LookAt tag
}

static
void kml_mt_array_schema(const char* field_name, const char* display_name,
                         const char* type)
{
  writer->writeStartElement(QStringLiteral("gx:SimpleArrayField"));
  writer->writeAttribute(QStringLiteral("name"), field_name);
  writer->writeAttribute(QStringLiteral("type"), type);
  writer->writeTextElement(QStringLiteral("displayName"), display_name);
  writer->writeEndElement(); // Close gx:SimpleArrayField tag
}

static void kml_write()
{
  const global_trait* traits = get_traits();

  // Parse options
  export_lines = (0 == strcmp("1", opt_export_lines));
  export_points = (0 == strcmp("1", opt_export_points));
  export_track = (0 ==  strcmp("1", opt_export_track));
  floating = (!! strcmp("0", opt_floating));
  extrude = (!! strcmp("0", opt_extrude));
  rotate_colors = (!! opt_rotate_colors);
  trackdata = (!! strcmp("0", opt_trackdata));
  trackdirection = (!! strcmp("0", opt_trackdirection));
  line_width = atol(opt_line_width);
  precision = atol(opt_precision);

  writer->writeStartDocument();
  // FIXME: This write of a blank line is needed for Qt 4.6 (as on Centos 6.3)
  // to include just enough whitespace between <xml/> and <gpx...> to pass
  // diff -w.  It's here for now to shim compatibility with our zillion
  // reference files, but this blank link can go away some day.
  writer->writeCharacters(QStringLiteral("\n"));

  writer->setAutoFormatting(true);

  writer->writeStartElement(QStringLiteral("kml"));
  writer->writeAttribute(QStringLiteral("xmlns"), QStringLiteral("http://www.opengis.net/kml/2.2"));
  writer->writeAttribute(QStringLiteral("xmlns:gx"), QStringLiteral("http://www.google.com/kml/ext/2.2"));

  writer->writeStartElement(QStringLiteral("Document"));

  if (realtime_positioning) {
    writer->writeTextElement(QStringLiteral("name"), QStringLiteral("GPS position"));
  } else {
    writer->writeTextElement(QStringLiteral("name"), QStringLiteral("GPS device"));
  }

  if (current_time().isValid()) {
    writer->writeTextElement(QStringLiteral("snippet"), QStringLiteral("Created ") +
                             current_time().toString());
  }

  kml_write_AbstractView();

  // Style settings for bitmaps
  if (route_waypt_count()) {
    kml_write_bitmap_style(kmlpt_route, ICON_RTE, nullptr);
  }

  if (track_waypt_count()) {
    if (trackdirection) {
      kml_write_bitmap_style(kmlpt_other, ICON_TRK, "track-none");
      for (int i = 0; i < 16; i++) {
        kml_write_bitmap_style(kmlpt_other, QString(ICON_DIR).arg(i), QString("track-%1").arg(i));
      }
    } else {
      kml_write_bitmap_style(kmlpt_track, ICON_TRK, nullptr);
    }
    if (export_track)
      kml_write_bitmap_style(kmlpt_multitrack, ICON_MULTI_TRK,
                             "track-none");
  }

  kml_write_bitmap_style(kmlpt_waypoint, ICON_WPT, nullptr);

  if (track_waypt_count() || route_waypt_count()) {
    writer->writeStartElement(QStringLiteral("Style"));
    writer->writeAttribute(QStringLiteral("id"), QStringLiteral("lineStyle"));
    kml_output_linestyle(opt_line_color, line_width);
    writer->writeEndElement(); // Close Style tag
  }

  if (traits->trait_geocaches) {
    kml_gc_make_balloonstyle();
  }

  if (traits->trait_heartrate ||
      traits->trait_cadence ||
      traits->trait_power ||
      traits->trait_temperature ||
      traits->trait_depth) {
    writer->writeStartElement(QStringLiteral("Schema"));
    writer->writeAttribute(QStringLiteral("id"), QStringLiteral("schema"));

    if (traits->trait_heartrate) {
      kml_mt_array_schema(kmt_heartrate, "Heart Rate", "int");
    }
    if (traits->trait_cadence) {
      kml_mt_array_schema(kmt_cadence, "Cadence", "int");
    }
    if (traits->trait_power) {
      kml_mt_array_schema(kmt_power, "Power", "float");
    }
    if (traits->trait_temperature) {
      kml_mt_array_schema(kmt_temperature, "Temperature", "float");
    }
    if (traits->trait_depth) {
      kml_mt_array_schema(kmt_depth, "Depth", "float");
    }
    writer->writeEndElement(); // Close Schema tag
  }

  if (waypt_count()) {
    if (!realtime_positioning) {
      writer->writeStartElement(QStringLiteral("Folder"));
      writer->writeTextElement(QStringLiteral("name"), QStringLiteral("Waypoints"));
    }

    waypt_disp_all(kml_waypt_pr);

    if (!realtime_positioning) {
      writer->writeEndElement(); // Close Folder tag
    }
  }

  // Output trackpoints
  if (track_waypt_count()) {
    if (!realtime_positioning) {
      writer->writeStartElement(QStringLiteral("Folder"));
      writer->writeTextElement(QStringLiteral("name"), QStringLiteral("Tracks"));
    }

    kml_init_color_sequencer(track_count());
    if (export_track) {
      track_disp_all(kml_mt_hdr, kml_mt_tlr, nullptr);
    }

    track_disp_all(kml_track_hdr, kml_track_tlr,
                   kml_track_disp);

    if (!realtime_positioning) {
      writer->writeEndElement(); // Close Folder tag
    }
  }

  // Output routes
  if (route_waypt_count()) {
    if (!realtime_positioning) {
      writer->writeStartElement(QStringLiteral("Folder"));
      writer->writeTextElement(QStringLiteral("name"), QStringLiteral("Routes"));

      kml_init_color_sequencer(route_count());
      route_disp_all(kml_route_hdr,
                     kml_route_tlr, kml_route_disp);
      writer->writeEndElement(); // Close Folder tag
    }
  }

  writer->writeEndElement(); // Close Document tag.
  writer->writeEndElement(); // Close kml tag.
}

/*
 * This depends on the table being sorted correctly.
 */
static const
char*
kml_get_posn_icon(int freshness)
{
  int n_stations = sizeof(kml_tracking_icons) / sizeof(kml_tracking_icons[0]);

  for (int i = 0; i < n_stations ; i++) {
    if (freshness >= kml_tracking_icons[i].freshness) {
      return kml_tracking_icons[i].icon;
    }
  }
  return ICON_NOSAT;
}


static route_head* posn_trk_head = nullptr;

static void
kml_wr_position(Waypoint* wpt)
{
  static gpsbabel::DateTime last_valid_fix;

  kml_wr_init(posnfilenametmp);

  if (!posn_trk_head) {
    posn_trk_head = route_head_alloc();
    track_add_head(posn_trk_head);
  }

  if (!last_valid_fix.isValid()) {
    last_valid_fix = current_time();
  }

  /* We want our waypoint to have a name, but not our trackpoint */
  if (wpt->shortname.isEmpty()) {
    if (wpt->fix == fix_none) {
      wpt->shortname = "ESTIMATED Position";
    } else {
      wpt->shortname = "Position";
    }
  }

  switch (wpt->fix) {
  case fix_none:
    wpt->shortname = "ESTIMATED Position";
    break;
  case fix_unknown:
    break;
  default:
    last_valid_fix = wpt->GetCreationTime();
  }

  wpt->icon_descr = kml_get_posn_icon(wpt->GetCreationTime().toTime_t() - last_valid_fix.toTime_t());


  /* In order to avoid clutter while we're sitting still, don't add
     track points if we've not moved a minimum distance from the
     beginning of our accumulated track. */
  {
    Waypoint* newest_posn= posn_trk_head->waypoint_list.back();

    if (radtometers(gcdist(RAD(wpt->latitude), RAD(wpt->longitude),
                           RAD(newest_posn->latitude), RAD(newest_posn->longitude))) > 50) {
      track_add_wpt(posn_trk_head, new Waypoint(*wpt));
    } else {
      /* If we haven't move more than our threshold, pretend
       * we didn't move at  all to prevent Earth from jittering
       * the zoom levels on us.
       */
      wpt->latitude = newest_posn->latitude;
      wpt->longitude = newest_posn->longitude;
    }
  }

  waypt_add(wpt);
  kml_write();
  waypt_del(wpt);

  /*
   * If we are keeping only a recent subset of the trail, trim the
   * head here.
   */
  while (max_position_points &&
         (posn_trk_head->rte_waypt_ct >= max_position_points)) {
    Waypoint* tonuke = posn_trk_head->waypoint_list.front();
    track_del_wpt(posn_trk_head, tonuke);
  }

  kml_wr_deinit();
}

ff_vecs_t kml_vecs = {
  ff_type_file,
  FF_CAP_RW_ALL, /* Format can do RW_ALL */
  kml_rd_init,
  kml_wr_init,
  kml_rd_deinit,
  kml_wr_deinit,
  kml_read,
  kml_write,
  nullptr,
  kml_args,
  CET_CHARSET_UTF8, 1,	/* CET-REVIEW */
  { nullptr, nullptr, nullptr, kml_wr_position_init, kml_wr_position, kml_wr_position_deinit },
  nullptr
};
