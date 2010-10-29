/*
	Support for Google Earth & Keyhole "kml" format.

	Copyright (C) 2005, 2006, 2007, 2008, 2009, 2010  Robert Lipe,
           robertlipe@gpsbabel.org
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
#include "defs.h"
#include "xmlgeneric.h"
#include "grtcirc.h"

#ifdef __WIN32__
# include <windows.h>
#endif

// options
static char *opt_deficon = NULL;
static char *opt_export_lines = NULL;
static char *opt_export_points = NULL;
static char *opt_export_track = NULL;
static char *opt_line_width = NULL;
static char *opt_line_color = NULL;
static char *opt_floating = NULL;
static char *opt_extrude = NULL;
static char *opt_trackdata = NULL;
static char *opt_trackdirection = NULL;
static char *opt_units = NULL;
static char *opt_labels = NULL;
static char *opt_max_position_points = NULL;

static int export_lines;
static int export_points;
static int export_track;
static int floating;
static int extrude;
static int trackdata;
static int trackdirection;
static int max_position_points;
static int export_track;
static int line_width;

static int indent_level;

static waypoint *wpt_tmp;
static int wpt_tmp_queued;
static const char *posnfilename;
static char *posnfilenametmp;

static gbfile *ofd;

#define COORD_FORMAT "%.6f"
#define ALT_FORMAT   "%.2f"  // Beyond a centimeter is fantasy.

typedef enum  {
  kmlpt_unknown,
  kmlpt_waypoint,
  kmlpt_track,
  kmlpt_route,
  kmlpt_multitrack,
  kmlpt_other
} kml_point_type;

static int realtime_positioning;
static int do_indentation = 1;
static bounds kml_bounds;
static time_t kml_time_min;
static time_t kml_time_max;

#define TD(FMT,DATA) kml_write_xml(0, "<tr><td>" FMT " </td></tr>\n", DATA)
#define TD2(FMT,DATA, DATA2) kml_write_xml(0, "<tr><td>" FMT " </td></tr>\n", DATA, DATA2)

//  Icons provided and hosted by Google.  Used with permission.
#define ICON_BASE "http://earth.google.com/images/kml-icons/"

static const char kml22_hdr[] =
  "<kml xmlns=\"http://www.opengis.net/kml/2.2\"\n"
  "\txmlns:gx=\"http://www.google.com/kml/ext/2.2\">\n";

// Multitrack ids to correlate Schema to SchemaData
static const char kmt_heartrate[] = "heartrate";
static const char kmt_cadence[] = "cadence";
static const char kmt_temperature[] = "temperature";
static const char kmt_depth[] = "depth";
static const char kmt_power[] = "power";


static
arglist_t kml_args[] = {
	{"deficon", &opt_deficon, "Default icon name", NULL, ARGTYPE_STRING, ARG_NOMINMAX },
	{"lines", &opt_export_lines,
         "Export linestrings for tracks and routes",
         "1", ARGTYPE_BOOL, ARG_NOMINMAX },
	{"points", &opt_export_points,
         "Export placemarks for tracks and routes",
         "1", ARGTYPE_BOOL, ARG_NOMINMAX },
	{"line_width", &opt_line_width,
         "Width of lines, in pixels",
         "6", ARGTYPE_INT, ARG_NOMINMAX },
	{"line_color", &opt_line_color,
         "Line color, specified in hex AABBGGRR",
         "99ffac59", ARGTYPE_STRING, ARG_NOMINMAX },
	{"floating", &opt_floating,
	 "Altitudes are absolute and not clamped to ground",
	 "0", ARGTYPE_BOOL, ARG_NOMINMAX },
	{"extrude", &opt_extrude,
	 "Draw extrusion line from trackpoint to ground",
	 "0", ARGTYPE_BOOL, ARG_NOMINMAX },
	{"track", &opt_export_track,
	 "Write KML track (default = 0)",
	 "0", ARGTYPE_BOOL, ARG_NOMINMAX },
	{"trackdata", &opt_trackdata,
	 "Include extended data for trackpoints (default = 1)",
	 "1", ARGTYPE_BOOL, ARG_NOMINMAX },
	{"trackdirection", &opt_trackdirection,
	 "Indicate direction of travel in track icons (default = 0)",
	 "0", ARGTYPE_BOOL, ARG_NOMINMAX },
	{"units", &opt_units,
	 "Units used when writing comments ('s'tatute, 'm'etric,' 'n'autical, 'a'viation)",
	 "s", ARGTYPE_STRING, ARG_NOMINMAX },
	{"labels", &opt_labels,
	 "Display labels on track and routepoints  (default = 1)",
	 "1", ARGTYPE_BOOL, ARG_NOMINMAX },
	{"max_position_points", &opt_max_position_points,
	 "Retain at most this number of position points  (0 = unlimited)",
	 "0", ARGTYPE_INT, ARG_NOMINMAX },
	ARG_TERMINATOR
};

static
struct {
	int freshness;
	const char *icon;
} kml_tracking_icons[] = {
 { 60, ICON_BASE "youarehere-60.png" }, // Red
 { 30, ICON_BASE "youarehere-30.png" }, // Yellow
 { 0,  ICON_BASE "youarehere-0.png" }, // Green
};

#define ICON_NOSAT ICON_BASE "youarehere-warning.png";
#define ICON_WPT "http://maps.google.com/mapfiles/kml/pal4/icon61.png"
#define ICON_TRK ICON_BASE "track-directional/track-none.png"
#define ICON_RTE ICON_BASE "track-directional/track-none.png"
#define ICON_MULTI_TRK ICON_BASE "track-directional/track-0.png"
#define ICON_DIR ICON_BASE "track-directional/track-%d.png" // format string where next arg is rotational degrees.

#define MYNAME "kml"

#if ! HAVE_LIBEXPAT
static void
kml_rd_init(const char *fname)
{
	fatal(MYNAME ": This build excluded KML support because expat was not installed.\n");
}

static void
kml_read(void)
{
}
#else

static xg_callback wpt_s, wpt_e;
static xg_callback wpt_name, wpt_desc, wpt_coord, wpt_icon, trk_coord, wpt_time;

static
xg_tag_mapping kml_map[] = {
	{ wpt_s, 	cb_start, 	"/Placemark" },
	{ wpt_e, 	cb_end, 	"/Placemark" },
	{ wpt_name, 	cb_cdata, 	"/Placemark/name" },
	{ wpt_desc, 	cb_cdata, 	"/Placemark/description" },
	{ wpt_time, 	cb_cdata, 	"/Placemark/TimeStamp/when" },
	// Alias for above used in KML 2.0
	{ wpt_time, 	cb_cdata, 	"/Placemark/TimeInstant/timePosition" },
	{ wpt_coord, 	cb_cdata, 	"/Placemark/Point/coordinates" },
	{ wpt_icon, 	cb_cdata, 	"/Placemark/Style/Icon/href" },
	{ trk_coord, 	cb_cdata, 	"/Placemark/MultiGeometry/LineString/coordinates" },
	{ trk_coord, 	cb_cdata, 	"/Placemark/GeometryCollection/LineString/coordinates" },
	{ trk_coord, 	cb_cdata,	"/Placemark/Polygon/outerBoundaryIs/LinearRing/coordinates" },
	{ trk_coord, 	cb_cdata, 	"/Placemark/LineString/coordinates" },
	{ NULL, 	0, 		NULL }
};

static
const char * kml_tags_to_ignore[] = {
	"kml",
	"Document",
	"Folder",
	NULL,
};

void wpt_s(const char *args, const char **unused)
{
	wpt_tmp = waypt_new();
	wpt_tmp_queued = 0;
}

void wpt_e(const char *args, const char **unused)
{
	if (wpt_tmp_queued) {
		waypt_add(wpt_tmp);
	}
	wpt_tmp_queued = 0;
}

void wpt_name(const char *args, const char **unused)
{
	if (args) wpt_tmp->shortname = xstrdup(args);
}

void wpt_desc(const char *args, const char **unused)
{
	if (args) {
		char *tmp, *c;

		tmp = xstrdup((char *)args);
		c = lrtrim(tmp);
		if (*c) {
			wpt_tmp->description = xstrappend(wpt_tmp->description, c);
		}
		xfree(tmp);
	}
}

void wpt_time(const char *args, const char **unused)
{
	wpt_tmp->creation_time = xml_parse_time(args, &wpt_tmp->microseconds);
}

void wpt_coord(const char *args, const char **attrv)
{
	int n = 0;
	double lat, lon, alt;
	// Alt is actually optional. 
	n = sscanf(args, "%lf,%lf,%lf", &lon, &lat, &alt);
	if (n >= 2) {
		wpt_tmp->latitude = lat;
		wpt_tmp->longitude = lon;
	}
	if (n == 3) {
		wpt_tmp->altitude = alt;
	}
	wpt_tmp_queued = 1;
}

void wpt_icon(const char *args, const char **unused)
{
	if (wpt_tmp)  {
		wpt_tmp->icon_descr = xstrdup(args);
		wpt_tmp->wpt_flags.icon_descr_is_dynamic = 1;
	}
}

void trk_coord(const char *args, const char **attrv)
{
	int consumed = 0;
	double lat, lon, alt;
	waypoint *trkpt;
        int n = 0;

	route_head *trk_head = route_head_alloc();
	if (wpt_tmp->shortname) {
		trk_head->rte_name  = xstrdup(wpt_tmp->shortname);
	}
	track_add_head(trk_head);

	while ((n = sscanf(args, "%lf,%lf,%lf %n", &lon, &lat, &alt, &consumed)) > 0) {

		trkpt = waypt_new();
		trkpt->latitude = lat;
		trkpt->longitude = lon;

		// Line malformed or two-arg format without alt .  Rescan.
		if (2 == n) {
		  sscanf(args, "%lf,%lf %n", &lon, &lat, &consumed);
		}

		if (3 == n) {
		  trkpt->altitude = alt;
		}

		track_add_wpt(trk_head, trkpt);

		args += consumed;
	}
}

static
void
kml_rd_init(const char *fname)
{
	xml_init(fname, kml_map, NULL);
	xml_ignore_tags(kml_tags_to_ignore);
}

static
void
kml_read(void)
{
	xml_read();
}
#endif

static void
kml_rd_deinit(void)
{
	xml_deinit();
}

static void
kml_wr_init(const char *fname)
{
	char u = 's';
        waypt_init_bounds(&kml_bounds);
        kml_time_min = 0;
        kml_time_max = 0;

	if (opt_units) {
		u = tolower(opt_units[0]);
	}

	switch(u) {
		case 's': fmt_setunits(units_statute); break;
		case 'm': fmt_setunits(units_metric); break;
		case 'n': fmt_setunits(units_nautical); break;
		case 'a': fmt_setunits(units_aviation); break;
		default: fatal("Units argument '%s' should be 's' for statute units, 'm' for metric, 'n' for nautical or 'a' for aviation.\n", opt_units); break;
	}
	/*
	 * Reduce race conditions with network read link.
	 */
	ofd = gbfopen(fname, "w", MYNAME);
}

/*
 * The magic here is to try to ensure that posnfilename is atomically
 * updated.
 */
static void
kml_wr_position_init(const char *fname)
{
	posnfilename = fname;
	posnfilenametmp = xstrappend(xstrdup(fname), "-");
	realtime_positioning = 1;

	/*
	 * 30% of our output file is whitespace.  Since parse time
	 * matters in this mode, turn the pretty formatting off.
	 */
	do_indentation = 0;

	max_position_points = atoi(opt_max_position_points);
}

static void
kml_wr_deinit(void)
{
	gbfclose(ofd);

	if (posnfilenametmp) {
#if __WIN32__
		MoveFileExA(posnfilenametmp, posnfilename,
		MOVEFILE_REPLACE_EXISTING);
#endif
		rename(posnfilenametmp, posnfilename);
	}
	ofd = NULL;
}

static void
kml_wr_position_deinit(void)
{
//	kml_wr_deinit();
	if (posnfilenametmp) {
		xfree(posnfilenametmp);
		posnfilenametmp = NULL;
	}
}

/*
 *  Indent is a direction to change indention level.
 * If positive, increase one level after printing this line.
 * If zero, just print this line at the current indent leve.
 * If negative, descrease the indent level.
 */
static void
kml_write_xml(int indent, const char *fmt, ...)
{
	va_list args;
	int i;
	va_start(args, fmt);

	if (indent < 0) indent_level--;

	if (fmt[1] != '!' && do_indentation) {
		for (i = 0; i < indent_level; i++) {
			gbfputs("  ", ofd);
		}
	}

	gbvfprintf(ofd, fmt, args);

	if (indent > 0) indent_level++;

	va_end(args);
}

/*
 * Write an optional tag with a value that may need to be entity escaped.
 * Never changes indention leve, but does honour it.
 */
static void
kml_write_xmle(const char *tag, const char *fmt, ...)
{
	va_list args;
	int i;
	va_start(args, fmt);

	if (fmt && *fmt) {
		char *tmp_ent = xml_entitize(fmt);
		int needs_escaping = 0;
		for (i = 0; i < indent_level; i++) {
			gbfputs("  ", ofd);
		}
		if (strspn(tmp_ent, "&'<>\""))
			needs_escaping = 1;
		gbfprintf(ofd, "<%s>", tag);
                if (needs_escaping) gbfprintf(ofd, "<![CDATA[");
		gbvfprintf(ofd, tmp_ent, args);
                if (needs_escaping) gbfprintf(ofd, "]]>");
		gbfprintf(ofd, "</%s>\n", tag);
		xfree(tmp_ent);
	}
}

void
kml_output_linestyle(char *color, int width) {
  // Style settings for line strings
  kml_write_xml(1, "<LineStyle>\n");
  kml_write_xml(0, "<color>%s</color>\n", opt_line_color);
  kml_write_xml(0, "<width>%d</width>\n", width);
  kml_write_xml(-1, "</LineStyle>\n");
}


#define hovertag(h) h ? 'h' : 'n'
static void kml_write_bitmap_style_(const char *style, const char * bitmap,
				    int highlighted, int force_heading)
{
        int is_track = !strncmp(style, "track", 5);
        int is_multitrack = !strncmp(style, "multiTrack", 5);

	kml_write_xml(0, "<!-- %s %s style -->\n",
		highlighted ? "Highlighted" : "Normal", style);
	kml_write_xml(1, "<Style id=\"%s_%c\">\n", style, hovertag(highlighted));

	kml_write_xml(1, "<IconStyle>\n");
	if (highlighted) {
		kml_write_xml(0, "<scale>1.2</scale>\n");
	} else {
          if (is_track) {
		kml_write_xml(0, "<scale>.5</scale>\n");
          }
        }
	/* Our icons are pre-rotated, so nail them to the maps. */
	if (force_heading) {
		kml_write_xml(0, "<heading>0</heading>\n");
	}
	kml_write_xml(1, "<Icon>\n");
	kml_write_xml(0, "<href>%s</href>\n", bitmap);
	kml_write_xml(-1, "</Icon>\n");
	kml_write_xml(-1, "</IconStyle>\n");

	if (is_track && !highlighted) {
		kml_write_xml(1, "<LabelStyle>\n");
		kml_write_xml(0, "<scale>0</scale>\n");
		kml_write_xml(-1, "</LabelStyle>\n");
	}

	if (is_multitrack) {
		kml_output_linestyle(opt_line_color,
                                     highlighted ? line_width + 2 :
                                                   line_width);
        }

	kml_write_xml(-1, "</Style>\n");
}

/* A wrapper for the above function to emit both a highlighted
 * and non-highlighted version of the style to allow the icons
 * to magnify slightly on a rollover.
 */
static void kml_write_bitmap_style(kml_point_type pt_type, const char *bitmap,
				  const char *customstyle)
{
	int force_heading = 0;
	const char *style;
	switch (pt_type) {
		case kmlpt_track: style = "track"; break;
		case kmlpt_route: style = "route"; break;
		case kmlpt_waypoint: style = "waypoint"; break;
		case kmlpt_multitrack: style = "multiTrack"; break;
		case kmlpt_other: style = customstyle; force_heading = 1; break;
		default: fatal("kml_output_point: unknown point type"); break;
	}

	kml_write_bitmap_style_(style, bitmap, 0, force_heading);
	kml_write_bitmap_style_(style, bitmap, 1, force_heading);

	kml_write_xml(1, "<StyleMap id=\"%s\">\n", style);
	kml_write_xml(1, "<Pair>\n");
	kml_write_xml(0, "<key>normal</key>\n");
	kml_write_xml(0, "<styleUrl>#%s_%c</styleUrl>\n",style, hovertag(0));
	kml_write_xml(-1, "</Pair>\n");
	kml_write_xml(1, "<Pair>\n");
	kml_write_xml(0, "<key>highlight</key>\n");
	kml_write_xml(0, "<styleUrl>#%s_%c</styleUrl>\n",style, hovertag(1));
	kml_write_xml(-1, "</Pair>\n");
	kml_write_xml(-1, "</StyleMap>\n");
}

static void kml_output_timestamp(const waypoint *waypointp)
{
	char time_string[64];
	if (waypointp->creation_time) {
		xml_fill_in_time(time_string, waypointp->creation_time, waypointp->microseconds, XML_LONG_TIME);
		if (time_string[0]) {
			kml_write_xml(0, "<TimeStamp><when>%s</when></TimeStamp>\n",
				time_string);
		}
	}
}

/*
 * Output the track summary.
 */
static
void kml_output_trkdescription(const route_head *header, computed_trkdata *td)
{
	char *max_alt_units;
	double max_alt;
	char *min_alt_units;
	double min_alt;
	char *distance_units;
	double distance;

	if (!td || !trackdata) {
		return;
	}

	max_alt = fmt_altitude(td->max_alt, &max_alt_units);
	min_alt = fmt_altitude(td->min_alt, &min_alt_units);
	distance = fmt_distance(td->distance_meters, &distance_units);

	kml_write_xml(0, "<Snippet/>\n");

	kml_write_xml(1, "<description>\n");
	kml_write_xml(1, "<![CDATA[<table>\n");

	if (header->rte_desc) {
		TD("<b>Description</b> %s", header->rte_desc);
	}
	TD2("<b>Distance</b> %.1f %s", distance, distance_units);
	if (min_alt != -unknown_alt) {
		TD2("<b>Min Alt</b> %.3f %s", min_alt, min_alt_units);
	}
	if (max_alt != unknown_alt) {
		TD2("<b>Max Alt</b> %.3f %s", max_alt, max_alt_units);
	}
	if (td->min_spd) {
		char *spd_units;
		double spd = fmt_speed(td->min_spd, &spd_units);
		TD2("<b>Min Speed</b> %.1f %s", spd, spd_units);
	}
	if (td->max_spd) {
		char *spd_units;
		double spd = fmt_speed(td->max_spd, &spd_units);
		TD2("<b>Max Speed</b> %.1f %s", spd, spd_units);
	}
	if (td->max_spd && td->start && td->end) {
		char *spd_units;
		time_t elapsed = td->end - td->start;
		double spd = fmt_speed(td->distance_meters / elapsed, &spd_units);
		if (spd > 1.0)  {
			TD2("<b>Avg Speed</b> %.1f %s", spd, spd_units);
		}
	}
	if (td->avg_hrt) {
		TD("<b>Avg Heart Rate</b> %.1f bpm", td->avg_hrt);
	}
	if (td->min_hrt < td->max_hrt) {
		TD("<b>Min Heart Rate</b> %d bpm", td->min_hrt);
	}
	if (td->max_hrt) {
		TD("<b>Max Heart Rate</b> %d bpm", td->max_hrt);
	}
	if (td->avg_cad) {
		TD("<b>Avg Cadence</b> %.1f rpm", td->avg_cad);
	}
	if (td->max_cad) {
		TD("<b>Max Cadence</b> %d rpm", td->max_cad);
	}
	if (td->start && td->end) {
		char time_string[64];

		xml_fill_in_time(time_string, td->start, 0, XML_LONG_TIME);
		TD("<b>Start Time</b> %s ", time_string);
		xml_fill_in_time(time_string, td->end, 0, XML_LONG_TIME);
		TD("<b>End Time</b> %s ", time_string);
	}

	kml_write_xml(-1, "</table>]]>\n");
	kml_write_xml(-1, "</description>\n");

	/* We won't always have times. Garmin saved tracks, for example... */
	if (td->start && td->end) {
		char time_string[64];
		kml_write_xml(1, "<TimeSpan>\n");
		xml_fill_in_time(time_string, td->start, 0, XML_LONG_TIME);
		kml_write_xml(0, "<begin>%s</begin>\n", time_string);
		xml_fill_in_time(time_string, td->end, 0, XML_LONG_TIME);
		kml_write_xml(0, "<end>%s</end>\n", time_string);
		kml_write_xml(-1, "</TimeSpan>\n");
	}
}


static
void kml_output_header(const route_head *header, computed_trkdata*td)
{
	if (!realtime_positioning)  {
		kml_write_xml(1,  "<Folder>\n");
	}
	kml_write_xmle("name", header->rte_name);
	kml_output_trkdescription(header, td);

	if (export_points && header->rte_waypt_ct > 0) {
	       // Put the points in a subfolder
	       kml_write_xml(1,  "<Folder>\n");
	       kml_write_xml(0,  "<name>Points</name>\n");
	}
}

static
int kml_altitude_known(const waypoint *waypoint)
{
	if (waypoint->altitude == unknown_alt) {
		return 0;
	}
	// We see way more data that's sourceed at 'zero' than is actually
	// precisely at 0 MSL.
	if (fabs(waypoint->altitude) < .01) {
		return 0;
	}
	return 1;
}

static
void kml_write_coordinates(const waypoint *waypointp)
{
	if (kml_altitude_known(waypointp)) {
		kml_write_xml(0, "<coordinates>"
				COORD_FORMAT "," COORD_FORMAT "," ALT_FORMAT
				"</coordinates>\n",
		  waypointp->longitude,
		  waypointp->latitude,
		  waypointp->altitude);
	} else {
		kml_write_xml(0, "<coordinates>"
					COORD_FORMAT "," COORD_FORMAT
				  "</coordinates>\n",
		  waypointp->longitude,
		  waypointp->latitude);
	}
}

/* Rather than a default "top down" view, view from the side to highlight
 * topo features.
 */
static void kml_output_lookat(const waypoint *waypointp)
{
	kml_write_xml(1, "<LookAt>\n");
	kml_write_xml(0, "<longitude>%f</longitude>\n", waypointp->longitude);
	kml_write_xml(0, "<latitude>%f</latitude>\n", waypointp->latitude);
	kml_write_xml(0, "<tilt>66</tilt>\n");
	kml_write_xml(-1, "</LookAt>\n");
}

static void kml_output_positioning(void)
{
	if (floating) {
		kml_write_xml(0, "<altitudeMode>absolute</altitudeMode>\n");
        }

	if (extrude) {
		kml_write_xml(0, "<extrude>1</extrude>\n");
	}
}

/* Output something interesing when we can for route and trackpoints */
static void kml_output_description(const waypoint *pt)
{
	char *alt_units;
	double alt;

	if (!trackdata) {
		return;
	}

	alt = fmt_altitude(pt->altitude, &alt_units);

	kml_write_xml(1, "<description><![CDATA[\n");
	kml_write_xml(1, "<table>\n");

	TD("Longitude: %f", pt->longitude);
	TD("Latitude: %f", pt->latitude);
	if (kml_altitude_known(pt)) TD2("Altitude: %.3f %s", alt, alt_units);
	if (pt->heartrate) TD("Heart rate: %d", pt->heartrate);
	if (pt->cadence) TD("Cadence: %d", pt->cadence);
	/* Which unit is this temp in? C? F? K? */
	if WAYPT_HAS(pt, temperature) TD("Temperature: %.1f", pt->temperature);
	if WAYPT_HAS(pt, depth) {
		char *depth_units;
		double depth = fmt_distance(pt->depth, &depth_units);
		TD2("Depth: %.1f %s", depth, depth_units);
	}
	if WAYPT_HAS(pt, speed) {
		char *spd_units;
		double spd = fmt_speed(pt->speed, &spd_units);
		TD2("Speed: %.1f %s", spd, spd_units);
	}
	if WAYPT_HAS(pt, course) TD("Heading: %.1f", pt->course);
	/* This really shouldn't be here, but as of this writing,
	 * Earth can't edit/display the TimeStamp.
	 */
	if (pt->creation_time) {
		char time_string[64];

		xml_fill_in_time(time_string, pt->creation_time,
					pt->microseconds, XML_LONG_TIME);
		 if (time_string[0]) {
			 TD("Time: %s", time_string);
		 }
	}

	kml_write_xml(-1, "</table>\n");
	kml_write_xml(-1, "]]></description>\n");
}

static void kml_recompute_time_bounds(const waypoint *waypointp) {
  if (waypointp->creation_time && (waypointp->creation_time < kml_time_min)) {
    kml_time_min = waypointp->creation_time;
  }
  if (waypointp->creation_time > kml_time_max) {
    kml_time_max = waypointp->creation_time;
    if (kml_time_min == 0) {
      kml_time_min = waypointp->creation_time;
    }
  }
}

static void kml_output_point(const waypoint *waypointp, kml_point_type pt_type) {
  const char *style;

  waypt_add_to_bounds(&kml_bounds, waypointp);
  kml_recompute_time_bounds(waypointp);

  switch (pt_type) {
    case kmlpt_track: style = "#track"; break;
    case kmlpt_route: style = "#route"; break;
    default: fatal("kml_output_point: unknown point type"); break;
  }

  switch (pt_type) {
    case kmlpt_track: style = "#track"; break;
    case kmlpt_route: style = "#route"; break;
    default: fatal("kml_output_point: unknown point type"); break;
  }

  if (export_points) {
	kml_write_xml(1, "<Placemark>\n");
	if (atoi(opt_labels)) {
		kml_write_xmle("name", waypointp->shortname);
	}
	kml_write_xml(0, "<Snippet/>\n");
	kml_output_description(waypointp);
	kml_output_lookat(waypointp);
	kml_output_timestamp(waypointp);


        if (opt_deficon) {
		kml_write_xml(1, "<Style>\n");
		kml_write_xml(1, "<IconStyle>\n");
		kml_write_xml(1, "<Icon>\n");
		kml_write_xml(0, "<href>%s</href>\n", opt_deficon);
		kml_write_xml(-1, "</Icon>\n");
		kml_write_xml(-1, "</IconStyle>\n");
		kml_write_xml(-1, "</Style>\n");
        } else {
          if (trackdirection && (pt_type == kmlpt_track)) {
		char buf[100];
		if (waypointp->speed < 1)
			snprintf(buf, sizeof(buf), "%s-none", style);
		else
			snprintf(buf, sizeof(buf), "%s-%d", style,
				(int) (waypointp->course / 22.5 + .5) % 16);
		kml_write_xml(0, "<styleUrl>%s</styleUrl>\n", buf);
	  } else {
		kml_write_xml(0, "<styleUrl>%s</styleUrl>\n", style);
	  }
        }

	kml_write_xml(1, "<Point>\n");
        kml_output_positioning();

	if (extrude) {
		kml_write_xml(0, "<extrude>1</extrude>\n");
	}
	kml_write_coordinates(waypointp);
	kml_write_xml(-1, "</Point>\n");

	kml_write_xml(-1, "</Placemark>\n");
  }
}

static void kml_output_tailer(const route_head *header)
{
  queue *elem, *tmp;

  if (export_points && header->rte_waypt_ct > 0) {
    kml_write_xml(-1, "</Folder>\n");
  }

  // Add a linestring for this track?
  if (export_lines && header->rte_waypt_ct > 0) {
    int needs_multigeometry = 0;
    QUEUE_FOR_EACH(&header->waypoint_list, elem, tmp) {
      waypoint *tpt = (waypoint *) elem;
      int first_in_trk = tpt->Q.prev == &header->waypoint_list;
      if (!first_in_trk && tpt->wpt_flags.new_trkseg) {
        needs_multigeometry = 1;
        break;
        }
    }
    kml_write_xml(1, "<Placemark>\n");
    kml_write_xml(0, "<name>Path</name>\n");
    kml_write_xml(0, "<styleUrl>#lineStyle</styleUrl>\n");
    if (header->line_color.bbggrr >= 0 || header->line_width >= 0) {
      kml_write_xml(1, "<Style>\n");
      kml_write_xml(1, "<LineStyle>\n");
      if (header->line_color.bbggrr >= 0)
        kml_write_xml(0, "<color>%02x%06x</color>\n",
                      header->line_color.opacity, header->line_color.bbggrr);
      if (header->line_width >= 0)
        kml_write_xml(0, "<width>%d</width>\n",header->line_width);
      kml_write_xml(-1, "</LineStyle>\n");
      kml_write_xml(-1, "</Style>\n");
    }
    if (needs_multigeometry)
      kml_write_xml(1, "<MultiGeometry>\n");

    QUEUE_FOR_EACH(&header->waypoint_list, elem, tmp) {
      waypoint *tpt = (waypoint *) elem;
      int first_in_trk = tpt->Q.prev == &header->waypoint_list;
      if (tpt->wpt_flags.new_trkseg) {
        if(!first_in_trk) {
          kml_write_xml(-1, "</coordinates>\n");
          kml_write_xml(-1, "</LineString>\n");
        }
        kml_write_xml(1, "<LineString>\n");
        kml_output_positioning();
        kml_write_xml(0, "<tessellate>1</tessellate>\n");
        kml_write_xml(1, "<coordinates>\n");
      }
      if (kml_altitude_known(tpt)) {
        kml_write_xml(0, COORD_FORMAT "," COORD_FORMAT "," ALT_FORMAT "\n",
                         tpt->longitude, tpt->latitude, tpt->altitude);
       } else {
         kml_write_xml(0, COORD_FORMAT "," COORD_FORMAT "\n",
                          tpt->longitude,
                          tpt->latitude);
       }
    }
    kml_write_xml(-1, "</coordinates>\n");
    kml_write_xml(-1, "</LineString>\n");
    if (needs_multigeometry)
      kml_write_xml(-1, "</MultiGeometry>\n");
    kml_write_xml(-1, "</Placemark>\n");
  }

  if (!realtime_positioning)  {
    kml_write_xml(-1, "</Folder>\n");
  }
}

/*
 * Completely different writer for geocaches.
 */
static
void kml_gc_make_ballonstyle(void)
{
	// BalloonStyle for Geocaches.
	kml_write_xml(1, "<Style id=\"geocache\"><BalloonStyle><text><![CDATA[\n");
	kml_write_xml(0, "<a href=\"http://www.geocaching.com\"><img src=\"http://www.geocaching.com/images/nav/logo_sub.gif\"> </a>\n");

	kml_write_xml(0, "<p><a href=\"http://www.geocaching.com/seek/cache_details.aspx?wp=$[gc_num]\"><b>$[gc_num]</b></a> <b>$[gc_name]</b> \n");
	kml_write_xml(0, "a $[gc_type], by <b>$[gc_placer]</b> [<a href=\"http://www.geocaching.com/profile?id=$[gc_placer_id]\">profile</a>]<br/>\n");
	kml_write_xml(0, "(ratings out of 5 stars. 1 is easiest, 5 is hardest)<br/>\n");
	kml_write_xml(0, "Difficulty: <img src=\"http://www.geocaching.com/images/stars/$[gc_diff_stars].gif\" alt=\"$[gc_diff]\" width=\"61\" height=\"13\">\n");
	kml_write_xml(0, "&nbsp;Terrain: <img src=\"http://www.geocaching.com/images/stars/$[gc_terr_stars].gif\" alt=\"$[gc_terr]\" width=\"61\" height=\"13\"><br />\n");
	kml_write_xml(0, "Size: <img src=\"http://www.geocaching.com/images/icons/container/$[gc_cont_icon].gif\" width=\"45\" height=\"12\">&nbsp;($[gc_cont_icon])<br />\n");

	kml_write_xml(0, "$[gc_issues]\n");
	kml_write_xml(0, "$[gc_short_desc]\n");
	kml_write_xml(0, "$[gc_long_desc]\n");
	kml_write_xml(-1, "]]></text></BalloonStyle></Style>\n");
}

static
char *
kml_lookup_gc_icon(const waypoint *waypointp)
{
	const char *icon;
	char *rb;

	/* This could be done so much better in C99 with designated
	 * initializers...
	 */
	switch (waypointp->gc_data->type) {
		case gt_traditional: icon = "2.png"; break;
		case gt_multi: icon = "3.png"; break;
		case gt_virtual: icon = "4.png"; break;
		case gt_letterbox: icon = "5.png"; break;
		case gt_event: icon = "6.png"; break;
		case gt_ape: icon = "7.png"; break;
		case gt_locationless: icon = "8.png"; break; // No unique icon.
		case gt_suprise: icon = "8.png"; break;
		case gt_webcam: icon = "11.png"; break;
		case gt_cito: icon = "13.png"; break;
		case gt_earth:  icon = "earthcache.png"; break;
		case gt_mega: icon = "453.png"; break;
		case gt_wherigo: icon = "1858.png"; break;
		default: icon = "8.png"; break;
	}

	xasprintf(&rb, "http://www.geocaching.com/images/kml/%s", icon);
	return rb;
}

static const
char *
kml_lookup_gc_container(const waypoint *waypointp)
{
	const char *cont;

	switch (waypointp->gc_data->container) {
		case gc_micro: cont="micro"; break;
		case gc_regular: cont="regular"; break;
		case gc_large: cont="large"; break;
		case gc_small: cont="small"; break;
		case gc_virtual: cont="virtual"; break;
		case gc_other: cont="other"; break;
		default: cont="not_chosen"; break;
	}

	return cont;
}

// Not thread safe.  Return strings are small and it's silly to xasprintf/free
// them so we use a static buffer.

char * kml_gc_mkstar(int rating)
{
	static char tmp[40];
	if (0 == rating % 10) {
		snprintf(tmp, sizeof(tmp), "stars%d", rating / 10);
	} else {
		snprintf(tmp, sizeof(tmp), "stars%d_%d", rating / 10, rating % 10);
	}

	return tmp;
}

static void kml_geocache_pr(const waypoint *waypointp)
{
	char *p, *is;

	kml_write_xml(1, "<Placemark>\n");

	kml_write_xml(1, "<name>\n");
	kml_write_xml(0, "<![CDATA[%s]]>\n", waypointp->url_link_text);
	kml_write_xml(-1, "</name>\n");

	// Timestamp
	kml_output_timestamp(waypointp);

	kml_write_xml(0, "<styleUrl>#geocache</styleUrl>\n");
	is = kml_lookup_gc_icon(waypointp);
	kml_write_xml(1, "<Style>\n");
	kml_write_xml(1, "<IconStyle>\n");
	kml_write_xml(1, "<Icon>\n");
	kml_write_xml(0, "<href>%s</href>\n", is);
	kml_write_xml(-1, "</Icon>\n");
	kml_write_xml(-1, "</IconStyle>\n");
	kml_write_xml(-1, "</Style>\n");

	kml_write_xml(1, "<ExtendedData>\n");

	if (waypointp->shortname) {
		p = xml_entitize(waypointp->shortname);
		kml_write_xml(0, "<Data name=\"gc_num\"><value>%s</value></Data>\n", p);
		xfree(p);
	}

	if (waypointp->url_link_text) {
		p = xml_entitize(waypointp->url_link_text);
		kml_write_xml(0, "<Data name=\"gc_name\"><value>%s</value></Data>\n", p);
		xfree(p);
	}

	if (waypointp->gc_data->placer) {
		p = xml_entitize(waypointp->gc_data->placer);
		kml_write_xml(0, "<Data name=\"gc_placer\"><value>%s</value></Data>\n", p);
		xfree(p);
	}

	kml_write_xml(0, "<Data name=\"gc_placer_id\"><value>%d</value></Data>\n", waypointp->gc_data->placer_id);

	kml_write_xml(0, "<Data name=\"gc_diff_stars\"><value>%s</value></Data>\n", kml_gc_mkstar(waypointp->gc_data->diff));
	kml_write_xml(0, "<Data name=\"gc_terr_stars\"><value>%s</value></Data>\n", kml_gc_mkstar(waypointp->gc_data->terr));

	kml_write_xml(0, "<Data name=\"gc_cont_icon\"><value>%s</value></Data>\n", kml_lookup_gc_container(waypointp));

 	 // Highlight any issues with the cache, such as temp unavail
	 // or archived.
	kml_write_xml(0, "<Data name=\"gc_issues\"><value>");
	if (waypointp->gc_data->is_archived == status_true) {
		kml_write_xml(0, "&lt;font color=\"red\"&gt;This cache has been archived.&lt;/font&gt;&lt;br/&gt;\n");
	} else if (waypointp->gc_data->is_available == status_false) {
		kml_write_xml(0, "&lt;font color=\"red\"&gt;This cache is temporarily unavailable.&lt;/font&gt;&lt;br/&gt;\n");
	}
	kml_write_xml(0, "</value></Data>\n");

	kml_write_xml(0, "<Data name=\"gc_type\"><value>%s</value></Data>\n", gs_get_cachetype(waypointp->gc_data->type));
	kml_write_xml(0, "<Data name=\"gc_short_desc\"><value><![CDATA[%s]]></value></Data>\n", waypointp->gc_data->desc_short.utfstring ? waypointp->gc_data->desc_short.utfstring : "");
	kml_write_xml(0, "<Data name=\"gc_long_desc\"><value><![CDATA[%s]]></value></Data>\n", waypointp->gc_data->desc_long.utfstring ? waypointp->gc_data->desc_long.utfstring : "");

	kml_write_xml(-1, "</ExtendedData>\n");

	// Location
	kml_write_xml(1, "<Point>\n");
	kml_write_coordinates(waypointp);

	kml_write_xml(-1, "</Point>\n");
	kml_write_xml(-1, "</Placemark>\n");

	xfree(is);
}

/*
 * WAYPOINTS
 */

static void kml_waypt_pr(const waypoint *waypointp)
{
	const char *icon;

#if 0 // Experimental
	if(realtime_positioning) {
		kml_write_xml(1, "<LookAt>\n");
		kml_write_xml(0, "<longitude>%f</longitude>\n", waypointp->longitude);
		kml_write_xml(0, "<latitude>%f</latitude>\n", waypointp->latitude);
		kml_write_xml(0, "<altitude>1000</altitude>\n");
		kml_write_xml(-1, "</LookAt>\n");
	}
#endif
	waypt_add_to_bounds(&kml_bounds, waypointp);
	kml_recompute_time_bounds(waypointp);

	if (waypointp->gc_data->diff && waypointp->gc_data->terr) {
		kml_geocache_pr(waypointp);
		return;
	}

	kml_write_xml(1, "<Placemark>\n");

	kml_write_xmle("name", waypointp->shortname);

	// Description
	if (waypointp->url && waypointp->url[0]) {
		char * odesc = xml_entitize(waypointp->url);
		kml_write_xml(0, "<Snippet/>\n");
		kml_write_xml(0, "<description>\n");
		if (waypointp->url_link_text && waypointp->url_link_text[0])  {
			char *olink = xml_entitize(waypointp->url_link_text);
			kml_write_xml(0, "<![CDATA[<a href=\"%s\">%s</a>]]>", odesc, olink);
			xfree(olink);
		} else {
			gbfputs(odesc, ofd);
		}

		kml_write_xml(0, "</description>\n");
		xfree(odesc);
	} else {
		if (strcmp(waypointp->shortname, waypointp->description))
			kml_write_xmle("description", waypointp->description);
	}

	// Timestamp
	kml_output_timestamp(waypointp);

	// Icon - but only if it looks like a URL.
	icon = opt_deficon ? opt_deficon : waypointp->icon_descr;
	if (icon && strstr(icon, "://")) {
		kml_write_xml(1, "<Style>\n");
		kml_write_xml(1, "<IconStyle>\n");
		kml_write_xml(1, "<Icon>\n");
		kml_write_xml(0, "<href>%s</href>\n", icon);
		kml_write_xml(-1, "</Icon>\n");
		kml_write_xml(-1, "</IconStyle>\n");
		kml_write_xml(-1, "</Style>\n");
	} else {
		kml_write_xml(0, "<styleUrl>#waypoint</styleUrl>\n");
	}

	// Location
	kml_write_xml(1, "<Point>\n");
	kml_output_positioning();
	kml_write_coordinates(waypointp);
	kml_write_xml(-1, "</Point>\n");

	kml_write_xml(-1, "</Placemark>\n");
}

/*
 * TRACKPOINTS
 */

static void kml_track_hdr(const route_head *header)
{
	computed_trkdata *td;
	track_recompute(header, &td);
	if (header->rte_waypt_ct > 0 && (export_lines || export_points)) {
		kml_output_header(header, td);
	}
	xfree(td);
}

static void kml_track_disp(const waypoint *waypointp)
{
	kml_output_point(waypointp, kmlpt_track);
}

static void kml_track_tlr(const route_head *header)
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
// Somewhat tortured to reduce duplication of iteration and formatting.
typedef enum {
  sl_unknown = 0,
  sl_char,
  sl_uchar,
  sl_int,
  sl_float,
  sl_double,
} sl_element;
static void kml_mt_simple_array(const route_head *header,
                                const char *name, const char *fmt_string,
                                int offset, sl_element type)
{
  queue *elem, *tmp;
  kml_write_xml(1, "<gx:SimpleArrayData name=\"%s\">\n", name);

  QUEUE_FOR_EACH(&header->waypoint_list, elem, tmp) {

    char *datap = (char*) elem + offset;

    switch(type) {
      case sl_char: {
        char data = *(char *) datap;
        kml_write_xmle("gx:value", fmt_string, data);
        }
        break;
      case sl_uchar: {
        unsigned char data = *(unsigned char *) datap;
        kml_write_xmle("gx:value", fmt_string, data);
        }
        break;
      case sl_int: {
        int data = *(int *) datap;
        kml_write_xmle("gx:value", fmt_string, data);
        }
        break;
      case sl_float: {
        float data = *(float *) datap;
        kml_write_xmle("gx:value", fmt_string, data);
        }
        break;
      case sl_double: {
        double data = *(double *) datap;
        kml_write_xmle("gx:value", fmt_string, data);
        }
        break;
      default:
        fatal(MYNAME ": invalid type passed to kml_mt_simple_array.\n");
    }
  }
  kml_write_xml(-1, "</gx:SimpleArrayData>\n");
}

// True if at least two points in the track have timestamps.
static int track_has_time(const route_head *header)
{
  queue *elem, *tmp;
  int points_with_time = 0;
  QUEUE_FOR_EACH(&header->waypoint_list, elem, tmp) {
    waypoint *tpt = (waypoint *)elem;

    if (tpt->creation_time) {
      points_with_time++;
      if (points_with_time >= 2)
        return 1;
    }
  }
  return 0;
}

// Simulate a track_disp_all callback sequence for a single track.
static void write_as_linestring(const route_head *header)
{
  queue *elem, *tmp;
  kml_track_hdr(header);
  QUEUE_FOR_EACH(&header->waypoint_list, elem, tmp) {
    waypoint *tpt = (waypoint *)elem;
    kml_track_disp(tpt);
  }
  kml_track_tlr(header);

}

static void kml_mt_hdr(const route_head *header)
{
  queue *elem, *tmp;
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

  kml_write_xml(1, "<Placemark>\n");
  kml_write_xmle("name", header->rte_name);
  kml_write_xml(0, "<styleUrl>#multiTrack</styleUrl>\n");
  kml_write_xml(1, "<gx:Track>\n");
  kml_output_positioning();

  QUEUE_FOR_EACH(&header->waypoint_list, elem, tmp) {
    char time_string[64];
    waypoint *tpt = (waypoint *)elem;

    // Add it to our bounding box so our default LookAt/flyto does a good
    // thing.
    waypt_add_to_bounds(&kml_bounds, tpt);
    if (tpt->creation_time) {
      xml_fill_in_time(time_string, tpt->creation_time, tpt->microseconds,
                       XML_LONG_TIME);
      if (time_string[0]) {
          kml_write_xmle("when", time_string);
      }
    } else {
      kml_write_xml(0, "<when />\n");
    }
  }

  // TODO: How to handle clamped, floating, extruded, etc.?
  QUEUE_FOR_EACH(&header->waypoint_list, elem, tmp) {
    waypoint *tpt = (waypoint *)elem;

    if (kml_altitude_known(tpt)) {
      kml_write_xmle("gx:coord", COORD_FORMAT " " COORD_FORMAT " " ALT_FORMAT,
                    tpt->longitude, tpt->latitude,tpt->altitude);
    } else {
      kml_write_xmle("gx:coord", COORD_FORMAT " " COORD_FORMAT,
                    tpt->longitude, tpt->latitude);
    }

    // Capture interesting traits to see if we need to do an ExtendedData
    // section later.
    if (tpt->cadence)
      has_cadence = 1;
    if (WAYPT_HAS(tpt, depth))
      has_depth = 1;
    if (tpt->heartrate)
      has_heartrate = 1;
    if (WAYPT_HAS(tpt, temperature))
      has_temperature = 1;
    if (tpt->power)
      has_power = 1;
  }

  if (has_cadence || has_depth || has_heartrate || has_temperature || 
      has_power) {
    kml_write_xml(1, "<ExtendedData>\n");
    kml_write_xml(1, "<SchemaData schemaUrl=\"#schema\">\n");

    if (has_cadence)
      kml_mt_simple_array(header, kmt_cadence, "%u",
                          offsetof(waypoint, cadence), sl_uchar);

    if (has_depth)
      kml_mt_simple_array(header, kmt_depth, "%.1f",
                          offsetof(waypoint, depth), sl_double);

    if (has_heartrate)
      kml_mt_simple_array(header, kmt_heartrate, "%u",
                          offsetof(waypoint, heartrate), sl_uchar);

    if (has_temperature)
      kml_mt_simple_array(header, kmt_temperature, "%.1f",
                          offsetof(waypoint, temperature), sl_float);

    if (has_power)
      kml_mt_simple_array(header, kmt_power, "%.1f",
                          offsetof(waypoint, power), sl_float);

    kml_write_xml(-1, "</SchemaData>\n");
    kml_write_xml(-1, "</ExtendedData>\n");
  }
}

static void kml_mt_tlr(const route_head *header)
{
  if (track_has_time(header)) {
    kml_write_xml(-1, "</gx:Track>\n");
    kml_write_xml(-1, "</Placemark>\n");
  }
}

/*
 * ROUTES
 */

static void kml_route_hdr(const route_head *header)
{
        kml_output_header(header, NULL);
}

static void kml_route_disp(const waypoint *waypointp)
{
        kml_output_point(waypointp, kmlpt_route);
}

static void kml_route_tlr(const route_head *header)
{
        kml_output_tailer(header);
}

// For Earth 5.0, we write a LookAt that encompasses
// the bounding box of our entire data set and set the event times
// to include all our data.
void kml_write_AbstractView(void) {
  double bb_size;

  kml_write_xml(1, "<LookAt>\n");

  if (kml_time_min || kml_time_max) {
    kml_write_xml(1, "<gx:TimeSpan>\n");
    if (kml_time_min) {
      char time_string[64];
      xml_fill_in_time(time_string, kml_time_min, 0, XML_LONG_TIME);
      if (time_string[0]) {
        kml_write_xml(0, "<begin>%s</begin>\n", time_string);
      }
    }
    if (kml_time_max) {
      char time_string[64];
      time_t time_max;
      // In realtime tracking mode, we fudge the end time by a few minutes
      // to ensure that the freshest data (our current location) is contained
      // within the timespan.   Earth's time may not match the GPS because
      // we may not be running NTP, plus it's polling a file (sigh) to read
      // the network position.  So we shove the end of the timespan out to
      // ensure the right edge of that time slider includes us.
      //
      time_max = realtime_positioning ? kml_time_max + 600 : kml_time_max;
      xml_fill_in_time(time_string, time_max, 0, XML_LONG_TIME);
      if (time_string[0]) {
        kml_write_xml(0, "<end>%s</end>\n", time_string);
      }
    }
    kml_write_xml(-1, "</gx:TimeSpan>\n");
  }

 // If our BB spans the antemeridian, flip sign on one.
 // This doesn't make our BB optimal, but it at least prevents us from
 // zooming to the wrong hemisphere.
 if (kml_bounds.min_lon * kml_bounds.max_lon < 0) {
   kml_bounds.min_lon = -kml_bounds.max_lon;
 }

  kml_write_xml(0, "<longitude>%f</longitude>\n",
                (kml_bounds.min_lon + kml_bounds.max_lon) / 2);
  kml_write_xml(0, "<latitude>%f</latitude>\n",
                (kml_bounds.min_lat + kml_bounds.max_lat) / 2);

  // It turns out the length of the diagonal of the bounding box gives us a
  // reasonable guess for setting the camera altitude.
  bb_size = gcgeodist(kml_bounds.min_lat, kml_bounds.min_lon,
                             kml_bounds.max_lat, kml_bounds.max_lon);
  // Clamp bottom zoom level.  Otherwise, a single point zooms to grass.
  if (bb_size < 1000) {
    bb_size = 1000;
  }
  kml_write_xml(0, "<range>%f</range>\n", bb_size * 1.3);

  kml_write_xml(-1, "</LookAt>\n");
}

static
void kml_mt_array_schema(const char *field_name, const char *display_name,
             const char *type)
{
	kml_write_xml(1, "<gx:SimpleArrayField name=\"%s\" type=\"%s\">\n",
				field_name, type);
	kml_write_xml(0, "  <displayName>%s</displayName>\n", display_name);
	kml_write_xml(-1, "</gx:SimpleArrayField>\n");
}

void kml_write(void)
{
	char import_time[100];
	time_t now;
	const global_trait* traits =  get_traits();

	// Parse options
	export_lines = (0 == strcmp("1", opt_export_lines));
	export_points = (0 == strcmp("1", opt_export_points));
	export_track = (0 ==  strcmp("1", opt_export_track));
	floating = (!! strcmp("0", opt_floating));
	extrude = (!! strcmp("0", opt_extrude));
	trackdata = (!! strcmp("0", opt_trackdata));
	trackdirection = (!! strcmp("0", opt_trackdirection));
	line_width = atol(opt_line_width);

	kml_write_xml(0, "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n");

	kml_write_xml(1, kml22_hdr);

	kml_write_xml(1, "<Document>\n");

	now = current_time();
	strftime(import_time, sizeof(import_time), "%c", localtime(&now));
	if (realtime_positioning)
		kml_write_xml(0, "<name>GPS position</name>\n");
	else
		kml_write_xml(0, "<name>GPS device</name>\n");

	if (now) {
		kml_write_xml(0, "<Snippet>Created %s</Snippet>\n", import_time);
	}

	// Style settings for bitmaps
	if (route_waypt_count()) {
		kml_write_bitmap_style(kmlpt_route, ICON_RTE, NULL);
	}

	if (track_waypt_count()) {
		if (trackdirection) {
		  int i;
	          kml_write_bitmap_style(kmlpt_other, ICON_TRK, "track-none");
		  for (i = 0; i < 16; i++) {
		    char buf1[100];
		    char buf2[100];

		    sprintf(buf1, "track-%d", i);
		    sprintf(buf2, ICON_DIR, i);
 		    kml_write_bitmap_style(kmlpt_other, buf2, buf1);
		  }
		} else {
		  kml_write_bitmap_style(kmlpt_track, ICON_TRK, NULL);
	        }
                if (export_track)
	          kml_write_bitmap_style(kmlpt_multitrack, ICON_MULTI_TRK,
                                         "track-none");
	}

	kml_write_bitmap_style(kmlpt_waypoint, ICON_WPT, NULL);

	if (track_waypt_count() || route_waypt_count()) {
		kml_write_xml(1, "<Style id=\"lineStyle\">\n");
		kml_output_linestyle(opt_line_color, line_width);
		kml_write_xml(-1, "</Style>\n");
	}

	if (traits->trait_geocaches) {
		kml_gc_make_ballonstyle();
	}

	if(traits->trait_heartrate ||
		traits->trait_cadence ||
		traits->trait_power ||
		traits->trait_temperature ||
		traits->trait_depth) {
		kml_write_xml(1, "<Schema id=\"schema\">\n");

		if(traits->trait_heartrate) {
			kml_mt_array_schema(kmt_heartrate, "Heart Rate", "int");
		}
		if(traits->trait_cadence) {
			kml_mt_array_schema(kmt_cadence, "Cadence", "int");
		}
		if(traits->trait_power) {
			kml_mt_array_schema(kmt_power, "Power", "float");
		}
		if(traits->trait_temperature) {
			kml_mt_array_schema(kmt_temperature, "Temperature", "float");
		}
		if(traits->trait_depth) {
			kml_mt_array_schema(kmt_depth, "Depth", "float");
		}
		kml_write_xml(-1, "</Schema>\n");
	}

        if (waypt_count()) {
		if (!realtime_positioning) {
			kml_write_xml(1, "<Folder>\n");
			kml_write_xml(0, "<name>Waypoints</name>\n");
		}

		waypt_disp_all(kml_waypt_pr);

		if (!realtime_positioning) {
			kml_write_xml(-1, "</Folder>\n");
		}
	}

	// Output trackpoints
	if (track_waypt_count()) {
		if (!realtime_positioning) {
			kml_write_xml(1,  "<Folder>\n");
			kml_write_xml(0,  "<name>Tracks</name>\n");
		}

		if (export_track)
			track_disp_all(kml_mt_hdr, kml_mt_tlr, NULL);

		track_disp_all(kml_track_hdr, kml_track_tlr,
			       kml_track_disp);

		if (!realtime_positioning) {
			kml_write_xml(-1,  "</Folder>\n");
		}
	}

	// Output routes
	if (route_waypt_count()) {
		if (!realtime_positioning) {
			kml_write_xml(1,  "<Folder>\n");
			kml_write_xml(0,  "<name>Routes</name>\n");

			route_disp_all(kml_route_hdr,
				kml_route_tlr, kml_route_disp);
			kml_write_xml(-1,  "</Folder>\n");
		}
        }

        kml_write_AbstractView();
	kml_write_xml(-1, "</Document>\n");
	kml_write_xml(-1, "</kml>\n");
}

/*
 * This depends on the table being sorted correctly.
 */
static const
char *
kml_get_posn_icon(int freshness)
{
	int i;
	int n_stations = sizeof(kml_tracking_icons) / sizeof(kml_tracking_icons[0]);

	for (i = 0; i < n_stations ; i++) {
		if (freshness >= kml_tracking_icons[i].freshness)
			return kml_tracking_icons[i].icon;
	}
	return ICON_NOSAT;
}


static route_head *posn_trk_head = NULL;

static void
kml_wr_position(waypoint *wpt)
{
	static time_t last_valid_fix;

	kml_wr_init(posnfilenametmp);

	if (!posn_trk_head) {
		posn_trk_head = route_head_alloc();
		track_add_head(posn_trk_head);
	}

	if (last_valid_fix == 0) last_valid_fix = current_time();

	/* We want our waypoint to have a name, but not our trackpoint */
	if (!wpt->shortname) {
		if (wpt->fix == fix_none) {
			wpt->shortname = xstrdup("ESTIMATED Position");
		} else {
			wpt->shortname = xstrdup("Position");
		}
	}

	switch (wpt->fix) {
		case fix_none:
			if (wpt->shortname) {
				xfree (wpt->shortname);
			}
			wpt->shortname = xstrdup("ESTIMATED Position");
			break;
		case fix_unknown:
			break;
		default:
			last_valid_fix = wpt->creation_time;
	}

	wpt->icon_descr = kml_get_posn_icon(wpt->creation_time - last_valid_fix);


	/* In order to avoid clutter while we're sitting still, don't add
 	   track points if we've not moved a minimum distance from the
	   beginnning of our accumulated track. */
	{
  	waypoint *newest_posn= (waypoint *) QUEUE_LAST(&posn_trk_head->waypoint_list);

	if(radtometers(gcdist(RAD(wpt->latitude), RAD(wpt->longitude),
		RAD(newest_posn->latitude), RAD(newest_posn->longitude))) > 50) {
		track_add_wpt(posn_trk_head, waypt_dupe(wpt));
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
		waypoint *tonuke = (waypoint *) QUEUE_FIRST(&posn_trk_head->waypoint_list);
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
	NULL,
	kml_args,
	CET_CHARSET_UTF8, 1,	/* CET-REVIEW */
	{ NULL, NULL, NULL, kml_wr_position_init, kml_wr_position, kml_wr_position_deinit }
};
