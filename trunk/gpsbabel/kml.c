/* 
	Support for Google Earth & Keyhole "kml" format.

	Copyright (C) 2005, 2006, 2007 Robert Lipe, robertlipe@usa.net
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

#ifdef __WIN32__
# include <windows.h>
#endif

// options
static char *opt_deficon = NULL;
static char *opt_export_lines = NULL;
static char *opt_export_points = NULL;
static char *opt_line_width = NULL;
static char *opt_line_color = NULL;
static char *opt_floating = NULL;
static char *opt_extrude = NULL;
static char *opt_trackdata = NULL;
static char *opt_units = NULL;
static char *opt_labels = NULL;
static char *opt_max_position_points = NULL;

static int export_lines;
static int export_points;
static int floating;
static int extrude;
static int trackdata;
static int max_position_points;

static int indent_level;

static waypoint *wpt_tmp;
static int wpt_tmp_queued;
static const char *posnfilename;
static char *posnfilenametmp;

static FILE *ofd;

typedef struct {
  double latitude;
  double longitude;
  double altitude;
} point3d;

static int      point3d_list_len;
static point3d *point3d_list;
static int realtime_positioning;
static int do_indentation = 1;

#define TD(FMT,DATA) kml_write_xml(0, "<tr><td>" FMT " </td></tr>\n", DATA)
#define TD2(FMT,DATA, DATA2) kml_write_xml(0, "<tr><td>" FMT " </td></tr>\n", DATA, DATA2)

static const char kml22_hdr[] =  
	"<kml xmlns=\"http://earth.google.com/kml/2.2\"\n"
	"\txmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\">\n";
// 	No "baked in" schemaLocation, per Google recommendation.
//	"\txsi:schemaLocation=\"http://earth.google.com/kml/2.2 \n"
//	"\thttp://code.google.com/apis/kml/schema/kml22beta.xsd\">\n";
	
static const char kml21_hdr[] =  
	"<kml xmlns=\"http://earth.google.com/kml/2.1\"\n"
	"\txmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\">\n";
// 	No "baked in" schemaLocation, per Google recommendation.
//	"\txsi:schemaLocation=\"http://earth.google.com/kml/2.1 \n"
//	"\thttp://code.google.com/apis/kml/schema/kml21.xsd\">\n";

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
         "64eeee17", ARGTYPE_STRING, ARG_NOMINMAX },
	{"floating", &opt_floating, 
	 "Altitudes are absolute and not clamped to ground", 
	 "0", ARGTYPE_BOOL, ARG_NOMINMAX },
	{"extrude", &opt_extrude, 
	 "Draw extrusion line from trackpoint to ground", 
	 "0", ARGTYPE_BOOL, ARG_NOMINMAX },
	{"trackdata", &opt_trackdata, 
	 "Include extended data for trackpoints (default = 1)", 
	 "1", ARGTYPE_BOOL, ARG_NOMINMAX },
	{"units", &opt_units, 
	 "Units used when writing comments ('s'tatute or 'm'etric)", 
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
	char *icon;
} kml_tracking_icons[] = {
 { 60, "http://maps.google.com/mapfiles/kml/pal4/icon15.png" }, // Red
 { 30, "http://maps.google.com/mapfiles/kml/pal4/icon31.png" }, // Yellow
 { 0, "http://maps.google.com/mapfiles/kml/pal4/icon62.png" }, // Green
};

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

static xg_callback	wpt_s, wpt_e;
static xg_callback	wpt_name, wpt_desc, wpt_coord, wpt_icon, trk_coord;

static 
xg_tag_mapping kml_map[] = {
	{ wpt_s, 	cb_start, 	"/Placemark" },
	{ wpt_e, 	cb_end, 	"/Placemark" },
	{ wpt_name, 	cb_cdata, 	"/Placemark/name" },
	{ wpt_desc, 	cb_cdata, 	"/Placemark/description" },
	{ wpt_coord, 	cb_cdata, 	"/Placemark/Point/coordinates" },
	{ wpt_icon, 	cb_cdata, 	"/Placemark/Style/Icon/href" },
	{ trk_coord, 	cb_cdata, 	"/Placemark/MultiGeometry/LineString/coordinates" },
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

void wpt_coord(const char *args, const char **attrv)
{
	sscanf(args, "%lf,%lf,%lf", &wpt_tmp->longitude, &wpt_tmp->latitude, &wpt_tmp->altitude);
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

	route_head *trk_head = route_head_alloc();
	if (wpt_tmp->shortname) {
		trk_head->rte_name  = xstrdup(wpt_tmp->shortname);
	}
	track_add_head(trk_head);
	
	while (3 == sscanf(args, "%lf,%lf,%lf %n", &lon, &lat, &alt, &consumed)){
		trkpt = waypt_new();	
		trkpt->latitude = lat;
		trkpt->longitude = lon;
		trkpt->altitude = alt;

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

	if (opt_units) {
		u = tolower(opt_units[0]);
	} 

	switch(u) {
		case 's': fmt_setunits(units_statute); break;
		case 'm': fmt_setunits(units_metric); break;
		default: fatal("Units argument '%s' should be 's' for statute units or 'm' for metric.", opt_units); break;
	}
	/*
	 * Reduce race conditions with network read link.
	 */	
	ofd = xfopen(fname, "w", MYNAME);
}

/* 
 * The magic here is to try to ensure that posnfilename is atomically
 * updated.
 */
static void
kml_wr_position_init(const char *fname)
{
	posnfilename = fname;;
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
	fclose(ofd);

	if (posnfilenametmp) {
#if __WIN32__
		MoveFileEx(posnfilenametmp, posnfilename,
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
			fputs("  ", ofd);
		}
	}

	vfprintf(ofd, fmt, args);

	if (indent > 0) indent_level++;

	va_end(args);
}

/*
 * Write an optional tag with a value that may need to be entity escaped.
 * Never changes indention leve, but does honour it.
 */
static void
kml_write_xmle(const char *tag, const char *v)
{
	int i;
	if (v && *v) {
		char *tmp_ent = xml_entitize(v);
		for (i = 0; i < indent_level; i++) {
			fputs("  ", ofd);
		}
		fprintf(ofd, "<%s>%s</%s>\n",tag, tmp_ent, tag);
		xfree(tmp_ent);
	}
}

#define hovertag(h) h ? 'h' : 'n'
static void kml_write_bitmap_style_(const char *style, const char * bitmap, 
				       int highlighted)
{
	kml_write_xml(0, "<!-- %s %s style -->\n", 
		highlighted ? "Highlighted" : "Normal", style);
	kml_write_xml(1, "<Style id=\"%s_%c\">\n", style, hovertag(highlighted));
	kml_write_xml(1, "<IconStyle>\n");
	if (highlighted) {
		kml_write_xml(0, "<scale>1.2</scale>\n");
	}
	kml_write_xml(1, "<Icon>\n");
	kml_write_xml(0, "<href>%s</href>\n", bitmap);
	kml_write_xml(-1, "</Icon>\n");
	kml_write_xml(-1, "</IconStyle>\n");
	kml_write_xml(-1, "</Style>\n");
}  

/* A wrapper for the above function to emit both a highlighted
 * and non-highlighted version of the style to allow the icons
 * to magnify slightly on a rollover.
 */
static void kml_write_bitmap_style(const char *style, const char *bitmap)
{
	kml_write_bitmap_style_(style, bitmap, 0);
	kml_write_bitmap_style_(style, bitmap, 1);

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

	max_alt = fmt_distance(td->max_alt, &max_alt_units);
	min_alt = fmt_distance(td->min_alt, &min_alt_units);
	distance = fmt_distance(td->distance_meters, &distance_units);

	kml_write_xml(0, "<Snippet/>\n");

	kml_write_xml(1, "<description>\n");
	kml_write_xml(1, "<![CDATA[<table>\n");

	if (header->rte_desc) {
		TD("<b>Description</b> %s", header->rte_desc);
	}
	TD2("<b>Distance</b> %.1f %s", distance, distance_units);
	if (min_alt != unknown_alt) {
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
		TD("<b>Start Time:</b> %s ", time_string);
		xml_fill_in_time(time_string, td->end, 0, XML_LONG_TIME);
		TD("<b>End Time:</b> %s ", time_string);
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

        // Create an array for holding waypoint coordinates so that we
        // can produce a LineString at the end.
        point3d_list = (point3d *) xmalloc(header->rte_waypt_ct * sizeof(point3d));
        point3d_list_len = 0;
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

/* Output something interesing when we can for route and trackpoints */
static void kml_output_description(const waypoint *pt)
{
	char *alt_units;
	double alt;

	if (!trackdata) {
		return;
	}

	alt = fmt_distance(pt->altitude, &alt_units);

	kml_write_xml(1, "<description><![CDATA[\n");
	kml_write_xml(1, "<table>\n");

	TD("Longitude: %f", pt->longitude);
	TD("Latitude: %f", pt->latitude);
	if (pt->altitude != unknown_alt) TD2("Altitude: %.3f %s", alt, alt_units);
	if (pt->heartrate) TD("Heart rate: %d", pt->heartrate);
	if (pt->cadence) TD("Cadence: %d", pt->cadence);
	if WAYPT_HAS(pt, temperature) TD("Temperature: %.1f", pt->temperature);
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

static void kml_output_point(const waypoint *waypointp, const char *style)
{
  // Save off this point for later use
  point3d *pt = &point3d_list[point3d_list_len];
  point3d_list_len++;

  pt->longitude = waypointp->longitude;
  pt->latitude = waypointp->latitude;
  pt->altitude = waypointp->altitude == unknown_alt ? 0.0 : waypointp->altitude;

  if (export_points) {
	kml_write_xml(1, "<Placemark>\n");
	if (atoi(opt_labels)) {
		kml_write_xmle("name", waypointp->shortname);
	}
	kml_write_xml(0, "<Snippet/>\n");
	kml_output_description(waypointp);
	kml_output_lookat(waypointp);
	kml_output_timestamp(waypointp);
	kml_write_xml(0, "<styleUrl>%s</styleUrl>\n", style);
#if 0
	// If we were to try to spin track icon to indication direction
	// of motion, it might look something like this.  Unfortunately,
	// doing that causes a huge performance problem in Google Earth.
	kml_write_xml(0, "<Style><IconStyle><heading>%f</heading></IconStyle></Style>\n", 360 - waypointp->course);
#endif
	kml_write_xml(1, "<Point>\n");
        if (floating) {
		kml_write_xml(0, "<altitudeMode>absolute</altitudeMode>\n");
        }
	if (extrude) {
		kml_write_xml(0, "<extrude>1</extrude>\n");
	}
	kml_write_xml(0, "<coordinates>%f,%f,%f</coordinates>\n",
		pt->longitude, pt->latitude, pt->altitude);
	kml_write_xml(-1, "</Point>\n");


	kml_write_xml(-1, "</Placemark>\n");
  }
}


static void kml_output_tailer(const route_head *header)
{
  int i;

  if (export_points && point3d_list_len > 0) {
    kml_write_xml(-1, "</Folder>\n");
  }
  
  // Add a linestring for this track?
  if (export_lines && point3d_list_len > 0) {
    kml_write_xml(1, "<Placemark>\n");
    kml_write_xml(0, "<name>Path</name>\n");
    kml_write_xml(0, "<styleUrl>#lineStyle</styleUrl>\n");
    kml_write_xml(1, "<LineString>\n");
    if (floating) {
      kml_write_xml(0, "<altitudeMode>absolute</altitudeMode>\n");
    }
    if (extrude) {
      kml_write_xml(0, "<extrude>1</extrude>\n");
    }
    kml_write_xml(0, "<tessellate>1</tessellate>\n");
    kml_write_xml(1, "<coordinates>\n");
    for (i = 0; i < point3d_list_len; ++i)
      kml_write_xml(0, "%f,%f,%f\n", 
              point3d_list[i].longitude,
              point3d_list[i].latitude,
              point3d_list[i].altitude);
    
    kml_write_xml(-1, "</coordinates>\n");
    kml_write_xml(-1, "</LineString>\n");
    kml_write_xml(-1, "</Placemark>\n");
  }
  
  xfree(point3d_list);
  point3d_list = NULL;
  
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
	switch (waypointp->gc_data.type) {
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
		case gt_mega: icon = "6.png"; break; // No unique icon yet.
		default: icon = "8.png"; break;
	}

	xasprintf(&rb, "http://www.geocaching.com/images/kml/%s", icon);
	return rb;
}

static 
char *
kml_lookup_gc_container(const waypoint *waypointp)
{
	char *cont;

	switch (waypointp->gc_data.container) {
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
	kml_write_xml(0, "<Data name=\"gc_num\"><value>%s</value></Data>\n", waypointp->shortname);

	p = xml_entitize(waypointp->url_link_text);
	kml_write_xml(0, "<Data name=\"gc_name\"><value>%s</value></Data>\n", p);
	xfree(p);

	p = xml_entitize(waypointp->gc_data.placer);
	kml_write_xml(0, "<Data name=\"gc_placer\"><value>%s</value></Data>\n", p);
	xfree(p);

	kml_write_xml(0, "<Data name=\"gc_placer_id\"><value>%d</value></Data>\n", waypointp->gc_data.placer_id);

	kml_write_xml(0, "<Data name=\"gc_diff_stars\"><value>%s</value></Data>\n", kml_gc_mkstar(waypointp->gc_data.diff));
	kml_write_xml(0, "<Data name=\"gc_terr_stars\"><value>%s</value></Data>\n", kml_gc_mkstar(waypointp->gc_data.terr));

	kml_write_xml(0, "<Data name=\"gc_cont_icon\"><value>%s</value></Data>\n", kml_lookup_gc_container(waypointp));

 	 // Highlight any issues with the cache, such as temp unavail 
	 // or archived.
	kml_write_xml(0, "<Data name=\"gc_issues\"><value>");
	if (waypointp->gc_data.is_archived == 1) {
		kml_write_xml(0, "&lt;font color=\"red\"&gt;This cache has been archived.&lt;/font&gt;<br/>\n");
	} else if (waypointp->gc_data.is_available == 0) {
		kml_write_xml(0, "&lt;font color=\"red\"&gt;This cache is temporarily unavailable.&lt;/font&gt;<br/>\n");
	}
	kml_write_xml(0, "</value></Data>\n");

	kml_write_xml(0, "<Data name=\"gc_type\"><value>%s</value></Data>\n", gs_get_cachetype(waypointp->gc_data.type));
	kml_write_xml(0, "<Data name=\"gc_short_desc\"><value><![CDATA[%s]]></value></Data>\n", waypointp->gc_data.desc_short.utfstring ? waypointp->gc_data.desc_short.utfstring : "");
	kml_write_xml(0, "<Data name=\"gc_long_desc\"><value><![CDATA[%s]]></value></Data>\n", waypointp->gc_data.desc_long.utfstring ? waypointp->gc_data.desc_long.utfstring : "");
	
	kml_write_xml(-1, "</ExtendedData>\n");

	// Location
	double lat = waypointp->latitude;;
	double lng = waypointp->longitude;
// optionally "fuzz" lat/lng here.
	kml_write_xml(1, "<Point>\n");
	kml_write_xml(0, "<coordinates>%f,%f,%f</coordinates>\n",
		lng, lat,
		waypointp->altitude == unknown_alt ? 0.0 : waypointp->altitude);

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

	if (waypointp->gc_data.diff && waypointp->gc_data.terr) {
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
			fputs(odesc, ofd);
		}

		kml_write_xml(0, "</description>\n");
		xfree(odesc);
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

	if (extrude) {
		kml_write_xml(0, "<extrude>1</extrude>\n");
	}

        if (floating) {
		kml_write_xml(0, "<altitudeMode>absolute</altitudeMode>\n");
        }

	kml_write_xml(0, "<coordinates>%f,%f,%f</coordinates>\n",
		waypointp->longitude, waypointp->latitude, 
		waypointp->altitude == unknown_alt ? 0.0 : waypointp->altitude);
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
	kml_output_header(header, td);
	xfree(td);
}

static void kml_track_disp(const waypoint *waypointp)
{
	kml_output_point(waypointp, "#track");
}

static void kml_track_tlr(const route_head *header) 
{
	kml_output_tailer(header);
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
        kml_output_point(waypointp, "#route");
}

static void kml_route_tlr(const route_head *header) 
{
        kml_output_tailer(header);
}

void kml_write(void)
{
	char import_time[100];
	time_t now;

	// Parse options
	export_lines = (0 == strcmp("1", opt_export_lines));
	export_points = (0 == strcmp("1", opt_export_points));
	floating = (!! strcmp("0", opt_floating));
	extrude = (!! strcmp("0", opt_extrude));
	trackdata = (!! strcmp("0", opt_trackdata));

	kml_write_xml(0, "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n");

	/*
	 * This is a bit cowardly.   Our geocache writer takes advantage
	 * of KML 2.2 features present only in Earth 4.2 and newer.  The
	 * output is actually compatible with Earth versions back to 4.0
	 * for the non-geocaching case, so we'll be conservative and not 
	 * output the new namespace if we don't need it. 
	 */
	if (geocaches_present) {
		kml_write_xml(1, kml22_hdr);
	} else  {
		kml_write_xml(1, kml21_hdr);
	}

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
		kml_write_bitmap_style("route", "http://maps.google.com/mapfiles/kml/pal4/icon61.png");
	}

	if (track_waypt_count()) {
		kml_write_bitmap_style("track", "http://maps.google.com/mapfiles/kml/pal4/icon60.png");
	}

	kml_write_bitmap_style("waypoint", "http://maps.google.com/mapfiles/kml/pal4/icon61.png");
        
	if (track_waypt_count() || route_waypt_count()) {
		// Style settings for line strings
		kml_write_xml(1, "<Style id=\"lineStyle\">\n");
		kml_write_xml(1, "<LineStyle>\n");
		kml_write_xml(0, "<color>%s</color>\n", opt_line_color);
		kml_write_xml(0, "<width>%s</width>\n", opt_line_width);
		kml_write_xml(-1, "</LineStyle>\n");
		kml_write_xml(-1, "</Style>\n");
	}

	if (geocaches_present) {
		kml_gc_make_ballonstyle();
	}

	if (!realtime_positioning) {
		kml_write_xml(1, "<Folder>\n");
		kml_write_xml(0, "<name>Waypoints</name>\n");
	}

	waypt_disp_all(kml_waypt_pr);

	if (!realtime_positioning) {
		kml_write_xml(-1, "</Folder>\n");
	}

	// Output trackpoints
	if (track_waypt_count()) {
		if (!realtime_positioning) {
			kml_write_xml(1,  "<Folder>\n");
			kml_write_xml(0,  "<name>Tracks</name>\n");
		}

		track_disp_all(kml_track_hdr, kml_track_tlr, kml_track_disp);

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

	kml_write_xml(-1, "</Document>\n");
	kml_write_xml(-1, "</kml>\n");
}

/*
 * This depends on the table being sorted correctly.
 */
static 
char *
kml_get_posn_icon(int freshness)
{
	int i;
	for (i = 0; i < sizeof(kml_tracking_icons) / sizeof(kml_tracking_icons[0]); i++) {
		if (freshness >= kml_tracking_icons[i].freshness)
			return kml_tracking_icons[i].icon;
	}
	return "http://maps.google.com/mapfiles/kml/pal3/icon59.png";
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

	track_add_wpt(posn_trk_head, waypt_dupe(wpt));

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
