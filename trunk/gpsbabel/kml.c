/* 
	Support for Google Earth & Keyhole "kml" format.

	Copyright (C) 2005 Robert Lipe, robertlipe@usa.net
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

// options
static char *opt_deficon = NULL;
static char *opt_export_lines = NULL;
static char *opt_export_points = NULL;
static char *opt_line_width = NULL;
static char *opt_line_color = NULL;
static char *opt_floating = NULL;

static int export_lines;
static int export_points;
static int floating;

static waypoint *wpt_tmp;
static int wpt_tmp_queued;

static FILE *fd;
static FILE *ofd;

typedef struct {
  double latitude;
  double longitude;
  double altitude;
} point3d;

static int      point3d_list_len;
static point3d *point3d_list;

static
arglist_t kml_args[] = {
	{"deficon", &opt_deficon, "Default icon name", NULL, ARGTYPE_STRING },
	{"lines", &opt_export_lines, 
         "Export linestrings for tracks and routes",
         "1", ARGTYPE_BOOL },
	{"points", &opt_export_points, 
         "Export placemarks for tracks and routes",
         "1", ARGTYPE_BOOL },
	{"line_width", &opt_line_width, 
         "Width of lines, in pixels",
         "6", ARGTYPE_BOOL },
	{"line_color", &opt_line_color, 
         "Line color, specified in hex AABBGGRR",
         "64eeee17", ARGTYPE_BOOL },
	{"floating", &opt_floating, 
	 "Altitudes are absolute and not clamped to ground", 
	 "0", ARGTYPE_BOOL },
	{0, 0, 0, 0, 0}
};

#define MYNAME "kml"

#if NO_EXPAT
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
static xg_callback	wpt_name, wpt_desc, wpt_coord, trk_coord;

static 
xg_tag_mapping kml_map[] = {
	{ wpt_s, 	cb_start, 	"/Placemark" },
	{ wpt_e, 	cb_end, 	"/Placemark" },
	{ wpt_name, 	cb_cdata, 	"/Placemark/name" },
	{ wpt_desc, 	cb_cdata, 	"/Placemark/description" },
	{ wpt_coord, 	cb_cdata, 	"/Placemark/Point/coordinates" },
	{ trk_coord, 	cb_cdata, 	"/Placemark/MultiGeometry/LineString/coordinates" },
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

#if 0
void wpt_name_s(const char *args, const char **attrv)
{
	    const char **avp = &attrv[0];
	    while (*avp) {
	            if (0 == strcmp(avp[0], "id")) {
	                    wpt_tmp->shortname = xstrdup(avp[1]);
	            }
	            avp+=2;
	    }
}
#endif

void wpt_name(const char *args, const char **unused)
{
	if (args) wpt_tmp->shortname = xstrdup(args);
}

void wpt_desc(const char *args, const char **unused)
{
	if (args) wpt_tmp->description = xstrappend(wpt_tmp->description, args);
}

void wpt_coord(const char *args, const char **attrv)
{
	sscanf(args, "%lf,%lf,%lf", &wpt_tmp->longitude, &wpt_tmp->latitude, &wpt_tmp->altitude);
	wpt_tmp_queued = 1;
}

void trk_coord(const char *args, const char **attrv)
{
	int consumed = 0;
	double lat, lon, alt;
	waypoint *trkpt;

	route_head *trk_head = route_head_alloc();
	track_add_head(trk_head);
	
	while (3 == sscanf(args,"%lf,%lf,%lf %n", &lon, &lat, &alt, &consumed)){
		trkpt = waypt_new();	
		trkpt->latitude = lat;
		trkpt->longitude = lon;
		trkpt->altitude = alt;

		route_add_wpt(trk_head, trkpt);

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
	ofd = xfopen(fname, "w", MYNAME);
}

static void
kml_wr_deinit(void)
{
	fclose(ofd);
}

static void kml_write_bitmap_style(const char *style, int bitmap, 
	                               int x, int y, int width, int height)
{
	fprintf(ofd, "<Style id=\"%s\">\n", style);
	fprintf(ofd, "<icon xlink:href=\"root://icons/bitmap-%d.png?x=%d&amp;y=%d&amp;w=%d&amp;h=%d\">\n",
	      bitmap, x, y, width, height);
	fprintf(ofd, "  root://icons/bitmap-%d.png?x=%d&amp;y=%d&amp;w=%d&amp;h=%d\n",
	      bitmap, x, y, width, height);
	fprintf(ofd, "</icon>\n");
	fprintf(ofd, "</Style>\n");
}  


static void kml_output_timestamp(const waypoint *waypointp)
{
	if (waypointp->creation_time) {
	fprintf(ofd, "\t  <TimeInstant>\n");
	fprintf(ofd, "\t    ");
		xml_write_time(ofd, waypointp->creation_time, "timePosition");
	fprintf(ofd, "\t  </TimeInstant>\n");
	}
}


static void kml_output_header(const route_head *header)
{
        fprintf(ofd, "<Folder>\n");
	fprintf(ofd, "  <visibility>1</visibility>\n");
	write_optional_xml_entity(ofd, "  ", "name", header->rte_name);
	write_optional_xml_entity(ofd, "  ", "desc", header->rte_desc);

        if (export_points && header->rte_waypt_ct > 0) {
          // Put the points in a subfolder
          fprintf(ofd, "  <Folder>\n");
          fprintf(ofd, "    <visibility>1</visibility>\n");
          fprintf(ofd, "    <name>Points</name>\n");
        }

        // Create an array for holding waypoint coordinates so that we
        // can produce a LineString at the end.
        point3d_list = (point3d *) xmalloc(header->rte_waypt_ct * sizeof(point3d));
        point3d_list_len = 0;
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
	fprintf(ofd, "\t<Placemark>\n");
	fprintf(ofd, "\t  <styleUrl>%s</styleUrl>\n", style);
	fprintf(ofd, "\t  <Point>\n");
        if (floating) {
          fprintf(ofd, "\t    <altitudeMode>absolute</altitudeMode>\n");
        }
	fprintf(ofd, "\t    <coordinates>%f,%f,%f</coordinates>\n",
		pt->longitude, pt->latitude, pt->altitude);
	fprintf(ofd, "\t  </Point>\n");

	// Timestamp
	kml_output_timestamp(waypointp);

	fprintf(ofd, "\t</Placemark>\n");
  }
}


static void kml_output_tailer(const route_head *header)
{
  int i;

  if (export_points && point3d_list_len > 0) {
    fprintf(ofd, "  </Folder>\n");
  }
  
  // Add a linestring for this track?
  if (export_lines && point3d_list_len > 0) {
    fprintf(ofd, "\t<Placemark>\n");
    fprintf(ofd, "\t  <styleUrl>#lineStyle</styleUrl>\n");
    fprintf(ofd, "\t  <name>Path</name>\n");
    fprintf(ofd, "\t  <MultiGeometry>\n");
    fprintf(ofd, "\t    <LineString>\n");
    if (floating) {
      fprintf(ofd, "\t      <altitudeMode>absolute</altitudeMode>\n");
    }
    fprintf(ofd, "\t      <coordinates>\n");
    for (i = 0; i < point3d_list_len; ++i)
      fprintf(ofd, "%f,%f,%f ", 
              point3d_list[i].longitude,
              point3d_list[i].latitude,
              point3d_list[i].altitude);
    
    fprintf(ofd, "\n\t      </coordinates>\n");
    fprintf(ofd, "\t    </LineString>\n");
    fprintf(ofd, "\t  </MultiGeometry>\n");
    fprintf(ofd, "\t</Placemark>\n");
  }
  
  xfree(point3d_list);
  point3d_list = NULL;
  
  fprintf(ofd, "</Folder>\n");
}

/*
 * WAYPOINTS
 */

static void kml_waypt_pr(const waypoint *waypointp)
{
	fprintf(ofd, "\t<Placemark>\n");
	write_optional_xml_entity(ofd, "\t", "name", waypointp->shortname);
	fprintf(ofd, "\t  <styleUrl>#waypoint</styleUrl>\n");

	// Description
	if (waypointp->url) {
		char * odesc = xml_entitize(waypointp->url);
		fprintf(ofd, "\t  <description>\n");
		if (waypointp->url_link_text)  {
			char *olink = xml_entitize(waypointp->url_link_text);
			fprintf(ofd, "<a href=\"%s\">%s</a>", odesc, olink);
			xfree(olink);
		}
		else
			fputs(odesc, ofd);
		fprintf(ofd, "\n\t</description>\n");
		xfree(odesc);
	}

	// Location
	fprintf(ofd, "\t  <Point>\n");
	fprintf(ofd, "\t\t<coordinates>%f,%f,%f</coordinates>\n",
		waypointp->longitude, waypointp->latitude, 
		waypointp->altitude == unknown_alt ? 0.0 : waypointp->altitude);
	fprintf(ofd, "\t  </Point>\n");

	// Timestamp
	kml_output_timestamp(waypointp);

	fprintf(ofd, "\t</Placemark>\n");
}

/*
 * TRACKPOINTS
 */

static void kml_track_hdr(const route_head *header) 
{
	kml_output_header(header);
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
        kml_output_header(header);
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
  // Parse options
  export_lines = (0 == strcmp("1", opt_export_lines));
  export_points = (0 == strcmp("1", opt_export_points));
  floating = (!! strcmp("0", opt_floating));

	fprintf(ofd, "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n");
	fprintf(ofd, "<Document xmlns:xlink=\"http://www.w3/org/1999/xlink\">\n");
	// TODO(akirmse): Put in device name, maybe time?
	fprintf(ofd, "<name>GPS device</name>\n");
	fprintf(ofd, "<visibility>1</visibility>\n");

	// Style settings for bitmaps
	kml_write_bitmap_style("track", 4, 128, 0, 32, 32);
	kml_write_bitmap_style("waypoint", 4, 160, 0, 32, 32);
	kml_write_bitmap_style("route", 4, 160, 0, 32, 32);
        
        // Style settings for line strings
        fprintf(ofd, "<Style id=\"lineStyle\">\n");
        fprintf(ofd, "  <LineStyle>\n");
        fprintf(ofd, "    <color>%s</color>\n", opt_line_color);
        fprintf(ofd, "    <width>%s</width>\n", opt_line_width);
        fprintf(ofd, "  </LineStyle>\n");
        fprintf(ofd, "</Style>\n");

	fprintf(ofd, "<Folder>\n");
	fprintf(ofd, "<name>Waypoints</name>\n");
	fprintf(ofd, "<visibility>1</visibility>\n");

	waypt_disp_all(kml_waypt_pr);

	fprintf(ofd, "</Folder>\n");

	// Output trackpoints
	fprintf(ofd, "<Folder>\n");
	fprintf(ofd, "<name>Tracks</name>\n");
	fprintf(ofd, "<visibility>1</visibility>\n");
	track_disp_all(kml_track_hdr, kml_track_tlr, kml_track_disp);
	fprintf(ofd, "</Folder>\n");
  
	// Output routes
	fprintf(ofd, "<Folder>\n");
	fprintf(ofd, "<name>Routes</name>\n");
	fprintf(ofd, "<visibility>1</visibility>\n");
	route_disp_all(kml_route_hdr, kml_route_tlr, kml_route_disp);
	fprintf(ofd, "</Folder>\n");

	fprintf(ofd, "</Document>\n");
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
	CET_CHARSET_UTF8, 1	/* CET-REVIEW */
};
