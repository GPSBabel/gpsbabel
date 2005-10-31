/*
    Access to  U.S. Census Bureau "tiger" format.

    Copyright (C) 2002 Robert Lipe, robertlipe@usa.net

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
#include "csv_util.h"

static FILE *file_in;
static FILE *file_out;
static short_handle mkshort_handle;
static short_handle mkshort_whandle;

#define MYNAME "GPSUTIL"

static double maxlat, maxlon, minlat, minlon;
static int rec_cnt;
static char *nolabels = NULL;
static char *genurl = NULL;
static char *suppresswhite = NULL;
static char *iconismarker = NULL;
static char *snlen = NULL;

static char *margin  = NULL; 
static char *xpixels = NULL; 
static char *ypixels = NULL;
static char *oldthresh = NULL;
static char *oldmarker  = NULL;
static char *newmarker  = NULL;
static char *unfoundmarker  = NULL;

static int scalev;
static int short_length;
static double thresh_days;

/*
 *   The code bracketed by CLICKMAP is to generate clickable image maps
 *   for a web browser.   It's functional, but is missing the math to do
 *   the projection transformations.   Some trig geek can finish that.
 */
#if CLICKMAP
static FILE *linkf;
static char *clickmap = NULL;
#endif


static
arglist_t tiger_args[] = {
	{"nolabels", &nolabels, "Suppress labels on generated pins.",
		NULL, ARGTYPE_BOOL },
	{"genurl", &genurl, "Generate file with lat/lon for centering map.",
		NULL, ARGTYPE_OUTFILE },
	{"margin", &margin, "Margin for map.  Degrees or percentage.",
		"15%", ARGTYPE_FLOAT},
	{"snlen", &snlen, "Max shortname length when used with -s.",
		"10", ARGTYPE_INT, "1", NULL},
	{"oldthresh", &oldthresh, 
		"Days after which points are considered old.",
		"14", ARGTYPE_INT},
	{"oldmarker", &oldmarker, "Marker type for old points.",
		"redpin", ARGTYPE_STRING},
	{"newmarker", &newmarker, "Marker type for new points.",
		"greenpin", ARGTYPE_STRING},
	{"suppresswhite", &suppresswhite,
		"Suppress whitespace in generated shortnames", 
		NULL, ARGTYPE_BOOL },
	{"unfoundmarker", &unfoundmarker, "Marker type for unfound points.",
		"bluepin", ARGTYPE_STRING},
	{"xpixels", &xpixels, "Width in pixels of map.",
		"768", ARGTYPE_INT},
	{"ypixels", &ypixels, "Height in pixels of map.",
		"768", ARGTYPE_INT},
	{"iconismarker", &iconismarker,
		"The icon description is already the marker", NULL,
		ARGTYPE_BOOL },
#if CLICKMAP
	{"clickmap", &clickmap, "Generate Clickable map web page.",
		NULL, ARGTYPE_BOOL},
#endif
	{0, 0, 0, 0, 0}
};


static void
rd_init(const char *fname)
{
	file_in = xfopen(fname, "r", MYNAME);
	mkshort_handle = mkshort_new_handle();
}

static void
rd_deinit(void)
{
	fclose(file_in);
	mkshort_del_handle(&mkshort_handle);
}

static void
wr_init(const char *fname)
{
	file_out = xfopen(fname, "w", MYNAME);
	thresh_days = strtod(oldthresh, NULL);
}

static void
wr_deinit(void)
{
	fclose(file_out);
}

static void
data_read(void)
{
	double lat,lon;
	char desc[100];
	char icon[100];
	char ibuf[1024];
	waypoint *wpt_tmp;

	while (fgets(ibuf, sizeof(ibuf), file_in)) {
		if( sscanf(ibuf, "%lf,%lf:%100[^:]:%100[^\n]", 
				&lon, &lat, icon, desc)) {
			wpt_tmp = waypt_new();

			wpt_tmp->longitude = lon;
			wpt_tmp->latitude = lat;
			wpt_tmp->description = xstrdup(desc);
			wpt_tmp->shortname = mkshort(mkshort_handle, desc);

			waypt_add(wpt_tmp);
		}
	}
}

static void
tiger_disp(const waypoint *wpt)
{
	const char *pin;
	double lat = wpt->latitude;
	double lon = wpt->longitude;

	if (iconismarker)
		pin = wpt->icon_descr ? wpt->icon_descr : "";
	else if (wpt->icon_descr && strstr(wpt->icon_descr, "-unfound"))
		pin = unfoundmarker;
	else if (wpt->creation_time > current_time() - 3600 * 24 * thresh_days)
		pin = newmarker;
	else
		pin = oldmarker;

	if (genurl) {
		if (lat > maxlat) maxlat = lat;
		if (lon > maxlon) maxlon = lon;
		if (lat < minlat) minlat = lat;
		if (lon < minlon) minlon = lon;
	}

	fprintf(file_out, "%f,%f:%s", lon, lat, pin);
	if (!nolabels) {
		char *temp = NULL;
		char *desc = csv_stringclean(wpt->description, ":");
		if (global_opts.synthesize_shortnames)
		{
			temp = desc;
			desc = mkshort(mkshort_whandle, desc);
		}
		fprintf(file_out, ":%s", desc);
		if (temp != NULL) desc = temp;
		xfree(desc);
	}
	fprintf(file_out, "\n");
}

#if CLICKMAP
static void
map_plot(const waypoint *wpt)
{
	static int x,y;

	/* Replace with real math. */
	x+=10;
	y+=10;

	fprintf(linkf, "<area shape=\"circle\" coords=\"%d,%d,7\" href=\"%s\" alt=\"%s\"\n", x, y, wpt->url, wpt->description);
}
#endif /* CLICKMAP */

static double
dscale(double distance)
{
	/*
	 * If we have any specified margin options  factor those in now.  
	 * A additional little boundary is helpful because Tiger always 
	 * puts the pin above the actual coord and if we don't pad the 
	 * top will be clipped.   It also makes the maps more useful to 
	 * have a little bit of context around the pins on the border.
	 */

	if (strchr(margin, '%'))
		return distance + strtod(margin, NULL) / 100.0 * distance;
	else
		return strtod(margin, NULL) + distance;
}

static void
data_write(void)
{
	double latsz,lonsz;
	maxlat = -9999.0;
	maxlon = -9999.0;
	minlat = 9999.0;
	minlon = 9999.0;
	rec_cnt = 0;

	short_length = atoi(snlen);
	mkshort_whandle = mkshort_new_handle();

	if (suppresswhite) {
		setshort_whitespace_ok(mkshort_whandle, 0);
	}

	setshort_length(mkshort_whandle, short_length);

	fprintf(file_out, "#tms-marker\n");
	waypt_disp_all(tiger_disp);

	if (genurl) {
		FILE *urlf;

		urlf = xfopen(genurl, "w", MYNAME);
		latsz = fabs(maxlat - minlat); 
		lonsz = fabs(maxlon - minlon); 

		/*
		 * Center the map along X and Y axis the midpoint of
		 * our min and max coords each way.   
		 */
		fprintf(urlf, "lat=%f&lon=%f&ht=%f&wid=%f",
				minlat + (latsz/2.0),
				minlon + (lonsz/2.0),
				dscale(latsz),
				dscale(lonsz));

		fprintf(urlf, "&iwd=%s&iht=%s", xpixels, ypixels);
		fclose(urlf);
#if CLICKMAP
		if (clickmap) {
			linkf = xfopen(clickmap, "w", MY NAME);
			fprintf(linkf, "<map name=\"map\">\n");
			waypt_disp_all(map_plot);
			fprintf(linkf, "</map>\n");
			fclose(linkf);
			linkf = NULL;
		}
#endif
	}

	mkshort_del_handle(&mkshort_whandle);
}


ff_vecs_t tiger_vecs = {
	ff_type_file, 
	FF_CAP_RW_WPT,
	rd_init,
	wr_init,
	rd_deinit,
	wr_deinit,
	data_read,
	data_write,
	NULL, 
	tiger_args,
	CET_CHARSET_ASCII, 0	/* CET-REVIEW */
};
