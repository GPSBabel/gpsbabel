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
#include "magellan.h"
#include "csv_util.h"

static FILE *file_in;
static FILE *file_out;
static FILE *linkf;
static void *mkshort_handle;
static void *mkshort_whandle;

#define MYNAME "GPSUTIL"

static double maxlat, maxlon, minlat, minlon, latsum, lonsum;
int rec_cnt;
static char *nolabels = NULL;
static char *genurl = NULL;
static char *margin  = "15%";
static char *xpixels = "768";
static char *ypixels = "768";
static char *snlen = NULL;
static char *oldthresh = "14";
static char *oldmarker  = "redpin";
static char *newmarker  = "greenpin";
static char *unfoundmarker  = "bluepin";
static char *suppresswhite = NULL;

int scalev;
int short_length;
int thresh_days;

/*
 *   The code bracketed by CLICKMAP is to generate clickable image maps
 *   for a web browser.   It's functional, but is missing the math to do
 *   the projection transformations.   Some trig geek can finish that.
 */
#if CLICKMAP
static char *clickmap = NULL;
#endif


static
arglist_t tiger_args[] = {
	{"nolabels", &nolabels, "Suppress labels on generated pins.",
		ARGTYPE_BOOL },
	{"genurl", &genurl, "Generate file with lat/lon for centering map.",
		ARGTYPE_OUTFILE },
	{"margin", &margin, "Margin for map.  Degrees or percentage.",
		ARGTYPE_FLOAT},
	{"snlen", &snlen, "Max shortname length when used with -s.",
		ARGTYPE_INT},
	{"oldthresh", &oldthresh, "Days after which points are considered old.",
		ARGTYPE_INT},
	{"oldmarker", &oldmarker, "Marker type for old points.",
		ARGTYPE_STRING},
	{"newmarker", &newmarker, "Marker type for new points.",
		ARGTYPE_STRING},
	{"suppresswhite", &suppresswhite,
		"Suppress whitespace in generated shortnames", ARGTYPE_BOOL },
	{"unfoundmarker", &unfoundmarker, "Marker type for unfound points.",
		ARGTYPE_STRING},
	{"xpixels", &xpixels, "Width in pixels of map.",
		ARGTYPE_INT},
	{"ypixels", &ypixels, "Height in pixels of map.",
		ARGTYPE_INT},
#if CLICKMAP
	{"clickmap", &clickmap, "Generate Clickable map web page.",
		ARGTYPE_BOOL},
#endif
	{0, 0, 0, 0}
};


static void
rd_init(const char *fname)
{
	file_in = fopen(fname, "r");
	mkshort_handle = mkshort_new_handle();

	if (file_in == NULL) {
		fatal(MYNAME ": Cannot open %s for reading\n", fname);
	}
}

static void
rd_deinit(void)
{
	fclose(file_in);
	mkshort_del_handle(mkshort_handle);
}

static void
wr_init(const char *fname)
{
	file_out = fopen(fname, "w");
	thresh_days = strtod(oldthresh, NULL);

	if (file_out == NULL) {
		fatal(MYNAME ": Cannot open %s for writing\n", fname);
	}
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
			wpt_tmp = xcalloc(sizeof (*wpt_tmp), 1);

			wpt_tmp->position.longitude.degrees = lon;
			wpt_tmp->position.latitude.degrees = lat;
			wpt_tmp->description = xstrdup(desc);
			wpt_tmp->shortname = mkshort(mkshort_handle, desc);

			waypt_add(wpt_tmp);
		}
	}
}

static void
tiger_disp(const waypoint *wpt)
{
	char *pin;
	double lat = wpt->position.latitude.degrees;
	double lon = wpt->position.longitude.degrees;

	if (wpt->icon_descr && strstr(wpt->icon_descr, "-unfound"))
		pin = unfoundmarker;
	else if (wpt->creation_time > time(0) - 3600 * 24 * thresh_days)
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
		char *desc = csv_stringclean(wpt->description, ":");
		if (global_opts.synthesize_shortnames)
			desc = mkshort(mkshort_whandle, desc);
		fprintf(file_out, ":%s", desc);
		xfree(desc);
	}
	fprintf(file_out, "\n");
}

static void
map_plot(const waypoint *wpt)
{
	static int x,y;

	/* Replace with real math. */
	x+=10;
	y+=10;

	fprintf(linkf, "<area shape=\"circle\" coords=\"%d,%d,7\" href=\"%s\" alt=\"%s\"\n", x, y, wpt->url, wpt->description);
}

static double
dscale(double distance)
{
	/*
	 * If we have any specified margin options  factor those in now.  
	 * A additional little boundary is helpful becuase Tiger always 
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

	if (snlen)
		short_length = atoi(snlen);
	else
		short_length = 10;
	mkshort_whandle = mkshort_new_handle();

	if (suppresswhite) {
		setshort_whitespace_ok(mkshort_whandle, 0);
	}

	setshort_length(mkshort_whandle, short_length);

	fprintf(file_out, "#tms-marker\n");
	waypt_disp_all(tiger_disp);

	if (genurl) {
		FILE *urlf;

		urlf = fopen(genurl, "w");
		if (urlf == NULL) {
			fatal(MYNAME ": Cannot open '%s' for writing\n", 
					genurl);
		} 
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
			linkf = fopen(clickmap, "w");
			if (linkf == NULL) {
				fatal(MYNAME ": Cannot open '%s' for writing\n",
						clickmap);
			}
			fprintf(linkf, "<map name=\"map\">\n");
			waypt_disp_all(map_plot);
			fprintf(linkf, "</map>\n");
			fclose(linkf);
			linkf = NULL;
		}
#endif
	}

	mkshort_del_handle(mkshort_whandle);
}


ff_vecs_t tiger_vecs = {
	rd_init,
	wr_init,
	rd_deinit,
	wr_deinit,
	data_read,
	data_write,
	tiger_args,
};
