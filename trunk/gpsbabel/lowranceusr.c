/*
	Access to Lowrance USR files.
	Contributed to gpsbabel by Jason Rust (jrust at rustyparts.com)

	Copyright (C) 2005 Robert Lipe, robertlipe@usa.net

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
#include <string.h>
#include <math.h> /* for lat/lon conversion */

typedef struct lowranceusr_icon_mapping {
	const long int	value;
	const char		*icon;
} lowranceusr_icon_mapping_t;

/* Taken from iFinder 1.8 */
const lowranceusr_icon_mapping_t lowranceusr_icon_value_table[] = {
	{ 10000, "diamond 1" },
	{ 10001, "diamond 2" },
	{ 10002, "diamond 3" },
	{ 10003, "x 1" },
	{ 10004, "x 2" },
	{ 10005, "x 3" },
	{ 10006, "cross" },
	{ 10007, "house" },
	{ 10008, "car" },
	{ 10009, "store" },
	{ 10010, "gas station" },
	{ 10011, "fork and spoon" },
	{ 10012, "telephone" },
	{ 10013, "airplane" },
	{ 10014, "exit sign" },
	{ 10015, "stop sign" },
	{ 10016, "exclamation" },
	{ 10017, "traffic light" },
	{ 10018, "american flag" },
	{ 10019, "person" },
	{ 10020, "restrooms" },
	{ 10021, "tree" },
	{ 10022, "mountains" },
	{ 10023, "campsite" },
	{ 10024, "picnic table" },
	{ 10025, "deer" },
	{ 10026, "deer tracks" },
	{ 10027, "turkey tracks" },
	{ 10028, "tree stand" },
	{ 10029, "bridge" },
	{ 10030, "skull and crossbones" },
	{ 10031, "fish" },
	{ 10032, "two fish" },
	{ 10033, "dive flag" },
	{ 10034, "wreck" },
	{ 10035, "anchor" },
	{ 10036, "boat" },
	{ 10037, "boat ramp" },
	{ 10038, "flag buoy" },
	{ 10039, "dam" },
	{ 10040, "swimmer" },
	{ 10041, "pier"},

	{ 10038, "Micro-Cache" },   	/* icon for "flag buoy" */
	{ 10030, "Virtual cache" }, 	/* icon for "skull and crossbones" */
	{ 10032, "Multi-Cache" },   	/* icon for "two fish" */
	{ 10003, "Unknown Cache" },   	/* icon for "x 1" */
	{ 10018, "Locationless (Reverse) Cache" }, /* Icon for "american flag" */
	{ 10007, "Post Office" },  	/* Icon for "house" */
	{ 10019, "Event Cache" }, 	/* Icon for "person" */
	{ 10020, "Webcam Cache" }, 	/* Icon for "restrooms" */
	{	 -1, NULL }
};

static FILE *file_in;
static FILE *file_out;
static void *mkshort_handle;

static unsigned int waypt_out_count;

#define MYNAME "Lowrance USR"

#define MAXUSRSTRINGSIZE	256
#define SEMIMINOR		   6356752.3142
#define DEGREESTORADIANS	0.017453292
#define SECSTO2000		  946713599

const char *
lowranceusr_find_desc_from_icon_number(const int icon)
{
	const lowranceusr_icon_mapping_t *i;

	for (i = lowranceusr_icon_value_table; i->icon; i++) {
		if (icon == i->value) {
			return i->icon;
		}
	}

	return "";
}

long int
lowranceusr_find_icon_number_from_desc(const char *desc)
{
	const lowranceusr_icon_mapping_t *i;
	long int def_icon = 10001;

	if (!desc) {
		return def_icon;
	}

	for (i = lowranceusr_icon_value_table; i->icon; i++) {
		if (case_ignore_strcmp(desc,i->icon) == 0) {
			return i->value;
		}
	}

	return def_icon;
}
static int
lowranceusr_fread(void *buff, size_t size, size_t members, FILE * fp) 
{
	size_t br;

	br = fread(buff, size, members, fp);

	if (br != members) {
		fatal(MYNAME ": requested to read %d bytes, read %d bytes.\n", members, br);
	}

	return (br);
}

static
arglist_t lowranceusr_args[] = {
	{0, 0, 0, 0,0 }
};

static void
rd_init(const char *fname)
{
	file_in = xfopen(fname, "rb", MYNAME);
}

static void
rd_deinit(void)
{
	fclose(file_in);
}

static void
wr_init(const char *fname)
{
	file_out = xfopen(fname, "wb", MYNAME);
	mkshort_handle = mkshort_new_handle();
	waypt_out_count = 0;
}

static void
wr_deinit(void)
{
	fclose(file_out);
	mkshort_del_handle(mkshort_handle);
}

/**
 * Latitude and longitude for USR coords are in the lowrance mercator meter
 * format in WGS84.  The below code converts them to degrees.
 */
static double 
lon_mm_to_deg(double x) {
	return x / (DEGREESTORADIANS * SEMIMINOR);
}

static long 
lon_deg_to_mm(double x) {
	return (long)(x * SEMIMINOR * DEGREESTORADIANS);
}

static double 
lat_mm_to_deg(double x) {
	return (2 * atan(exp(x / SEMIMINOR)) - M_PI / 2) / DEGREESTORADIANS;
}

static long
lat_deg_to_mm(double x) {
	return (long)(SEMIMINOR * log(tan((x * DEGREESTORADIANS + M_PI / 2) / 2)));
}

static void
data_read(void)
{
	char buff[MAXUSRSTRINGSIZE + 1];
	short int NumWaypoints, MajorVersion, MinorVersion;
	int i;
	long int TextLen;

	lowranceusr_fread(&buff[0], 2, 1, file_in);
	MajorVersion = le_read16(&buff[0]);
	lowranceusr_fread(&buff[0], 2, 1, file_in);
	MinorVersion = le_read16(&buff[0]);
	
	if (MajorVersion < 2) {
		fatal(MYNAME ": input file is from an old version of the USR file and is not supported\n");
	}

	lowranceusr_fread(&buff[0], 2, 1, file_in);
	NumWaypoints = le_read16(&buff[0]);
	for (i = 0; i < NumWaypoints; i++) {
		waypoint *wpt_tmp;

		wpt_tmp = waypt_new();

		/* Object num */
		lowranceusr_fread(&buff[0], 2, 1, file_in);
		lowranceusr_fread(&buff[0], 4, 1, file_in);
		wpt_tmp->latitude = lat_mm_to_deg(le_read32(&buff[0]));
		lowranceusr_fread(&buff[0], 4, 1, file_in);
		wpt_tmp->longitude = lon_mm_to_deg(le_read32(&buff[0]));
		lowranceusr_fread(&buff[0], 4, 1, file_in);
		wpt_tmp->altitude = le_read32(&buff[0]);

		lowranceusr_fread(&buff[0], 4, 1, file_in);
		TextLen = buff[0];
		lowranceusr_fread(&buff[0], TextLen, 1, file_in);
		buff[TextLen] = '\0';
		wpt_tmp->shortname = xstrdup(buff);

		lowranceusr_fread(&buff[0], 4, 1, file_in);
		TextLen = buff[0];
		if (TextLen) {
			lowranceusr_fread(&buff[0], TextLen, 1, file_in);
			buff[TextLen] = '\0';
			wpt_tmp->description = xstrdup(buff);
		}

		lowranceusr_fread(&buff[0], 4, 1, file_in);
		/* Time is number of seconds since Jan. 1, 2000 */
		wpt_tmp->creation_time = SECSTO2000 + le_read32(&buff[0]);

		/* Symbol ID */
		lowranceusr_fread(&buff[0], 4, 1, file_in);
		wpt_tmp->icon_descr = lowranceusr_find_desc_from_icon_number(le_read32(&buff[0]));

		/* Waypoint Type (USER, TEMPORARY, POINT_OF_INTEREST) */
		lowranceusr_fread(&buff[0], 2, 1, file_in);

		waypt_add(wpt_tmp);
	}
}

static void
lowranceusr_waypt_pr(const waypoint *wpt)
{
	long int TextLen, Lat, Lon, Time, SymbolId;
	short int WayptType;
	char *name;
	char *comment;

	/* our personal waypoint counter */
	fwrite(&waypt_out_count, 2, 1, file_out);
	waypt_out_count++;

	Lat = lat_deg_to_mm(wpt->latitude);
	fwrite(&Lat, 4, 1, file_out);
	Lon = lon_deg_to_mm(wpt->longitude);
	fwrite(&Lon, 4, 1, file_out);
	fwrite(&wpt->altitude, 4, 1, file_out);

	/* Try and make sure we have a name */
	if ((! wpt->shortname) || global_opts.synthesize_shortnames) {
		if (wpt->description && global_opts.synthesize_shortnames) {
			name = mkshort(mkshort_handle, wpt->description);
		} else if (wpt->shortname) {
			name = xstrdup(wpt->shortname);
		} else if (wpt->description) {
			name = xstrdup(wpt->description);
		} else {
			name = xstrdup("");
		}
	} else {
		name = xstrdup(wpt->shortname);
	}

	TextLen = strlen(name);
	fwrite(&TextLen, 4, 1, file_out);
	fwrite(name, 1, TextLen, file_out);
	xfree(name);

	/**
	 * Comments aren't used by the iFinder yet so they just take up space...
	 */
	if (0 && wpt->description && strcmp(wpt->description, wpt->shortname) != 0) {
		comment = xstrdup(wpt->description);
		TextLen = strlen(comment);
		fwrite(&TextLen, 4, 1, file_out);
		fwrite(comment, 1, TextLen, file_out);
		xfree(comment);
	} else {
		TextLen = 0;
		fwrite(&TextLen, 4, 1, file_out);
	}

	if (wpt->creation_time > SECSTO2000) {
		Time = wpt->creation_time - SECSTO2000;
	} else {
		Time = SECSTO2000 + 1;
	}
	fwrite(&Time, 4, 1, file_out);

	if (get_cache_icon(wpt) && wpt->icon_descr && (strcmp(wpt->icon_descr, "Geocache Found") != 0)) {
		SymbolId = lowranceusr_find_icon_number_from_desc(get_cache_icon(wpt));
	} else {
		SymbolId = lowranceusr_find_icon_number_from_desc(wpt->icon_descr);
	}

	fwrite(&SymbolId, 4, 1, file_out);

	/* USER waypoint type */
	WayptType = 0;
	fwrite(&WayptType, 2, 1, file_out);
}

static void
data_write(void)
{
	short int NumWaypoints, MajorVersion, MinorVersion, NumRoutes, NumTrails, NumIcons;
	setshort_length(mkshort_handle, 15);
	MajorVersion = 2;
	MinorVersion = 0;
	NumWaypoints = waypt_count();

	fwrite(&MajorVersion, 2, 1, file_out);
	fwrite(&MinorVersion, 2, 1, file_out);
	fwrite(&NumWaypoints, 2, 1, file_out);
	waypt_disp_all(lowranceusr_waypt_pr);

	/* We don't support these yet... */
	NumRoutes = 0;
	fwrite(&NumRoutes, 2, 1, file_out);
	NumIcons = 0;
	fwrite(&NumIcons, 2, 1, file_out);
	NumTrails = 0;
	fwrite(&NumTrails, 2, 1, file_out);
}


ff_vecs_t lowranceusr_vecs = {
	ff_type_file,
	FF_CAP_RW_WPT,
	rd_init,
	wr_init,
	rd_deinit,
	wr_deinit,
	data_read,
	data_write,
	NULL, 
	lowranceusr_args
};
