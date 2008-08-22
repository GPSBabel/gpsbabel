/*

    Support for embedded Exif-GPS information.
    
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

#include "defs.h"
#include "config.h"
#include "garmin_tables.h"
#include "jeeps/gpsmath.h"
#include "strptime.h"
#include <ctype.h>

#define MYNAME "exif"

#define UNKNOWN_TIMESTAMP 999999999

typedef struct exif_tag_s {
	gbuint16 tag;
	gbuint16 type;
	gbint32 count;
	gbuint32 offs;
} exif_tag_t;

static gbfile *fin;
static gbsize_t exif_ifd, gps_ifd;
static char byte_order;
static waypoint *wpt;
static gbsize_t fileoffs;
static char *opt_filename;
static time_t timestamp;
 
static
arglist_t exif_args[] = {
	{"filename", &opt_filename, "Set waypoint name to source filename.", "Y", ARGTYPE_BOOL, ARG_NOMINMAX}, 
	ARG_TERMINATOR
};

#define EXIF_IFD	-1
#define GPS_IFD		-2

/*******************************************************************************
* %%%        global callbacks called by gpsbabel main process              %%% *
*******************************************************************************/

static void
exif_rd_init(const char *fname)
{
	fin = gbfopen_le(fname, "rb", MYNAME);
}

static void 
exif_rd_deinit(void)
{
	gbfclose(fin);
}

#if 0
static int
exif_tag_size(const exif_tag_t *tag)
{
	int size;
	
	switch(tag->type) {
		case 1:
		case 2: 
		case 7: size = 1; break;
		case 3: size = 2; break;
		case 4:
		case 9: size = 4; break;
		case 5:
		case 10: size = 8; break;
		default:
			return 0;
	}
	return size * tag->count;
}
#endif

static double
exif_read_double(const gbsize_t offs)
{
	unsigned int num, den;
	
	if (offs) gbfseek(fin, fileoffs + offs, SEEK_SET);
	
	num = gbfgetuint32(fin);
	den = gbfgetuint32(fin);
	if (den == 0) den = 1;
	
	return (double)num / den;
}

static double
exif_read_coord(const exif_tag_t *tag)
{
		double deg, min, sec;
		
		deg = exif_read_double(tag->offs);
		if (tag->count == 1) return deg;

		min = exif_read_double(0);
		deg += (min / 60);
		if (tag->count == 2) return deg;

		sec = exif_read_double(0);
		deg += (sec / 3600);

		return deg;
}

static char *
exif_read_string(const exif_tag_t *tag)
{
	if (tag->count > 4) {
		gbfseek(fin, fileoffs + tag->offs, SEEK_SET);
		return gbfgetcstr(fin);
	}
	else {
		char buff[5];
		if (fin->big_endian) be_write32(buff, tag->offs);
		else le_write32(buff, tag->offs);
		if (tag->count < 5) buff[tag->count] = '\0';
		else buff[4] = '\0';
		return xstrdup(buff);
	}
}

static time_t
exif_read_timestamp(const exif_tag_t *tag)
{
	double hour, min, sec;
	
	hour = exif_read_double(tag->offs);
	min = exif_read_double(0);
	sec = exif_read_double(0);
	
	return ((int)hour * SECONDS_PER_HOUR) + ((int)min * 60) + (int)sec;
}

static int
exif_sort_tags_cb(const void *a, const void *b)
{
	const exif_tag_t *ea = a;
	const exif_tag_t *eb = b;
	return (int)ea->offs - (int)eb->offs;
}

static gbsize_t
exif_read_tags(const int ifd)
{
	int entries;
	exif_tag_t *tags = NULL;
	gbsize_t next_ifd = 0;
	double gpsdop = unknown_alt;
	char speed_ref = 'K';
	char lat_ref = '*';
	char lon_ref = '*';
	char mode = 'N';
	int datum = DATUM_WGS84;
	
	entries = gbfgetint16(fin);
	if (entries > 0) {
		int i;
		tags = xmalloc(entries * sizeof(*tags));
		for (i = 0; i < entries; i++) {
			tags[i].tag = gbfgetuint16(fin);
			tags[i].type = gbfgetuint16(fin);
			tags[i].count = gbfgetint32(fin);
			tags[i].offs = gbfgetuint32(fin);
		}
	}

	next_ifd = gbfgetuint32(fin);

	if (entries > 0) {
		int i;
		char *str, *c;
		struct tm tm;

		if (entries > 1) /* avoid backward seek */
			qsort(tags, entries, sizeof(*tags), exif_sort_tags_cb);

		for (i = 0; i < entries; i++) {
			exif_tag_t *tag = &tags[i];
			
			switch(ifd) {
				case 0:
					switch(tag->tag) {
						case 0x8769: 
							exif_ifd = tag->offs;
							break;
						case 0x8825: 
							gps_ifd = tag->offs;
							break;
					}
					break;
				case 1:		/* IFD1 */
					break;
				case -1:	/* Exif */
					switch(tag->tag) {

						case 0x9003: /* DateTimeOriginal */
							str = exif_read_string(tag);
							c = strptime(str, "%Y:%m:%d %H:%M:%S", &tm);
							if (c && (*c == '\0'))
								wpt->creation_time = mklocaltime(&tm);
							xfree(str);
							break;
					}
					break;
				case -2:	/* GPS */
					switch(tag->tag) {

						case 0x0001: /* GPSLatitudeRef */
							str = exif_read_string(tag);
							lat_ref = *str & 127;
							xfree(str);
							break;

						case 0x0002: /* GPSLatitude */ 
							wpt->latitude = exif_read_coord(tag);
							break;

						case 0x0003: /* GPSLongitudeRef */
							str = exif_read_string(tag);
							lon_ref = *str & 127;
							xfree(str);
							break;

						case 0x0004: /* GPSLongitude */ 
							wpt->longitude = exif_read_coord(tag);
							break;

						case 0x0005: /* GPSAltitudeRef */ 
							break;

						case 0x0006: /* GPSAltitude */
							wpt->altitude = exif_read_double(tag->offs);
							break;

						case 0x0007: /* GPSTimeStamp */
							timestamp = exif_read_timestamp(tag);
							break;

						case 0x0008: /* GPSSatellites */
							str = exif_read_string(tag);
							wpt->sat = atoi(str);
							xfree(str);
							break;
							
						case 0x000a: /* GPSMeasureMode */
							str = exif_read_string(tag);
							mode = *str & 127;
							xfree(str);
							break;
							
						case 0x000b: /* GPSDOP */
							gpsdop = exif_read_double(tag->offs);
							break;

						case 0x000c: /* GPSSpeedRef */
							str = exif_read_string(tag);
							speed_ref = *str & 127;
							xfree(str);
							break;

						case 0x000d: /* GPSSpeed */
							WAYPT_SET(wpt, speed, exif_read_double(tag->offs));
							break;

						case 0x0012: /* GPSMapDatum */
							str = exif_read_string(tag);
							datum = gt_lookup_datum_index(str, MYNAME);
							if (datum < 0)
								fatal(MYNAME ": Unknown GPSMapDatum \"%s\"!\n", str);
							xfree(str);
							break;
					}
					break;
			}
		}
		xfree(tags);
	}

	if (ifd == GPS_IFD) {

		/* Did we get our minimum data ? */
		if ((wpt->latitude == unknown_alt) || (wpt->longitude == unknown_alt)) {
			warning(MYNAME ": GPSLatitude and/or GPSLongitude not set!\n");
			waypt_free(wpt);
			wpt = NULL;
			return 0;
		}
		
		if WAYPT_HAS(wpt, speed) {
			switch(speed_ref) {
				case 'K':
					wpt->speed = KPH_TO_MPS(wpt->speed);
					break;
				case 'M':
					wpt->speed = MPH_TO_MPS(wpt->speed);
					break;
				case 'N':
					wpt->speed = KNOTS_TO_MPS(wpt->speed);
					break;
				default:
					wpt->speed = 0;
					WAYPT_UNSET(wpt, speed);
					warning(MYNAME ": Unknown GPSSpeedRef unit %c (0x%02x)!\n", speed_ref, speed_ref);
			}
		}
		
		if (lat_ref == 'S') wpt->latitude *= -1;
		else if (lat_ref != 'N') warning(MYNAME ": GPSLatitudeRef not set! Using N(orth).\n");
		if (lon_ref == 'W') wpt->longitude *= -1;
		else if (lon_ref != 'E') warning(MYNAME ": GPSLongitudeRef not set! Using E(east).\n");
		
		if (datum != DATUM_WGS84) {
			double alt;
			GPS_Math_WGS84_To_Known_Datum_M(wpt->latitude, wpt->longitude, 0.0,
				&wpt->latitude, &wpt->longitude, &alt, datum);
		}
		
		if (mode == '2') {
			wpt->fix = fix_2d;
			if (gpsdop != unknown_alt) wpt->hdop = gpsdop;
		}
		else if (mode == '3') {
			wpt->fix = fix_3d;
			if (gpsdop != unknown_alt) wpt->pdop = gpsdop;
		}
	}

	return next_ifd;
}

static void
exif_read(void)
{
	gbint32 code = 0;
	
	fileoffs = 0;
	wpt = NULL;
	
	while (!gbfeof(fin)) {

		unsigned char c = (unsigned)gbfgetc(fin);

		code = (code << 8) | c;
		if (code == 0x45786966) { /* Look for "Exif" */
			
			gbsize_t next_ifd;
			gbint32 ifd;
			
			int order = gbfgetint32(fin);
			switch(order) {
				case 0x49490000: /* "II" - Intel */
					byte_order = 'I';
					break;
				case 0x4D4D0000: /* "MM" - Motorola */
					byte_order = 'M';
					break;
				
				default:
					continue;
			}
			fin->big_endian = (byte_order == 'M');
			if (gbfgetint16(fin) != 0x002a) continue;

			fileoffs = gbftell(fin) - 4;
			
			next_ifd = gbfgetuint32(fin);
			if (! next_ifd) continue;	/* No IFD0 ? */
			
			ifd = gps_ifd = exif_ifd = 0;
			timestamp = UNKNOWN_TIMESTAMP;

			/* we need the wpt not only during GPS IFD */
			wpt = waypt_new();
			wpt->latitude = unknown_alt;
			wpt->longitude = unknown_alt;

			while (next_ifd) {
				gbfseek(fin, fileoffs + next_ifd, SEEK_SET);
				next_ifd = exif_read_tags(ifd++);
			}
			if (exif_ifd) {
				gbfseek(fin, fileoffs + exif_ifd, SEEK_SET);
				(void) exif_read_tags(EXIF_IFD);
			}
			if (gps_ifd) {
				gbfseek(fin, fileoffs + gps_ifd, SEEK_SET);
				(void) exif_read_tags(GPS_IFD);
			}
			else {
				warning(MYNAME ": No Exif-GPS information in file \"%s\"!\n", fin->name);
				waypt_free(wpt);
				wpt = NULL;
			}
			
			if (! wpt) return;
			
			if (wpt->creation_time && (timestamp != UNKNOWN_TIMESTAMP)) {
				struct tm tm;
				tm = *gmtime(&wpt->creation_time);
				tm.tm_hour = 0;
				tm.tm_min = 0;
				tm.tm_sec = 0;
				wpt->creation_time = mkgmtime(&tm) + timestamp;
			}

			if (opt_filename) {
				char *c, *cx;
				char *str = xstrdup(fin->name);

				cx = str;
				if ((c = strrchr(cx, ':'))) cx = c + 1;
				if ((c = strrchr(cx, '\\'))) cx = c + 1;
				if ((c = strrchr(cx, '/'))) cx = c + 1;
				if (((c = strchr(cx, '.'))) && (c != cx)) *c = '\0';

				if (wpt->shortname) xfree(wpt->shortname);
				wpt->shortname = xstrdup(cx);
				xfree(str);
			}
			waypt_add(wpt);
			
			return;
		}
	}
	warning(MYNAME ": No Exif header in file \"%s\"!\n", fin->name);
}

#if 0
static void
exif_wr_init(const char *fname)
{
	fout = gbfopen(fname, "w", MYNAME);
}

static void
exif_wr_deinit(void)
{
	gbfclose(fout);
}

static void
exif_write(void)
{
}
#endif

/**************************************************************************/

ff_vecs_t exif_vecs = {
	ff_type_file,
	{ 
		ff_cap_read	/* waypoints */, 
	  	ff_cap_none 	/* tracks */, 
	  	ff_cap_none 	/* routes */
	},
	exif_rd_init,
	NULL,			/* exif_wr_init, */
	exif_rd_deinit,	
	NULL,			/* exif_wr_deinit, */
	exif_read,
	NULL,			/* exif_write */
	NULL,
	exif_args,
	CET_CHARSET_ASCII, 0
};

/**************************************************************************/
