/*
    Acess to Garmin MapSource files.
    Based on information provided by Ian Cowley.

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

#include <stdio.h>
#include <string.h>

#include "defs.h"
#include <ctype.h>

static FILE *mps_file_in;
static FILE *mps_file_out;
static void *mkshort_handle;

#define MYNAME "MAPSOURCE" 

/*
 *  File header.  MsRcd ... Nov_18_2002 14:11:40
 */
char mps_hdr[] = {
	0x4d, 0x73, 0x52, 0x63, 0x64, 0x00, 0x02, 0x00, 
	0x00, 0x00, 0x44, 0x67, 0x00, 0x1b, 0x00, 0x00,
	0x00, 0x41, 0x96, 0x01, 0x53, 0x51, 0x41, 0x00, 
	0x4f, 0x63, 0x74, 0x20, 0x32, 0x32, 0x20, 0x32,
	0x30, 0x30, 0x31, 0x00, 0x31, 0x35, 0x3a, 0x34, 
	0x35, 0x3a, 0x30, 0x35, 0x00
}; 

char mps_ftr[] = {
	0x02, 0x00, 0x00, 0x00, 0x56, 0x00, 0x01
};

typedef struct icon_mapping {
	const int symnum;
	const char *icon;
} icon_mapping_t;

/*
 * This list is meant for Mapsource version 4.07 and newer and is derived
 * from the list at http://home.online.no/~sigurdhu/MapSource-text.htm .
 * Someone more motivated than myself is encouraged to type the rest of
 * these in.
 */
static icon_mapping_t icon_table[] = {
	{ 0, "Marina" },
	{ 6, "Bank" },
	{ 6, "ATM" },
	{ 11, "Restaraunt" },
	{ 18, "Waypoint" },
	{ 150, "Boating" },
	{ 150, "Boat Ramp" },
	{ 151, "Campground" },
	{ 151, "Camping" },
	{ 159, "Park" },
	{ 170, "Car" },
	{ -1, NULL },
};

char *snlen;

static
arglist_t mps_args[] = {
	{"snlen", &snlen, "Length of generated shortnames" },
	{0, 0, 0}
};


const char *
mps_find_desc_from_icon_number(const int icon)
{
	icon_mapping_t *i;

	for (i = icon_table; i->icon; i++) {
		if (icon == i->symnum) {
			return i->icon;
		}
	}
	return "Waypoint";
}

int
mps_find_icon_number_from_desc(const char *desc)
{
	icon_mapping_t *i;
	int def_icon = 18;

	if (!desc) 
		return def_icon;

	for (i = icon_table; i->icon; i++) {
		if (desc == i->icon) {
			return i->symnum;
		}
	}
	return def_icon;
}

static void
mps_rd_init(const char *fname, const char *args)
{
	mps_file_in = fopen(fname, "rb");
	if (mps_file_in == NULL) {
		fatal(MYNAME ": '%s' for reading\n", fname);
	}
}

static void
mps_rd_deinit(void)
{
	fclose(mps_file_in);
}

static void
mps_wr_init(const char *fname, const char *args)
{
	mps_file_out = fopen(fname, "wb");
	if (mps_file_out == NULL) {
		fatal(MYNAME ": '%s' for writing\n", fname);
		exit(1);
	}
}

static void
mps_wr_deinit(void)
{
	fclose(mps_file_out);
}

/*
 * get characters until and including terminating NULL from mps_file_in 
 * and write into buf.
 */
static void
mps_readstr(char *buf, size_t sz)
{
	int c;
	while (sz-- && (c = fgetc (mps_file_in)) != EOF) {
		*buf++ = c;
		if (c == 0)  {
			return;
		}
	}
}


static void
mps_read(void)
{
	char hdr[100];
	char tbuf[100];
	char wptname[256];
	char wptdesc[256];
	int reclen;
	int lat;
	int lon;
	waypoint *wpt;

	fread(hdr, 45, 1, mps_file_in);
	for(;;)
	{
		long next_rec;
		char icon;
		fread(&reclen, 4, 1, mps_file_in);
		reclen = le_read32(&reclen);
		fread(tbuf, 1, 1, mps_file_in); /* 'W' */
		/*
		 * Use this to seek to next record to make us more immune
		 * to changes in file format.
		 */
		next_rec = ftell(mps_file_in) + reclen;
		/*
		 * Each record has a 'W'.  When we run out of those, it's EOF.
		 */
		if (tbuf[0] != 'W') 
			break;
		wpt = xcalloc(sizeof(*wpt), 1);

		mps_readstr(wptname, sizeof(wptname));
		fread(tbuf, 9, 1, mps_file_in); /* unknown, 0 */
		fread(tbuf, 12, 1, mps_file_in); /* unknown, 0xff */
		fread(tbuf, 6, 1, mps_file_in); /* unknown 0 0 ff ff ff ff  */

		fread(&lat, 4, 1, mps_file_in); 
		fread(&lon, 4, 1, mps_file_in); 
		lat = le_read32(&lat);
		lon = le_read32(&lon);
		
		fread(tbuf, 9, 1, mps_file_in); /* alt, format unknown */

		mps_readstr(wptdesc, sizeof(wptdesc));

		fread(tbuf, 17, 1, mps_file_in); /* unknown */
		fread(&icon, 1, 1, mps_file_in); /* unknown */
		fseek(mps_file_in, next_rec, SEEK_SET);

		wpt->shortname = xstrdup(wptname);
		wpt->description = xstrdup(wptdesc);
		wpt->position.latitude.degrees = lat / 2147483648.0 * 180.0;
		wpt->position.longitude.degrees = lon / 2147483648.0 * 180.0;
		wpt->icon_descr = mps_find_desc_from_icon_number(icon);
		waypt_add(wpt);
	}

}

static void
mps_waypt_pr(const waypoint *wpt)
{
	char *src;
	char *ident;
	int reclen;
	char zbuf[100];
	char ffbuf[100];
	char display = 1;
	char icon;
	int lat = wpt->position.latitude.degrees  / 180.0 * 2147483648.0;
	int lon = wpt->position.longitude.degrees  / 180.0 * 2147483648.0;

	if(wpt->description) src = wpt->description;
	if(wpt->notes) src = wpt->notes;
	ident = global_opts.synthesize_shortnames ?
				mkshort(mkshort_handle, src) :
				wpt->shortname;

	reclen = 87 + strlen(ident) + strlen(wpt->description);

	memset(zbuf, 0, sizeof(zbuf));
	memset(ffbuf, 0xff, sizeof(ffbuf));

	icon = mps_find_icon_number_from_desc(wpt->icon_descr);

	le_write32(&reclen, reclen);
	fwrite(&reclen, 4, 1, mps_file_out);
	fwrite("W", 1, 1, mps_file_out);
	fputs(ident, mps_file_out);
	fwrite(zbuf, 1, 1, mps_file_out);
	fwrite(zbuf, 9, 1, mps_file_out);
	fwrite(ffbuf, 12, 1, mps_file_out);
	fwrite(zbuf, 2, 1, mps_file_out);
	fwrite(ffbuf, 4, 1, mps_file_out);

	le_write32(&lat, lat);
	le_write32(&lon, lon);
	fwrite(&lat, 4, 1, mps_file_out);
	fwrite(&lon, 4, 1, mps_file_out);

	fwrite(zbuf, 9, 1, mps_file_out);
	fputs(wpt->description, mps_file_out);
	fwrite(zbuf, 10, 1, mps_file_out);
	fwrite(&display, 1, 1, mps_file_out); /* Show waypoint w/ name */
	fwrite(zbuf, 7, 1, mps_file_out);
	fwrite(&icon, 1, 1, mps_file_out);
	fwrite(zbuf, 23, 1, mps_file_out);
}

void
mps_write(void)
{
	int short_length;

	if (snlen)
		short_length = atoi(snlen);
	else
		short_length = 10;

	mkshort_handle = mkshort_new_handle();

	setshort_length(mkshort_handle, short_length);
	setshort_whitespace_ok(mkshort_handle, 0);

	fwrite(mps_hdr, sizeof(mps_hdr), 1, mps_file_out);
	waypt_disp_all(mps_waypt_pr);
	fwrite(mps_ftr, sizeof(mps_ftr), 1, mps_file_out);
	mkshort_del_handle(mkshort_handle);

}

ff_vecs_t mps_vecs = {
	mps_rd_init,
	mps_wr_init,
	mps_rd_deinit,
	mps_wr_deinit,
	mps_read,
	mps_write,
	mps_args
};
