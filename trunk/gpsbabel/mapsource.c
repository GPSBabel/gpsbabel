/*
    Access to Garmin MapSource files.
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
#include "garmin_tables.h"
#include <ctype.h>

static	FILE	*mps_file_in;
static	FILE	*mps_file_out;
static	FILE	*mps_file_temp;
static	void	*mkshort_handle;

static	int		mps_ver_in = 0;
static	int		mps_ver_out = 0;
static	int		mps_ver_temp = 0;

/* Temporary pathname used when merging gpsbabel output with an existing file */
static	char	tempname[256];
static	char	origname[256];

static	const waypoint	*prevRouteWpt;

#define MYNAME "MAPSOURCE" 
#define ISME 0
#define NOTME 1

char *snlen;
char *mpsverout;
char *mpsmergeout = NULL;

static
arglist_t mps_args[] = {
	{"snlen", &snlen, "Length of generated shortnames", ARGTYPE_INT },
	{"mpsverout", &mpsverout, "Version of mapsource file to generate (3,4,5)", ARGTYPE_INT },
	{"mpsmergeout", &mpsmergeout, "Merge output with existing file", ARGTYPE_BOOL },
	{0, 0, 0, 0}
};

static void 
mps_noop(const route_head *wp)
{
	/* no-op */
}

const char *
mps_find_desc_from_icon_number(const int icon, garmin_formats_e garmin_format)
{
	icon_mapping_t *i;

	for (i = icon_table; i->icon; i++) {
		switch (garmin_format) {
			case MAPSOURCE:
				if (icon == i->mpssymnum)
					return i->icon;
				break;
			case PCX:
			case GARMIN_SERIAL:
				if (icon == i->pcxsymnum)
					return i->icon;
				break;
			default:
				fatal(MYNAME ": unknown garmin format");
		}
	}
	return "Waypoint";
}

int
mps_find_icon_number_from_desc(const char *desc, garmin_formats_e garmin_format)
{
	icon_mapping_t *i;
	int def_icon = 18;

	if (!desc)
		return def_icon;

	for (i = icon_table; i->icon; i++) {
		if (case_ignore_strcmp(desc,i->icon) == 0) {
			switch (garmin_format) {
			case MAPSOURCE:
				return i->mpssymnum;
			case PCX:
			case GARMIN_SERIAL:
				return i->pcxsymnum;
			default:
				fatal(MYNAME ": unknown garmin format");
			}
		}
	}
	return def_icon;
}

int mps_converted_icon_number(const int icon_num, const int mpsver, garmin_formats_e garmin_format)
{
	int def_icon = 18;

	switch (garmin_format) {
	case MAPSOURCE:
		if (mpsver == 5) return icon_num;
		if (mpsver == 4) {
			/* Water hydrant */
			if (icon_num == 139) return def_icon;
			else return icon_num;
		}
		else {
			/* the Contact icons - V3 doesn't have anything like this */
			if ((icon_num >= 119) && (icon_num <= 138)) return def_icon;
			/* the Geocache icons - V3 use the Circle with X */
			if ((icon_num >= 117) && (icon_num <= 118)) return 65;
			/* Water hydrant */
			if (icon_num == 139) return def_icon;
			return icon_num;
		}

	case PCX:
	case GARMIN_SERIAL:
		if (mpsver == 5) return icon_num;
		if (mpsver == 4) {
			/* Water hydrant */
			if (icon_num == 8282) return def_icon;
			else return icon_num;
		}
		/* the Contact icons - V3 doesn't have anything like this */
		if ((icon_num >= 8257) && (icon_num <= 8276)) return def_icon;
		/* the Geocache icons - V3 use the Circle with X */
		if ((icon_num >= 8255) && (icon_num <= 8256)) return 179;
		/* Water hydrant */
		if (icon_num == 8282) return def_icon;
		return icon_num;

	default:
		fatal(MYNAME ": unknown garmin format");
	}
	return def_icon;
}

static void
mps_rd_init(const char *fname)
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
mps_wr_init(const char *fname)
{
	if (mpsmergeout) {
		mps_file_out = fopen(fname, "rb");
		if (mps_file_out == NULL) {
			mpsmergeout = NULL;
		}
		else {
			fclose(mps_file_out);
			srand((unsigned) time(NULL));

			for (;;) {
				/* create a temporary name  based on a random char and the existing name */
				/* then test if it already exists, if so try again with another rand num */
				/* yeah, yeah, so there's probably a library function for this           */
				sprintf(tempname, "%s.%08x", fname, rand());
				mps_file_temp = fopen(tempname, "rb");
				if (mps_file_temp == NULL) break;
				fclose(mps_file_temp);
			}
			rename(fname, tempname);
			mps_file_temp = fopen(tempname, "rb");
			strcpy(origname, fname);	/* save in case we need to revert the renamed file */
		}
	}

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

	if (mpsmergeout) {
		fclose(mps_file_temp);
		remove(tempname);
	}
}

/*
 * get characters until and including terminating NULL from mps_file_in 
 * and write into buf.
 */
static void
mps_readstr(FILE *mps_file, char *buf, size_t sz)
{
	int c;
	while (sz-- && (c = fgetc (mps_file)) != EOF) {
		*buf++ = c;
		if (c == 0)  {
			return;
		}
	}
}

/*
 * read in from file to check a) valid format b) version of data formating
 * MRCB
 */
static void
mps_fileHeader_r(FILE *mps_file, int *mps_ver)
{
	char hdr[100];
	int reclen;

	mps_readstr(mps_file, hdr, sizeof(hdr));
	if ( strcmp( hdr, "MsRcd" )) {
		fatal(MYNAME ": This doesn't look like a mapsource file.\n");
	}
	/* Read record length of "format details" section */
	fread(&reclen, 4, 1, mps_file);
	reclen = le_read32(&reclen);
	/* Read the "format details" in plus the trailing null */
	fread( hdr, 3, 1, mps_file);
	if (hdr[0] != 'D')  {
		/* No flag for the "data" section */
		fatal(MYNAME ": This doesn't look like a mapsource file.\n");
	}
	if (hdr[1] == 'd')  {
		*mps_ver = 3;
	}
	else if ((hdr[1] > 'd') && (hdr[1] <= 'h')) {
		*mps_ver = 4;
	}
	else if ((hdr[1] > 'h') && (hdr[1] <= 'i')) {
		*mps_ver = 5;
	}
	else {
		fatal(MYNAME ": Unsuppported version of mapsource file.\n");
	}
	/* Skip reliably over the "format details" section */
	fseek( mps_file, reclen+1-3, SEEK_CUR); 
	/* Read record length of "program signature" section */
	fread(&reclen, 4, 1, mps_file);
	reclen = le_read32(&reclen);
	/* Skip reliably over the "program signature" section */
	fseek(mps_file, reclen+1, SEEK_CUR); 
}

/*
 * write out to file 
 * MRCB
 */
static void
mps_fileHeader_w(FILE *mps_file, int mps_ver)
{
	char hdr[100];
	int reclen;

	strcpy (hdr, "MsRc");
	fwrite(hdr, 4, 1, mps_file);

	/* Between versions 3 & 5 this value is 'd', but might change in the future */
	strcpy(hdr, "d");
	fwrite(hdr, 2, 1, mps_file);	/* include trailing NULL char */

	/* Start of a "Data" section */
	hdr[0] = 'D';
	/* if (mps_ver == 3) */
	hdr[1] = 'd';						/* equates to V3.02 */
	if (mps_ver == 4) hdr[1] = 'g';		/* equates to V4.06 */
	if (mps_ver == 5) hdr[1] = 'i';		/* equates to V5.0 */
	hdr[2] = 0;

	reclen = 2;							/* this is 3 byte record */
	le_write32(&reclen, reclen);
	fwrite(&reclen, 4, 1, mps_file);
	fwrite(hdr, 3, 1, mps_file);		/* reclen + 1 */

	hdr[0] = 'A';
	/* if (mps_ver == 3) */
	hdr[1] = 0x2E; hdr[2] = 0x01;		/* equates to V3.02 */
	hdr[3] = 'S'; 
	hdr[4] = 'Q'; 
	hdr[5] = 'A';
	hdr[6] = 0;
	strcpy(hdr+7,"Oct 20 1999");
	strcpy(hdr+19,"12:50:03");
	if (mps_ver == 4) {
		hdr[1] = 0x96;					/* equates to V4.06 */
		strcpy(hdr+7,"Oct 22 2001");
		strcpy(hdr+19,"15:45:05");
	}
	if (mps_ver == 5) {
		hdr[1] = 0xF4;					/* equates to V5.0 */
		strcpy(hdr+7,"Jul  3 2003");
		strcpy(hdr+19,"08:35:39");
	}

	reclen = 27;						/* pre measured! */
	le_write32(&reclen, reclen);
	fwrite(&reclen, 4, 1, mps_file);
	fwrite(hdr, 28, 1, mps_file);		/* reclen + 1  - can't use this as reclen may be wrongendian now */
}

/*
 * read in from file a mapsetname record
 * there should always be one of these at the end of the file
 * MRCB
 */
static int
mps_mapsetname_r(FILE *mps_file, int mps_ver)
{
	char hdr[100];
	int reclen;

	fread(&reclen, 4, 1, mps_file);
	reclen = le_read32(&reclen);

	fread(hdr, 1, 1, mps_file);
	if (hdr[0] == 'V') {
		/* this IS a mapsetname */
		
		/* At the moment we're not doing anything with mapsetnames, but here's the template code as if we were
		mps_readstr(mps_file, hdr, sizeof(hdr));
		char mapsetnamename[very large number?];
		strcpy(mapsetnamename,hdr);
		char mapsetnameAutonameFlag;
		fread(&mapsetnameAutonameFlag, 1, 1, mps_file); */

		fseek( mps_file, reclen, SEEK_CUR); 
		return ISME;
	}
	else {
		/* Not a mapsetname */
		fseek( mps_file, -5, SEEK_CUR); 
		return NOTME;
	}
}


/*
 * write out to file a mapsetname record
 * there should always be one of these at the end of the file
 * MRCB
 */
static void
mps_mapsetname_w(FILE *mps_file, int mps_ver)
{
	char hdr[100];
	int reclen;

	hdr[0] = 'V';	/* mapsetname start of record indicator			*/
	hdr[1] = 0;		/* zero length null terminated string			*/
	hdr[2] = 1;		/* mapsetname autoname flag set to DO autoname	*/
	reclen = 2;		/* three bytes of the V record					*/
	le_write32(&reclen, reclen);
	fwrite(&reclen, 4, 1, mps_file);
	fwrite(hdr, 3, 1, mps_file);		/* reclen + 1 */
}


/*
 * read in from file a waypoint record
 * MRCB
 */
static void
mps_waypoint_r(FILE *mps_file, int mps_ver, waypoint **wpt)
{
	char tbuf[100];
	char wptname[256];
	char wptdesc[256];
	int lat;
	int lon;
	int	icon;

	waypoint	*thisWaypoint;
	double	mps_altitude = unknown_alt;
	double	mps_proximity = unknown_alt;
	double	mps_depth = unknown_alt;

	thisWaypoint = waypt_new();
	*wpt = thisWaypoint;

	mps_readstr(mps_file, wptname, sizeof(wptname));

	if ((mps_ver == 4) || (mps_ver == 5)) {
		fread(tbuf, 4, 1, mps_file);				/* class */
		mps_readstr(mps_file, tbuf, sizeof(tbuf));	/* country */
	}

	fread(tbuf, 22, 1, mps_file);					/* unknown */

	fread(&lat, 4, 1, mps_file); 
	fread(&lon, 4, 1, mps_file); 
	lat = le_read32(&lat);
	lon = le_read32(&lon);
	
	fread(tbuf, 1, 1, mps_file);					/* altitude validity */
	if (tbuf[0] == 1) {
		fread(&mps_altitude,sizeof(mps_altitude),1,mps_file);
	}
	else {
		mps_altitude = unknown_alt;
		fread(tbuf,sizeof(mps_altitude),1, mps_file);
	}

	mps_readstr(mps_file, wptdesc, sizeof(wptdesc));

	fread(tbuf, 1, 1, mps_file);			/* proximity validity */
	if (tbuf[0] == 1) {
		fread(&mps_proximity,sizeof(mps_proximity),1,mps_file);
	}
	else {
		mps_proximity = unknown_alt;
		fread(tbuf,sizeof(mps_proximity),1, mps_file);
	}

	fread(tbuf, 4, 1, mps_file);			/* display flag */
	fread(tbuf, 4, 1, mps_file);			/* colour */
	fread(&icon, 4, 1, mps_file);			/* display symbol */
	icon = le_read32(&icon);

	mps_readstr(mps_file, tbuf, sizeof(tbuf));		/* city */
	mps_readstr(mps_file, tbuf, sizeof(tbuf));		/* state */
	mps_readstr(mps_file, tbuf, sizeof(tbuf));		/*facility */

	fread(tbuf, 1, 1, mps_file);			/* unknown */

	fread(tbuf, 1, 1, mps_file);			/* depth validity */
	if (tbuf[0] == 1) {
		fread(&mps_depth,sizeof(mps_depth),1,mps_file);
	}
	else {
		mps_depth = unknown_alt;
		fread(tbuf,sizeof(mps_depth),1, mps_file);
	}

	if ((mps_ver == 4) || (mps_ver == 5)) {
		fread(tbuf, 7, 1, mps_file);		/* unknown */
	}
	else {
		fread(tbuf, 2, 1, mps_file);		/* unknown */
	}

	thisWaypoint->shortname = xstrdup(wptname);
	thisWaypoint->description = xstrdup(wptdesc);
	thisWaypoint->latitude = lat / 2147483648.0 * 180.0;
	thisWaypoint->longitude = lon / 2147483648.0 * 180.0;
	thisWaypoint->altitude = mps_altitude;

	/* might need to change this to handle version dependent icon handling */
	thisWaypoint->icon_descr = mps_find_desc_from_icon_number(icon, MAPSOURCE);
	waypt_add(thisWaypoint);

	return;

}

/*
 * write out to file a waypoint record
 * MRCB
 */
static void
mps_waypoint_w(FILE *mps_file, int mps_ver, const waypoint *wpt)
{
	unsigned char hdr[100];
	int reclen;
	int lat = wpt->latitude  / 180.0 * 2147483648.0;
	int lon = wpt->longitude  / 180.0 * 2147483648.0;
	int	icon;
	char *src;
	char *ident;
	char zbuf[25];
	char ffbuf[25];
	int display = 1;
	int colour = 0;			/*  (unknown colour) black is 1, white is 16 */

	double	mps_altitude = wpt->altitude;
	double	mps_proximity = unknown_alt;
	double	mps_depth = unknown_alt;

	if(wpt->description) src = wpt->description;
	if(wpt->notes) src = wpt->notes;
	ident = global_opts.synthesize_shortnames ?
				mkshort(mkshort_handle, src) :
				wpt->shortname;

	memset(zbuf, 0, sizeof(zbuf));
	memset(ffbuf, 0xff, sizeof(ffbuf));

	/* might need to change this to handle version dependent icon handling */
	icon = mps_find_icon_number_from_desc(wpt->icon_descr, MAPSOURCE);

	if (get_cache_icon(wpt) && wpt->icon_descr && (strcmp(wpt->icon_descr, "Geocache Found") != 0)) {
		icon = mps_find_icon_number_from_desc(get_cache_icon(wpt), MAPSOURCE);
	}

	icon = mps_converted_icon_number(icon, mps_ver, MAPSOURCE);

	/* two NULL (0x0) bytes at end of each string */
	reclen = strlen(ident) + ((wpt->description) ? strlen(wpt->description) : 0) + 2;	
	if ((mps_ver == 4) || (mps_ver == 5)) {
		/* v4.06 & V5.0*/
		reclen += 85;				/* "W" (1) + strlen(name) + NULL (1) + class(4) + country(sz) +  
										unknown(22) + lat(4) + lon(4) + alt(9) + strlen(desc) + NULL (1) + 
										 prox(9) + display(4) + colour(4) + symbol(4) + city(sz) + state(sz) + 
										facility(sz) + unknown2(1) + depth(9) + unknown3(7) */
									/* -1 as reclen is interpreted from zero meaning a reclength of one */
	}
	else {
		/* v3.02 */
		reclen += 75;				/* "W" (1) + strlen(name) + NULL (1) + unknown(22) + lat(4) +  
										lon(4) + alt(9) + strlen(desc) + NULL (1) + prox(9) + display(4) + 
										colour(4) + symbol(4) + city(sz) + state(sz) + facility(sz) + 
										unknown2(1) + depth(9) + unknown3(2) */
									/* -1 as reclen is interpreted from zero meaning a reclength of one */
	}

	le_write32(&reclen, reclen);
	fwrite(&reclen, 4, 1, mps_file);
	fwrite("W", 1, 1, mps_file);
	fputs(ident, mps_file);
	fwrite(zbuf, 1, 1, mps_file);	/* NULL termination to ident */

	if ((mps_ver == 4) || (mps_ver == 5)) {
		fwrite(zbuf, 4, 1, mps_file);	/* class */
		fwrite(zbuf, 1, 1, mps_file);	/* country */
		fwrite(zbuf, 4, 1, mps_file);	/* unknown */
		fwrite(ffbuf, 12, 1, mps_file);	/* unknown */
		fwrite(zbuf, 2, 1, mps_file);	/* unknown */
		fwrite(ffbuf, 4, 1, mps_file);	/* unknown */
	}
	else {
		fwrite(zbuf, 13, 1, mps_file);
		fwrite(ffbuf, 8, 1, mps_file);
		fwrite(zbuf, 1, 1, mps_file);
	}

	le_write32(&lat, lat);
	le_write32(&lon, lon);
	fwrite(&lat, 4, 1, mps_file);
	fwrite(&lon, 4, 1, mps_file);

	if (mps_altitude == unknown_alt) {
		fwrite(zbuf, 9, 1, mps_file);
	}
	else {
		hdr[0] = 1;
		fwrite(hdr, 1 , 1, mps_file);
		fwrite(&mps_altitude, 8 , 1, mps_file);
	}

	if (wpt->description) fputs(wpt->description, mps_file);
	fwrite(zbuf, 1, 1, mps_file);	/* NULL termination */

	if (mps_proximity == unknown_alt) {
		fwrite(zbuf, 9, 1, mps_file);
	}
	else {
		hdr[0] = 1;
		fwrite(hdr, 1 , 1, mps_file);
		fwrite(&mps_proximity, 8 , 1, mps_file);
	}

	le_write32(&display, display);
	fwrite(&display, 4, 1, mps_file);	/* Show waypoint w/ name */

	le_write32(&colour, colour);
	fwrite(&colour, 4, 1, mps_file);

	le_write32(&icon, icon);
	fwrite(&icon, 4, 1, mps_file);

	fwrite(zbuf, 3, 1, mps_file);		/* city, state, facility */

	fwrite(zbuf, 1, 1, mps_file);		/* unknown */

	if (mps_depth == unknown_alt) {
		fwrite(zbuf, 9, 1, mps_file);
	}
	else {
		hdr[0] = 1;
		fwrite(hdr, 1 , 1, mps_file);
		fwrite(&mps_depth, 8 , 1, mps_file);
	}

	fwrite(zbuf, 2, 1, mps_file);		/* unknown */
	if ((mps_ver == 4) || (mps_ver == 5)) {
		fwrite(zbuf, 5, 1, mps_file);	/* unknown */
	}
}

static void
mps_waypoint_w_wrapper(const waypoint *wpt)
{
	mps_waypoint_w(mps_file_out, mps_ver_out, wpt);
}

/*
 * read in from file a route record
 * MRCB
 */
static void
mps_route_r(FILE *mps_file, int mps_ver, route_head **rte)
{
	char tbuf[100];
	char rtename[256];
	char wptname[256];
	int lat;
	int lon;
	short int	rte_autoname = 0;
	int	interlinkStepCount;
	int	thisInterlinkStep;

	time_t	dateTime = 0;
	route_head *rte_head;
	unsigned int rte_count;

	waypoint	*thisWaypoint;
	waypoint	*tempWpt;

	double	mps_altitude = unknown_alt;
	double	mps_depth = unknown_alt;


	mps_readstr(mps_file, rtename, sizeof(rtename));
	fread(&rte_autoname, 2, 1, mps_file);	/* autoname flag */
	rte_autoname = le_read16(&rte_autoname);

	fread(&lat, 4, 1, mps_file); 
	fread(&lon, 4, 1, mps_file); 
	lat = le_read32(&lat);			/* max lat of whole route */
	lon = le_read32(&lon);			/* max lon of whole route */

	fread(tbuf, 1, 1, mps_file);			/* altitude validity */
	if (tbuf[0] == 1) {
		fread(&mps_altitude,sizeof(mps_altitude),1,mps_file);	/* max alt of the whole route */
	}
	else {
		mps_altitude = unknown_alt;
		fread(tbuf,sizeof(mps_altitude),1, mps_file);
	}

	fread(&lat, 4, 1, mps_file); 
	fread(&lon, 4, 1, mps_file); 
	lat = le_read32(&lat);			/* min lat of whole route */
	lon = le_read32(&lon);			/* min lon of whole route */

	fread(tbuf, 1, 1, mps_file);			/* altitude validity */
	if (tbuf[0] == 1) {
		fread(&mps_altitude,sizeof(mps_altitude),1,mps_file);	/* min alt of the whole route */
	}
	else {
		mps_altitude = unknown_alt;
		fread(tbuf,sizeof(mps_altitude),1, mps_file);
	}

	fread(&rte_count, 4, 1, mps_file);			/* number of waypoints in route */
	rte_count = le_read32(&rte_count);

	rte_head = route_head_alloc();
	rte_head->rte_name = xstrdup(rtename);
	route_add_head(rte_head);
	*rte = rte_head;

	rte_count--;			/* need to loop round for one less than the number of waypoints */

	while (rte_count--) {

		mps_readstr(mps_file, wptname, sizeof(wptname));
		if ((mps_ver == 4) || (mps_ver == 5)) {
			fread(tbuf, 4, 1, mps_file);				/* class */
			mps_readstr(mps_file, tbuf, sizeof(tbuf));	/* country */
			fread(tbuf, 22, 1, mps_file);				/* unknown */
			fread(tbuf, 19, 1, mps_file);				/* unknown */
		}
		else {
			fread(tbuf, 22, 1, mps_file);				/* unknown */
			fread(tbuf, 18, 1, mps_file);				/* unknown */
		}

		/* link details */
		fread(&interlinkStepCount, 4, 1, mps_file);					/* supposedly always 2 */
		interlinkStepCount = le_read32(&interlinkStepCount);
		/* first end of link */
		fread(&lat, 4, 1, mps_file); 
		fread(&lon, 4, 1, mps_file); 
		lat = le_read32(&lat);
		lon = le_read32(&lon);
	
		fread(tbuf, 1, 1, mps_file);			/* altitude validity */
		if (tbuf[0] == 1) {
			fread(&mps_altitude,sizeof(mps_altitude),1,mps_file);
		}
		else {
			mps_altitude = unknown_alt;
			fread(tbuf,sizeof(mps_altitude),1, mps_file);
		}

		/* with MapSource routes, the real waypoint details are held as a separate waypoint, so copy from there
		   if found */
		tempWpt = find_waypt_by_name(wptname);

		if (tempWpt != NULL) {
			thisWaypoint = waypt_dupe(tempWpt);
		}
		else {
			thisWaypoint = xcalloc(sizeof(*thisWaypoint), 1);
			thisWaypoint->shortname = xstrdup(wptname);
			thisWaypoint->latitude = lat / 2147483648.0 * 180.0;
			thisWaypoint->longitude = lon / 2147483648.0 * 180.0;
			thisWaypoint->altitude = mps_altitude;
		}

		route_add_wpt(rte_head, thisWaypoint);

		/* take two off the count since we separately read the start and end parts of the link */
		for (thisInterlinkStep = interlinkStepCount - 2; thisInterlinkStep > 0; thisInterlinkStep--) {
			/* Could do this by doing a calculation on length of each co-ordinate and just doing one read
			   but doing it this way makes it easier in the future to make use of this data */
			fread(tbuf, 4, 1, mps_file);	/* lat */
			fread(tbuf, 4, 1, mps_file);	/* lon */
			fread(tbuf, 1, 1, mps_file);	/* altitude validity */
			fread(tbuf, 8, 1, mps_file);	/* altitude */
		}

		/* other end of link */
		fread(&lat, 4, 1, mps_file); 
		fread(&lon, 4, 1, mps_file); 
		lat = le_read32(&lat);
		lon = le_read32(&lon);
	
		fread(tbuf, 1, 1, mps_file);			/* altitude validity */
		if (tbuf[0] == 1) {
			fread(&mps_altitude,sizeof(mps_altitude),1,mps_file);
		}
		else {
			mps_altitude = unknown_alt;
			fread(tbuf,sizeof(mps_altitude),1, mps_file);
		}

		fread(tbuf, 1, 1, mps_file);			/* NULL */

		fread(tbuf, 4, 1, mps_file);			/* link max lat */
		fread(tbuf, 4, 1, mps_file);			/* link max lon */
		fread(tbuf, 9, 1, mps_file);			/* link max alt validity + alt */

		fread(tbuf, 4, 1, mps_file);			/* link min lat */
		fread(tbuf, 4, 1, mps_file);			/* link min lon */
		fread(tbuf, 9, 1, mps_file);			/* link min alt validity + alt */

	}		/* while (trk_count--) */

	/* when the loop is done, there's still one waypoint to read with a small trailer */
	/* all we want is the waypoint name; lat, lon and alt are already set from above  */
	mps_readstr(mps_file, wptname, sizeof(wptname));

	if ((mps_ver == 4) || (mps_ver == 5)) {
		fread(tbuf, 4, 1, mps_file);				/* class */
		mps_readstr(mps_file, tbuf, sizeof(tbuf));	/* country */
		fread(tbuf, 22, 1, mps_file);				/* unknown */
		fread(tbuf, 19, 1, mps_file);				/* unknown */
	}
	else {
		fread(tbuf, 22, 1, mps_file);				/* unknown */
		fread(tbuf, 18, 1, mps_file);				/* unknown */
	}

	fread(tbuf, 5, 1, mps_file);					/* 5 byte trailer */
	/* with MapSource routes, the real waypoint details are held as a separate waypoint, so copy from there
		if found */
	tempWpt = find_waypt_by_name(wptname);

	if (tempWpt != NULL) {
		thisWaypoint = waypt_dupe(tempWpt);
	}
	else {
		thisWaypoint = xcalloc(sizeof(*thisWaypoint), 1);
		thisWaypoint->shortname = xstrdup(wptname);
		thisWaypoint->latitude = lat / 2147483648.0 * 180.0;
		thisWaypoint->longitude = lon / 2147483648.0 * 180.0;
		thisWaypoint->altitude = mps_altitude;
	}

	route_add_wpt(rte_head, thisWaypoint);

	return;
}

/*
 * write out to file a route header
 * MRCB
 */
static void
mps_routehdr_w(FILE *mps_file, int mps_ver, const route_head *rte)
{
	unsigned int reclen;
	unsigned int rte_datapoints;
	unsigned int colour = 0;		/* unknown colour */
	int			rname_len;
	char		*rname;
	char		hdr[20];
	char		zbuf[20];
	char		*src;
	char		*ident;

	waypoint	*testwpt;
	time_t		uniqueValue;
	int			allWptNameLengths;

	double		maxlat=-90.0;
	double		maxlon=-180.0;
	double		minlat=90.0;
	double		minlon=180.0;
	double		maxalt=unknown_alt;
	double		minalt=unknown_alt;

	int lat;
	int lon;

	queue *elem, *tmp;

	prevRouteWpt = NULL;		/* clear the stateful flag used to know when the start of route wpts happens */

	memset(zbuf, 0, sizeof(zbuf));

	/* total nodes (waypoints) this route */
	rte_datapoints = 0;
	allWptNameLengths = 0;

	if (rte->waypoint_list.next) {		/* this test doesn't do what I want i.e test if this is a valid route - treat as a placeholder for now */
		QUEUE_FOR_EACH(&rte->waypoint_list, elem, tmp) {
			testwpt = (waypoint *)elem;
			if (rte_datapoints == 0) {
				uniqueValue = testwpt->creation_time;
			}
			if (testwpt->latitude > maxlat) maxlat = testwpt->latitude;
			if (testwpt->latitude < minlat) minlat = testwpt->latitude;
			if (testwpt->longitude > maxlon) maxlon = testwpt->longitude;
			if (testwpt->longitude < minlon) minlon = testwpt->longitude;
			if (testwpt->altitude != unknown_alt) {
				if ((testwpt->altitude > maxalt) || 
					(maxalt == unknown_alt)) maxalt = testwpt->altitude;
				if ((testwpt->altitude < minalt) ||
					(minalt == unknown_alt)) minalt = testwpt->altitude;
			}

			if(testwpt->description) src = testwpt->description;
			if(testwpt->notes) src = testwpt->notes;
			ident = global_opts.synthesize_shortnames ?
						mkshort(mkshort_handle, src) :
						testwpt->shortname;
			allWptNameLengths += strlen(ident) + 1;

			rte_datapoints++;
		}		

		if (uniqueValue == 0) {
			uniqueValue = time(NULL);
		}

		/* route name */
		if (!rte->rte_name) {
			sprintf(hdr, "Route%04x", uniqueValue);
			rname = xstrdup(hdr);
		}
		else
			rname = xstrdup(rte->rte_name);

		rname_len = strlen(rname);
		reclen = rname_len + 42;		/* "T" (1) + strlen(tname) + NULL (1) + autoname flag (2) + 
										route lat lon max (2x4) + route max alt (9) +
										route lat lon min (2x4) + route min alt (9) +
										num route datapoints value (4) */
		
		/* V3 - each waypoint: waypoint name + NULL (1) + unknown (22) + unknown (18) */
		/* V4,5 - each waypoint: waypoint name + NULL (1) + class (4) + country + NULL (1) + 
								unknown (22) + unknown (19) */
		/* V* - each route link: 0x00000002 (4) + end 1 lat (4) + end 1 lon (4) + end 1 alt (9) +
								end 2 lat (4) + end 2 lon (4) + end 2 alt (9) + NULL (1) +
								link max lat (4) + link max lon (4) + link max alt (9) +
								link min lat (4) + link min lon (4) + link min alt (9) */

		if ((mps_ver == 4) || (mps_ver == 5)) {
			reclen += allWptNameLengths + rte_datapoints * 46 + 
					(rte_datapoints - 1) * 73 + 4;				/* link details plus overall trailing bytes */
		}
		else {
			reclen += allWptNameLengths + rte_datapoints * 40 + 
					(rte_datapoints - 1) * 73 + 4;				/* link details plus overall trailing bytes */
		}

		le_write32(&reclen, reclen);
		fwrite(&reclen, 4, 1, mps_file);

		hdr[0] = 'R';
		fwrite(hdr, 1, 1, mps_file);

		fwrite(rname, rname_len, 1, mps_file);
		xfree(rname);

		hdr[0] = 0;						/* NULL of string termination */
		hdr[1] = 0;						/* don't autoname */
		hdr[2] = 0;						/* MSB of don't autoname */
		fwrite(hdr, 3, 1, mps_file);	/* NULL string terminator + route autoname flag */

		lat = maxlat / 180.0 * 2147483648.0;
		lon = maxlon / 180.0 * 2147483648.0;

		le_write32(&lat, lat);
		le_write32(&lon, lon);

		fwrite(&lat, 4, 1, mps_file);
		fwrite(&lon, 4, 1, mps_file);

		if (maxalt == unknown_alt) {
			fwrite(zbuf, 9, 1, mps_file);
		}
		else {
			hdr[0] = 1;
			fwrite(hdr, 1 , 1, mps_file);
			fwrite(&maxalt, 8 , 1, mps_file);
		}

		lat = minlat / 180.0 * 2147483648.0;
		lon = minlon / 180.0 * 2147483648.0;

		le_write32(&lat, lat);
		le_write32(&lon, lon);

		fwrite(&lat, 4, 1, mps_file);
		fwrite(&lon, 4, 1, mps_file);

		if (minalt == unknown_alt) {
			fwrite(zbuf, 9, 1, mps_file);
		}
		else {
			hdr[0] = 1;
			fwrite(hdr, 1 , 1, mps_file);
			fwrite(&minalt, 8 , 1, mps_file);
		}

		le_write32(&rte_datapoints, rte_datapoints);
		fwrite(&rte_datapoints, 4, 1, mps_file);
	}
}

static void
mps_routehdr_w_wrapper(const route_head *rte)
{
	mps_routehdr_w(mps_file_out, mps_ver_out, rte);
}


/*
 * write out to file a route datapoint
 * MRCB
 */
static void
mps_routedatapoint_w(FILE *mps_file, int mps_ver, const waypoint *rtewpt)
{
	unsigned char hdr[10];
	int lat;
	int lon;
	time_t	t = rtewpt->creation_time;
	char zbuf[20];
	char ffbuf[20];
	char *src;
	char *ident;
	int			reclen;

	int			maxlat;
	int			maxlon;
	int			minlat;
	int			minlon;
	double		maxalt=unknown_alt;
	double		minalt=unknown_alt;

	double	mps_altitude;

	memset(zbuf, 0, sizeof(zbuf));
	memset(ffbuf, 0xff, sizeof(ffbuf));

	if (prevRouteWpt != NULL) {
		/* output the route link details */
		reclen = 2;
		le_write32(&reclen, reclen);
		fwrite(&reclen, 4, 1, mps_file);

		/* output end point 1 */
		lat = prevRouteWpt->latitude  / 180.0 * 2147483648.0;
		lon = prevRouteWpt->longitude  / 180.0 * 2147483648.0;
		le_write32(&lat, lat);
		le_write32(&lon, lon);

		fwrite(&lat, 4, 1, mps_file);
		fwrite(&lon, 4, 1, mps_file);

		mps_altitude = prevRouteWpt->altitude;
		if (mps_altitude == unknown_alt) {
			fwrite(zbuf, 9, 1, mps_file);
		}
		else {
			hdr[0] = 1;
			fwrite(hdr, 1 , 1, mps_file);
			fwrite(&mps_altitude, 8 , 1, mps_file);
		}

		/* output end point 2 */
		lat = rtewpt->latitude  / 180.0 * 2147483648.0;
		lon = rtewpt->longitude  / 180.0 * 2147483648.0;
		le_write32(&lat, lat);
		le_write32(&lon, lon);

		fwrite(&lat, 4, 1, mps_file);
		fwrite(&lon, 4, 1, mps_file);

		mps_altitude = rtewpt->altitude;
		if (mps_altitude == unknown_alt) {
			fwrite(zbuf, 9, 1, mps_file);
		}
		else {
			hdr[0] = 1;
			fwrite(hdr, 1 , 1, mps_file);
			fwrite(&mps_altitude, 8 , 1, mps_file);
		}

		if (rtewpt->latitude > prevRouteWpt->latitude) {
			maxlat = rtewpt->latitude  / 180.0 * 2147483648.0;
			minlat = prevRouteWpt->latitude  / 180.0 * 2147483648.0;
		}
		else {
			minlat = rtewpt->latitude  / 180.0 * 2147483648.0;
			maxlat = prevRouteWpt->latitude  / 180.0 * 2147483648.0;
		}

		if (rtewpt->longitude > prevRouteWpt->longitude) {
			maxlon = rtewpt->longitude  / 180.0 * 2147483648.0;
			minlon = prevRouteWpt->longitude  / 180.0 * 2147483648.0;
		}
		else {
			minlon = rtewpt->longitude  / 180.0 * 2147483648.0;
			maxlon = prevRouteWpt->longitude  / 180.0 * 2147483648.0;
		}

		if (rtewpt->altitude != unknown_alt) maxalt = rtewpt->altitude;
		if (rtewpt->altitude != unknown_alt) minalt = rtewpt->altitude;
		if (prevRouteWpt->altitude != unknown_alt) {
			if ((prevRouteWpt->altitude > maxalt) || 
				(maxalt == unknown_alt)) maxalt = prevRouteWpt->altitude;
			if ((prevRouteWpt->altitude < minalt) ||
				(minalt == unknown_alt)) minalt = prevRouteWpt->altitude;
		}
		
		fwrite (zbuf, 1, 1, mps_file);

		/* output max coords of the link */
		le_write32(&maxlat, maxlat);
		le_write32(&maxlon, maxlon);

		fwrite(&maxlat, 4, 1, mps_file);
		fwrite(&maxlon, 4, 1, mps_file);

		if (maxalt == unknown_alt) {
			fwrite(zbuf, 9, 1, mps_file);
		}
		else {
			hdr[0] = 1;
			fwrite(hdr, 1 , 1, mps_file);
			fwrite(&maxalt, 8 , 1, mps_file);
		}

		/* output min coords of the link */
		le_write32(&minlat, minlat);
		le_write32(&minlon, minlon);

		fwrite(&minlat, 4, 1, mps_file);
		fwrite(&minlon, 4, 1, mps_file);

		if (minalt == unknown_alt) {
			fwrite(zbuf, 9, 1, mps_file);
		}
		else {
			hdr[0] = 1;
			fwrite(hdr, 1 , 1, mps_file);
			fwrite(&minalt, 8 , 1, mps_file);
		}

	}

	if(rtewpt->description) src = rtewpt->description;
	if(rtewpt->notes) src = rtewpt->notes;
	ident = global_opts.synthesize_shortnames ?
				mkshort(mkshort_handle, src) :
				rtewpt->shortname;

	fputs(ident, mps_file);
	fwrite(zbuf, 1, 1, mps_file);	/* NULL termination to ident */

	if ((mps_ver == 4) || (mps_ver == 5)) {
		/* unknown */
		fwrite(zbuf, 4, 1, mps_file);			/* class */
		fwrite(zbuf, 1, 1, mps_file);			/* country - i.e. empty string */

		/* unknown, exactly as for waypoints */
		fwrite(zbuf, 4, 1, mps_file);
		fwrite(ffbuf, 12, 1, mps_file);
		fwrite(zbuf, 2, 1, mps_file);
		fwrite(ffbuf, 4, 1, mps_file);

		fwrite(zbuf, 1, 1, mps_file);
		hdr[0] = 3;
		fwrite(hdr, 1, 1, mps_file);
		fwrite(zbuf, 17, 1, mps_file);
	}
	else {
		/* unknown, exactly as for waypoints */
		fwrite(zbuf, 13, 1, mps_file);
		fwrite(ffbuf, 8, 1, mps_file);
		fwrite(zbuf, 1, 1, mps_file);


		/* unknown */
		fwrite(zbuf, 1, 1, mps_file);
		hdr[0] = 3;
		fwrite(hdr, 1, 1, mps_file);
		fwrite(zbuf, 16, 1, mps_file);
	}

	prevRouteWpt = rtewpt;

}

static void
mps_routedatapoint_w_wrapper(const waypoint *rte)
{
	mps_routedatapoint_w(mps_file_out, mps_ver_out, rte);
}


/*
 * write out to file a route trailer
 * MRCB
 */
static void
mps_routetrlr_w(FILE *mps_file, int mps_ver, const route_head *rte)
{
	char		hdr[2];
	int			value = 0;

	hdr[0] = 1;

	if (rte->waypoint_list.next) {		/* this test doesn't do what I want i.e test if this is a valid route - treat as a placeholder for now */
		fwrite(&value, 4, 1, mps_file);
		fwrite(hdr, 1, 1, mps_file);
	}
}
	
static void
mps_routetrlr_w_wrapper(const route_head *rte)
{
	mps_routetrlr_w(mps_file_out, mps_ver_out, rte);
}


/*
 * read in from file a route record
 * MRCB
 */
static void
mps_track_r(FILE *mps_file, int mps_ver, route_head **trk)
{
	char tbuf[100];
	char trkname[256];
	int lat;
	int lon;

	int	dateTime = 0;
	route_head *track_head;
	unsigned int trk_count;

	waypoint	*thisWaypoint;
	double	mps_altitude = unknown_alt;
	double	mps_depth = unknown_alt;

	mps_readstr(mps_file, trkname, sizeof(trkname));
	fread(tbuf, 1, 1, mps_file);				/* display flag */
	fread(tbuf, 4, 1, mps_file);				/* colour */

	fread(&trk_count, 4, 1, mps_file);			/* number of datapoints in tracklog */
	trk_count = le_read32(&trk_count);

	track_head = route_head_alloc();
	track_head->rte_name = xstrdup(trkname);
	track_add_head(track_head);
	*trk = track_head;

	while (trk_count--) {

		fread(&lat, 4, 1, mps_file); 
		fread(&lon, 4, 1, mps_file); 
		lat = le_read32(&lat);
		lon = le_read32(&lon);
	
		fread(tbuf, 1, 1, mps_file);			/* altitude validity */
		if (tbuf[0] == 1) {
			fread(&mps_altitude,sizeof(mps_altitude),1,mps_file);
		}
		else {
			mps_altitude = unknown_alt;
			fread(tbuf,sizeof(mps_altitude),1, mps_file);
		}

		fread(tbuf, 1, 1, mps_file);			/* date/time validity */
		if (tbuf[0] == 1) {
			fread(&dateTime,sizeof(dateTime),1,mps_file);
		}
		else {
			fread(tbuf,sizeof(dateTime),1, mps_file);
		}

		fread(tbuf, 1, 1, mps_file);			/* depth validity */
		if (tbuf[0] == 1) {
			fread(&mps_depth,sizeof(mps_depth),1,mps_file);
		}
		else {
			mps_depth = unknown_alt;
			fread(tbuf,sizeof(mps_depth),1, mps_file);
		}

		thisWaypoint = xcalloc(sizeof(*thisWaypoint), 1);
		thisWaypoint->latitude = lat / 2147483648.0 * 180.0;
		thisWaypoint->longitude = lon / 2147483648.0 * 180.0;
		thisWaypoint->creation_time = le_read32(&dateTime);
		thisWaypoint->centiseconds = 0;
		thisWaypoint->altitude = mps_altitude;
		route_add_wpt(track_head, thisWaypoint);

	}		/* while (trk_count--) */

	return;

}

/*
 * write out to file a tracklog header
 * MRCB
 */
static void
mps_trackhdr_w(FILE *mps_file, int mps_ver, const route_head *trk)
{
	unsigned int reclen;
	unsigned int trk_datapoints;
	unsigned int colour = 0;		/* unknown colour */
	int			tname_len;
	char		*tname;
	char		hdr[20];
	waypoint	*testwpt;
	time_t		uniqueValue;

	queue *elem, *tmp;

	/* total nodes (waypoints) this track */
	trk_datapoints = 0;
	if (trk->waypoint_list.next) {	/* this test doesn't do what I want i.e test if this is a valid track - treat as a placeholder for now */
		QUEUE_FOR_EACH(&trk->waypoint_list, elem, tmp) {
			if (trk_datapoints == 0) {
				testwpt = (waypoint *)elem;
				uniqueValue = testwpt->creation_time;
			}
			trk_datapoints++;
		}

		if (uniqueValue == 0) {
			uniqueValue = time(NULL);
		}

		/* track name */
		if (!trk->rte_name) {
			sprintf(hdr, "Track%04x", uniqueValue);
			tname = xstrdup(hdr);
		}
		else
			tname = xstrdup(trk->rte_name);

		tname_len = strlen(tname);
		reclen = tname_len + 11;		/* "T" (1) + strlen(tname) + NULL (1) + display flag (1) + colour (4) +
										num track datapoints value (4) */
		
		reclen += (trk_datapoints * 31) - 1;	/* lat (4) + lon (4) + alt (9) + date (5) + depth (9) ;*/
												/* -1 is because reclen starts from 0 which means a length of 1 */
		le_write32(&reclen, reclen);
		fwrite(&reclen, 4, 1, mps_file);

		hdr[0] = 'T';
		fwrite(hdr, 1, 1, mps_file);

		fwrite(tname, tname_len, 1, mps_file);
		xfree(tname);

		hdr[0] = 0;
		hdr[1] = 1;
		fwrite(hdr, 2, 1, mps_file);	/* NULL string terminator + display flag */

		le_write32(&colour, colour);
		fwrite(&colour, 4, 1, mps_file);

		le_write32(&trk_datapoints, trk_datapoints);
		fwrite(&trk_datapoints, 4, 1, mps_file);
	}

}

static void
mps_trackhdr_w_wrapper(const route_head *trk)
{
	mps_trackhdr_w(mps_file_out, mps_ver_out, trk);
}


/*
 * write out to file a tracklog datapoint
 * MRCB
 */
static void
mps_trackdatapoint_w(FILE *mps_file, int mps_ver, const waypoint *wpt)
{
	unsigned char hdr[10];
	int lat = wpt->latitude  / 180.0 * 2147483648.0;
	int lon = wpt->longitude  / 180.0 * 2147483648.0;
	time_t	t = wpt->creation_time;
	char zbuf[10];

	double	mps_altitude = wpt->altitude;
	double	mps_proximity = unknown_alt;
	double	mps_depth = unknown_alt;

	memset(zbuf, 0, sizeof(zbuf));

	le_write32(&lat, lat);
	le_write32(&lon, lon);
	fwrite(&lat, 4, 1, mps_file);
	fwrite(&lon, 4, 1, mps_file);

	if (mps_altitude == unknown_alt) {
		fwrite(zbuf, 9, 1, mps_file);
	}
	else {
		hdr[0] = 1;
		fwrite(hdr, 1 , 1, mps_file);
		fwrite(&mps_altitude, 8 , 1, mps_file);
	}

	if (t > 0) {					/* a valid time is assumed to > 0 */
		hdr[0] = 1;
		fwrite(hdr, 1 , 1, mps_file);
		le_write32(&t, t);
		fwrite(&t, 4, 1, mps_file);
	}
	else {
		fwrite(zbuf, 5, 1, mps_file);
	}

	if (mps_depth == unknown_alt) {
		fwrite(zbuf, 9, 1, mps_file);
	}
	else {
		hdr[0] = 1;
		fwrite(hdr, 1 , 1, mps_file);
		fwrite(&mps_depth, 8 , 1, mps_file);
	}
}

static void
mps_trackdatapoint_w_wrapper(const waypoint *wpt)
{
	mps_trackdatapoint_w(mps_file_out, mps_ver_out, wpt);
}


static void
mps_read(void)
{
	waypoint	*wpt;
	route_head	*rte;
	route_head	*trk;

	char		recType;
	int			reclen;
	int			morework;

	mps_ver_in = 0;		/* although initialised at declaration, what happens if there are two mapsource
						   input files? */
	mps_fileHeader_r(mps_file_in, &mps_ver_in);

#ifdef DUMP_ICON_TABLE
	printf("static icon_mapping_t icon_table[] = {\n");
#endif

	morework = 1;
	while (morework && !feof(mps_file_in)) {

		/* Read record length of next section */
		fread(&reclen, 4, 1, mps_file_in);
		reclen = le_read32(&reclen);

		/* Read the record type "flag" in - using fread in case in the future need more than one char */
		fread(&recType, 1, 1, mps_file_in);
		switch (recType) {
		case 'W':
			/* Waypoint record */
			/* With routes, we need the waypoint info that reveals, for example, the symbol type */
			mps_waypoint_r(mps_file_in, mps_ver_in, &wpt);
#ifdef DUMP_ICON_TABLE
			printf("\t{  %4u, \"%s\" },\n", icon, wpt->shortname);
#endif
			break;

		case 'R':
			/* Route record */
			mps_route_r(mps_file_in, mps_ver_in, &rte);
			break;

		case 'T':
			/* Track record */
			mps_track_r(mps_file_in, mps_ver_in, &trk);
			break;

		case 'V':
			/* Mapset record */
			mps_mapsetname_r(mps_file_in, mps_ver_in);
			/* Last record in the file */
			morework = 0;
			break;
		default:
			/* Unknown record type.  Skip over it. */
			fseek(mps_file_in, reclen, SEEK_CUR); 
		}

	}	/* while (!feof(mps_file_in)) */

#ifdef DUMP_ICON_TABLE
	printf("\t{ -1, NULL },\n");
	printf("};\n");
#endif

	return ;

}

void
mps_write(void)
{
	int			short_length;
	waypoint	*wpt;
	route_head	*rte;
	route_head	*trk;

	char		recType;
	int			reclen;
	int			reclen2;
	unsigned int			tocopy;

	unsigned char	copybuf[8192];

	if (snlen)
		short_length = atoi(snlen);
	else
		short_length = 10;

	if (mpsmergeout) {
		/* need to skip over the merging header and test merge version */
		mps_fileHeader_r(mps_file_temp, &mps_ver_temp);

		if (mpsverout) {
			if (mps_ver_temp != atoi(mpsverout)) {
				fclose(mps_file_temp);
				fclose(mps_file_out);
				remove(origname);
				rename(tempname,origname);
				fatal (MYNAME ": merge source version is %d, requested out version is %d\n", mps_ver_temp, atoi(mpsverout));
			}
		}
		else {
			mpsverout = xmalloc(10);
			sprintf(mpsverout,"%d", mps_ver_temp);
		}
	}

	if (mpsverout)
		mps_ver_out = atoi(mpsverout);
	else
		mps_ver_out = 3;

	mkshort_handle = mkshort_new_handle();

	setshort_length(mkshort_handle, short_length);
	setshort_whitespace_ok(mkshort_handle, 0);

	mps_fileHeader_w(mps_file_out, mps_ver_out);

	if ((mpsmergeout) && (global_opts.objective != wptdata)) {
		while (!feof(mps_file_temp)) {

			fread(&reclen, 4, 1, mps_file_temp);
			reclen2 = le_read32(&reclen);

			/* Read the record type "flag" in - using fread in case in the future need more than one char */
			fread(&recType, 1, 1, mps_file_temp);

			if (recType == 'W')  {
				fwrite(&reclen, 4, 1, mps_file_out);	/* write out untouched */
				fwrite(&recType, 1, 1, mps_file_out);

				for(tocopy = reclen2; tocopy > 0; tocopy -= sizeof(copybuf)) {
					fread(copybuf, (tocopy > sizeof(copybuf) ? sizeof(copybuf) : tocopy), 1, mps_file_temp);
					fwrite(copybuf, (tocopy > sizeof(copybuf) ? sizeof(copybuf) : tocopy), 1, mps_file_out);
				}
			}
			else break;
		}	/* while (!feof(mps_file_temp)) */
	}	/* if (mpsmergeout) */

	if (global_opts.objective == wptdata) {

		if (mpsmergeout) {
			/* since we're processing waypoints, we should read in from whatever version and write out */
			/* in the selected version */
			while (!feof(mps_file_temp)) {

				fread(&reclen, 4, 1, mps_file_temp);
				reclen2 = le_read32(&reclen);

				/* Read the record type "flag" in - using fread in case in the future need more than one char */
				fread(&recType, 1, 1, mps_file_temp);

				if (recType == 'W')  {
					mps_waypoint_r(mps_file_temp, mps_ver_temp, &wpt);
				}
				else break;
			}
		}
		waypt_disp_all(mps_waypoint_w_wrapper);
	}

	if ((mpsmergeout) && (global_opts.objective != rtedata)) {
		while (!feof(mps_file_temp)) {

			if (recType == 'R')  {
				fwrite(&reclen, 4, 1, mps_file_out);	/* write out untouched */
				fwrite(&recType, 1, 1, mps_file_out);

				for(tocopy = reclen2; tocopy > 0; tocopy -= sizeof(copybuf)) {
					fread(copybuf, (tocopy > sizeof(copybuf) ? sizeof(copybuf) : tocopy), 1, mps_file_temp);
					fwrite(copybuf, (tocopy > sizeof(copybuf) ? sizeof(copybuf) : tocopy), 1, mps_file_out);
				}
			}
			else break;
			fread(&reclen, 4, 1, mps_file_temp);
			reclen2 = le_read32(&reclen);

			/* Read the record type "flag" in - using fread in case in the future need more than one char */
			fread(&recType, 1, 1, mps_file_temp);

		}	/* while (!feof(mps_file_temp)) */
	}	/* if (mpsmergeout) */

	if (global_opts.objective == rtedata) {

		if (mpsmergeout) {
			/* since we're processing routes, we should read in from whatever version and write out */
			/* in the selected version */
			while (!feof(mps_file_temp)) {

				if (recType == 'R')  {
					mps_route_r(mps_file_temp, mps_ver_temp, &rte);
				}
				else break;

				fread(&reclen, 4, 1, mps_file_temp);
				reclen2 = le_read32(&reclen);

				/* Read the record type "flag" in - using fread in case in the future need more than one char */
				fread(&recType, 1, 1, mps_file_temp);
			}
		}
		/* need to make sure there is a "real" waypoint for each route waypoint */
		route_disp_all(mps_noop, mps_noop, mps_waypoint_w_wrapper);
		route_disp_all(mps_routehdr_w_wrapper, mps_routetrlr_w_wrapper, mps_routedatapoint_w_wrapper);
	}

	if ((mpsmergeout) && (global_opts.objective != trkdata)) {
		while (!feof(mps_file_temp)) {

			if (recType == 'T')  {
				fwrite(&reclen, 4, 1, mps_file_out);	/* write out untouched */
				fwrite(&recType, 1, 1, mps_file_out);

				for(tocopy = reclen2; tocopy > 0; tocopy -= sizeof(copybuf)) {
					fread(copybuf, (tocopy > sizeof(copybuf) ? sizeof(copybuf) : tocopy), 1, mps_file_temp);
					fwrite(copybuf, (tocopy > sizeof(copybuf) ? sizeof(copybuf) : tocopy), 1, mps_file_out);
				}
			}
			else break;
			fread(&reclen, 4, 1, mps_file_temp);
			reclen2 = le_read32(&reclen);

			/* Read the record type "flag" in - using fread in case in the future need more than one char */
			fread(&recType, 1, 1, mps_file_temp);

		}	/* while (!feof(mps_file_temp)) */
	}	/* if (mpsmergeout) */

	if (global_opts.objective == trkdata) {
		if (mpsmergeout) {
			/* since we're processing tracks, we should read in from whatever version and write out
			   in the selected version */
			while (!feof(mps_file_temp)) {

				if (recType == 'T')  {
					mps_track_r(mps_file_temp, mps_ver_temp, &trk);
				}
				else break;

				fread(&reclen, 4, 1, mps_file_temp);
				reclen2 = le_read32(&reclen);

				/* Read the record type "flag" in - using fread in case in the future need more than one char */
				fread(&recType, 1, 1, mps_file_temp);
			}
		}
		track_disp_all(mps_trackhdr_w_wrapper, mps_noop, mps_trackdatapoint_w_wrapper);
	}

	if (mpsmergeout) {
		/* should now be reading a mapset - since we would write out an empty one,
		   let's use the one from the merge file which may well have decent data in */
		fwrite(&reclen, 4, 1, mps_file_out);	/* write out untouched */
		fwrite(&recType, 1, 1, mps_file_out);

		for(tocopy = reclen2; tocopy > 0; tocopy -= sizeof(copybuf)) {
			fread(copybuf, (tocopy > sizeof(copybuf) ? sizeof(copybuf) : tocopy), 1, mps_file_temp);
			fwrite(copybuf, (tocopy > sizeof(copybuf) ? sizeof(copybuf) : tocopy), 1, mps_file_out);
		}
	}
	else mps_mapsetname_w(mps_file_out, mps_ver_out);

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
