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

static FILE *mps_file_in;
static FILE *mps_file_out;
static FILE *mps_file_temp;
static void *mkshort_handle;

static int	mps_ver_in = 0;
static int	mps_ver_out = 0;
static int	mps_ver_temp = 0;

#define MYNAME "MAPSOURCE" 
#define ISME 0
#define NOTME 1

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

char *snlen;
char *mpsverout;

static
arglist_t mps_args[] = {
	{"snlen", &snlen, "Length of generated shortnames", ARGTYPE_INT },
	{"mpsverout", &mpsverout, "Version of mapsource file to generate (3,4,5)", ARGTYPE_INT },
	{0, 0, 0, 0}
};

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
 * //MRCB
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
 * //MRCB
 */
static void
mps_fileHeader_w(FILE *mps_file, int mps_ver)
{
	char hdr[100];
	int reclen;

	strcpy (hdr, "MsRc");
	fwrite(hdr, 4, 1, mps_file);

	/* Between versions 3 & 5 this value is 'd', but might change in 
	 * the future
	 */
	strcpy(hdr, "d");
	fwrite(hdr, 2, 1, mps_file);	/* include trailing NULL char */

	/* Seemingly another version related char */
	hdr[0] = 'D';
	/* if (mps_ver == 3)  */
	hdr[1] = 'd';	/* equates to V3.02 */
	if (mps_ver == 4) hdr[1] = 'g';	/* equates to V4.06 */
	if (mps_ver == 5) hdr[1] = 'i';	/* equates to V5.0 */
	hdr[2] = 0;

	reclen = 2;						/* this is 3 byte record */
	le_write32(&reclen, reclen);
	fwrite(&reclen, 4, 1, mps_file);
	fwrite(hdr, 3, 1, mps_file);	/* reclen + 1 */

	hdr[0] = 'A';
	/* if (mps_ver == 3)  */
	hdr[1] = 0x2E; hdr[2] = 0x01;	/* equates to V3.02 */
	hdr[3] = 'S'; 
	hdr[4] = 'Q'; 
	hdr[5] = 'A';
	hdr[6] = 0;
	strcpy(hdr+7,"Oct 20 1999");
	strcpy(hdr+19,"12:50:03");
	if (mps_ver == 4) {
		hdr[1] = 0x96;	/* equates to V4.06 */
		strcpy(hdr+7,"Oct 22 2001");
		strcpy(hdr+19,"15:45:05");
	}
	if (mps_ver == 5) {
		hdr[1] = 0xF4;	/* equates to V5.0 */
		strcpy(hdr+7,"Jul  3 2003");
		strcpy(hdr+19,"08:35:39");
	}

	reclen = 27;					/* pre measured! */
	le_write32(&reclen, reclen);
	fwrite(&reclen, 4, 1, mps_file);
	fwrite(hdr, 28, 1, mps_file);	/* reclen + 1  - can't use this as reclen may be wrongendian now */
}

/*
 * read in from file a mapsetname record
 * there should always be one of these at the end of the file
 * //MRCB
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
		/* this IS a mapsetname
		
		// At the moment we're not doing anything with mapsetnames, but here's the template code as if we were
		// mps_readstr(mps_file, hdr, sizeof(hdr));
		// char mapsetnamename[very large number?];
		// strcpy(mapsetnamename,hdr);
		// char mapsetnameAutonameFlag;
		// fread(&mapsetnameAutonameFlag, 1, 1, mps_file);
		*/

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
 * //MRCB
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
 * //MRCB
 */
static int
mps_waypoint_r(FILE *mps_file, int mps_ver, waypoint **wpt)
{
	unsigned char hdr[100];
	int reclen;
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

	fread(&reclen, 4, 1, mps_file);
		reclen = le_read32(&reclen);

	fread(hdr, 1, 1, mps_file);

	if (hdr[0] == 'W') {
		/* this IS a waypoint */
		
		thisWaypoint = xcalloc(sizeof(*thisWaypoint), 1);
		*wpt = thisWaypoint;

		mps_readstr(mps_file, wptname, sizeof(wptname));

		if ((mps_ver == 4) || (mps_ver == 5)) {
			fread(tbuf, 4, 1, mps_file);		/* class */
			mps_readstr(mps_file, tbuf, sizeof(tbuf));	/* country */
		}

		fread(tbuf, 22, 1, mps_file);			/* unknown */

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
		thisWaypoint->position.latitude.degrees = lat / 2147483648.0 * 180.0;
		thisWaypoint->position.longitude.degrees = lon / 2147483648.0 * 180.0;
		thisWaypoint->position.altitude.altitude_meters = mps_altitude;
		thisWaypoint->icon_descr = mps_find_desc_from_icon_number(icon, MAPSOURCE);
		/* waypt_add(thisWaypoint); */

		return ISME;
	}
	else {
		/* Not a waypoint */
		fseek(mps_file, -5, SEEK_CUR); 
		return NOTME;
	}
}

/*
 * write out to file a waypoint record
 * //MRCB
		 */
static void
mps_waypoint_w(FILE *mps_file, int mps_ver, const waypoint *wpt)
{
	unsigned char hdr[100];
	int reclen;
	int lat = wpt->position.latitude.degrees  / 180.0 * 2147483648.0;
	int lon = wpt->position.longitude.degrees  / 180.0 * 2147483648.0;
	int	icon;
	char *src;
	char *ident;
	char zbuf[100];
	char ffbuf[100];
	int display = 1;
	int colour = 0;			/*  (unknown colour) black is 1, white is 16 */

	double	mps_altitude = wpt->position.altitude.altitude_meters;
	double	mps_proximity = unknown_alt;
	double	mps_depth = unknown_alt;

	if(wpt->description) src = wpt->description;
	if(wpt->notes) src = wpt->notes;
	ident = global_opts.synthesize_shortnames ?
				mkshort(mkshort_handle, src) :
				wpt->shortname;

	memset(zbuf, 0, sizeof(zbuf));
	memset(ffbuf, 0xff, sizeof(ffbuf));

	icon = mps_find_icon_number_from_desc(wpt->icon_descr, MAPSOURCE);

	if (get_cache_icon(wpt) && wpt->icon_descr && (strcmp(wpt->icon_descr, "Geocache Found") != 0)) {
		icon = mps_find_icon_number_from_desc(get_cache_icon(wpt), MAPSOURCE);
	}

	reclen = strlen(ident) + strlen(wpt->description) + 2;	/* two NULL (0x0) bytes at end of each string */
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

	fputs(wpt->description, mps_file);
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
 * read in from file a track record
 * //MRCB
 */
static int
mps_track_r(FILE *mps_file, int mps_ver, route_head **trk)
{
	unsigned char hdr[100];
	int reclen;
	char tbuf[100];
	char trkname[256];
	int lat;
	int lon;

	time_t	dateTime = 0;
	route_head *track_head;
	unsigned int trk_count;

	waypoint	*thisWaypoint;
	double	mps_altitude = unknown_alt;
	double	mps_depth = unknown_alt;


	fread(&reclen, 4, 1, mps_file);
	reclen = le_read32(&reclen);

	fread(hdr, 1, 1, mps_file);

	if (hdr[0] == 'T') {
		/* this IS a track */

		mps_readstr(mps_file, trkname, sizeof(trkname));
		fread(tbuf, 1, 1, mps_file);			/* display flag */
		fread(tbuf, 4, 1, mps_file);			/* colour */

		fread(&trk_count, 4, 1, mps_file);			/* number of datapoints in tracklog */
		trk_count = le_read32(&trk_count);

		track_head = route_head_alloc();
		track_head->rte_name = xstrdup(trkname);
		route_add_head(track_head);
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
			thisWaypoint->position.latitude.degrees = lat / 2147483648.0 * 180.0;
			thisWaypoint->position.longitude.degrees = lon / 2147483648.0 * 180.0;
			thisWaypoint->creation_time = dateTime;
			thisWaypoint->centiseconds = 0;
			thisWaypoint->position.altitude.altitude_meters = mps_altitude;
			route_add_wpt(track_head, thisWaypoint);
/* Mark, why is this here:	thisWaypoint->position.longitude.degrees); */

		}		/* while (trk_count--) */
		return ISME;
	}
	else {
		/* Not a track */
		fseek(mps_file, -5, SEEK_CUR); 
		return NOTME;
	}
}

/*
 * write out to file a tracklog header
 * //MRCB
 */
static void
mps_trackhdr_w(FILE *mps_file, int mps_ver, const route_head *trk)
{
	unsigned int reclen;
	unsigned int trk_datapoints;
	unsigned int colour = 0;		/* unknown colour */
	int		tname_len;
	char *tname;
	char hdr[2];

	queue *elem, *tmp;

	/* track name */
	if (!trk->rte_name)
		tname = xstrdup("Track");
	else
		tname = xstrdup(trk->rte_name);

	tname_len = strlen(tname);
	reclen = tname_len + 11;		/* "T" (1) + strlen(tname) + NULL (1) + display flag (1) + colour (4) +
									   num track datapoints value (4) */
	
	/* total nodes (waypoints) this track */
	trk_datapoints = 0;
	QUEUE_FOR_EACH(&trk->waypoint_list, elem, tmp) {
		trk_datapoints++;
	}		

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

static void
mps_trackhdr_w_wrapper(const route_head *trk)
{
	mps_trackhdr_w(mps_file_out, mps_ver_out, trk);
}


/*
 * write out to file a tracklog datapoint
 * //MRCB
 */
static void
mps_trackdatapoint_w(FILE *mps_file, int mps_ver, const waypoint *wpt)
{
	unsigned char hdr[10];
	int lat = wpt->position.latitude.degrees  / 180.0 * 2147483648.0;
	int lon = wpt->position.longitude.degrees  / 180.0 * 2147483648.0;
	time_t	t = wpt->creation_time;
	char zbuf[10];

	double	mps_altitude = wpt->position.altitude.altitude_meters;
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
	route_head	*trk;

	mps_ver_in = 0;		/* although initialised at declaration, what happens if there are two mapsource
						   input files? */
	mps_fileHeader_r(mps_file_in, &mps_ver_in);

#ifdef DUMP_ICON_TABLE
	printf("static icon_mapping_t icon_table[] = {\n");
#endif

	while (mps_waypoint_r(mps_file_in, mps_ver_in, &wpt) == ISME) {
		if (global_opts.objective == wptdata) {
			waypt_add(wpt);
		}
		else {
			xfree(wpt);		/* xcalloc was used */
		}
#ifdef DUMP_ICON_TABLE
		printf("\t{  %4u, \"%s\" },\n", icon, wpt->shortname);
#endif
	}

	/* while (mps_route_r(mps_file_in, mps_ver_in, &rte) == ISME) {
		if (global_opts.objective != rtedata) {
			route_free(trk);			/* rather inefficient to have read it all in just to free it,
										   but it's not that bad * /
		}
	} */

	while (mps_track_r(mps_file_in, mps_ver_in, &trk) == ISME) {
		if (global_opts.objective != trkdata) {
			route_free(trk);			/* rather inefficient to have read it all in just to free it,
										   but it's not that bad */
		}
	}

	if (mps_mapsetname_r(mps_file_in, mps_ver_in) != ISME) {
		fatal(MYNAME ": Mapsource file not terminated corrected.\n");
	}

#ifdef DUMP_ICON_TABLE
		printf("\t{ -1, NULL },\n");
		printf("};\n");
#endif
}

static void
mps_waypt_pr(const waypoint *wpt)
{
	char *src;
	char *ident;
	int reclen;
	char zbuf[25];
	char ffbuf[25];
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

	icon = mps_find_icon_number_from_desc(wpt->icon_descr, MAPSOURCE);

	if (get_cache_icon(wpt) && wpt->icon_descr && (strcmp(wpt->icon_descr, "Geocache Found") != 0)) {
		icon = mps_find_icon_number_from_desc(get_cache_icon(wpt), MAPSOURCE);
	}

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

static void 
mps_noop(const route_head *wp)
{
	/* no-op */
}

void
mps_write(void)
{
	int short_length;

	if (snlen)
		short_length = atoi(snlen);
	else
		short_length = 10;

	if (mpsverout)
		mps_ver_out = atoi(mpsverout);
	else
		mps_ver_out = 3;

	mkshort_handle = mkshort_new_handle();

	setshort_length(mkshort_handle, short_length);
	setshort_whitespace_ok(mkshort_handle, 0);

	mps_fileHeader_w(mps_file_out, mps_ver_out);

	if (global_opts.objective == wptdata) {
		waypt_disp_all(mps_waypoint_w_wrapper);
	}
	if (global_opts.objective == rtedata) {
	}
	if (global_opts.objective == trkdata) {
		route_disp_all(mps_trackhdr_w_wrapper, mps_noop, mps_trackdatapoint_w_wrapper);
	}

	mps_mapsetname_w(mps_file_out, mps_ver_out);

	/* fwrite(mps_hdr, sizeof(mps_hdr), 1, mps_file_out);
	waypt_disp_all(mps_waypt_pr);
	fwrite(mps_ftr, sizeof(mps_ftr), 1, mps_file_out); */

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
