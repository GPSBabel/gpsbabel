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

static icon_mapping_t icon_table[] = {
	{   107, "Airport" },
	{    73, "Amusement Park" },
	{    55, "Ball Park" },
	{     6, "Bank" },
	{    13, "Bar" },
	{   104, "Beach" },
	{     1, "Bell" },
	{    37, "Boat Ramp" },
	{    74, "Bowling" },
	{    93, "Bridge" },
	{    94, "Building" },
	{    38, "Campground" },
	{    56, "Car" },
	{    75, "Car Rental" },
	{    76, "Car Repair" },
	{    95, "Cemetery" },
	{    96, "Church" },
	{    65, "Circle with X" },
	{    72, "City (Capitol)" },
	{    71, "City (Large)" },
	{    70, "City (Medium)" },
	{    69, "City (Small)" },
	{    97, "Civil" },
	{   119, "Contact, Afro" },
	{   120, "Contact, Alien" },
	{   121, "Contact, Ball Cap" },
	{   122, "Contact, Big Ears" },
	{   123, "Contact, Biker" },
	{   124, "Contact, Bug" },
	{   125, "Contact, Cat" },
	{   126, "Contact, Dog" },
	{   127, "Contact, Dreadlocks" },
	{   128, "Contact, Female1" },
	{   129, "Contact, Female2" },
	{   130, "Contact, Female3" },
	{   131, "Contact, Goatee" },
	{   132, "Contact, Kung-Fu" },
	{   133, "Contact, Pig" },
	{   134, "Contact, Pirate" },
	{   135, "Contact, Ranger" },
	{   136, "Contact, Smiley" },
	{   137, "Contact, Spike" },
	{   138, "Contact, Sumo" },
	{    52, "Controlled Area" },
	{    89, "Convenience Store" },
	{    98, "Crossing" },
	{    51, "Dam" },
	{    53, "Danger Area" },
	{    87, "Department Store" },
	{     4, "Diver Down Flag 1" },
	{     5, "Diver Down Flag 2" },
	{    41, "Drinking Water" },
	{    63, "Exit" },
	{    77, "Fast Food" },
	{     7, "Fishing Area" },
	{    78, "Fitness Center" },
	{    64, "Flag" },
	{   105, "Forest" },
	{     8, "Gas Station" },
	{   117, "Geocache" },
	{   118, "Geocache Found" },
	{    99, "Ghost Town" },
	{   113, "Glider Area" },
	{    68, "Golf Course" },
	{     2, "Green Diamond" },
	{    15, "Green Square" },
	{   108, "Heliport" },
	{     9, "Horn" },
	{    57, "Hunting Area" },
	{    44, "Information" },
	{   100, "Levee" },
	{    12, "Light" },
	{    90, "Live Theater" },
	{    59, "Lodging" },
	{    20, "Man Overboard" },
	{     0, "Marina" },
	{    43, "Medical Facility" },
	{    66, "Mile Marker" },
	{   101, "Military" },
	{    60, "Mine" },
	{    79, "Movie Theater" },
	{    80, "Museum" },
	{    21, "Navaid, Amber" },
	{    22, "Navaid, Black" },
	{    23, "Navaid, Blue" },
	{    24, "Navaid, Green" },
	{    25, "Navaid, Green/Red" },
	{    26, "Navaid, Green/White" },
	{    27, "Navaid, Orange" },
	{    28, "Navaid, Red" },
	{    29, "Navaid, Red/Green" },
	{    30, "Navaid, Red/White" },
	{    31, "Navaid, Violet" },
	{    32, "Navaid, White" },
	{    33, "Navaid, White/Green" },
	{    34, "Navaid, White/Red" },
	{   102, "Oil Field" },
	{   115, "Parachute Area" },
	{    46, "Park" },
	{    45, "Parking Area" },
	{    81, "Pharmacy" },
	{    47, "Picnic Area" },
	{    82, "Pizza" },
	{    83, "Post Office" },
	{   109, "Private Field" },
	{    36, "Radio Beacon" },
	{     3, "Red Diamond" },
	{    16, "Red Square" },
	{    10, "Residence" },
	{    11, "Restaurant" },
	{    54, "Restricted Area" },
	{    39, "Restroom" },
	{    84, "RV Park" },
	{    91, "Scales" },
	{    48, "Scenic Area" },
	{    85, "School" },
	{   116, "Seaplane Base" },
	{    19, "Shipwreck" },
	{    58, "Shopping Center" },
	{   112, "Short Tower" },
	{    40, "Shower" },
	{    49, "Skiing Area" },
	{    14, "Skull and Crossbones" },
	{   110, "Soft Field" },
	{    86, "Stadium" },
	{   106, "Summit" },
	{    50, "Swimming Area" },
	{   111, "Tall Tower" },
	{    42, "Telephone" },
	{    92, "Toll Booth" },
	{    67, "TracBack Point" },
	{    61, "Trail Head" },
	{    62, "Truck Stop" },
	{   103, "Tunnel" },
	{   114, "Ultralight Area" },
	{    18, "Waypoint" },
	{    17, "White Buoy" },
	{    35, "White Dot" },
	{    88, "Zoo" },
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
		if (case_ignore_strcmp(desc,i->icon) == 0) {
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
#ifdef DUMP_ICON_TABLE
	printf("static icon_mapping_t icon_table[] = {\n");
#endif

	for(;;)
	{
		long next_rec;
		unsigned short int icon;
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

#ifdef DUMP_ICON_TABLE
		printf("\t{  %4u, \"%s\" },\n", icon, wpt->shortname);
#endif
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

	if (get_cache_icon(wpt)) {
		icon = mps_find_icon_number_from_desc(get_cache_icon(wpt));
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
