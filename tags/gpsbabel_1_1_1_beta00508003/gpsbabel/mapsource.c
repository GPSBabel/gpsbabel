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
	{ 0, "Anchor" },
	{ 0, "Marina" },
	{ 1, "Bell" },
	{ 2, "Green Diamond" },
	{ 3, "Red Diamond" },
	{ 4, "Diver Down Flag 1" },
	{ 5, "Diver Down Flag 2" },
	{ 6, "Bank" },
	{ 6, "ATM" },
	{ 7, "Fishing Area" },
	{ 8, "Gas Station" },
	{ 9, "Horn" },
	{ 10, "Residence" },
	{ 11, "Restaurant" },
	{ 12, "Light" },
	{ 13, "Bar" },
	{ 14, "Skull and Crossbones" },
	{ 15, "Green Square" },
	{ 16, "Red Square" },
	{ 17, "White buoy" },
	{ 18, "Waypoint" },
	{ 19, "Shipwreck" },
	{ 21, "Man Overboard" },
	{ 22, "Navaid Amber" },
	{ 23, "Navaid Black" },
	{ 24, "Navaid Blue" },
	{ 25, "Navaid Green" },
	{ 26, "Navaid Green/Red" },
	{ 27, "Navaid Green/White" },
	{ 28, "Navaid Orange" },
	{ 29, "Navaid Red" },
	{ 30, "Navaid Red/Green" },
	{ 31, "Navaid Red/White" },
	{ 32, "Navaid Violet" },
	{ 33, "Navaid White" },
	{ 34, "Navaid White/Green" },
	{ 35, "Navaid White/Red" },
	{ 36, "White Dot" },
	{ 37, "Radio Beacon" },
	{ 150, "Boat Ramp" },
	{ 151, "Campground" },
	{ 151, "Camping" },
	{ 152, "Restrooms" },
	{ 153, "Showers" },
	{ 154, "Drinking Water" },
	{ 155, "Telephone" },
	{ 156, "Medical Facility" },
	{ 157, "Information" },
	{ 158, "Parking Area" },
	{ 159, "Park" },
	{ 160, "Picnic Area" },
	{ 161, "Scenic Area" },
	{ 162, "Skiing Area" },
	{ 163, "Swimming Area" },
	{ 164, "Dam" },
	{ 165, "Controlled Area" },
	{ 166, "Danger Area" },
	{ 167, "Restricted Area" },
	{ 169, "Ball Park" },
	{ 170, "Car" },
	{ 171, "Hunting Area" },
	{ 172, "Shopping Center" },
	{ 173, "Lodging" },
	{ 174, "Mine" },
	{ 175, "Trail Head" },
	{ 176, "Truck Stop" },
	{ 177, "Exit" },
	{ 178, "Flag" },
	{ 179, "Circle with x" },
	{ 8195, "Mile Marker" },
	{ 8196, "TracBack Point" },
	{ 8197, "Golf Course" },
	{ 8198, "City (Small)" },
	{ 8199, "City (Medium)" },
	{ 8200, "City (Large)" },
	{ 8203, "City (Capitol)" },
	{ 8204, "Amusement Park" },
	{ 8205, "Bowling" },
	{ 8206, "Car Rental" },
	{ 8207, "Car Repair" },
	{ 8208, "Fast Food" },
	{ 8209, "Fitness Center" },
	{ 8210, "Movie Theater" },
	{ 8211, "Museum" },
	{ 8212, "Pharmacy" },
	{ 8213, "Pizza" },
	{ 8214, "Post Office" },
	{ 8215, "RV Park" },
	{ 8216, "School" },
	{ 8217, "Stadium" },
	{ 8218, "Department Store" },
	{ 8219, "Zoo" },
	{ 8220, "Convenience Store" },
	{ 8221, "Live Theater" },
	{ 8226, "Scales" },
	{ 8227, "Toll Booth" },
	{ 8233, "Bridge" },
	{ 8234, "Building" },
	{ 8235, "Cemetery" },
	{ 8236, "Church" },
	{ 8237, "Civil" },
	{ 8238, "Crossing" },
	{ 8239, "Ghost Town" },
	{ 8240, "Levee" },
	{ 8241, "Military" },
	{ 8242, "Oil Field" },
	{ 8243, "Tunnel" },
	{ 8244, "Beach" },
	{ 8245, "Forest" },
	{ 8246, "Summit" },
	{ 8255, "Geocache" },
	{ 8256, "Geocache Found" },
	{ 8257, "Contact Smiley" },
	{ 8258, "Contact Ball Cap" },
	{ 8259, "Contact Big Ears" },
	{ 8260, "Contact Spike" },
	{ 8261, "Contact Goatee" },
	{ 8262, "Contact Afro" },
	{ 8263, "Contact Dreadlocks" },
	{ 8264, "Contact Female 1" },
	{ 8265, "Contact Female 2" },
	{ 8266, "Contact Female 3" },
	{ 8267, "Contact Ranger" },
	{ 8268, "Contact Kung-Fu" },
	{ 8269, "Contact Sumo" },
	{ 8270, "Contact Pirate" },
	{ 8271, "Contact Biker" },
	{ 8272, "Contact Alien" },
	{ 8273, "Contact Bug" },
	{ 8274, "Contact Cat" },
	{ 8275, "Contact Dog" },
	{ 8276, "Contact Pig" },
	{ 16384, "Airport" },
	{ 16388, "Heliport" },
	{ 16389, "Private Field" },
	{ 16390, "Soft Field" },
	{ 16391, "Tall Tower" },
	{ 16392, "Short Tower" },
	{ 16393, "Glider Area" },
	{ 16394, "Ultralight Area" },
	{ 16395, "Parachute Area" },
	{ 16402, "Seaplane Base" },

#if SOMEONE_ELSE
/* FIXME: Find icons for these */
	{ 1, "Virtual cache"},
	{ 2, "Micro-Cache"},
	{ 3, "Multi-Cache"},
#endif 
	{ -1, NULL },
};

static icon_mapping_t new_icon_table[] = {
	{ 0, "Anchor" },
	{ 1, "Bell" },
	{ 2, "Green Diamond" },
	{ 3, "Red Diamond" },
	{ 4, "Diver Down Flag 1" },
	{ 5, "Diver Down Flag 2" },
	{ 6, "Bank" },
	{ 7, "Fishing Area" },
	{ 8, "Gas Station" },
	{ 9, "Horn" },
	{ 10, "Residence" },
	{ 11, "Restaurant" },
	{ 12, "Light" },
	{ 13, "Bar" },
	{ 14, "Skull and Crossbones" },
	{ 15, "Green Square" },
	{ 16, "Red Square" },
	{ 17, "White buoy" },
	{ 18, "Waypoint" },
	{ 19, "Shipwreck" },
	{ 20, "Man Overboard" },
	{ 21, "Navaid Amber" },
	{ 22, "Navaid Black" },
	{ 23, "Navaid Blue" },
	{ 24, "Navaid Green" },
	{ 25, "Navaid Green/Red" },
	{ 26, "Navaid Green/White" },
	{ 27, "Navaid Orange" },
	{ 28, "Navaid Red" },
	{ 29, "Navaid Red/Green" },
	{ 30, "Navaid Red/White" },
	{ 31, "Navaid Violet" },
	{ 32, "Navaid White" },
	{ 33, "Navaid White/Green" },
	{ 34, "Navaid White/Red" },
	{ 35, "White Dot" },
	{ 36, "Radio Beacon" },
	{ 37, "Boat Ramp" },
	{ 38, "Campground" },
	{ 39, "Restrooms" },
	{ 40, "Showers" },
	{ 41, "Drinking Water" },
	{ 42, "Telephone" },
	{ 43, "Medical Facility" },
	{ 44, "Information" },
	{ 45, "Parking Area" },
	{ 46, "Park" },
	{ 47, "Picnic Area" },
	{ 48, "Scenic Area" },
	{ 49, "Skiing Area" },
	{ 50, "Swimming Area" },
	{ 51, "Dam" },
	{ 52, "Controlled Area" },
	{ 53, "Danger Area" },
	{ 54, "Restricted Area" },
	{ 55, "Ball Park" },
	{ 56, "Car" },
	{ 57, "Hunting Area" },
	{ 58, "Shopping Center" },
	{ 59, "Lodging" },
	{ 60, "Mine" },
	{ 61, "Trail Head" },
	{ 62, "Truck Stop" },
	{ 63, "Exit" },
	{ 64, "Flag" },
	{ 65, "Circle with x" },
	{ 66, "Mile Marker" },
	{ 67, "TracBack Point" },
	{ 68, "Golf Course" },
	{ 69, "City (Small)" },
	{ 70, "City (Medium)" },
	{ 71, "City (Large)" },
	{ 72, "City (Capitol)" },
	{ 73, "Amusement Park" },
	{ 74, "Bowling" },
	{ 75, "Car Rental" },
	{ 76, "Car Repair" },
	{ 77, "Fast Food" },
	{ 78, "Fitness Center" },
	{ 79, "Movie Theater" },
	{ 80, "Museum" },
	{ 81, "Pharmacy" },
	{ 82, "Pizza" },
	{ 83, "Post Office" },
	{ 84, "RV Park" },
	{ 85, "School" },
	{ 86, "Stadium" },
	{ 87, "Department Store" },
	{ 88, "Zoo" },
	{ 89, "Convenience Store" },
	{ 90, "Live Theater" },
	{ 91, "Scales" },
	{ 92, "Toll Booth" },
	{ 93, "Bridge" },
	{ 94, "Building" },
	{ 95, "Cemetery" },
	{ 96, "Church" },
	{ 97, "Civil" },
	{ 98, "Crossing" },
	{ 99, "Ghost Town" },
	{ 100, "Levee" },
	{ 101, "Military" },
	{ 102, "Oil Field" },
	{ 103, "Tunnel" },
	{ 104, "Beach" },
	{ 105, "Forest" },
	{ 106, "Summit" },
	{ 107, "Airport" },
	{ 108, "Heliport" },
	{ 109, "Private Field" },
	{ 110, "Soft Field" },
	{ 111, "Tall Tower" },
	{ 112, "Short Tower" },
	{ 113, "Glider Area" },
	{ 114, "Ultralight Area" },
	{ 115, "Parachute Area" },
	{ 116, "Seaplane Base" },
	{ 117, "Geocache" },
	{ 118, "Geocache Found" },
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
