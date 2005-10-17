/* 
	Garmin GPS Database Reader/Writer
	
	Copyright (C) 2005 Olaf Klein, o.b.klein@t-online.de
	Mainly based on mapsource.c,
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

/*
	History:
	
	    2005/06/27: initial release (reader only)
	    2005/07/26: added write support
	    2005/07/27: replaced "tricky code" in route reader
	    2005/07/28: fixed handling of single point routes
			new option "via"
			new option "ver"
			fixed compiler warnings
	    2005/07/29: fixed compiler warnings
*/

#include <stdio.h>
#include <string.h>
#include <ctype.h>

#include "defs.h"
#include "garmin_tables.h"
#include "jeeps/gpsmath.h"

#define MYNAME "gdb"

#undef GDB_DEBUG

#define GDB_VER_MIN			1
#define GDB_VER_MAX			2

#define GDB_DEFAULTWPTCLASS		0
#define GDB_HIDDENROUTEWPTCLASS		8

#define GDB_NAME_BUFFERLEN	1024
#define GDB_URL_BUFFERLEN	4096	/* Safety first */
#define GDB_NOTES_BUFFERLEN	4096	/* (likewise)   */

#define DEFAULTICONVALUE	18
#define DEFAULTICONDESCR	"Waypoint"

#ifdef UTF8_SUPPORT
# define GDB_UTF8_ENABLED 1
#else
# define GDB_UTF8_ENABLED 0
#endif

typedef enum {
	GDB_DISPLAY_SYMBOL_ONLY = 0,
	GDB_DISPLAY_SYMBOL_AND_NAME = 1,
	GDB_DISPLAY_SYMBOL_AND_COMMENT = 2,
} gdb_display_type;

/* %%% local vars %%% */

static FILE *fin, *fout;
static char *fin_name, *fout_name;

static int gdb_ver = 1;
static int gdb_debug = 0;
static int gdb_via;		/* 0 = read and write hidden points too; 1 = drop */
static int gdb_category;

static route_head *gdb_hidden = NULL;
static short_handle gdb_short_handle;

#define GDB_OPT_VER		"ver"
#define GDB_OPT_VIA		"via"
#define GDB_OPT_CATEGORY	"cat"

static char *gdb_opt_category = NULL;
static char *gdb_opt_ver = NULL;
static char *gdb_opt_via = NULL;

static arglist_t gdb_args[] = {
	{GDB_OPT_CATEGORY, &gdb_opt_category, 
	    "Default category on output (1..16)", NULL, ARGTYPE_INT},
	{GDB_OPT_VER, &gdb_opt_ver, 
	    "Version of gdb file to generate (1,2)", "2", ARGTYPE_INT},
	{GDB_OPT_VIA, &gdb_opt_via, 
	    "Drop route points, if they don't have an aquivalent waypoint (hidden points)", NULL, ARGTYPE_BOOL},
	{0, 0, 0, 0, 0}
};

/********************************************************************************************************/

/* %%% 1-1 functions from mapsource, should by shared!!! %%% */

/*
 * Add a waypoint that we've already written out to our list
 *
 */
static void
gdb_add_to_hidden(const waypoint *wpt)
{
	waypoint *tmp = waypt_dupe(wpt);
	route_add_wpt(gdb_hidden, tmp);
}

static waypoint *
gdb_find_wpt_q_by_name(const queue *whichQueue, const char *name)
{
	queue *elem, *tmp;
	waypoint *waypointp;

	QUEUE_FOR_EACH(whichQueue, elem, tmp) {
		waypointp = (waypoint *) elem;
		if (0 == strcmp(waypointp->shortname, name)) {
			return waypointp;
		}
	}
	return NULL;
}

static const char *
gdb_find_desc_from_icon_number(const int icon, garmin_formats_e garmin_format)
{
	static char custom[] = "Custom 63";
	icon_mapping_t *i;

	if (icon >= 500 && icon <= 563)
	{
	    snprintf(custom, sizeof(custom), "Custom %d", icon - 500);
	    return &custom[0];
	}

	for (i = garmin_icon_table; i->icon; i++) {
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
				fatal(MYNAME ": unknown garmin format.\n");
		}
	}
	return DEFAULTICONDESCR;
}

static int
gdb_find_icon_number_from_desc(const char *desc, garmin_formats_e garmin_format)
{
	icon_mapping_t *i;
	int def_icon = DEFAULTICONVALUE;
	int n;

	if (!desc)
		return def_icon;

	/*
	 * If we were given a numeric icon number as a description 
	 * (i.e. 8255), just return that.
	 */
	n = atoi(desc);
	if (n)  {
		return n;
	}

	for (i = garmin_icon_table; i->icon; i++) {
		if (case_ignore_strcmp(desc,i->icon) == 0) {
			switch (garmin_format) {
			case MAPSOURCE:
				return i->mpssymnum;
			case PCX:
			case GARMIN_SERIAL:
				return i->pcxsymnum;
			default:
				fatal(MYNAME ": unknown garmin format.\n");
			}
		}
	}
	return def_icon;
}

static int
gdb_detect_rtept_class(const waypoint *wpt)
{
	if (gdb_find_wpt_q_by_name((queue *)&gdb_hidden->waypoint_list, wpt->shortname) == NULL)
	    return (int)GDB_HIDDENROUTEWPTCLASS;
	else
	    return (int)GDB_DEFAULTWPTCLASS;
}


/* %%% local functions (read support) %%% */

#ifdef GDB_DEBUG
static void
gdb_print_buff(const char *buff, int count, const char *comment)
{
	int i;
	printf(MYNAME ": dump of %s : ", comment);
	for (i = 0; i < count; i++)
	{
	    printf("%02x ", buff[i] & 0xFF);
	}
	printf("\n");
	fflush(stdout);
}
#endif

static waypoint *
gdb_create_rte_wpt(const char *name, double lat, double lon, double alt)
{
	waypoint *wpt;
	
	wpt = find_waypt_by_name(name);
	if (wpt == NULL)
	{
	    if (gdb_via != 0) return NULL;
	    wpt = gdb_find_wpt_q_by_name((queue *)&gdb_hidden->waypoint_list, name);
	}
	if (wpt != NULL) 
		wpt = waypt_dupe(wpt);
	else
	{
	    wpt = waypt_new();
	    wpt->shortname = xstrdup(name);
	    wpt->latitude = lat;
	    wpt->longitude = lon;
	    wpt->altitude = alt;
	    wpt->depth = unknown_alt;
	}
	return wpt;
}

static size_t
gdb_fread(void *target, size_t size)
{
	size_t result;

	result = fread(target, 1, size, fin);
	if (result < size)
	{
	    if (feof(fin) != 0)
		fatal(MYNAME ": unexpected end of file \"%s\"!\n", fin_name);
	    else
		fatal(MYNAME ": I/O error occured during read from \"%s\"!\n", fin_name);
	}
	return result;
}

static int
gdb_fread_str(char *dest, size_t maxlen)
{
	int c;
	int res = 0;
	
	while (maxlen-- > 0)
	{
	    c = fgetc(fin);
	    if ( c != EOF )
	    {
		if (c < 0)
		    fatal(MYNAME ": I/O error (%d) while read from \"%s\"!\n", +c, fin_name);
		*dest++ = c;
		if ( c == 0 ) return res;
		res++;
	    }
	    else
	    {
		*dest++ = '\0';
		return res;
	    }
	}
	fatal(MYNAME ": local buffer overflow detected, please report!\n");
	return 0;
}

static int
gdb_fread_le(void *dest, size_t size, int bit_count, const char *prefix, const char *field)
{
	char buff[32];
	unsigned char *c = dest;
	short *sh = dest;
	int *li = dest;
	double *db = dest;
	
	if ((bit_count >> 3) != size)
	    fatal(MYNAME "%s: Internal error (gdb_le_read/%d/%d/%s)!\n", prefix, (int)size, bit_count >> 3, field);
	    
	switch(bit_count)
	{
	    case 8:
		gdb_fread(c, sizeof(*c));
		if (gdb_debug)
		    printf(MYNAME "%s: gdb_fread_le : %d -> %s (0x%x))\n", prefix, *c, field, *c);
		return *c;
	    case 16:
		if (sizeof(*sh) != size) fatal(MYNAME ": internal decl.!\n");
		gdb_fread(sh, sizeof(*sh));
		*sh = le_read16(sh);
		if (gdb_debug)
		    printf(MYNAME "%s: gdb_fread_le : %d -> %s (0x%x))\n", prefix, *sh, field, *sh);
		return *sh;
	    case 32:
		gdb_fread(li, 4);
		*li = le_read32(li);
		if (gdb_debug)
		    printf(MYNAME "%s: gdb_fread_le : %d -> %s (0x%x)\n", prefix, *li, field, *li);
		return *li;
	    case 64:
		gdb_fread(buff, sizeof(*db));
		le_read64(db, buff);
		if (gdb_debug)
		    printf(MYNAME "%s: gdb_fread_le : %g -> %s\n", prefix, *db, field);
		return 0;
	    default:
		fatal(MYNAME "%s: unsupported bit count (%d) in gdb_le_read!\n", prefix, bit_count);	    
	}
	return 0;
}

static int
gdb_fread_flag(const char value)	/* read one byte and compare to value */
{
	char c;

	gdb_fread(&c, 1);
	return (c == value);
}

static void
gdb_is_valid(int is, const char *prefix, const char *comment)
{
	if (is == 0) 
	{
	    printf(MYNAME ": Reading database \"%s\"\n", fin_name);
	    fatal(MYNAME  "-%s: Found error in data (%s)!\n", prefix, comment);
	}
}

static void
gdb_is_validf(int is, const char *prefix, const char *format, ...)
{
	va_list args;
	
	if (is != 0) return;
	
	va_start(args, format);
	if (fin_name != NULL)
	    printf(MYNAME "-%s: Reading from database \"%s\"\n", prefix, fin_name);
	else
	    printf(MYNAME "-%s: Writing to database \"%s\"\n", prefix, fout_name);
	printf(MYNAME "-%s: ", prefix);
	vprintf(format, args);
	va_end(args);

	fatal("\n");
}

/********************************************************************************************************/
/* %%%                                   read file header                                               */
/********************************************************************************************************/

static void
gdb_read_file_header(void)
{
	char buff[128];
	int i, reclen;

	const char *prefix = "read_head";
/* 
	We starts with standard binary read.
	A gdb_fread_str works too, but if we get a wrong file as input,
	the file validation my be comes too late. For example a XML base file normally 
	has no binary zeros inside and produce, if big enought, a buffer overflow. 
	The following message "local buffer overflow detected..." could be
	misinterpreted.
*/
	
	if (6 != fread(buff, 1, 6, fin))
	    fatal(MYNAME ": Invalid file \"%s\"!\n", fin_name);
	    
	if (strcmp(buff, "MsRcf") != 0)
	    fatal(MYNAME ": Invalid file \"%s\"!\n", fin_name);
	    
	gdb_fread(&reclen, 4);
	reclen = le_read32(&reclen);
	
	gdb_is_valid(reclen == gdb_fread_str(buff, sizeof(buff)), prefix, "Invalid record length");
	if (buff[0] != 'D')
	    fatal(MYNAME ": Invalid file \"%s\"!\n", fin_name);

	switch(buff[1])
	{
	    case 'k':
		gdb_ver = 1;
		break;
	    case 'l':
		gdb_ver = 2;
		break;
	    default:
		fatal(MYNAME ": Non supported GDB version!\n");
	}
	
	if (global_opts.verbose_status > 0)
	    printf(MYNAME ": Found Garmin GPS Database version %d.0\n", gdb_ver);
	
	gdb_fread(&reclen, 4);
	reclen = le_read32(&reclen);
	gdb_is_valid(reclen < sizeof(buff), prefix, "Invalid record length");
	gdb_fread(buff, reclen);
	
	gdb_is_valid(0 == gdb_fread_str(buff, sizeof(buff)), prefix, "header");
	
	i = gdb_fread_str(buff, sizeof(buff));
	gdb_is_valid((i == 9) && (strcmp(buff, "MapSource") == 0), prefix, "MapSource magic");
}

/********************************************************************************************************/
/* %%%                                     read waypoint                                               */
/********************************************************************************************************/

static waypoint *
gdb_read_wpt(const size_t fileofs, int *wptclass)
{
	char xname[GDB_NAME_BUFFERLEN];
	char xnotes[GDB_NOTES_BUFFERLEN];
	char xurl[GDB_URL_BUFFERLEN];
	int xclass;
	int xlat, xlon, xdisplay, xcolour, xicon, xtime;
	short xcat;
	double xdepth = unknown_alt;
	double xalt = unknown_alt;
	double xproximity = unknown_alt;
	waypoint *res;
	char buff[128];
	size_t pos, delta;
	
	const char *prefix = "wpt_read";

	
/********************************************************************************************************/
/*	record structure

	zstring name
	dword	class
	zstring	country
	 4 * 0x00		subclass part 1
	12 * 0xFF		subclass part 2
	 2 * 0x00		subclass part 3
	 4 * 0xFF		unknown
	dword latitude
	dword longitude
	if (1) +8 altitude = (1 or 9)
	zstring comment
	dword display flag
	dword display colour
	dword 	icon
	zstring city		?
	zstring state		?
	zstring facility	?
	char	unknown		?
	double	depth 		(if flag)
	zstring url
	word 	category 			-> offset 79
	double	temp 		(if flag)
 */	
/********************************************************************************************************/

	gdb_is_valid(gdb_fread_str(xname, sizeof(xname)) > 0, prefix, "new waypoint");

	gdb_fread_le(&xclass, sizeof(xclass), 32, prefix, "class");
	gdb_fread_str(buff, sizeof(buff));				/* country */
	
	gdb_fread(buff, 22);
	xlat = gdb_fread_le(&xlat, sizeof(xlat), 32, prefix, "latitude");
	xlon = gdb_fread_le(&xlon, sizeof(xlon), 32, prefix, "longitude");
	
	if (gdb_fread_flag(1)) 						/* altitude flag */
	    gdb_fread_le(&xalt, sizeof(xalt), 64, prefix, "altitude");
	
	gdb_fread_str(xnotes, sizeof(xnotes));				/* notes */
	
	if (gdb_fread_flag(1))						/* proximity flag */
	    gdb_fread_le(&xproximity, sizeof(xproximity), 64, prefix, "proximity");
	
	xdisplay = gdb_fread_le(&xdisplay, sizeof(xdisplay), 32, prefix, "display");
	xcolour = gdb_fread_le(&xcolour, sizeof(xcolour), 32, prefix, "colour");
	xicon = gdb_fread_le(&xicon, sizeof(xicon), 32, prefix, "icon");

	gdb_fread_str(buff, sizeof(buff));				/* city */
	gdb_fread_str(buff, sizeof(buff));				/* state */
	gdb_fread_str(buff, sizeof(buff));				/* facility */
	
	gdb_fread(buff, 1);						/* unknown */
	
	if (gdb_fread_flag(1))						/* depth flag */
	    gdb_fread_le(&xdepth, sizeof(xdepth), 64, prefix, "depth");
	
	gdb_fread(buff, 1);
	gdb_fread(buff, 1);

	if (gdb_fread_flag(0))
	    gdb_fread(buff, 3);
	else
	    gdb_fread(buff, 2);
	
	do								/* undocumented & unused string */
	{
	    gdb_fread(buff, 1);
	}
	while (buff[0] != 0);

	gdb_fread_str(xurl, sizeof(xurl));				/* URL */
	
	xcat = gdb_fread_le(&xcat, sizeof(xcat), 16, prefix, "category");
	
	if (gdb_fread_flag(1))						/* temperature flag */
	    gdb_fread(buff, 8);						/* temperature */

	/* Here comes 1 .. 6 unknown bytes
	   !!! 6 only if class > 0 !!!
	   the field seems to be a time stamp */
	
	pos = ftell(fin);
	delta = fileofs - pos;
	gdb_is_valid(delta > 0, prefix, "waypoint final");
	
	if ((delta & 1) == 0)
	{
	    gdb_fread(buff, 1);
	    delta--;
	}
	
	xtime = 0;
	if (gdb_fread_flag(1))
	{
	    gdb_is_valid(delta==5, prefix, "Waypoint time");
	    gdb_fread_le(&xtime, sizeof(xtime), 32, prefix, "time");
	}
	    else
		gdb_is_valid(delta==1, prefix, "No waypoint time");
	
	*wptclass = xclass;
	
	res = waypt_new();
	res->shortname = xstrdup(xname);
	if (xurl[0] != '\0') res->url = xstrdup(xurl);
	if (xnotes[0] != '\0') res->notes = xstrdup(xnotes);
	res->latitude = GPS_Math_Semi_To_Deg(xlat);
	res->longitude = GPS_Math_Semi_To_Deg(xlon);
	res->altitude = xalt;
	res->creation_time = xtime;
#if 0
	res->depth = xdepth;
	res->proximity = xproximity;
	res->garmin_data = xcalloc(1, sizeof(garmin_data_t));
	res->garmin_data->colour = xcolour;
	res->garmin_data->category = xcat;
	res->garmin_data->display = xdisplay;
#endif
	/* might need to change this to handle version dependent icon handling */
	res->icon_descr = gdb_find_desc_from_icon_number(xicon, MAPSOURCE);

	gdb_is_validf(fabs(res->latitude) <= 90.0, prefix, "%s has invalid latitude (%f)", 
	    res->shortname, res->latitude);

	return res;
}

/********************************************************************************************************/
/* %%%                                     read route                                                   */
/********************************************************************************************************/

static route_head *
gdb_read_route(void)
{
	char xname[GDB_NAME_BUFFERLEN];
	char xwptname[GDB_NAME_BUFFERLEN];
	int xclass;
	double xalt;
	double xlat = 0;	/* compiler warnings */
	double xlon = 0;	/* compiler warnings */
	
	char buff[256];
	int count, origin;
	int isteps;
	int semilat, semilon;
	int maxlat, maxlon, minlon, minlat;
	char auto_name;
	
	route_head *route;
	waypoint *wpt;
	
	const char *prefix =  "rte_read_head";
	const char *prefix1 = "rte_read_loop";
	const char *prefix2 = "rte_ils_loop";
	const char *prefix3 = "rte_read_final";
	
	gdb_is_valid(gdb_fread_str(xname, sizeof(xname)) > 0, prefix, "Route has no name");
	
	gdb_fread_le(&auto_name, sizeof(auto_name), 8, prefix, "auto name");
	if (gdb_fread_flag(0))					/* max. data flag */
	{
	    gdb_fread_le(buff, 4, 32, prefix, "max. latitude");
	    gdb_fread_le(buff, 4, 32, prefix, "max. longitude");
	
	    gdb_fread(buff, 1);
	    if (buff[0] == 1) gdb_fread_le(buff, 8, 64, prefix, "max. altitude");
	    
	    gdb_fread_le(buff, 4, 32, prefix, "min. latitude");
	    gdb_fread_le(buff, 4, 32, prefix, "min. longitude");

	    gdb_fread(buff, 1);
	    if (buff[0] == 1)
		gdb_fread_le(buff, 8, 64, prefix, "min. altitude");
	}
	    
	gdb_fread_le(&count, sizeof(count), 32, prefix, "count");
	
	if (count == 0) 
	    fatal(MYNAME "%s: !!! Empty routes are not allowed !!!\n", prefix);
	
	route = route_head_alloc();
	route->rte_name = xstrdup(xname);
	route_add_head(route);

#ifdef GDB_DEBUG
	printf(MYNAME " - route: \"%s\" with %d point(s)\n", xname, count);
#endif
	origin = count;
	
	while (count--)
	{
	    gdb_fread_str(xwptname, sizeof(xwptname));			/* waypoint name */
	
	    gdb_fread_le(&xclass, sizeof(xclass), 32, prefix1, "class");	/* class */
	    gdb_fread_str(buff, sizeof(buff));					/* country */
	    
	    gdb_fread(buff, 22);						/* sub class data */
	    gdb_fread(buff, 1);
	    gdb_is_valid(buff[0] == 0, prefix1, "Should by zero byte (1)");

	    /* The next thing is the unknown 0x03 0x00 .. 0x00 (18 bytes) */
	    /* OK: this should be, but i've seen exceptions (...cannot verify the first byte */
	    gdb_fread(buff, 18); 

	    gdb_fread_le(&isteps, sizeof(isteps), 32, prefix1, "interlink steps");
	    
	    if (isteps <= 0) 						/* ??? end of route or error ??? */
	    {
		gdb_is_valid(count == 0, prefix3, "Zero interlink steps within route");
		
		gdb_fread(buff, 1);
		gdb_is_valid((buff[0] == 1), prefix3, "last seq.(1)");
		
		if (gdb_ver > 1)
		    gdb_fread(buff, 8);					/* Unknown 8 bytes since gdb v2 */

		gdb_fread(buff, 1);
		gdb_is_valid((buff[0] == 0), prefix3, "last seq.(2)");
		
		wpt = gdb_create_rte_wpt(xwptname, xlat, xlon, xalt);
		if (wpt != NULL)
		    route_add_wpt(route, wpt);
		return route;
	    }
	    
	    gdb_fread_le(&semilat, sizeof(semilat), 32, prefix1, "semi-latitude");
	    gdb_fread_le(&semilon, sizeof(semilon), 32, prefix1, "semi-longitude");
	    xlat = GPS_Math_Semi_To_Deg(semilat);
	    xlon = GPS_Math_Semi_To_Deg(semilon);
	    
	    gdb_is_validf(fabs(xlat) <= 90.0, prefix1, "Invalid latitude (%f)", xlat);
	    
	    if (gdb_fread_flag(1))					/* altitude flag */
		gdb_fread_le(&xalt, sizeof(xalt), 64, prefix1, "altitude");
	    else
		xalt = unknown_alt;

	    wpt = gdb_create_rte_wpt(xwptname, xlat, xlon, xalt);
	    if (wpt != NULL)
		route_add_wpt(route, wpt);
	    
	    while (--isteps > 0)
	    {
		gdb_fread_le(&semilat, sizeof(semilat), 32, prefix2, "semi-latitude");
		gdb_fread_le(&semilon, sizeof(semilon), 32, prefix2, "semi-longitude");
		gdb_fread(buff, 1);				/* altitude flag */
		if (buff[0] == 1)
		    gdb_fread_le(&xalt, sizeof(xalt), 64, prefix2, "altitude");

		xlat = GPS_Math_Semi_To_Deg(semilat);
		xlon = GPS_Math_Semi_To_Deg(semilon);
		gdb_is_validf(fabs(xlat) <= 90.0, prefix2, "Invalid latitude (%f)", xlat);
	    }
	    
	    gdb_fread(buff, 1);
	    gdb_is_valid(buff[0] == 0, prefix1, "\"Zero\" byte expected");

	    gdb_fread_le(&maxlat, sizeof(maxlat), 32, prefix1, "max. latitude");
	    gdb_fread_le(&maxlon, sizeof(maxlon), 32, prefix1, "max. longitude");

	    if (gdb_fread_flag(1))				/* link max alt validity + alt */
		gdb_fread(buff, 8);				

	    gdb_fread_le(&minlat, sizeof(minlat), 32, prefix1, "min. latitude");
	    gdb_fread_le(&minlon, sizeof(minlon), 32, prefix1, "min. longitude");

	    if (gdb_fread_flag(1))				/* link min alt validity + alt */
		gdb_fread(buff, 2 * sizeof(int));				
		
	    if (gdb_ver > 1)
		gdb_fread(buff, 8);				/* unknown 8 bytes since gdb v2 */
	}
	
	/* This should normally never happen; end of route is handled in main loop before this */
	
	fatal(MYNAME "-%s: Unexpected end of route \"%s\"!", prefix1, xname);
    return 0;
}


static route_head *
gdb_read_track(const size_t max_file_pos)
{
	char xname[GDB_NAME_BUFFERLEN];
	unsigned char xdisplay;
	int xcolour;
	int xlat;
	int xlon;
	int xtime = 0;
	double xalt = unknown_alt;
	double xdepth = unknown_alt;
	double xtemp;
	
	char buff[128];
	int count;
	
	route_head *track;
	waypoint *wpt;
	
	const char *prefix0 = "trk_read";
	const char *prefix = "trk_read_loop";
	
	gdb_fread_str(xname, sizeof(xname));
	
	gdb_fread_le(&xdisplay, sizeof(xdisplay), 8, prefix0, "display");
	gdb_fread_le(&xcolour, sizeof(xcolour), 32, prefix0, "colour");
	gdb_fread_le(&count, sizeof(count), 32, prefix0, "count");

	track = route_head_alloc();
	track->rte_name = xstrdup(xname);
	track_add_head(track);
	
	while (count--) 
	{
	    gdb_fread_le(&xlat, sizeof(xlat), 32, prefix, "latitude");
	    gdb_fread_le(&xlon, sizeof(xlon), 32, prefix, "longitude");
	    
	    gdb_fread(buff, 1);						/* altitude flag */
	    if (buff[0] == 1)
		gdb_fread_le(&xalt, sizeof(xalt), 64, prefix, "altitude");
	    
	    gdb_fread(buff, 1);						/* date/time flag */
	    if (buff[0] == 1)
		gdb_fread_le(&xtime, sizeof(xtime), 32, prefix, "time");
	    	    
	    gdb_fread(buff, 1);						/* depth flag */
	    if (buff[0] == 1)
		gdb_fread_le(&xdepth, sizeof(xdepth), 64, prefix, "depth");
	    
	    gdb_fread(buff, 1);						/* temperature flag */
	    if (buff[0] == 1)
		gdb_fread_le(&xtemp, sizeof(xtemp), 64, prefix, "temperature");
	    
	    wpt = waypt_new();
	    
	    wpt->latitude = GPS_Math_Semi_To_Deg(xlat);
	    wpt->longitude = GPS_Math_Semi_To_Deg(xlon);
	    wpt->creation_time = xtime;
	    wpt->centiseconds = 0;
	    wpt->altitude = xalt;
	    wpt->depth = xdepth;
	    
	    gdb_is_validf(fabs(wpt->latitude) <= 90.0, prefix, "Invalid latitude (%f)", wpt->latitude);
	    
	    route_add_wpt(track, wpt);
	}
	
	gdb_fread(buff, 1);
	
	return track;
}

/*******************************************************************************/

static void
gdb_read_data(void)
{
	int reclen, warnings;
	char typ;
	size_t curpos, anchor;
	int wptclass;
	
	const char *prefix = "main_read_loop";

	gdb_hidden = route_head_alloc();
	track_add_head(gdb_hidden);

	warnings = 0;
	
	anchor = ftell(fin);

	/* we go twice through the file to keep sure, all waypoints 
	   are loaded before any route has to be handled */
	
	while (feof(fin) == 0)
	{
	    
	    gdb_fread_le(&reclen, sizeof(reclen), 32, prefix, "record length");
	    gdb_is_valid(reclen > 0 && reclen < 0x1F00000, prefix, "record length");
	    gdb_fread(&typ, 1);
	    
	    curpos = ftell(fin);
	    
	    if (typ == 'W')
	    {
		int delta;
		waypoint *wpt;
		
		wpt = gdb_read_wpt(curpos + reclen, &wptclass);
		if (wpt != NULL )
		{
		    if (wptclass == 0)
		        waypt_add(wpt);
		    else if (gdb_via == 0)
		        route_add_wpt(gdb_hidden, wpt);
		    else
			waypt_free(wpt);
		}
		delta = (int)((curpos + reclen) - ftell(fin));
		if (delta != 0)
		{
		    if ((warnings & 1) == 0)
		    {
			warnings |= 1;
			warning(MYNAME "-%s: At least one incomplete waypoint read (%d byte(s) left).\n", prefix, delta);
		    }
		    fseek(fin, curpos + reclen, SEEK_SET);
		}
		continue;
	    }
	    else if (typ == 'V')
		break;
		
    	    fseek(fin, curpos + reclen, SEEK_SET);
	}

	clearerr(fin);
    	fseek(fin, anchor, SEEK_SET);

	while (feof(fin) == 0)
	{
	    gdb_fread_le(&reclen, sizeof(reclen), 32, prefix, "record length");
	    gdb_is_valid(reclen > 0 && reclen < 0x1F00000, prefix, "record length");
	    gdb_fread(&typ, 1);
	    
	    curpos = ftell(fin);
	    
	    if ((typ == 'R') || (typ == 'T'))
	    {
		int flag, delta;
		
		if (typ == 'R')
		{
		    gdb_read_route();
		    flag = 2; 
		}
		else
		{
		    gdb_read_track(curpos + reclen); 
		    flag = 4;
		}
		delta = (int)((curpos + reclen) - ftell(fin));
		if (delta != 0)
		{
		    if ((delta != reclen) && ((warnings & flag) == 0))
		    {
			warnings |= flag;
			warning(MYNAME "-%s: At least one incomplete %s (gdb v%d.0, %d byte(s) left).\n", 
			    prefix, (typ == 'R') ? "route" : "track", gdb_ver, delta);
		    }
    		    fseek(fin, curpos + reclen, SEEK_SET);
		}
	    }
	    else 
	    {
		if (typ == 'V') break;
		
		switch(typ)
		{
		    case 'D': break;
		    case 'L': break;
		    case 'W': break;
		    default: warning(MYNAME "-%s: Found unknown record type \"%c\"!\n", prefix, typ);
		}
    		fseek(fin, curpos + reclen, SEEK_SET);
	    }
	}
	
	/* finally kill our temporary queue */
	track_del_head(gdb_hidden);
}

/*******************************************************************************/
/* %%%                         write support                               %%% */
/*******************************************************************************/

/* helpers */

static waypoint **
gdb_route_point_list(const route_head *route, int *count)
{
	waypoint **result;
	queue *elem, *tmp;
	int i = 0;
	
	QUEUE_FOR_EACH((queue *)&route->waypoint_list, elem, tmp)
	{
	    waypoint *wpt = (waypoint *)elem;
	    if ((gdb_via == 0) || 
		(gdb_detect_rtept_class(wpt) == GDB_DEFAULTWPTCLASS)) i++;
	}
	
	*count = i;
	if (i == 0) return NULL;
	
	result = xcalloc(i, sizeof(*result));

	i = 0;
	QUEUE_FOR_EACH((queue *)&route->waypoint_list, elem, tmp)
	{
	    waypoint *wpt = (waypoint *)elem;
	    if ((gdb_via == 0) || 
		(gdb_detect_rtept_class(wpt) == GDB_DEFAULTWPTCLASS)) 
	    result[i++] = wpt;
	}
	
	return result;
}

static void 
gdb_fwrite(const void *data, const size_t size)
{
	fwrite(data, size, 1, fout);
}

static void 
gdb_fwrite_str(const char *str, const int len)
{

	if (len >= 0)			
	    gdb_fwrite(str, len);	/* write a string with fixed length */
	else
	{
	    char *tmp = str_utf8_to_cp1252((str != NULL) ? str : "");
	    gdb_fwrite(tmp, strlen(tmp) + 1);
	    xfree(tmp);
	}
}

static void 
gdb_fwrite_le(const void *data, const size_t size)
{
	int i;
	short s;
	char buff[8];
	
	switch(size)
	{
	    case 1:
		gdb_fwrite(data, 1);
		break;
		
	    case 2: 		/* sizeof(short): */
		s = *(short *)data;
		le_write16(&s, s);
		gdb_fwrite(&s, 2);
		break;
		
	    case 4:		/* sizeof(int): */
		i = *(int *)data;
		le_write32(&i, i);
		gdb_fwrite(&i, 4);
		break;

	    case 8:		/* sizeof(double): */
		le_read64(buff, data);
		gdb_fwrite(buff, 8);
		break;
		
	    default:
		fatal(MYNAME "-write_le: Unsupported data size (%ld)!\n", size);
	}
}

static void
gdb_fwrite_alt(const double alt, const double unknown_value)
{
	char c0 = 0;
	char c1 = 1;
	
	if (alt != unknown_value)	/* proximity / depth / altitude */
	{
	    gdb_fwrite(&c1, 1);
	    gdb_fwrite_le(&alt, sizeof(alt));
	}
	else
	    gdb_fwrite(&c0, 1);		/* no value */
}

static void 
gdb_fwrite_int(const int data)
{
	gdb_fwrite_le(&data, sizeof(data));
}

static void 
gdb_fwrite_icon(const waypoint *wpt)	/* partly taken from mapsource.c */
{
	int icon;
	char buff[128];
	
	if (	/* handle custom icons, which are linked to -2 in garmin_tables.c */
	    (wpt->icon_descr != NULL) && 
	    (sscanf(wpt->icon_descr, "%s%d", buff, &icon) == 2) &&
	    (case_ignore_strcmp(buff, "Custom") == 0) &&
	    (icon >= 0) && (icon <= 63)
	   ) 
	{
	    icon += 500;
	}
	else
	{
	    /* might need to change this to handle version dependent icon handling */
	    icon = gdb_find_icon_number_from_desc(wpt->icon_descr, MAPSOURCE);
	    if (get_cache_icon(wpt) /* && wpt->icon_descr && (strcmp(wpt->icon_descr, "Geocache Found") != 0)*/) 
	    {
		icon = gdb_find_icon_number_from_desc(get_cache_icon(wpt), MAPSOURCE);
	    }
	}
	gdb_fwrite_le(&icon, sizeof(icon));
}

/*******************************************************************************/
/* %%%                       write file header                             %%% */
/*-----------------------------------------------------------------------------*/

static void
gdb_write_file_header(const struct tm *tm)
{
	char buff[128];
	char *c;
	int len;
	
	gdb_fwrite_str("MsRcf", -1);
	gdb_fwrite_int(2);
	
	strncpy(buff, "Dx", sizeof(buff));
	buff[1] = 'k' - 1 + gdb_ver;
	gdb_fwrite_str(buff, -1);
	
#if 0
	strncpy(buff, "A].SQA*Dec 27 2004*17:40:51", sizeof(buff));		/* MapSource V6.5 */
#else
	/* This is our "Watermark" to show this file was created by GPSbabel */
	/* !!! We should define the date use through Makefile !!! */
	strncpy(buff, "A].GPSBabel_1.2.7-beta*Sep 13 2005*20:10:00", sizeof(buff));	/* gpsbabel V1.2.7 BETA */
#endif
	len = strlen(buff);
	buff[2] = 2;

	c = buff;	
	while ((c = strchr(c, '*'))) *c++ = '\0';

	gdb_fwrite_int(len);
	gdb_fwrite_str(buff, len + 1);

	gdb_fwrite_str("MapSource", -1);			/* MapSource magic */
}

static void
gdb_reset_short_handle(void)
{
	if (gdb_short_handle != NULL)
	    mkshort_del_handle(&gdb_short_handle);
	    
	gdb_short_handle = mkshort_new_handle();
	
	setshort_length(gdb_short_handle, GDB_NAME_BUFFERLEN);
	setshort_badchars(gdb_short_handle, "");
	setshort_mustupper(gdb_short_handle, 0);
	setshort_mustuniq(gdb_short_handle, 1);
	setshort_whitespace_ok(gdb_short_handle, 1);
	setshort_repeating_whitespace_ok(gdb_short_handle, 1);
}

/*******************************************************************************/
/* %%%                         write waypoints                             %%% */
/*-----------------------------------------------------------------------------*/

static void 
gdb_write_waypt(const waypoint *wpt, const int hidden)
{
	int i;
	char ffbuf[32], zbuf[32];
	char c0 = 0;
	char c1 = 1;
	
	gdb_is_validf((fabs(wpt->latitude) <= 90), "wpt_write", 
	    "%s: Invalid latitude (%f) detected\n", wpt->shortname, wpt->latitude);

	memset(ffbuf, 0xFF, sizeof(ffbuf));
	memset(zbuf, 0x00, sizeof(zbuf));
	
	gdb_fwrite_str(wpt->shortname, -1);

	gdb_fwrite_int( (hidden != 0) ? 
	    GDB_HIDDENROUTEWPTCLASS : GDB_DEFAULTWPTCLASS); /* class */
	gdb_fwrite_str("", -1);				/* country */

	gdb_fwrite(zbuf, 4);				/* subclass part 1 */
	gdb_fwrite(ffbuf, 12);				/* subclass part 2 */
	gdb_fwrite(zbuf, 2);				/* subclass part 3 */
	gdb_fwrite(ffbuf, 4);				/* unknown */

	gdb_fwrite_int(GPS_Math_Deg_To_Semi(wpt->latitude));
	gdb_fwrite_int(GPS_Math_Deg_To_Semi(wpt->longitude));
	
	gdb_fwrite_alt(wpt->altitude, unknown_alt);	/* altitude */
	
	gdb_fwrite_str((wpt->notes != NULL) ? wpt->notes : wpt->description, -1);	/* notes/comment/descr */
	
#if 0
	if (garmin_data != NULL)
	{
		gdb_fwrite_alt(garmin_data->proximity, 0);		/* proximity */
		gdb_fwrite_int(garmin_data->display);			/* display */
		gdb_fwrite_int(garmin_data->colour);			/* colour */
	{
		gdb_fwrite(&c0, 1);					/* NO proximity */
		gdb_fwrite_int(GDB_DISPLAY_SYMBOL_AND_NAME);		/* display */
		gdb_fwrite_int(0);					/* colour */
	
	}
#else
	gdb_fwrite(&c0, 1);				/* NO proximity */
	gdb_fwrite_int(GDB_DISPLAY_SYMBOL_AND_NAME);	/* display */
	gdb_fwrite_int(0);				/* colour */
#endif
	gdb_fwrite_icon(wpt);				/* icon    */	
	gdb_fwrite_str("", -1);				/* city */
	gdb_fwrite_str("", -1);				/* state */
	gdb_fwrite_str("", -1);				/* facility */
	gdb_fwrite(zbuf, 1);				/* unknown */
#if 0
	gdb_fwrite_alt((garmin_data != NULL) ? garmin_data->depth : 0, 0);	/* depth */
#else
	gdb_fwrite(&c0, 1);				/* NO depth */
#endif
	gdb_fwrite(zbuf, 3);				/* three unknown bytes */
	gdb_fwrite(zbuf, 4);				/* four unknown bytes */

	gdb_fwrite_str(wpt->url, -1);			/* URL */

#if 0
	if (gdb_opt_category != NULL)			/* category */
	    i = gdb_category;
	else
	    i = (wpt->garmin_data != NULL) ? wpt->garmin_data->category : 0;
#else
	i = gdb_category;
#endif
	gdb_fwrite_le(&i, 2);

	gdb_fwrite(zbuf, 1);				/* temperature flag */
	
	if (wpt->creation_time != 0)			/* creation time */
	{
	    gdb_fwrite(&c1, 1);
	    gdb_fwrite_int(wpt->creation_time);
	}
	else 
	    gdb_fwrite(&c0, 1);

}

static void 
gdb_write_waypt_cb(const waypoint *wpt)			/* called by waypt_disp over all waypoints */
{
	int reclen;
	size_t pos;
	
	/* check for duplicate waypoints */
	if (NULL != gdb_find_wpt_q_by_name((queue *)&gdb_hidden->waypoint_list, wpt->shortname))
	    return;

	gdb_fwrite_int(0);
	gdb_fwrite_str("W", 1);
	
	pos = ftell(fout);
	gdb_write_waypt(wpt, 0);
	reclen = ftell(fout) - pos;
	
	fseek(fout, pos - 5, SEEK_SET);
	gdb_fwrite_int(reclen);
	
	fseek(fout, pos + reclen, SEEK_SET);

	route_add_wpt(gdb_hidden, waypt_dupe(wpt));	/* add this point to our internal queue */
}

static void
gdb_write_rtewpt_cb(const waypoint *wpt)		/* called by waypt_disp (route points) */
{
	int reclen;
	size_t pos;
	waypoint *tmp;

	tmp = gdb_find_wpt_q_by_name((queue *)&gdb_hidden->waypoint_list, wpt->shortname);
	if (tmp == NULL)
	{
	    tmp = find_waypt_by_name(wpt->shortname);
	    
	    gdb_fwrite_int(0);
	    gdb_fwrite_str("W", 1);
	
	    pos = ftell(fout);
	    gdb_write_waypt(wpt, (tmp == NULL));
	    reclen = ftell(fout) - pos;
	
	    fseek(fout, pos - 5, SEEK_SET);
	    gdb_fwrite_int(reclen);
	
	    fseek(fout, pos + reclen, SEEK_SET);

	    route_add_wpt(gdb_hidden, waypt_dupe(wpt));	/* add this point to our internal queue */
	}
}

/*******************************************************************************/
/* %%%                         write routes                                %%% */
/*-----------------------------------------------------------------------------*/

static void
gdb_write_route(const route_head *route, const waypoint **list, const int count)
{
	int i, wpt_class;
	char buff[128], zbuff[32], ffbuff[32];
	waypoint *prev = NULL;
	const char c0 = 0;
	const char c1 = 1;
	const char c3 = 3;
	double maxlat = -90;
	double minlat = +90;
	double maxlon = -180;
	double minlon = +180;
	double maxalt = -unknown_alt;
	double minalt = +unknown_alt;

	memset(zbuff, 0, sizeof(zbuff));
	memset(ffbuff, 0xFF, sizeof(ffbuff));
		
	for (i = 0; i < count; i++)
	{
	    const waypoint *wpt = list[i];
	    
	    if (wpt->latitude > maxlat) maxlat = wpt->latitude;
	    if (wpt->latitude < minlat) minlat = wpt->latitude;
	    if (wpt->longitude > maxlon) maxlon = wpt->longitude;
	    if (wpt->longitude < minlon) minlon = wpt->longitude;
	    if (wpt->altitude != unknown_alt)
	    {
		if (wpt->altitude > maxalt) maxalt = wpt->altitude;
		if (wpt->altitude < minalt) minalt = wpt->altitude;
	    }
	}

	{
	    char *cname;
	    
	    if (route->rte_name == NULL)
	    {
		snprintf(buff, sizeof(buff), "Route%04d", route->rte_num);
		cname = mkshort(gdb_short_handle, buff);
	    }
	    else
		cname = mkshort(gdb_short_handle, route->rte_name);
	    
	    gdb_fwrite_str(cname, -1);
	    xfree(cname);
	}

	gdb_fwrite(&c0, 1);					/* auto_name */
	
	if (count == 1) gdb_fwrite(&c1, 1);			/* skip max data */
	else
	{	
	    gdb_fwrite(&c0, 1);					/* ??? */
	    gdb_fwrite_int(GPS_Math_Deg_To_Semi(maxlat));	/* maximum latitude over route */
	    gdb_fwrite_int(GPS_Math_Deg_To_Semi(maxlon));	/* maximum longitude over route */
	    gdb_fwrite_alt(maxalt, unknown_alt);		/* maximum altitude over route */

	    gdb_fwrite_int(GPS_Math_Deg_To_Semi(minlat));	/* minimum latitude over route */
	    gdb_fwrite_int(GPS_Math_Deg_To_Semi(minlon));	/* minimum longitude over route */
	    gdb_fwrite_alt(minalt, -unknown_alt);		/* minimum altitude over route */
	}

    	gdb_fwrite_int(count);					/* number of points in route */	
	
	for (i = 0; i < count; i++)
	{
	    const waypoint *wpt = list[i];

	    wpt_class = gdb_detect_rtept_class(wpt);
	    
	    if (prev != NULL)
	    {
		gdb_fwrite_int(2);					/* route link details */

		gdb_fwrite_int(GPS_Math_Deg_To_Semi(prev->latitude));	/* ilink step 1 (end point 1) */
		gdb_fwrite_int(GPS_Math_Deg_To_Semi(prev->longitude));
		gdb_fwrite_alt(prev->altitude, unknown_alt);

		gdb_fwrite_int(GPS_Math_Deg_To_Semi(wpt->latitude));	/* ilink step 2 (end point 2) */
		gdb_fwrite_int(GPS_Math_Deg_To_Semi(wpt->longitude));
		gdb_fwrite_alt(wpt->altitude, unknown_alt);
		
		if (wpt->latitude > prev->latitude)			/* get maximum lat, lon and alt */
		{
		    maxlat = wpt->latitude;
		    minlat = prev->latitude;
		}
		else
		{
		    maxlat = prev->latitude;
		    minlat = wpt->latitude;
		}
		if (wpt->longitude > prev->longitude)
		{
		    maxlon = wpt->longitude;
		    minlon = prev->longitude;
		}
		else
		{
		    maxlon = prev->longitude;
		    minlon = wpt->longitude;
		}
		if (wpt->altitude != unknown_alt)
		{
		    maxalt = wpt->altitude;
		    minalt = wpt->altitude;
		}
		else
		{
		    maxalt = -unknown_alt;
		    minalt = +unknown_alt;
		}
		if (prev->altitude != unknown_alt)
		{
		    if (prev->altitude > maxalt) maxalt = prev->altitude;
		    if (prev->altitude < minalt) minalt = prev->altitude;
		}
		
		gdb_fwrite(&c0, 1);					/* ??? */
		
		gdb_fwrite_int(GPS_Math_Deg_To_Semi(maxlat));		/* maximum coords & altitude  */
		gdb_fwrite_int(GPS_Math_Deg_To_Semi(maxlon));
		gdb_fwrite_alt(maxalt, unknown_alt);

		gdb_fwrite_int(GPS_Math_Deg_To_Semi(minlat));		/* minimum coords & altitude */
		gdb_fwrite_int(GPS_Math_Deg_To_Semi(minlon));
		gdb_fwrite_alt(minalt, -unknown_alt);

		if (gdb_ver > 1)
		    gdb_fwrite(ffbuff, 8);
	    }
	    
	    gdb_fwrite_str(wpt->shortname, -1);				/* short name */
	    
	    gdb_fwrite_int(wpt_class);					/* class */
	    gdb_fwrite_str("", -1);					/* country */
	    
	    gdb_fwrite(zbuff, 4);					/* subclass part 1 */
	    gdb_fwrite(ffbuff, 12);					/* subclass part 2 */
	    gdb_fwrite(zbuff, 2);					/* subclass part 3 */
	    gdb_fwrite(ffbuff, 4);					/* unknown */
	    
	    gdb_fwrite(&c0, 1);					/* unknown value or string */
	    gdb_fwrite(&c3, 1);					/* unknown 18 bytes starting with 0x03 */
	    gdb_fwrite(zbuff, 3);
	    gdb_fwrite(ffbuff, 4);
	    gdb_fwrite(zbuff, 10);
	    
	    prev = (waypoint *)wpt;
	}
	
	gdb_fwrite_int(0);		/* Zero interlink steps */
	gdb_fwrite(&c1, 1);

	if (gdb_ver > 1)
	    gdb_fwrite(ffbuff, 8);
	    
	gdb_fwrite(&c0, 1);
}

static void
gdb_write_route_cb(const route_head *route)
{
	int reclen;
	size_t pos;
	int count;
	waypoint **list;
	
	list = gdb_route_point_list(route, &count);
	if (count == 0) return;						/* don't write empty routes */

	gdb_fwrite_int(0);
	gdb_fwrite_str("R", 1);
	
	pos = ftell(fout);
	gdb_write_route(route, (const waypoint**)list, count);
	reclen = ftell(fout) - pos;
	
	fseek(fout, pos - 5, SEEK_SET);
	gdb_fwrite_int(reclen);
	
	fseek(fout, pos + reclen, SEEK_SET);
	
	xfree(list);
}

/*******************************************************************************/
/* %%%                          write tracks                               %%% */
/*-----------------------------------------------------------------------------*/

static void 
gdb_write_track(const route_head *track)
{
	char buff[128];
	const char c0 = 0;
	const char c1 = 1;
	queue *elem, *tmp;
	int count = track->rte_waypt_ct;
	
	{
	    char *cname;
	    
	    if (track->rte_name == NULL)
	    {
		snprintf(buff, sizeof(buff), "Track%04d", track->rte_num);
		cname = mkshort(gdb_short_handle, buff);
	    }
	    else
		cname = mkshort(gdb_short_handle, track->rte_name);
		
	    gdb_fwrite_str(cname, -1);
	    xfree(cname);
	}
	
	gdb_fwrite(&c0, 1);				/* display */
	gdb_fwrite_int(0);				/* xcolour */
	gdb_fwrite_int(count);
	
	QUEUE_FOR_EACH((queue *)&track->waypoint_list, elem, tmp)
	{
	    waypoint *wpt = (waypoint *)elem;
	    
	    gdb_fwrite_int(GPS_Math_Deg_To_Semi(wpt->latitude));
	    gdb_fwrite_int(GPS_Math_Deg_To_Semi(wpt->longitude));
	    gdb_fwrite_alt(wpt->altitude, unknown_alt);	/* altitude */
	    

	    if (wpt->creation_time != 0)			/* creation time */
	    {
		gdb_fwrite(&c1, 1);
		gdb_fwrite_int(wpt->creation_time);
	    }
	    else 
		gdb_fwrite(&c0, 1);
	    	    
	    gdb_fwrite_alt(wpt->depth, unknown_alt);	/* depth */
	    gdb_fwrite(&c0, 1);				/* temperature */
	}
	gdb_fwrite(&c0, 1);
}

static void
gdb_write_track_cb(const route_head *track)			/* called from track_disp_all */
{
	int reclen;
	size_t pos;
	
	if (track->rte_waypt_ct <= 0) return;			/* don't write empty tracks */
	
	gdb_fwrite_int(0);
	gdb_fwrite_str("T", 1);
	
	pos = ftell(fout);
	
	gdb_write_track(track);
	
	reclen = ftell(fout) - pos;
	fseek(fout, pos - 5, SEEK_SET);
	gdb_fwrite_int(reclen);
	
	fseek(fout, pos + reclen, SEEK_SET);
}

/*******************************************************************************/

static void
gdb_write_data(void)
{
	char c1 = 1;

	gdb_hidden = route_head_alloc();	/* contains all written waypts & rtepts */
	track_add_head(gdb_hidden);		/* tracks comes later and we drop this before */

	if (doing_wpts) 
	{
	    waypt_disp_all(gdb_write_waypt_cb);
	}
	if (doing_rtes)
	{
	    gdb_reset_short_handle();
	    setshort_defname(gdb_short_handle, "Route");
	    
	    if (gdb_via == 0) 
	    {
		/* find out all route points we have to write as a "HIDDEN CLASS" waypoint */
		route_disp_all(NULL, NULL, gdb_write_rtewpt_cb);
	    }
	    route_disp_all(gdb_write_route_cb, NULL, NULL);
	}

	track_del_head(gdb_hidden);		/* vaporize our temporary queue */

	if (doing_trks)
	{
	    gdb_reset_short_handle();
	    setshort_defname(gdb_short_handle, "Track");
	    track_disp_all(gdb_write_track_cb, NULL, NULL);
	}

	gdb_fwrite_int(2);			/* finalize gdb with empty map segment */
	gdb_fwrite_str("V", -1);
	gdb_fwrite(&c1, 1);
}

/*******************************************************************************/

static void
gdb_init_opts(const char op)	/* 1 = read; 2 = write */
{
	gdb_via = 0;
	gdb_category = 0;
	gdb_ver = 2;
	
	if (gdb_opt_via != NULL)	/* opt_via present in both ops */
	{
	    if ((case_ignore_strcmp(gdb_opt_via, GDB_OPT_VIA) == 0) ||
		(*gdb_opt_via == '\0'))
		gdb_via = 1;
	    else
		gdb_via = atoi(gdb_opt_via);
	}
	
	if (op & 2)		/* writer opts */
	{
	    if ((gdb_opt_category != NULL) &&
		(case_ignore_strcmp(gdb_opt_category, GDB_OPT_CATEGORY) != 0) &&
		(*gdb_opt_category != '\0'))
	    {
		gdb_category = atoi(gdb_opt_category);
		if ((gdb_category < 1) || (gdb_category > 16))
		    fatal(MYNAME ": Unsupported category \"%s\"!\n", gdb_opt_category);
		gdb_category = 1 << --gdb_category;
	    }
	    
	    gdb_ver = atoi(gdb_opt_ver);
	    if ((gdb_ver < GDB_VER_MIN) || (gdb_ver > GDB_VER_MAX))
		fatal(MYNAME ": Unsupported version \"%s\"!\n", gdb_opt_ver);
	}
}

/*******************************************************************************/
/* %%% global cb's %%% */
/*******************************************************************************/

static void 
gdb_rd_init(const char *fname)
{
	gdb_init_opts(1);
	
	fin_name = xstrdup(fname);
	fin = xfopen(fname, "rb", MYNAME);
}

static void
gdb_wr_init(const char *fname)
{
	gdb_init_opts(2);
	
	fout_name = xstrdup(fname);
	fout = xfopen(fname, "wb", MYNAME);
	gdb_short_handle = NULL;
	gdb_hidden = NULL;
}

static void 
gdb_rd_deinit(void)
{
	fclose(fin);
	xfree(fin_name);
	fin_name = NULL;
}

static void
gdb_wr_deinit(void)
{
	fclose(fout);
	xfree(fout_name);
	fout_name = NULL;
	mkshort_del_handle(&gdb_short_handle);
}

static void 
gdb_read(void)
{
	gdb_read_file_header();
	gdb_read_data();
}

static void
gdb_write(void)
{
	gdb_write_file_header(NULL);
	gdb_write_data();
}

/*******************************************************************************/

ff_vecs_t gdb_vecs = {
	ff_type_file,
	FF_CAP_RW_ALL,
	gdb_rd_init,	
	gdb_wr_init,
	gdb_rd_deinit,
	gdb_wr_deinit,
	gdb_read,
	gdb_write,
	NULL, 
	gdb_args,
	CET_CHARSET_MS_ANSI, 1				/* CET-REVIEW */
};

/*******************************************************************************/
