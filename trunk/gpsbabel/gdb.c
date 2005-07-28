/* 
	Garmin GPS Database Reader
	
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

#include <stdio.h>
#include <string.h>
#include <ctype.h>

#include "defs.h"
#include "garmin_tables.h"
#include "jeeps/gpsmath.h"

#define MYNAME "gdb"

#define MPSNAMEBUFFERLEN	1024
#define MPSNOTESBUFFERLEN	4096
#define MPSDESCBUFFERLEN	4096

#define DEFAULTICONVALUE	18
#define DEFAULTICONDESCR	"Waypoint"

#ifdef UTF8_SUPPORT
# define GDB_UTF8_ENABLED 1
#else
# define GDB_UTF8_ENABLED 0
#endif

/* %%% local vars %%% */

FILE *fin;
static char *fin_name;
static int gdb_ver = 1;
static int gdb_debug = 0;
route_head *gdb_hidden;

/* %%% 1-1 functions from mapsource, should by shared!!! %%% */

waypoint *
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

const char *
gdb_find_desc_from_icon_number(const int icon, garmin_formats_e garmin_format)
{
	icon_mapping_t *i;

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
				fatal(MYNAME ": unknown garmin format");
		}
	}
	return DEFAULTICONDESCR;
}

#ifndef UTF8_SUPPORT
char *gdb_garmin_to_utf8(const char *s)
{
	int len;
	char *res;
	unsigned char c;
	char *src, *dst;

	if (s == NULL) return NULL;

	len = 0;
	src = (char *)s;
	while ('\0' != (c = *src++))
	{
	    len++;
	    if (c & 0x80) len++;
	    if (c == 0x80) len++;
	}

	src = (char *)s;
	dst = res = (void *) xmalloc(len + 1);
	while ('\0' != (c = *src++))
	{
	    if (c == 0x80)
	    {
		*dst++ = 0xe2;
		*dst++ = 0x82;
		*dst++ = 0xac;
	    }
	    else if (c & 0x80)
	    {
		*dst++ = (0xc0 | (c >> 6));
		*dst++ = (c & 0xbf);
	    }
	    else
	    {
		*dst++ = c;
	    }
	}
	*dst = '\0';
	return res;
}
#endif

/* %%% local functions (read support) %%% */

char *
gdb_convert_name_buff(char *buff, size_t buffsize)
{
#ifdef UTF8_SUPPORT
	char *tmp = str_garmin_to_utf8(buff);
#else
	char *tmp = gdb_garmin_to_utf8(buff);
#endif	
	strncpy(buff, tmp, buffsize);
	xfree(tmp);
	return buff;
}

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

waypoint *
gdb_create_rte_wpt(const char *name, double lat, double lon, double alt)
{
	waypoint *wpt;
	
	wpt = find_waypt_by_name(name);
	if (wpt != NULL) wpt = waypt_dupe(wpt);
	else
	{
	    gdb_find_wpt_q_by_name((queue *)&gdb_hidden->waypoint_list, name);
	    if (wpt != NULL) wpt = waypt_dupe(wpt);
	    else
	    {
		wpt = waypt_new();
		wpt->shortname = xstrdup(name);
		wpt->latitude = lat;
		wpt->longitude = lon;
		wpt->altitude = alt;
		wpt->depth = unknown_alt;
	    }
	}
	return wpt;
}

static size_t
gdb_fread(void *target, size_t size, size_t count, FILE *fin)
{
	size_t result;

	result = fread(target, size, count, fin);
	if (result < count)
	{
	    if (feof(fin) != 0)
		fatal(MYNAME ": unexpected end of file \"%s\"!\n", fin_name);
	    else
		fatal(MYNAME ": I/O error occured during read from \"%s\"!\n", fin_name);
	}
	return result;
}

static int
gdb_fread_str(FILE *fin, char *dest, size_t maxlen)
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
}

static int
gdb_fread_le(FILE *fin, void *dest, size_t size, int bit_count, const char *field)
{
	char buff[32];
	unsigned char *c = dest;
	short *sh = dest;
	int *li = dest;
	double *db = dest;
	
	if ((bit_count >> 3) != size)
	    fatal(MYNAME ": internal error (gdb_le_read/%d/%d/%s)!\n", size, bit_count >> 3, field);
	    
	switch(bit_count)
	{
	    case 8:
		gdb_fread(c, sizeof(*c), 1, fin);
		if (gdb_debug) printf(MYNAME ": gdb_fread_le : %d -> %s (0x%x))\n", *c, field, *c);
		return *c;
	    case 16:
		if (sizeof(*sh) != size) fatal(MYNAME ": internal decl.!\n");
		gdb_fread(sh, sizeof(*sh), 1, fin);
		*sh = le_read16(sh);
		if (gdb_debug) printf(MYNAME ": gdb_fread_le : %d -> %s (0x%x))\n", *sh, field, *sh);
		return *sh;
	    case 32:
		gdb_fread(li, 4, 1, fin);
		*li = le_read32(li);
		if (gdb_debug) printf(MYNAME ": gdb_fread_le : %d -> %s (0x%x)\n", *li, field, *li);
		return *li;
	    case 64:
		gdb_fread(buff, sizeof(*db), 1, fin);
		le_read64(db, buff);
		if (gdb_debug) printf(MYNAME ": gdb_fread_le : %g -> %s\n", *db, field);
		return 0;
	    default:
		fatal(MYNAME ": unsupported bit count (%d) in gdb_le_read!\n", bit_count);	    
	}
}

static void
gdb_is_valid(int is, const char *comment)
{
	if (is == 0) fatal(MYNAME ": error in database structure (%s)!\n", comment);
}

static void
gdb_read_file_header(void)
{
	char buff[128];
	int i, reclen;

/* 
	We starts with standard binary read.
	A gdb_fread_str works too, but if we get a wrong file as input,
	the file validation my be comes too late. For example a XML base file normally 
	has no binary zeros inside and produce, if big enought, a buffer overflow. 
	The following message "local buffer overflow detected..." could be
	misinterpreted.
*/
	
	if (6 != fread(buff, 1, 6, fin))
	    fatal(MYNAME ": invalid file \"%s\"!\n", fin_name);
	    
	if (strcmp(buff, "MsRcf") != 0)
	    fatal(MYNAME ": invalid file \"%s\"!\n", fin_name);
	    
	gdb_fread(&reclen, 4, 1, fin);
	reclen = le_read32(&reclen);
	
	gdb_is_valid(reclen == gdb_fread_str(fin, buff, sizeof(buff)), "header");
	
	gdb_is_valid(buff[0] == 'D', "header");
	switch(buff[1])
	{
	    case 'k':
		gdb_ver = 1;
		break;
	    case 'l':
		gdb_ver = 2;
		break;
	    default:
		fatal(MYNAME ": non supported gdb version!\n");
	}
	
	if (global_opts.verbose_status > 0)
	    printf(MYNAME ": found Garmin GPS Database version %d\n", gdb_ver);
	
	gdb_fread(&reclen, 4, 1, fin);
	reclen = le_read32(&reclen);
	gdb_is_valid(reclen < sizeof(buff), "header");
	gdb_fread(buff, reclen, 1, fin);
	
	gdb_is_valid(0 == gdb_fread_str(fin, buff, sizeof(buff)), "header");
	
	i = gdb_fread_str(fin, buff, sizeof(buff));
	gdb_is_valid((i == 9) && (strcmp(buff, "MapSource") == 0), "header");
}

waypoint *
gdb_read_wpt(const size_t fileofs, int *wptclass)
{
	char xname[MPSNAMEBUFFERLEN];
	char xdesc[MPSDESCBUFFERLEN];
	char xnotes[MPSNOTESBUFFERLEN];
	int xclass;
	int xlat, xlon, xdisplay, xcolour, xicon, xtime;
	short xcat;
	double xdepth = unknown_alt;
	double xalt = unknown_alt;
	waypoint *res;

	char *ctmp;
	char buff[128];
	
	size_t pos, delta;

	
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

	gdb_is_valid(gdb_fread_str(fin, xname, sizeof(xname)) > 0, "new waypoint");
	gdb_convert_name_buff(xname, sizeof(xname));

	gdb_fread_le(fin, &xclass, sizeof(xclass), 32, "xclass");
	gdb_fread_str(fin, buff, sizeof(buff));				/* country */
	
	gdb_fread(buff, 22, 1, fin);
	xlat = gdb_fread_le(fin, &xlat, sizeof(xlat), 32, "xlat");	/* latitude */
	xlon = gdb_fread_le(fin, &xlon, sizeof(xlon), 32, "xlon");	/* latitude */
	
	gdb_fread(buff, 1, 1, fin);
	if (buff[0] == 1)						/* altitude flag */
	{
	    gdb_fread_le(fin, &xalt, sizeof(xalt), 64, "xalt");		/* altitude */
	}
	
	gdb_fread_str(fin, xdesc, sizeof(xdesc));
	gdb_convert_name_buff(xdesc, sizeof(xdesc));
	
	gdb_fread(buff, 1, 1, fin);					/* proximity flag */
	if (buff[0] == 1) 
	{
	    gdb_fread(buff, 8, 1, fin);					/* proximity */
	}
	
	xdisplay = gdb_fread_le(fin, &xdisplay, sizeof(xdisplay), 32, "xdisplay");
	xcolour = gdb_fread_le(fin, &xcolour, sizeof(xcolour), 32, "xcolour");
	xicon = gdb_fread_le(fin, &xicon, sizeof(xicon), 32, "xicon");

	/* ToDo: convert the icon !!! */
		
	gdb_fread_str(fin, buff, sizeof(buff));				/* city */
	gdb_fread_str(fin, buff, sizeof(buff));				/* state */
	gdb_fread_str(fin, buff, sizeof(buff));				/* facility */
	gdb_fread(buff, 1, 1, fin);					/* unknown */
	
	gdb_fread(buff, 1, 1, fin);					/* depth flag */
	if (buff[0] == 1) 
	{
	    gdb_fread_le(fin, &xdepth, sizeof(xdepth), 64, "xdepth");	/* depth */
	}
	
	gdb_fread(buff, 1, 1, fin);
	gdb_fread(buff, 1, 1, fin);
	gdb_fread(buff, 1, 1, fin);
	
	if (buff[0] != 0)
	    gdb_fread(buff, 3, 1, fin);
	else
	    gdb_fread(buff, 4, 1, fin);

	gdb_fread_str(fin, xnotes, sizeof(xnotes));
	gdb_convert_name_buff(xnotes, sizeof(xnotes));
	
	xcat = gdb_fread_le(fin, &xcat, sizeof(xcat), 16, "xcat");
	
	gdb_fread(buff, 1, 1, fin);					/* temperature flag */
	if (buff[0] == 1) 
	{
	    gdb_fread(buff, 8, 1, fin);					/* temperature */
	}

	/* Here comes 1 .. 6 unknown bytes
	   !!! 6 only if class > 0 !!!
	   the field seems to be a time stamp */
	
	pos = ftell(fin);
	delta = fileofs - pos;
	gdb_is_valid(delta > 0, "waypoint final");
	
	if ((delta & 1) == 0)
	{
	    gdb_fread(buff, 1, 1, fin);
	    delta--;
	}
	
	xtime = 0;
	gdb_fread(buff, 1, 1, fin);
	if (buff[0] == 1)
	{
	    gdb_is_valid(delta==5, "??? waypoint time ???");
	    gdb_fread_le(fin, &xtime, sizeof(xtime), 32, "xtime");
	}
	    else
		gdb_is_valid(delta==1, "no waypoint time");
	
	*wptclass = xclass;
	
	res = waypt_new();
	res->shortname = xstrdup(xname);
	res->description = xstrdup(xdesc);
	res->notes = xstrdup(xnotes);
	res->latitude = GPS_Math_Semi_To_Deg(xlat);
	res->longitude = GPS_Math_Semi_To_Deg(xlon);
	res->altitude = xalt;
	res->creation_time = xtime;
	/* might need to change this to handle version dependent icon handling */
	res->icon_descr = gdb_find_desc_from_icon_number(xicon, MAPSOURCE);
	
	gdb_is_valid(fabs(res->latitude) <= 90.0 && fabs(res->longitude) <= 180.0, " - wpt read: invalid lat or lon");

	return res;
}

route_head *
gdb_read_route(void)
{
	char xname[MPSNAMEBUFFERLEN];
	char xwptname[MPSNAMEBUFFERLEN];
	int xclass;
	double xalt;
	double xlat, xlon;
	
	char buff[256];
	int count;
	int checked;
	int isteps, ilink;
	int semilat, semilon;
	
	route_head *route;
	waypoint *wpt;
	
	int i, j;
	size_t curpos;
	
	gdb_is_valid(gdb_fread_str(fin, xname, sizeof(xname)) > 0, "route start");
	gdb_convert_name_buff(xname, sizeof(xname));

	gdb_fread_le(fin, buff, 2, 16, "auto_name");
	gdb_fread_le(fin, buff, 4, 32, "max_lat");
	gdb_fread_le(fin, buff, 4, 32, "max_lon");
	
	gdb_fread(buff, 1, 1, fin);
	if (buff[0] == 1) gdb_fread_le(fin, buff, 8, 64, "max_alt");
	    
	gdb_fread_le(fin, buff, 4, 32, "min_lat");
	gdb_fread_le(fin, buff, 4, 32, "min_lon");

	gdb_fread(buff, 1, 1, fin);
	if (buff[0] == 1) gdb_fread_le(fin, buff, 8, 64, "min_alt");
	    
	gdb_fread_le(fin, &count, sizeof(count), 32, "rte_count");
	if (count <= 0) return NULL;
	
	route = route_head_alloc();
	route->rte_name = xstrdup(xname);
	route_add_head(route);

#if GDB_DEBUG
	printf(MYNAME " - route: \"%s\" with %d point(s)\n", route->rte_name, count);
#endif
	
	if (count <= 0) return route;
	
	count--;
	
	while (count--)
	{
	    gdb_fread_str(fin, xwptname, sizeof(xwptname));		/* name */
	    gdb_convert_name_buff(xwptname, sizeof(xwptname));
	    
	    gdb_fread_le(fin, &xclass, sizeof(xclass), 32, "xclass");	/* class */
	    gdb_fread_str(fin, buff, sizeof(buff));			/* country */
	    
	    gdb_fread(buff, 22, 1, fin);				/* sub class data */
	    i = 0;
	    while (i < sizeof(buff))
	    {
		gdb_fread(&buff[i], 1, 1, fin);
		if (buff[i] == 0) break;
		i++;
	    } 

	    /* The next thing is the unknown 0x03 0x00 .. 0x00 (18 bytes) */
	    gdb_fread(buff, 18, 1, fin);
	    
	    gdb_fread_le(fin, &isteps, sizeof(isteps), 32, "isteps");
	    
	    if (isteps <= 0) return route;	
	    
	    gdb_fread_le(fin, &semilat, sizeof(semilat), 32, "semilat");
	    gdb_fread_le(fin, &semilon, sizeof(semilon), 32, "semilon");
	    xlat = GPS_Math_Semi_To_Deg(semilat);
	    xlon = GPS_Math_Semi_To_Deg(semilon);
	    
	    gdb_is_valid(fabs(xlat) <= 90.0 && fabs(xlon) <= 180.0, " - rte: read loop: invalid lat or lon");
	    
	    xalt = unknown_alt;
	    gdb_fread(buff, 1, 1, fin);				/* altitude flag */
	    if (buff[0] == 1)
	    {
		gdb_fread_le(fin, &xalt, sizeof(xalt), 64, "xalt");
	    }

	    wpt = gdb_create_rte_wpt(xwptname, xlat, xlon, xalt);
	    route_add_wpt(route, wpt);
	    
	    for (ilink = isteps-1; ilink > 0; ilink--)
	    {
		gdb_fread_le(fin, &semilat, sizeof(semilat), 32, "semilat");
		gdb_fread_le(fin, &semilon, sizeof(semilon), 32, "semilon");
		gdb_fread(buff, 1, 1, fin);				/* altitude flag */
		if (buff[0] == 1) gdb_fread_le(fin, &xalt, sizeof(xalt), 64, "xalt");
		xlat = GPS_Math_Semi_To_Deg(semilat);
		xlon = GPS_Math_Semi_To_Deg(semilon);
		gdb_is_valid(fabs(xlat) <= 90.0 && fabs(xlon) <= 180.0, " - rte/ils: read loop: invalid lat or lon");
	    }
	    
	    gdb_fread(buff, 1, 1, fin);			/* NULL */
	    gdb_is_valid(buff[0] == 0, "should be zero byte");

	    gdb_fread(buff, 4, 1, fin);				/* link max lat */
	    gdb_fread(buff, 4, 1, fin);				/* link max lon */
	    gdb_fread(buff, 1, 1, fin);
	    if (buff[0] == 1) 
	    {
		gdb_fread(buff, 8, 1, fin);			/* link max alt validity + alt */
	    }
	    gdb_fread(buff, 4, 1, fin);				/* link min lat */
	    gdb_fread(buff, 4, 1, fin);				/* link min lon */

	    gdb_fread(buff, 1, 1, fin);
	    if (buff[0] == 1) 
	    {
		gdb_fread(buff, 8, 1, fin);			/* link min alt validity + alt */
	    }

	    /* find the end of the record */

	    curpos = ftell(fin);
	    
	    /* !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
	    the stuff here is very tricky and did not base on any 
	    any knowledgement, but seems to work.
	    The final structure varied from file to file and i 
	    could not find any connection with data, gdb version
	    and any unknown bytes and bits. Hmm.
	    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! */

	    memset(buff, 0, sizeof(buff));
	    checked = 0;

	    j = 18;
	    while (checked == 0 && j-- > 0)
	    {
		for (i=1; i<8; i++) buff[i-1] = buff[i];
		gdb_fread(&buff[7], 1, 1, fin);
		for (i=0; i<8; i++)
		{
		    if (buff[i] != -1) break;
		    if (i == 7)	checked = 1;
		}
		
	    }
	    if (checked == 0)
	    {
    		fseek(fin, curpos, SEEK_SET);
	    }
	    else
	    {
		while (1)
		{
		    gdb_fread(buff, 1, 1, fin);
		    if (buff[0] != -1)
		    {
			fseek(fin, ftell(fin)-1, SEEK_SET);
			break;		
		    }
		}
		
	    }
	}
	
	gdb_fread_str(fin, xwptname, sizeof(xwptname));			/* name */
	gdb_convert_name_buff(xwptname, sizeof(xwptname));
	    
#if GDB_DEBUG
	printf(MYNAME " - rte/fin: \"%s\"\n", xwptname);
#endif
	gdb_fread_le(fin, &xclass, sizeof(xclass), 32, "xclass");	/* class */
	gdb_fread_str(fin, buff, sizeof(buff));				/* country */
	    
	wpt = gdb_create_rte_wpt(xwptname, xlat, xlon, xalt);
	route_add_wpt(route, wpt);
	
	return route;
}


route_head *
gdb_read_track(const size_t max_file_pos)
{
	char xname[MPSNAMEBUFFERLEN];
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
	
	gdb_fread_str(fin, xname, sizeof(xname));
	gdb_convert_name_buff(xname, sizeof(xname));
	
	gdb_fread_le(fin, &xdisplay, sizeof(xdisplay), 8, "xdisplay");
	gdb_fread_le(fin, &xcolour, sizeof(xcolour), 32, "xcolour");
	gdb_fread_le(fin, &count, sizeof(count), 32, "count");

	track = route_head_alloc();
	track->rte_name = xstrdup(xname);
	track_add_head(track);
	
	while (count > 0) 
	{
	    count--;
	    
	    gdb_fread_le(fin, &xlat, sizeof(xlat), 32, "xlat");
	    gdb_fread_le(fin, &xlon, sizeof(xlon), 32, "xlon");
	    
	    gdb_fread(buff, 1, 1, fin);					/* altitude flag */
	    if (buff[0] == 1)
		gdb_fread_le(fin, &xalt, sizeof(xalt), 64, "xalt");
	    
	    gdb_fread(buff, 1, 1, fin);					/* date/time flag */
	    if (buff[0] == 1)
		gdb_fread_le(fin, &xtime, sizeof(xtime), 32, "xtime");
	    	    
	    gdb_fread(buff, 1, 1, fin);					/* depth flag */
	    if (buff[0] == 1)
		gdb_fread_le(fin, &xdepth, sizeof(xdepth), 64, "xdepth");
	    
	    gdb_fread(buff, 1, 1, fin);
	    if (buff[0] == 1)
		gdb_fread_le(fin, &xtemp, sizeof(xtemp), 64, "xtemp");
	    
	    wpt = waypt_new();
	    
	    wpt->latitude = GPS_Math_Semi_To_Deg(xlat);
	    wpt->longitude = GPS_Math_Semi_To_Deg(xlon);
	    wpt->creation_time = xtime;
	    wpt->centiseconds = 0;
	    wpt->altitude = xalt;
	    wpt->depth = xdepth;
	    
	    gdb_is_valid(fabs(wpt->latitude) <= 90.0 && fabs(wpt->longitude) <= 180.0, " - trk read loop: invalid lat or lon");
	    
	    route_add_wpt(track, wpt);
	}
	
	return track;
}


static void
gdb_read_data(void)
{
	int reclen, done;
	char typ;
	size_t curpos, deltapos;
	waypoint *wpt;
	int wptclass;
	
	done = 0;
	while (!feof(fin) && (done == 0))
	{

	    gdb_fread_le(fin, &reclen, sizeof(reclen), 32, "reclen");
	    gdb_is_valid(reclen > 0 && reclen < 0x1F00000, "gdb data loop");
	    gdb_fread(&typ, 1, 1, fin);
	    
	    curpos = ftell(fin);
	    
	    switch(typ)
	    {
		case 'W':
		    wpt = gdb_read_wpt(curpos + reclen, &wptclass);
		    if (wpt != NULL )
		    {
			if (wptclass == 0)
			    waypt_add(wpt);
			else
			    route_add_wpt(gdb_hidden, wpt);
		    }
		    deltapos = (curpos+reclen)-ftell(fin);
		    break;
		case 'T':
		    gdb_read_track(curpos + reclen);
		    break;
		case 'R':
		    gdb_read_route();
		    break;
		case 'L':
		    break;
		case 'V':
		    done = 1;
		    break;
		default:
		    printf(MYNAME ": found unknown record type \"%c\"!\n", typ);
	    }
    	    fseek(fin, curpos + reclen, SEEK_SET);
	}
}

/* %%% gobal callbacks %%% */

static void gdb_rd_init(const char *fname)
{
	fin_name = xstrdup(fname);
	fin = xfopen(fname, "rb", MYNAME);
	gdb_hidden = route_head_alloc();
	track_add_head(gdb_hidden);
}

static void gdb_rd_deinit(void)
{
	track_del_head(gdb_hidden);
	fclose(fin);
	xfree(fin_name);
}

static void gdb_read(void)
{
	gdb_read_file_header();
	gdb_read_data();
}


ff_vecs_t gdb_vecs = {
	ff_type_file,
	{ ff_cap_read, ff_cap_read, ff_cap_read },	/* FF_CAP_RW_ALL, !!! I hope !!! */
	gdb_rd_init,	
	NULL,		/* gdb_wr_init, */
	gdb_rd_deinit,
	NULL,		/* gdb_wr_deinit, */
	gdb_read,
	NULL,		/* gdb_write, */
	NULL, 
	NULL		/* gdb_args */
};
