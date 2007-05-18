/*

    Support for Garmin Points of Interest (.gpi files)
    
    Copyright (C) 2007 Olaf Klein, o.b.klein@gpsbabel.org

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

	* 2007/05/18: initial release (only a reader)

	ToDo:
	
	* Display mode ("Symbol & Name")
	* decode speed/proximity
	* support category from GMSD "Garmin Special Data"
	* ... and the writer
*/

#include "defs.h"
#include "jeeps/gpsmath.h"
#include <ctype.h>
#include <stdio.h>
#include <string.h>
#include <time.h>

#define MYNAME "garmin_poi"

#define DEFAULT_ICON	"Waypoint"

static arglist_t garmin_poi_args[] = {
	ARG_TERMINATOR
};

typedef struct {
	char S3[9];		/* "GRMRECnn" */
	time_t crdate;		/* creation date and time */
	char POI[4];		/* "POI" */
	char S8[3];
	gbint32 codepage;	/* code-page, i.e. 1252 */
	char *group;
	char *category;
} gpi_data_t;

static gbfile *fin;
static gpi_data_t *dt;

#define GPI_DBG 1
#undef GPI_DBG

#ifdef GPI_DBG
# define PP printf("@%1$6x (%1$8d): ", gbftell(fin))
#else
# define PP
#endif

/* read a string with embedded "EN" header */
static char *
read_string(const int *sz)
{
	char en[3];
	short slen;
	char *res;
	
	gbfread(&en, 1, 2, fin);
	
	en[2] = '\0';
	if (strcmp(en, "EN") != 0) {
		fatal(MYNAME  ": Out of sync ('EN' expected)!\n");
	}
	
	slen = gbfgetint16(fin);
	if (sz && (*sz != slen + 4)) {
		fatal(MYNAME  ": Out of sync (wrong string size)!\n");
	}
	
	if (slen > 0) {
		res = xmalloc(slen + 1);
		res[slen] = '\0';
		gbfread(res, 1, slen, fin);
	}
	else res = NULL;
	
	return res;
}


static void
read_header(void)
{
	int len, i;
#ifdef GPI_DBG
	struct tm tm;
	char stime[32];
#endif	

	i = gbfgetint32(fin);
	if (i != 0) i = gbfgetint32(fin);
	(void) gbfgetint32(fin);

	gbfread(&dt->S3, 1, sizeof(dt->S3) - 1, fin);	/* GRMRECnn */
	if (strncmp(dt->S3, "GRMREC", 6) != 0)
		fatal(MYNAME ": No GPI file!\n");

	PP;
	dt->crdate = gbfgetint32(fin);
#ifdef GPI_DBG
	tm = *localtime(&dt->crdate);
	tm.tm_year += 20;	/* !!! */
	tm.tm_mday -= 1;	/* !!! */
	strftime(stime, sizeof(stime), "%Y/%m/%d %H:%M:%S", &tm);
	printf("crdate = %lu (%s)\n", dt->crdate, stime);
#endif	
	
	(void) gbfgetint16(fin);	/* 0 */

	len = gbfgetint16(fin);
	gbfseek(fin, len, SEEK_CUR);	/* "my.gpi" */

	(void) gbfgetint32(fin);	/* 1 */
	(void) gbfgetint32(fin);	/* 12 */

	gbfread(&dt->POI, 1, sizeof(dt->POI) - 1, fin);
	if (strcmp(dt->POI, "POI") != 0)
		fatal(MYNAME ": Wrong or unsupported GPI file!\n");

	for (i = 0; i < 3; i++) (void)gbfgetc(fin);
	gbfread(&dt->S8, 1, sizeof(dt->S8) - 1, fin);

	dt->codepage = gbfgetint32(fin);

#ifdef GPI_DBG
	PP;
	printf("< leaving header\n");
#endif	
}

/* gpi tag handler */
static int read_tag(const char *caller, const int tag, waypoint *wpt);


/* read a single poi with all options */
static void
read_poi(const int sz)
{
	int pos, len;
	waypoint *wpt;
	
#if GPI_DBG
	PP;
	printf("> reading poi (size %d)\n", sz);
#endif	
	(void) gbfgetint32(fin);	/* sub-header size */
	
	pos = gbftell(fin);
	
	wpt = waypt_new();
	wpt->icon_descr = DEFAULT_ICON;
	
	wpt->latitude = GPS_Math_Semi_To_Deg(gbfgetint32(fin));
	wpt->longitude = GPS_Math_Semi_To_Deg(gbfgetint32(fin));
	
	(void) gbfgetint16(fin);	/* ? always 1 ? */
	(void) gbfgetc(fin);		/* seems to 1 when extra options present */
	
	len = gbfgetint32(fin);
	
	PP;
	wpt->shortname = read_string(&len);
#ifdef GPI_DBG
	printf("shortname = %s\n", wpt->shortname);
#endif	
	
	while (gbftell(fin) < (pos + sz - 4)) {
		int tag = gbfgetint32(fin);
		if (! read_tag("read_poi", tag, wpt)) break;
	}

	if (wpt->notes && !wpt->description) wpt->description = xstrdup(wpt->notes);
	if (wpt->description && !wpt->notes) wpt->notes = xstrdup(wpt->description);

	waypt_add(wpt);

#ifdef GPI_DBG
	PP;
	printf("< leaving poi\n");
#endif	
}

/* read poi's following a group header */
static void
read_poi_list(const int sz)
{
	int pos;
	
#ifdef GPI_DBG
	PP;
	printf("> reading poi list\n");
#endif
	pos = gbftell(fin);
	
	(void) gbfgetint32(fin);	/* mostly 23 (0x17) */

	(void) gbfgetint32(fin);	/* max-lat */
	(void) gbfgetint32(fin);	/* max-lon */
	(void) gbfgetint32(fin);	/* min-lat */
	(void) gbfgetint32(fin);	/* min-lon */
	
	(void) gbfgetc(fin);		/* three unknown bytes */
	(void) gbfgetc(fin);		/* ? should be zero ? */
	(void) gbfgetc(fin);
	
	(void) gbfgetint32(fin);	/* ? const 0x1000100 ? */
	
	while (gbftell(fin) < (pos + sz - 4)) {
		int tag = gbfgetint32(fin);
		if (! read_tag("read_poi_list", tag, NULL)) return;
	}
#ifdef GPI_DBG
	PP;
	printf("< leaving poi list\n");
#endif
}


static void
read_poi_group(const int sz, const int tag)
{
	int len, pos;

#ifdef GPI_DBG
	PP;
	printf("> reading poi group\n");
#endif
	pos = gbftell(fin);
	
	if (tag == 0x80009) {
		(void) gbfgetint32(fin);	/* ? offset to category data ? */
	}

	len = gbfgetint32(fin);	/* size of group string */
	PP;
	if (dt->group) xfree(dt->group);
	dt->group = read_string(&len);
#ifdef GPI_DBG
	printf("Group \"%s\"\n", dt->group);
#endif
	while (gbftell(fin) < (pos + sz)) {
		int subtag = gbfgetint32(fin);
		if (! read_tag("read_poi_group", subtag, NULL)) break;
	}
	
#ifdef GPI_DBG
	PP;
	printf("< leaving poi group\n");
#endif
}


/* gpi tag handler */
static int
read_tag(const char *caller, const int tag, waypoint *wpt)
{
	int pos, sz, len;
	
	sz = gbfgetint32(fin);
	pos = gbftell(fin);
	
#ifdef GPI_DBG
	PP;
	printf("%s: tag = 0x%x (size %d)\n", caller, tag, sz);
#endif
	if ((tag >= 0x80000) && (tag <= 0x800ff)) sz += 4;
	
	switch(tag) {
		case 0x3:	/* size = 12 ? sound */
		case 0x4:	/* size = 2  ? */
		case 0x6:	/* size = 2  ? */
			break;
			
		case 0x5:	/* group bitmap (BMP: 22x22/256) */
			break;

		case 0x7:	/* category */
			(void) gbfgetint32(fin);
			(void) gbfgetint16(fin);
			if (dt->category) xfree(dt->category);
			PP;
			dt->category = read_string(NULL);
#ifdef GPI_DBG
			printf("Category: \"%s\"\n", dt->category);
#endif
			break;
			
		case 0xa:	/* notes */
			len = gbfgetint32(fin);
			PP;
			wpt->notes = read_string(&len);
#ifdef GPI_DBG
			printf("Notes: \"%s\"\n", wpt->notes);
#endif
			break;
			
		case 0xe:	/* ? also notes ? */
			len = gbfgetint32(fin);
			(void) gbfgetc(fin);
			
			len = sz - 5;
			wpt->description = xmalloc(len + 1);
			wpt->description[len] = '\0';
			PP;
			gbfread(wpt->description, 1, len, fin);
#ifdef GPI_DBG
			printf("Descr: \"%s\"\n", wpt->description);
#endif
			
			break;

		case 0x80002:
			read_poi(sz);
			break;

		case 0x80008:
			read_poi_list(sz);
			break;

		case 0x9:	/* ? older versions / no category data ? */
		case 0x80009:	/* current POI loader */
			read_poi_group(sz, tag);
			break;
			
		case 0x8000b:	/* phone-number */
			/* ToDo */
			break;

		case 0x8000c:	/* address (street/city...) */
			/* ToDo */
			break;

		case 0x80012:	/* ? sounds / images ? */
			break;

		default:
			warning(MYNAME ": Unknown tag (0x%x). Please report!\n", tag);
			return 0;
	}
	gbfseek(fin, pos + sz, SEEK_SET);
	return 1;
}


/*******************************************************************************
* %%%        global callbacks called by gpsbabel main process              %%% *
*******************************************************************************/

static void
garmin_poi_rd_init(const char *fname)
{
	char cp[8];
	
	fin = gbfopen(fname, "r", MYNAME);
	dt = xcalloc(1, sizeof(*dt));

	read_header();
	
	if ((dt->codepage >= 1250) && (dt->codepage <= 1257)) {
		snprintf(cp, sizeof(cp), "CP%d", dt->codepage);
		cet_convert_init(cp, 1);
	}
	else warning(MYNAME ": Unsupported code page (%d).\n", dt->codepage);
}


static void 
garmin_poi_rd_deinit(void)
{
	if (dt->category) xfree(dt->category);
	if (dt->group) xfree(dt->group);
	xfree(dt);
	gbfclose(fin);
}


static void
garmin_poi_read(void)
{
	while (! gbfeof(fin)) {	/* main loop */
		int tag;
		
		tag = gbfgetint32(fin);
		if ((tag == 0xffff) || (tag == 0xff)) return;
		
		if (! read_tag("garmin_poi_read", tag, NULL)) return;
	};
}

/**************************************************************************/

ff_vecs_t garmin_poi_vecs = {
	ff_type_file,
	{ 
		ff_cap_read | ff_cap_none 	/* waypoints */, 
	  	ff_cap_none 			/* tracks */, 
	  	ff_cap_none 			/* routes */
	},
	garmin_poi_rd_init,	
	NULL,	/* garmin_poi_wr_init, */
	garmin_poi_rd_deinit,	
	NULL,	/* garmin_poi_wr_deinit, */
	garmin_poi_read,
	NULL,	/* garmin_poi_write, */
	NULL,
	garmin_poi_args,
	CET_CHARSET_MS_ANSI, 0
};

/**************************************************************************/
