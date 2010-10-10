/*

    Support for Memory-Map Navigator Overlay Files (.mmo)

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
 
/*
    History:
	
	2008/10/18: Initial release
	2008/10/19: Don't write empty names
		    Add options 'locked' and 'visible'
	2008/11/06: Fix enumeration of objects for empty routes or tracks
		    Add option "ver" (internal version) to writer
		    We support write of version:
		    * 0x11 - reported as " "Memory Map OS Edition 2004, Version 4.2.3 Build 432"
		    * 0x12 - most files in my test pool :-)
	2008/11/19: Fix routes with a loop but different start and end point
	2008/12/12: Fix object release error
	2010/09/10: Added read support for version 0x18
		    (test file created by Memory-Map European Edition, Version 5.4.2, Build 1089).
*/

#include <ctype.h>
#include <errno.h>
#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

#include "defs.h"
#include "avltree.h"

#define MYNAME "mmo"

// #define MMO_DBG

static char *opt_locked, *opt_visible, *opt_version;

static
arglist_t mmo_args[] = {
	{ "locked", &opt_locked, "Write items 'locked' [default no]", "0",
		ARGTYPE_BOOL, ARG_NOMINMAX },
	{ "visible", &opt_visible, "Write items 'visible' [default yes]", "1",
		ARGTYPE_BOOL, ARG_NOMINMAX },
	{ "ver", &opt_version, "Write files with internal version [n]", NULL,
		ARGTYPE_INT, "17", "18" },
	ARG_TERMINATOR
};

typedef struct mmo_data_s {
	int objid;		/* internal object id */
	char *name;
	const char *category;	/* currently not handled */
	gpsdata_type type;	/* type of "data" */
	time_t ctime;
	time_t mtime;
	int left;		/* number of un-readed route points */
	void *data;		/* can be a waypoint, a route or a track */
	int refct;
	struct mmo_data_s **members;
	unsigned char visible:1;
	unsigned char locked:1;
	unsigned char loaded:1;
} mmo_data_t;

static gbfile *fin, *fout;
static int mmo_version;
static int mmo_obj_ct;
static int mmo_object_id;
static gbuint32 mmo_filemark;
static gbuint16 wpt_object_id;
static gbuint16 rte_object_id;
static gbuint16 trk_object_id;
static gbuint16 cat_object_id;
static gbuint16 ico_object_id;
static gbuint16 pos_object_id;
static gbuint16 txt_object_id;
static gpsdata_type mmo_datatype;
static route_head *mmo_rte;

static avltree_t *category_names, *objects, *mmobjects, *category_ids;
static avltree_t *icons;

typedef struct mmo_icon_mapping_s {
	const int	value;
	const char	*icon;
} mmo_icon_mapping_t;

/* standard icons; no bitmaps in file */

static const mmo_icon_mapping_t mmo_icon_value_table[] = {
	{ 0x00, "Dot" },
	{ 0x01, "House" },
	{ 0x02, "Fuel" },
	{ 0x03, "Car" },
	{ 0x04, "Fish" },
	{ 0x05, "Boat" },
	{ 0x06, "Anchor" },
	{ 0x07, "Wreck" },
	{ 0x08, "Exit" },
	{ 0x09, "Skull" },
	{ 0x0A, "Flag" },
	{ 0x0B, "Camp" },
	{ 0x0C, "Man Overboard" },
	{ 0x0D, "Deer" },
	{ 0x0E, "First Aid" },
	{ 0x0F, "Trackback" },
	{ 0x10, "Tiny dot" },
	{ 0x11, "Triangle" },
	{ 0x12, "Square" },
	{ 0x13, "Circle" },
	{ 0x14, "Green bouy" },
	{ 0x15, "Red bouy" },
	{ 0x16, "Yellow bouy" },
	{ 0x17, "Geocache" },

	{ -1, NULL }
};

static const gbuint32 obj_type_ico = 0x00;
static const gbuint32 obj_type_rte = 0x14;
static const gbuint32 obj_type_trk = 0x1E;
static const gbuint32 obj_type_txt = 0x32;
static const gbuint32 obj_type_wpt = 0x3C;

/* helpers */

#ifdef MMO_DBG
static void
dbgprintf(const char *sobj, const char *fmt, ...)
{
	va_list args;
	va_start(args, fmt);

	printf(MYNAME "-%s: ", sobj);
	vprintf(fmt, args);
	va_end(args);
}

# define DBG(args) dbgprintf args
#else
# define DBG(args) do {} while (0) ;
#endif

static char *
mmo_readstr(void)
{
	char *res;
	int len;
	
	len = (unsigned)gbfgetc(fin);
	if (len == 0xFF) {
		len = gbfgetint16(fin);
		if (len < 0) fatal(MYNAME ": Invalid string length (%d)!\n", len);
	}
	res = xmalloc(len + 1);
	res[len] = '\0';
	if (len) {
		gbfread(res, len, 1, fin);
		if (len != strlen(res)) fatal(MYNAME ": Error in file structure!\n");
	}

	return res;
}


static int
mmo_fillbuf2(void *buf, const gbsize_t bufsz, const gbsize_t count, const int need_all)
{
	gbsize_t res;
	
	if (count > (int)bufsz) fatal(MYNAME ": Internal error (bufsz too small)!\n");
	
	memset(buf, 0xFF, count);
	res = gbfread(buf, 1, count, fin);
	if (need_all && (res < count)) fatal(MYNAME ": Unexpected end of file!\n");
	
	return res;
}
#define mmo_fillbuf(a,b,c) mmo_fillbuf2((a),sizeof((a)),(b),(c))

static void
mmo_printbuf(const char *buf, int count, const char *comment)
{
#ifdef MMO_DBG
	int i;
	printf("%s", comment);
	for (i = 0; i < count; i++) printf("%02X ", buf[i] & 0xFF);
	printf("- ");
	for (i = 0; i < count; i++) 
		if (isprint(buf[i])) printf("%c", buf[i] & 0xFF);
		else printf(".");
	printf("\n");
	fflush(stdout);
#endif
}

/******************************************************************************/

static mmo_data_t *
mmo_register_object(const int objid, const void *ptr, const gpsdata_type type)
{
	char key[16];
	mmo_data_t *data;
	
	data = xcalloc(1, sizeof(*data));
	data->data = (void *)ptr;
	data->visible = 1;
	data->locked = 0;
	data->type = type;
	data->objid = objid;
	
	snprintf(key, sizeof(key), "%d", objid);
	avltree_insert(objects, key, data);
	
	return data;
}


static int
mmo_get_objid(const void *ptr)
{
	const char *key;
	mmo_data_t *data;
	
	if ((key = avltree_first(objects, (void *)&data))) do {
		if (data->data == ptr) {
			return atoi(key);
		}
	} while ((key = avltree_next(objects, key, (void *)&data)));

	return 0;
}


static mmo_data_t *
mmo_get_object(const gbuint16 objid)
{
	char key[16];
	mmo_data_t *data;
	
	snprintf(key, sizeof(key), "%d", objid | 0x8000);
	if (! avltree_find(objects, key, (void *)&data)) {
#ifdef MMO_DBG
		gbfseek(fin, -2, SEEK_CUR);
		int ni, n;
		for (ni = 0; (n = gbfgetc(fin)) != EOF; ni++) {
			DBG(("mmo_get_object", "%04X %02X %c (%d)\n",
				ni, n, n >= 32 && n <= 126 ? (char)n : '.', n));
		}
#endif
		fatal(MYNAME ": Unregistered object id 0x%04X!\n", objid | 0x8000);
	}
	
	return data;
}

static waypoint *
mmo_get_waypt(mmo_data_t *data)
{
	data->refct++;
	if (data->refct == 1) return (waypoint *)data->data;
	else return waypt_dupe((waypoint *)data->data);
}

static void
mmo_release_avltree(avltree_t *tree, const int is_object)
{
	const char *key;
	char *name;
	
	if ((key = avltree_first(tree, (void *)&name))) {
		do {
			if (name == NULL) continue;
			if (is_object) {
				mmo_data_t *data = (mmo_data_t *)name;
				if (data->name) xfree(data->name);
				if ((data->type == wptdata) && (data->refct == 0))
					waypt_free((waypoint *)data->data);
			}
			xfree(name);
		} while ((key = avltree_next(tree, key, (void *)&name)));
	}
	avltree_done(tree);
}


static void
mmo_register_icon(const int id, const char *name)
{
	char key[16];
	
	snprintf(key, sizeof(key), "%d", id);
	avltree_insert(icons, key, xstrdup(name));
}


static mmo_data_t *mmo_read_object(void);


static void
mmo_end_of_route(mmo_data_t *data)
{
#ifdef MMO_DBG
	const char *sobj = "CObjRoute";
#endif
	route_head *rte = data->data;
	char buf[7];

	if (mmo_version >= 0x12) {
		mmo_fillbuf(buf, 7, 1);
		DBG((sobj, "route data (since 0x12): "));
		mmo_printbuf(buf, 7, "");
		
		rte->line_color.bbggrr = le_read32(&buf[0]);
		rte->line_color.opacity = 255 - (buf[6] * 51);
		DBG((sobj, "color = 0x%06X\n", rte->line_color.bbggrr));
		DBG((sobj, "transparency = %d (-> %d)\n", buf[6], rte->line_color.opacity));
		DBG((sobj, "for \"%s\" \n", data->name));
	}

	if (rte->rte_waypt_ct == 0) {	/* don't keep empty routes */
		route_del_head(rte);
		data->data = NULL;
	}
}


static void
mmo_read_category(mmo_data_t *data)
{
	int marker = gbfgetuint16(fin);

	if (marker & 0x8000) {
		mmo_data_t *tmp;
		
		DBG(("mmo_read_category", "reading category object\n"));
		gbfseek(fin, -2, SEEK_CUR);
		tmp = mmo_read_object();
		if (data) data->category = tmp->name;
	}
}


static void
mmo_read_CObjIcons(mmo_data_t *data)
{
#ifdef MMO_DBG
	const char *sobj = "CObjIcons";
#endif
	int icon_id;

	DBG((sobj, ":-----------------------------------------------------\n"));
	DBG((sobj, "name = \"%s\" [ visible=%s, id=0x%04X ]\n", 
		data->name, data->visible ? "yes" : "NO", data->objid));

	if (mmo_version >= 0x18) {
		gbuint16 u16;
		u16 = gbfgetuint16(fin);
		DBG((sobj, "unknown value = 0x%04X (since 0x18)\n", u16));
		u16 = gbfgetuint16(fin);
		DBG((sobj, "unknown value = 0x%04X (since 0x18)\n", u16));
		u16 = gbfgetuint16(fin);
		DBG((sobj, "unknown value = 0x%04X (since 0x18)\n", u16));
		u16 = gbfgetuint16(fin);
		DBG((sobj, "unknown value = 0x%04X (since 0x18)\n", u16));
	}
	gbuint16 u16;
	u16 = gbfgetuint16(fin);
	DBG((sobj, "unknown value = 0x%04X\n", u16));
	u16 = gbfgetuint16(fin);
	DBG((sobj, "unknown value = 0x%04X\n", u16));
	u16 = gbfgetuint16(fin);
	DBG((sobj, "unknown value = 0x%04X\n", u16));

	while ((icon_id = gbfgetuint32(fin))) {
		char *name;
		(void) gbfgetuint32(fin);
		(void) gbfgetuint32(fin);
		name = mmo_readstr();
		DBG((sobj, "bitmap(0x%08X) = \"%s\"\n", icon_id, name));
		mmo_register_icon(icon_id, name);
		xfree(name);
		// The next four bytes hold the length of the image,
		// read them and then skip the image data.
		gbfseek(fin, gbfgetuint32(fin), SEEK_CUR);
	}
}


static void
mmo_read_CObjWaypoint(mmo_data_t *data)
{
#ifdef MMO_DBG
	const char *sobj = "CObjWaypoint";
#endif
	waypoint *wpt;
	time_t time;
	int rtelinks;
	mmo_data_t **rtelink = NULL;
	char *str;
	char buf[16];
	int i, ux;
	
	DBG((sobj, ":-----------------------------------------------------\n"));
	DBG((sobj, "name = \"%s\" [ visible=%s, id=0x%04X ]\n", 
		data->name, data->visible ? "yes" : "NO", data->objid));

	data->data = wpt = waypt_new();
	wpt->shortname = xstrdup(data->name);

	time = data->mtime;
	if (! time) time = data->ctime;
	if (time > 0) wpt->creation_time = time;

	if (mmo_version >= 0x18) {
		gbuint16 u16;
		u16 = gbfgetuint16(fin);
		DBG((sobj, "unknown value = 0x%04X (since 0x18)\n", u16));
		u16 = gbfgetuint16(fin);
		DBG((sobj, "unknown value = 0x%04X (since 0x18)\n", u16));
		u16 = gbfgetuint16(fin);
		DBG((sobj, "unknown value = 0x%04X (since 0x18)\n", u16));
		u16 = gbfgetuint16(fin);
		DBG((sobj, "unknown value = 0x%04X (since 0x18)\n", u16));
	}

	wpt->latitude = gbfgetdbl(fin);
	wpt->longitude = gbfgetdbl(fin);

	DBG((sobj, "coordinates = %f / %f\n", wpt->latitude, wpt->longitude));

	rtelinks = gbfgetuint16(fin);
	if (rtelinks > 0) {

		rtelink = xcalloc(sizeof(*rtelink), rtelinks);
		DBG((sobj, "rtelinks = %d\n", rtelinks));

		for (i = 0; i < rtelinks; i++) {
			mmo_data_t *tmp;

			DBG((sobj, "read rtelink number %d\n", i + 1));
			rtelink[i] = tmp = mmo_read_object();
		}

	}
	
	str = mmo_readstr();	/* descr + url */
	if (strncmp(str, "_FILE_ ", 7) == 0) {
		char *cx, *cend;
		
		cx = lrtrim(str + 7);
		cend = strchr(cx, '\n');
		if (cend == NULL) cend = cx + strlen(cx);
		
		cx = lrtrim(xstrndup(cx, cend - cx));
		if (*cx) wpt->url = cx;
		else xfree(cx);

		if (*cend++) wpt->notes = xstrdup(cend);
		
		if (wpt->url) DBG((sobj, "url = \"%s\"\n", wpt->url));
	}
	else
		if (*str) wpt->notes = xstrdup(str);
	xfree(str);

	if (wpt->notes) DBG((sobj, "notes = \"%s\"\n", wpt->notes));

	mmo_fillbuf(buf, 12, 1);
	i = le_read32(&buf[8]);		/* icon */
	if (i != -1) {
		char key[16];
		char *name;
		
		snprintf(key, sizeof(key), "%d", i);
		if (avltree_find(icons, key, (void *)&name)) {
			wpt->icon_descr = xstrdup(name);
			wpt->wpt_flags.icon_descr_is_dynamic = 1;
			DBG((sobj, "icon = \"%s\"\n", wpt->icon_descr));
		}
#ifdef MMO_DBG
		else {
			DBG((sobj, "icon not found for 0x%08X\n", i));
		}
#endif
	}

	wpt->proximity = le_read_float(&buf[4]);
	if (wpt->proximity) {
		wpt->wpt_flags.proximity = 1;
		DBG((sobj, "proximity = %f\n", wpt->proximity));
	}

	str = mmo_readstr();	/* name on gps ??? option ??? */
	if (*str) {
		wpt->description = wpt->shortname;
		wpt->shortname = str;
		DBG((sobj, "name on gps = %s\n", str));
	}
	else xfree(str);

	ux = gbfgetuint32(fin);
	DBG((sobj, "proximity type = %d\n", ux));
	
	data->loaded = 1;

	if (rtelink) xfree(rtelink);
	else waypt_add(mmo_get_waypt(data));
}


static void
mmo_read_CObjRoute(mmo_data_t *data)
{
#ifdef MMO_DBG
	const char *sobj = "CObjRoute";
#endif
	int rtept;
	route_head *rte;
	char buf[16];
	int ux;

	DBG((sobj, ":-----------------------------------------------------\n"));
	DBG((sobj, "name = \"%s\" [ visible=%s, id=0x%04X ]\n", 
		data->name, data->visible ? "yes" : "NO", data->objid));

	data->data = rte = route_head_alloc();
	rte->rte_name = xstrdup(data->name);
	route_add_head(rte);

	if (mmo_version >= 0x18) {
		gbuint16 u16;
		u16 = gbfgetuint16(fin);
		DBG((sobj, "unknown value = 0x%04X (since 0x18)\n", u16));
		u16 = gbfgetuint16(fin);
		DBG((sobj, "unknown value = 0x%04X (since 0x18)\n", u16));
		u16 = gbfgetuint16(fin);
		DBG((sobj, "unknown value = 0x%04X (since 0x18)\n", u16));
		u16 = gbfgetuint16(fin);
		DBG((sobj, "unknown value = 0x%04X (since 0x18)\n", u16));
	}

	ux = gbfgetc(fin);		/* line label */
	DBG((sobj, "line label = %d\n", ux));

	data->left = rtept = gbfgetint16(fin);
	DBG((sobj, "route has %d point(s)\n", rtept));

	if (data->left <= 0) {
		if (mmo_version >= 0x12) mmo_fillbuf(buf, 7, 1);
		route_del_head(rte);
		data->data = NULL;

		return;
	}

	while (data->left > 0) {
		mmo_data_t *tmp;
		
		DBG((sobj, "read next waypoint\n"));
		tmp = mmo_read_object();
		if (tmp && tmp->data && (tmp->type = wptdata)) {
			waypoint *wpt;
			
			/* FIXME: At this point this waypoint maybe not fully loaded (initialized) !!!
				  We need a final procedure to handle this !!! */
			if (! tmp->loaded) {
				wpt = waypt_new();
				wpt->latitude = 0;
				wpt->longitude = 0;
				xasprintf(&wpt->shortname, "\01%p", tmp);
			}
			else wpt = mmo_get_waypt(tmp);
			
			route_add_wpt(rte, wpt);
			data->left--;
		}
	}

	if (mmo_version > 0x11) mmo_end_of_route(data);
}


static void
mmo_read_CObjTrack(mmo_data_t *data)
{
#ifdef MMO_DBG
	const char *sobj = "CObjTrack";
#endif
	int tp, ctp;
	route_head *trk;

	DBG((sobj, ":-----------------------------------------------------\n"));
	DBG((sobj, "name = \"%s\" [ visible=%s, id=0x%04X ]\n", 
		data->name, data->visible ? "yes" : "NO", data->objid));

	trk = route_head_alloc();
	trk->rte_name = xstrdup(data->name);
	track_add_head(trk);

	if (mmo_version >= 0x18) {
		gbuint16 u16;
		u16 = gbfgetuint16(fin);
		DBG((sobj, "unknown value = 0x%04X (since 0x18)\n", u16));
		u16 = gbfgetuint16(fin);
		DBG((sobj, "unknown value = 0x%04X (since 0x18)\n", u16));
		u16 = gbfgetuint16(fin);
		DBG((sobj, "unknown value = 0x%04X (since 0x18)\n", u16));
		u16 = gbfgetuint16(fin);
		DBG((sobj, "unknown value = 0x%04X (since 0x18)\n", u16));
	}

	tp = gbfgetint16(fin);
	DBG((sobj, "track has %d point(s)\n", tp));

	for (ctp = 0; ctp < tp; ctp++) {
		waypoint *wpt;
		char unk;
		
		wpt = waypt_new();
		
		wpt->latitude = gbfgetdbl(fin);
		wpt->longitude = gbfgetdbl(fin);
		DBG((sobj, "coordinates = %f / %f\n", wpt->latitude, wpt->longitude));
		unk = gbfgetc(fin);
		DBG((sobj, "Unknown = 0x%02X (%d)\n", unk, unk));
		
		wpt->creation_time = gbfgetint32(fin);
		wpt->altitude = gbfgetflt(fin);

		if (unk != 0) {
			gbuint16 ux;
			ux = gbfgetuint16(fin);
			DBG((sobj, "unknown value = 0x%04X (%d)\n", ux, ux));
			if (unk > 1) {
				gbuint16 ux;
				ux = gbfgetuint16(fin);
				DBG((sobj, "unknown value = 0x%04X (%d)\n", ux, ux));
			}
		}
		track_add_wpt(trk, wpt);
	}
	
	if (mmo_version > 0) {
		gbuint32 u32;
		
		u32 = gbfgetuint32(fin); 	/* Min. update interval */
		DBG((sobj, "min. update interval = %d\n", u32));
		u32 = gbfgetuint32(fin); 	/* unknown */
		DBG((sobj, "unknown value = 0x%08X (%d)\n", u32, u32));
		u32 = gbfgetuint32(fin); 	/* unknown */
		DBG((sobj, "unknown value = 0x%08X (%d)\n", u32, u32));
		u32 = gbfgetuint32(fin); 	/* unknown */
		DBG((sobj, "min. update distance = %d\n", u32));
		u32 = gbfgetuint32(fin); 	/* unknown */
		DBG((sobj, "track partition interval = %d\n", u32 / 60));
		u32 = gbfgetuint32(fin); 	/* unknown */
		DBG((sobj, "unknown value = 0x%08X (%d)\n", u32, u32));
		u32 = gbfgetuint32(fin); 	/* unknown */
		DBG((sobj, "tick interval = %d\n", u32 / 60));
		trk->line_color.bbggrr = gbfgetuint32(fin); 	/* rgb color */
		trk->line_color.opacity = 255;
		DBG((sobj, "color = 0x%06X\n", trk->line_color.bbggrr));
	}

	if (mmo_version >= 0x12) {
		char u8;
		
		u8 = gbfgetc(fin);
		DBG((sobj, "line width = %d - (since 0x12)\n", u8));
		u8 = gbfgetc(fin);
		DBG((sobj, "line style = %d - (since 0x12)\n", u8));
		u8 = gbfgetc(fin);
		DBG((sobj, "transparency = %d - (since 0x12)\n", u8));
		trk->line_color.opacity = 255 - (u8 * 51);

		if (mmo_version >= 0x16) {
			char u8;
			gbuint16 u16;
			
			u8 = gbfgetc(fin);
			DBG((sobj, "unknown value = 0x%02X (since 0x16)\n", u8));
			u16 = gbfgetuint16(fin);
			DBG((sobj, "unknown value = 0x%04X (since 0x16)\n", u16));
			u16 = gbfgetuint16(fin);
			DBG((sobj, "unknown value = 0x%04X (since 0x16)\n", u16));
		}
	}
	
	if (trk->rte_waypt_ct == 0) {
		track_del_head(trk);
		data->data = NULL;
	}
}


static void
mmo_read_CObjText(mmo_data_t *data)
{
#ifdef MMO_DBG
	const char *sobj = "CObjText";
#endif
	char buf[28];
	double lat, lon;
	char *text, *font;
			
	DBG((sobj, ":-----------------------------------------------------\n"));
	DBG((sobj, "name = \"%s\" [ visible=%s, id=0x%04X ]\n", 
		data->name, data->visible ? "yes" : "NO", data->objid));

	lat = gbfgetdbl(fin);
	lon = gbfgetdbl(fin);
	DBG((sobj, "coordinates = %f / %f\n", lat, lon));

	text = mmo_readstr();
	DBG((sobj, "text = \"%s\"\n", text));
	xfree(text);

	mmo_fillbuf(buf, 28, 1);

	font = mmo_readstr();
	DBG((sobj, "font = \"%s\"\n", font));
	xfree(font);

	mmo_fillbuf(buf, 25, 1);
}


static void
mmo_read_CObjCurrentPosition(mmo_data_t *data)
{
#ifdef MMO_DBG
	const char *sobj = "CObjCurrentPosition";
#endif
	char buf[24];
	double lat, lon;
				
	DBG((sobj, ":-----------------------------------------------------\n"));
	DBG((sobj, "name = \"%s\" [ visible=%s, id=0x%04X ]\n", 
		data->name, data->visible ? "yes" : "NO", data->objid));

	lat = gbfgetdbl(fin);
	lon = gbfgetdbl(fin);
	DBG((sobj, "coordinates = %f / %f\n", lat, lon));

	mmo_fillbuf(buf, 24, 1);

	if (mmo_version >= 0x14) {
		char *name;
		
		name = mmo_readstr();
		DBG((sobj, "name = \"%s\"\n", name));
		xfree(name);
		mmo_fillbuf(buf, 13, 1);
	}
}


static mmo_data_t *
mmo_read_object(void)
{
	int objid;
	mmo_data_t *data = NULL;

	// There are three cases:
	// a new object of a type that has not occurred previously in this file;
	// a new object; or
	// a back reference to an object that appears earlier in the file.

	objid = gbfgetuint16(fin);
	if (objid == 0xFFFF) {
		DBG(("mmo_read_object", "Registering new object type\n"));
		gbuint16 version;
		char *sobj;
		int len;
		
		objid = mmo_object_id++;

		version = gbfgetuint16(fin);
		is_fatal(version != mmo_version, MYNAME ": Invalid version identifier!\n");
		
		len = gbfgetint16(fin);
		
		sobj = xmalloc(len + 1);
		sobj[len] = '\0';
		gbfread(sobj, len, 1, fin);
		DBG(("mmo_read_object", "%s\n", sobj));
		
		if (strcmp(sobj, "CObjIcons") == 0) ico_object_id = objid;
		else if (strcmp(sobj, "CCategory") == 0) cat_object_id = objid;
		else if (strcmp(sobj, "CObjWaypoint") == 0) wpt_object_id = objid;
		else if (strcmp(sobj, "CObjRoute") == 0) rte_object_id = objid;
		else if (strcmp(sobj, "CObjTrack") == 0) trk_object_id = objid;
		else if (strcmp(sobj, "CObjCurrentPosition") == 0) pos_object_id = objid;
		else if (strcmp(sobj, "CObjText") == 0) txt_object_id = objid;
		else
			fatal(MYNAME ": Unknown Object \"%s\"!\n", sobj);
		xfree(sobj);
	}

	DBG(("mmo_read_object", "objid = 0x%04X\n", objid));

	if (objid & 0x8000) {
		data = mmo_register_object(mmo_object_id++, NULL, 0);
		data->name = mmo_readstr();

		if (objid != cat_object_id) {
			data->ctime = gbfgetuint32(fin);
			data->mtime = gbfgetuint32(fin);
			data->locked = gbfgetc(fin);
			data->visible = gbfgetc(fin);

			gbuint32 obj_type;
			obj_type = gbfgetuint32(fin);
#ifdef MMO_DBG
			gbuint32 expected_type = 0xFFFFFFFF;
			if      (objid == ico_object_id) expected_type = obj_type_ico;
			else if (objid == trk_object_id) expected_type = obj_type_trk;
			else if (objid == wpt_object_id) expected_type = obj_type_wpt;
			else if (objid == rte_object_id) expected_type = obj_type_rte;
			else if (objid == txt_object_id) expected_type = obj_type_txt;
			if (mmo_version >= 0x18) expected_type <<= 24;
			DBG(("mmo_read_object", "object type = 0x%08X\n", obj_type));
			if (obj_type != expected_type)
				DBG(("mmo_read_object", "   expected   0x%08X\n", expected_type));
#endif

			if (objid != ico_object_id) mmo_read_category(data);
			DBG(("mmo_read_object", "Category : %s\n",
				data->category ? data->category : "[No category]"));
		}

		if (objid == cat_object_id) ; 	/* do nothing */		
		else if (objid == ico_object_id) mmo_read_CObjIcons(data);
		else if (objid == trk_object_id) {
			data->type = trkdata;
			mmo_read_CObjTrack(data);
		}
		else if (objid == wpt_object_id) {
			data->type = wptdata;
			mmo_read_CObjWaypoint(data);
		}
		else if (objid == rte_object_id) {
			data->type = rtedata;
			mmo_read_CObjRoute(data);
		}
		else if (objid == pos_object_id) mmo_read_CObjCurrentPosition(data);
		else if (objid == txt_object_id) mmo_read_CObjText(data);
		else
			fatal(MYNAME ": Unregistered Object-ID 0x%04X\n", objid);
	}
	else data = mmo_get_object(objid);
	
	return data;
}

static void
mmo_finalize_rtept_cb(const waypoint *wptref)
{
	waypoint *wpt = (waypoint *)wptref;
	
	if ((wpt->shortname[0] == 1) && (wpt->latitude == 0) && (wpt->longitude == 0)) {
		mmo_data_t *data;
		waypoint *wpt2;

		sscanf(wpt->shortname + 1, "%p", &data);
		wpt2 = (waypoint *)data->data;

		wpt->latitude = wpt2->latitude;
		wpt->longitude = wpt2->longitude;

		xfree(wpt->shortname);
		wpt->shortname = xstrdup(wpt2->shortname);

		if (wpt2->description) wpt->description = xstrdup(wpt2->description);
		if (wpt2->notes) wpt->notes = xstrdup(wpt2->notes);
		if (wpt2->url) wpt->notes = xstrdup(wpt2->url);

		wpt->proximity = wpt2->proximity;
		wpt->wpt_flags.proximity = wpt2->wpt_flags.proximity;

		if (wpt2->icon_descr) {
			wpt->icon_descr = xstrdup(wpt2->icon_descr);
			wpt->wpt_flags.icon_descr_is_dynamic = 1;
		}
	}
}

/*******************************************************************************
* %%%        global callbacks called by gpsbabel main process              %%% *
*******************************************************************************/

static void
mmo_rd_init(const char *fname)
{
	int i;
	
	fin = gbfopen_le(fname, "rb", MYNAME);
	
	category_ids = avltree_init(0, MYNAME);
	objects = avltree_init(0, MYNAME);
	icons = avltree_init(0, MYNAME);
	
	ico_object_id = pos_object_id = txt_object_id = cat_object_id = 0;
	wpt_object_id = rte_object_id = trk_object_id = 0;
	
	mmo_object_id = 0x8001;
	
	i = 0;
	while (mmo_icon_value_table[i].icon) {
		mmo_register_icon(mmo_icon_value_table[i].value, mmo_icon_value_table[i].icon);
		i++;
	}
}


static void 
mmo_rd_deinit(void)
{
	route_disp_session(curr_session(), NULL, NULL, mmo_finalize_rtept_cb);

	mmo_release_avltree(icons, 0);
	mmo_release_avltree(category_ids, 0);
	mmo_release_avltree(objects, 1);
	gbfclose(fin);
}


static void
mmo_read(void)
{
#ifdef MMO_DBG
	const char *sobj = "main";
#endif
	gbfile *fx;
	int i;

	/* copy file to memory stream (needed for seek-ops and piped commands) */

	DBG(("main", "loading file \"%s\".\n", fin->name));
	
	fx = gbfopen(NULL, "wb", MYNAME);
	gbfcopyfrom(fx, fin, 0x7FFFFFFF);
	gbfrewind(fx);
	gbfclose(fin);
	fin = fx;
		
	mmo_obj_ct = gbfgetuint16(fin);
	DBG((sobj, "number of objects = %d\n", mmo_obj_ct));
	
	i = gbfgetuint16(fin);
	if (i != 0xFFFF) fatal(MYNAME ": Marker not equal to 0xFFFF!\n");

	mmo_version = gbfgetuint16(fin);
	DBG((sobj, "version = 0x%02X\n", mmo_version));

	mmo_filemark = 0xFFFF0000UL | be_read16(&mmo_version);
	DBG((sobj, "filemark = 0x%08X\n", mmo_filemark));

	gbfseek(fin, -4, SEEK_CUR);

	while (! gbfeof(fin)) {		/* main read loop */

		(void) mmo_read_object();

	}

#ifdef MMO_DBG
	printf("\n" MYNAME ":---------------------------------------\n");
	printf(MYNAME ": EOF reached, nice!!!\n");
	printf(MYNAME ": =======================================\n\n");
#endif	
}

/**************************************************************************/

static void
mmo_register_category_names(const char *name)
{
	char key[16];

	snprintf(key, sizeof(key), "%d", mmo_object_id);
	avltree_insert(category_names, name, xstrdup(key));
}


static void
mmo_writestr(const char *str)
{
	int len = strlen(str);

	if (len > 254) {
		len = len & 0x7FFF;
		gbfputc(0xFF, fout);
		gbfputint16(len, fout);
	}
	else gbfputc(len, fout);
	if (len) gbfwrite(str, len, 1, fout);
}


static void
mmo_enum_waypt_cb(const waypoint *wpt)
{
	mmo_obj_ct++;
}


static void
mmo_enum_route_cb(const route_head *rte)
{
	if (rte->rte_waypt_ct > 0) mmo_obj_ct++;
}


static int
mmo_write_obj_mark(const char *sobj, const char *name)
{
	char *key;
	gbuint16 nr;
	char buf[16];
	int res;
	
	if (avltree_find(mmobjects, sobj, (void *)&key)) {
		nr = (unsigned)atoi(key);
		gbfputuint16(nr, fout);
	}
	else {
		mmo_object_id++;
		snprintf(buf, sizeof(buf), "%u", mmo_object_id);

		DBG(("write", "object \"%s\", registered type \"%s\" (id = 0x%04X)\n",
			name, sobj, mmo_object_id));

		avltree_insert(mmobjects, sobj, xstrdup(buf));
		
		gbfputuint32(mmo_filemark, fout);
		gbfputuint16(strlen(sobj), fout);
		gbfwrite(sobj, strlen(sobj), 1, fout);
	}

	mmo_object_id++;
	res = mmo_object_id;
	mmo_writestr(name);

	return res;
}


static void
mmo_write_category(const char *sobj, const char *name)
{
	char *key;
	gbuint16 nr;
	
	if (avltree_find(category_names, name, (void *)&key)) {
		nr = (unsigned)atoi(key);
		gbfputuint16(nr & 0x7FFF, fout);
	}
	else {
		mmo_write_obj_mark(sobj, name);
		mmo_register_category_names(name);
	}
}


static int
mmo_write_obj_head(const char *sobj, const char *name, const time_t ctime, 
	const gbuint32 obj_type)
{
	int res;

	res = mmo_write_obj_mark(sobj, name);
	
	gbfputuint32(ctime, fout);
	gbfputuint32(ctime, fout);

	gbfputc(*opt_locked, fout);
	gbfputc(*opt_visible, fout);

	gbfputuint32(obj_type, fout);
	
	return res;
}


static void
mmo_write_wpt_cb(const waypoint *wpt)
{
	char *str, *cx;
	int objid;
	time_t time;
	int icon = 0;
	mmo_data_t *data;

	time = wpt->creation_time;
	if (time < 0) time = 0;
	
	if (mmo_datatype == trkdata) {
		gbfputdbl(wpt->latitude, fout);
		gbfputdbl(wpt->longitude, fout);
		gbfputc(0, fout);
		gbfputuint32(time, fout);
		if (wpt->altitude != unknown_alt) 
			gbfputflt(wpt->altitude, fout);
		else
			gbfputflt(0, fout);

		return;
	}

	DBG(("write", "waypoint \"%s\"\n", wpt->shortname ? wpt->shortname : "Mark"));

	objid = mmo_write_obj_head("CObjWaypoint",
		(wpt->shortname && *wpt->shortname) ? wpt->shortname : "Mark", time, obj_type_wpt);
	data = mmo_register_object(objid, wpt, wptdata);
	data->refct = 1;
	mmo_write_category("CCategory", (mmo_datatype == rtedata) ? "Waypoints" : "Marks");

	gbfputdbl(wpt->latitude, fout);
	gbfputdbl(wpt->longitude, fout);
	
	if (mmo_datatype == rtedata) {
		int i = mmo_get_objid(mmo_rte);
		gbfputuint16(1, fout); /* two extra bytes */
		gbfputuint16(i & 0x7FFF, fout);
	}
	else
		gbfputuint16(0, fout); /* extra bytes */

	if (wpt->url && *wpt->url) {
		str = xstrdup("_FILE_ ");
		str = xstrappend(str, wpt->url);
		str = xstrappend(str, "\n");
	}
	else str = xstrdup("");
	
	cx = wpt->notes;
	if (cx == NULL) cx = wpt->description;
	if (cx != NULL) {
		char *kml = NULL;
		
		if (strcmp(wpt->session->name, "kml") == 0) {
			utf_string tmp;

			tmp.utfstring = cx;
			tmp.is_html = 1;
			cx = kml = strip_html(&tmp);
		}
		str = xstrappend(str, cx);
		if (kml) xfree(kml);
	}
	mmo_writestr(str);
	xfree(str);
	
	gbfputuint32(0x01, fout);
	if WAYPT_HAS(wpt, proximity) gbfputflt((int) wpt->proximity, fout);
	else gbfputflt(0, fout);
	
	if (wpt->icon_descr) {
		int i = 0;
		
		while (mmo_icon_value_table[i].icon) {
			if (case_ignore_strcmp(wpt->icon_descr, mmo_icon_value_table[i].icon) == 0) {
				icon = mmo_icon_value_table[i].value;
				break;
			}
			i++;
		}
	}
	gbfputuint32(icon, fout);

	mmo_writestr("");		/* name on gps */
	gbfputuint32(0x00, fout);
}


static void
mmo_write_rte_head_cb(const route_head *rte)
{
	int objid;
	queue *elem, *tmp;
	time_t time = 0x7FFFFFFF;

	if (rte->rte_waypt_ct <= 0) return;

	mmo_rte = (route_head *)rte;

	QUEUE_FOR_EACH(&rte->waypoint_list, elem, tmp) {
		waypoint *wpt = (waypoint *)elem;

		if ((wpt->creation_time > 0) && (wpt->creation_time < time))
			time = wpt->creation_time;
	}
	if (time == 0x7FFFFFFF) time = gpsbabel_time;
	
	objid = mmo_write_obj_head("CObjRoute",
		(rte->rte_name && *rte->rte_name) ? rte->rte_name : "Route", time, obj_type_rte);
	mmo_register_object(objid, rte, rtedata);
	mmo_write_category("CCategory", "Route");
	gbfputc(0, fout); /* unknown */
	gbfputuint16(rte->rte_waypt_ct, fout);
}


static void
mmo_write_rte_tail_cb(const route_head *rte)
{
	queue *elem, *tmp;
	
	if (rte->rte_waypt_ct <= 0) return;

	DBG(("write", "route with %d point(s).\n", rte->rte_waypt_ct));

	if (mmo_version >= 0x12) {
		if (rte->line_color.bbggrr < 0) {
			gbfputuint32(0xFF, fout);	/* color; default red */
			gbfputc(0x01, fout);		/* Line width "normal" */
			gbfputc(0x00, fout);		/* Line style "solid"*/
			gbfputc(0x00, fout);		/* Transparency "Opaque" */
		}
		else {
			gbfputuint32(rte->line_color.bbggrr, fout);	/* color */
			gbfputc(0x01, fout);		/* Line width "normal" */
			gbfputc(0x00, fout);		/* Line style "solid"*/
			gbfputc((255 - rte->line_color.opacity) / 51, fout);	/* Transparency "Opaque" */
		}
	}

	QUEUE_FOR_EACH(&rte->waypoint_list, elem, tmp) {
		waypoint *wpt = (waypoint *)elem;
		int objid = mmo_get_objid(wpt);
		gbfputuint16(objid & 0x7FFF, fout);
	}
}


static void
mmo_write_trk_head_cb(const route_head *trk)
{
	int objid;

	if (trk->rte_waypt_ct <= 0) return;
	
	objid = mmo_write_obj_head("CObjTrack",
		(trk->rte_name && *trk->rte_name) ? trk->rte_name : "Track", gpsbabel_time, obj_type_trk);
	mmo_write_category("CCategory", "Track");
	gbfputuint16(trk->rte_waypt_ct, fout);
	
	mmo_register_object(objid, trk, trkdata);
}


static void
mmo_write_trk_tail_cb(const route_head *trk)
{
	if (trk->rte_waypt_ct <= 0) return;

	gbfputuint32(0x0A, fout);	/* Min. update interval */
	gbfputflt(0, fout);
	gbfputflt(0, fout);
	gbfputuint32(0x0F, fout);	/* Min. update distance */
	gbfputuint32(0xE10, fout);	/* Track partition interval */
	gbfputuint32(0x00, fout);	/* ??? */
	gbfputuint32(0x12C, fout);

	if (trk->line_color.bbggrr < 0) {
		gbfputuint32(0xFF0000, fout);	/* color; default blue */
		if (mmo_version >= 0x12) {
			gbfputc(0x01, fout);		/* Line width "normal" */
			gbfputc(0x00, fout);		/* Line style "solid"*/
			gbfputc(0x00, fout);		/* Transparency "Opaque" */
		}
	}
	else {
		gbfputuint32(trk->line_color.bbggrr, fout);	/* color */
		if (mmo_version >= 0x12) {
			gbfputc(0x01, fout);		/* Line width "normal" */
			gbfputc(0x00, fout);		/* Line style "solid"*/
			gbfputc((255 - trk->line_color.opacity) / 51, fout);	/* Transparency "Opaque" */
		}
	}
}

/**************************************************************************/

static void
mmo_wr_init(const char *fname)
{
	fout = gbfopen_le(fname, "wb", MYNAME);
	
	objects = avltree_init(0, MYNAME);
	mmobjects = avltree_init(0, MYNAME);
	category_names = avltree_init(0, MYNAME);

	mmo_object_id = 0x8000;
	mmo_obj_ct = 1;			/* ObjIcons always present */
	mmo_version = 0x12;		/* by default we write as version 0x12 */
	if (opt_version) {
		while (isspace(*opt_version)) opt_version++;
		errno = 0;
		mmo_version = strtol(opt_version, NULL, 0);
		if (errno || ((mmo_version != 0x11) && (mmo_version != 0x12))) {
			fatal(MYNAME ": Unsupported version identifier (%s)!\n", opt_version);
		}
	}
	DBG(("write", "version = 0x%02X\n", mmo_version));
	mmo_filemark = 0xFFFFUL | (mmo_version << 16);
}


static void
mmo_wr_deinit(void)
{
	mmo_release_avltree(mmobjects, 0);
	mmo_release_avltree(category_names, 0);
	mmo_release_avltree(objects, 1);

	gbfclose(fout);
}


static void
mmo_write(void)
{
	int i;
	
	/* find out number of objects we have to write */
	waypt_disp_all(mmo_enum_waypt_cb);
	route_disp_all(mmo_enum_route_cb, NULL, mmo_enum_waypt_cb);
	track_disp_all(mmo_enum_route_cb, NULL, NULL);
	
	gbfputuint16(mmo_obj_ct, fout);
	
	mmo_write_obj_head("CObjIcons", "Unnamed object", gpsbabel_time, obj_type_ico);
	for (i = 0; i < 5; i++) gbfputuint16(0, fout);

	mmo_datatype = wptdata;
	waypt_disp_all(mmo_write_wpt_cb);
	mmo_datatype = rtedata;
	route_disp_all(mmo_write_rte_head_cb, mmo_write_rte_tail_cb, mmo_write_wpt_cb);
	mmo_datatype = trkdata;
	track_disp_all(mmo_write_trk_head_cb, mmo_write_trk_tail_cb, mmo_write_wpt_cb);
}

/**************************************************************************/

ff_vecs_t mmo_vecs = {
	ff_type_file,
	FF_CAP_RW_ALL,	/* read and write waypoints, tracks and routes*/
	mmo_rd_init,	
	mmo_wr_init,
	mmo_rd_deinit,	
	mmo_wr_deinit,
	mmo_read,
	mmo_write,
	NULL,
	mmo_args,
	CET_CHARSET_MS_ANSI, 0

};

/**************************************************************************/
