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
	* 2007/05/20: added writer code with embedded bitmap
	* 2007/05/22: add support for multiple bounding boxes
	              (useful / required!) for large waypoints lists
	* 2007/05/23: add optional user bitmap
	* 2007/06/02: new method to compute center (mean) of bounds
	              avoid endless loop in group splitting

	ToDo:
	
	* Display mode ("Symbol & Name") ??? not in gpi ???
	* decode speed/proximity
	* support category from GMSD "Garmin Special Data"
*/

#include "defs.h"
#include "cet_util.h"
#include "jeeps/gpsmath.h"
#include "garmin_gpi.h"
#include <ctype.h>
#include <stdio.h>
#include <string.h>
#include <time.h>

#define MYNAME "garmin_gpi"

#define GPI_DBG 1
#undef GPI_DBG

#define DEFAULT_ICON	"Waypoint"
#define WAYPOINTS_PER_BLOCK	128

static char *opt_cat, *opt_pos, *opt_notes, *opt_hide_bitmap, *opt_descr, *opt_bitmap;

static arglist_t garmin_gpi_args[] = {
	{"bitmap", &opt_bitmap, "Use specified bitmap on output", 
		NULL, ARGTYPE_FILE, ARG_NOMINMAX},
	{"category", &opt_cat, "Default category on output", 
		"My points", ARGTYPE_STRING, ARG_NOMINMAX},
	{"hide", &opt_hide_bitmap, "Don't show gpi bitmap on device", 
		NULL, ARGTYPE_BOOL, ARG_NOMINMAX},
	{"descr", &opt_descr, "Write description to address field", 
		NULL, ARGTYPE_BOOL, ARG_NOMINMAX},
	{"notes", &opt_notes, "Write notes to address field", 
		NULL, ARGTYPE_BOOL, ARG_NOMINMAX},
	{"position", &opt_pos, "Write position to address field", 
		NULL, ARGTYPE_BOOL, ARG_NOMINMAX},
	ARG_TERMINATOR
};

typedef struct {
	int D2;
	char S3[9];		/* "GRMRECnn" */
	time_t crdate;		/* creation date and time */
	char POI[4];		/* "POI" */
	char S8[3];
	char *group;
	char *category;
} reader_data_t;

typedef struct writer_data_s {
	queue Q;
	int ct;
	int sz;
	bounds bds;
	struct writer_data_s *top_left;
	struct writer_data_s *top_right;
	struct writer_data_s *buttom_left;
	struct writer_data_s *buttom_right;
} writer_data_t;
	
typedef struct {
	gbint32 size;
	gbint16 res1;
	gbint16 res2;
	gbint32 image_offset;
	gbint32 header_size;
	gbint32 width;
	gbint32 height;
	gbint16 planes;
	gbint16 bpp;
	gbint32 compression_type;
	gbint32 image_data_size;
	gbint32 resolution_h;
	gbint32 resolution_v;
	gbint32 used_colors;
	gbint32 important_colors;
} bmp_header_t;

typedef struct {
	gbint16 index;
	gbint16 height;
	gbint16 width;
	gbint16 line_sz;
	gbint16 bpp;
	gbint16 fixed_0;
	gbint32 image_size;
	gbint32 fixed_2c;
	gbint32 flag1;
	gbint32 tr_color;
	gbint32 flag2;
	gbint32 size_2c;
} gpi_bitmap_header_t;


static gbfile *fin, *fout;
static gbint32 codepage;	/* code-page, i.e. 1252 */
static reader_data_t *rdata;
static writer_data_t *wdata;
static short_handle short_h;

#ifdef GPI_DBG
# define PP printf("@%1$6x (%1$8d): ", gbftell(fin))
#else
# define PP
#endif

/*******************************************************************************
* %%%                             gpi reader                               %%% *
*******************************************************************************/

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
	rdata->D2 = gbfgetint32(fin);

	gbfread(&rdata->S3, 1, sizeof(rdata->S3) - 1, fin);	/* GRMRECnn */
	if (strncmp(rdata->S3, "GRMREC", 6) != 0)
		fatal(MYNAME ": No GPI file!\n");

	PP;
	rdata->crdate = gbfgetint32(fin);
#ifdef GPI_DBG
	tm = *localtime(&rdata->crdate);
	tm.tm_year += 20;	/* !!! */
	tm.tm_mday -= 1;	/* !!! */
	strftime(stime, sizeof(stime), "%Y/%m/%d %H:%M:%S", &tm);
	printf("crdate = %lu (%s)\n", rdata->crdate, stime);
#endif	
	
	(void) gbfgetint16(fin);	/* 0 */

	len = gbfgetint16(fin);
	gbfseek(fin, len, SEEK_CUR);	/* "my.gpi" */

	(void) gbfgetint32(fin);	/* 1 */
	(void) gbfgetint32(fin);	/* 12 */

	gbfread(&rdata->POI, 1, sizeof(rdata->POI) - 1, fin);
	if (strcmp(rdata->POI, "POI") != 0)
		fatal(MYNAME ": Wrong or unsupported GPI file!\n");

	for (i = 0; i < 3; i++) (void)gbfgetc(fin);
	gbfread(&rdata->S8, 1, sizeof(rdata->S8) - 1, fin);

	codepage = gbfgetint32(fin);

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
	PP;
	len = gbfgetint32(fin);	/* sub-header size */
#if GPI_DBG
	printf("poi sublen = %1$d (0x%1$x)\n", len);
#endif	
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
	int pos, i;
	
	pos = gbftell(fin);
#ifdef GPI_DBG
	PP;
	printf("> reading poi list (-> %1$x / %1$d )\n", pos + sz);
#endif
	PP;
	i = gbfgetint32(fin);	/* mostly 23 (0x17) */
#ifdef GPI_DBG
	printf("list sublen = %1$d (0x%1$x)\n", i);
#endif	
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

	pos = gbftell(fin);
#ifdef GPI_DBG
	PP;
	printf("> reading poi group (-> %1$x / %1$d)\n", pos + sz);
#endif
	if (tag == 0x80009) {
		int subsz;
		
		PP;
		subsz = gbfgetint32(fin);	/* ? offset to category data ? */
#ifdef GPI_DBG
		printf("group sublen = %d (-> %x / %d)\n", subsz, pos + subsz + 4, pos + subsz + 4);
#endif
	}

	len = gbfgetint32(fin);	/* size of group string */
	PP;
	if (rdata->group) xfree(rdata->group);
	rdata->group = read_string(&len);
#ifdef GPI_DBG
	printf("Group \"%s\"\n", rdata->group);
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
	int pos, sz, len, flag;
	char *str;
#ifdef GPI_DBG
	int subtag;
#endif
	
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
			
		case 0x5:	/* group bitmap */
			break;

		case 0x7:	/* category */
			(void) gbfgetint32(fin);
			(void) gbfgetint16(fin);
			if (rdata->category) xfree(rdata->category);
			PP;
			rdata->category = read_string(NULL);
#ifdef GPI_DBG
			printf("Category: \"%s\"\n", rdata->category);
#endif
			break;
			
		case 0xa:	/* description */
			len = gbfgetint32(fin);
			PP;
			wpt->description = read_string(&len);
#ifdef GPI_DBG
			printf("Description: \"%s\"\n", wpt->description);
#endif
			break;
			
		case 0xe:	/* ? notes or description / or both ? */
			flag = gbfgetc(fin);
			if (flag == 0x01) {
				len = gbfgetint32(fin);
				PP;
				str = read_string(NULL);
			}
			else if (flag == 0x32) {
				len = gbfgetint16(fin);
				str = xmalloc(len + 1);
				str[len] = '\0';
				PP;
				gbfread(str, 1, len, fin);
			}
			else break;
#ifdef GPI_DBG
			printf("Notes: \"%s\"\n", str);
#endif
			if (wpt->description) wpt->notes = str;
			else wpt->description = str;
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
			
		case 0x8000b:	/* address (street/city...) */
			/* ToDo */
			break;

		case 0x8000c:	/* phone-number */
#ifdef GPI_DBG
			PP;
			subtag = gbfgetint32(fin);
			printf("phone-number tag %d\n", subtag);
			(void) gbfgetint16(fin); /* unknown / ? phone/fax/mobil ? */
			PP;
			len = gbfgetint16(fin);
			printf("phone-number len %d\n", len);
			str = xmalloc(len + 1);
			str[len] = '\0';
			PP;
			gbfread(str, 1, len, fin);
			printf("phone-number \"%s\"\n", str);
			xfree(str);
#endif
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
* %%%                             gpi writer                               %%% *
*******************************************************************************/

static void
write_string(const char *str)
{
	int len;
	
	len = strlen(str);

	gbfputint32(len + 4, fout);
	gbfwrite("EN", 1, 2, fout);
	gbfputint16(len, fout);
	gbfwrite(str, 1, len, fout);
}


static int
compare_wpt_cb(const queue *a, const queue *b)
{
	const waypoint *wa = (waypoint *) a;
	const waypoint *wb = (waypoint *) b;
	
	return strcmp(wa->shortname, wb->shortname);
}


static char
compare_strings(const char *s1, const char *s2)
{
	if (s1 == s2) return 0;
	else if (s1) {
		if (s2) return strcmp(s1, s2);
		else return 1;
	}
	else return 1;
}


static writer_data_t *
wdata_alloc()
{
	writer_data_t *res;

	res = xcalloc(1, sizeof(*res));
	QUEUE_INIT(&res->Q);
	waypt_init_bounds(&res->bds);

	return res;
}


static void
wdata_free(writer_data_t *data)
{
	queue *elem, *tmp;
	
	QUEUE_FOR_EACH(&data->Q, elem, tmp) {
		waypoint *wpt = (waypoint *)elem;
		
		if (wpt->extra_data) xfree(wpt->extra_data);
		waypt_free(wpt);
	}

	if (data->top_left) wdata_free(data->top_left);
	if (data->top_right) wdata_free(data->top_right);
	if (data->buttom_left) wdata_free(data->buttom_left);
	if (data->buttom_right) wdata_free(data->buttom_right);
	
	xfree(data);
}


static void
wdata_add_wpt(writer_data_t *data, waypoint *wpt)
{
	data->ct++;
	ENQUEUE_TAIL(&data->Q, &wpt->Q);
	waypt_add_to_bounds(&data->bds, wpt);
}


static void
wdata_check(writer_data_t *data)
{
	queue *elem, *tmp;
	double center_lat, center_lon;

	if ((data->ct <= WAYPOINTS_PER_BLOCK) ||
	   /* avoid endless loop for points (more than WAYPOINTS_PER_BLOCK) 
	      at same coordinates */
	   ((data->bds.min_lat >= data->bds.max_lat) && (data->bds.min_lon >= data->bds.max_lon))) {
	   	if (data->ct > 1)
			sortqueue(&data->Q, compare_wpt_cb);
		return;
	}

	/* compute the (mean) center of current bounds */

	center_lat = center_lon = 0;
	QUEUE_FOR_EACH(&data->Q, elem, tmp) {
		waypoint *wpt = (waypoint *) elem;
		center_lat += wpt->latitude;
		center_lon += wpt->longitude;
	}
	center_lat /= data->ct;
	center_lon /= data->ct;
	
	QUEUE_FOR_EACH(&data->Q, elem, tmp) {
		waypoint *wpt = (waypoint *) elem;
		writer_data_t **ref;

		if (wpt->latitude < center_lat) {
			if (wpt->longitude < center_lon)
				ref = &data->buttom_left;
			else
				ref = &data->buttom_right;
		} else {
			if (wpt->longitude < center_lon)
				ref = &data->top_left;
			else
				ref = &data->top_right;
		}

		if (*ref == NULL) *ref = wdata_alloc();

		data->ct--;
		dequeue(&wpt->Q);
		
		wdata_add_wpt(*ref, wpt);
	}
	
	if (data->top_left) wdata_check(data->top_left);
	if (data->top_right) wdata_check(data->top_right);
	if (data->buttom_left) wdata_check(data->buttom_left);
	if (data->buttom_right) wdata_check(data->buttom_right);
}


static int
wdata_compute_size(writer_data_t *data)
{
	queue *elem, *tmp;
	int res;
	
	res = 23;	/* bounds, ... of tag 0x80008 */
		
	QUEUE_FOR_EACH(&data->Q, elem, tmp) {
		waypoint *wpt = (waypoint *) elem;
		char *str;
		
		res += 12;		/* tag/sz/sub-sz */
		res += 19;		/* poi fixed size */
		res += strlen(wpt->shortname);
		res += 10;		/* tag(4) */
		
		str = NULL;
		if (opt_descr) {
			if (wpt->description && *wpt->description)
				str = xstrdup(wpt->description);
		}
		else if (opt_notes) {
			if (wpt->notes && *wpt->notes)
				str = xstrdup(wpt->notes);
		}
		else if (opt_pos)
			str = pretty_deg_format(wpt->latitude, wpt->longitude, 's', 0);
			
		if (str) {	/* this will be stored into street-address field */
			res += 22 + strlen(str);
			wpt->extra_data = str;
		}
		
		str = wpt->description;
		if (! str) str = wpt->notes;
		if (str) res += (16 + strlen(str));
	}

	if (data->top_left) res += wdata_compute_size(data->top_left);
	if (data->top_right) res += wdata_compute_size(data->top_right);
	if (data->buttom_left) res += wdata_compute_size(data->buttom_left);
	if (data->buttom_right) res += wdata_compute_size(data->buttom_right);

	data->sz = res;

	return res + 12;	/* 12 = caller needs info about tag header size */
}


static void
wdata_write(const writer_data_t *data)
{
	queue *elem, *tmp;
	
	gbfputint32(0x80008, fout);
	gbfputint32(data->sz, fout);
	gbfputint32(23, fout);	/* bounds + three bytes */

	gbfputint32(GPS_Math_Deg_To_Semi(data->bds.max_lat), fout);
	gbfputint32(GPS_Math_Deg_To_Semi(data->bds.max_lon), fout);
	gbfputint32(GPS_Math_Deg_To_Semi(data->bds.min_lat), fout);
	gbfputint32(GPS_Math_Deg_To_Semi(data->bds.min_lon), fout);
	
	gbfputc(0, fout);		/* three unknown bytes */
	gbfputc(0, fout);		/* ? should be zero ? */
	gbfputc(0, fout);
	
	gbfputint32(0x1000100, fout);	/* ? const 0x1000100 ? */
	
	QUEUE_FOR_EACH(&data->Q, elem, tmp) {
		char *str;
		int s0, s1;
		waypoint *wpt = (waypoint *)elem;
		
		str = wpt->description;
		if (! str) str = wpt->notes;

		gbfputint32(0x80002, fout);		
		
		s0 = s1 = 19 + strlen(wpt->shortname);
		s0 += 10;				/* tag(4) */
		if (str) s0 += (16 + strlen(str));	/* descr */

		if (wpt->extra_data)
			s0 += 22 + strlen((char *)wpt->extra_data);

		gbfputint32(s0, fout);	/* size of following data (tag) */
		gbfputint32(s1, fout);	/* basic size (without options) */
		
		gbfputint32(GPS_Math_Deg_To_Semi(wpt->latitude), fout);
		gbfputint32(GPS_Math_Deg_To_Semi(wpt->longitude), fout);
	
		gbfputint16(1, fout);	/* ? always 1 ? */
		gbfputc(0, fout);	/* seems to be 1 when extra options present */
		
		write_string(wpt->shortname);
		
		gbfputint32(4, fout);	/* tag(4) */
		gbfputint32(2, fout);
		if (opt_hide_bitmap) gbfputint16(0x3ff, fout);	/* values != 0 hides the bitmap */
		else gbfputint16(0, fout);
		
		if (str) {
			gbfputint32(0xa, fout);
			gbfputint32(strlen(str) + 8, fout);	/* string + string header */
			write_string(str);
		}
		
		str = (char *)wpt->extra_data;
		if (str) {
			gbfputint32(0x8000b, fout);
			gbfputint32(strlen(str) + 10, fout);
			gbfputint32(0x2, fout);		/* ? always 2 ? */
			gbfputint16(0x10, fout);	/* 0x10 = StreetAddress */
			write_string(str);
		}
	}
	
	if (data->top_left) wdata_write(data->top_left);
	if (data->top_right) wdata_write(data->top_right);
	if (data->buttom_left) wdata_write(data->buttom_left);
	if (data->buttom_right) wdata_write(data->buttom_right);
}


static void
write_category(const char *category, const char *image, const int image_sz)
{
	int sz;
	
	sz = wdata_compute_size(wdata);
	sz += 8;	/* string header */
	sz += strlen(opt_cat);
	
	gbfputint32(0x80009, fout);
	if ((! opt_hide_bitmap) && image_sz)
		gbfputint32(sz + image_sz + 8, fout);
	else
		gbfputint32(sz, fout);
	gbfputint32(sz, fout);
	write_string(opt_cat);

	wdata_write(wdata);

	if ((! opt_hide_bitmap) && image_sz) {
		gbfputint32(5, fout);
		gbfputint32(image_sz, fout);
		gbfwrite(image, 1, image_sz, fout);
	}
}


static void
write_header(void)
{
	time_t time = gpsbabel_time;	/* !!! ZERO during leaktest !!! */

	if (time != 0) {
		struct tm tm = *gmtime(&time);
		tm.tm_year -= 20;
		time = mkgmtime(&tm);
		time += SECONDS_PER_DAY;
	}
	
	gbfputint32(0, fout);
	gbfputint32(0x16, fout);
	gbfwrite("GRMREC00", 1, 8, fout);
	gbfputint32(time, fout);
	gbfputint16(0, fout);
	gbfputint16(6, fout);
	gbfwrite("my.gpi", 1, 6, fout);
	gbfputint32(1, fout);
	gbfputint32(0xc, fout);
	gbfwrite("POI", 1, 3, fout);
	gbfputc(0, fout);
	gbfputc(0, fout);
	gbfputc(0, fout);
	gbfwrite("00", 1, 2, fout);
	gbfputint32(codepage, fout);
}


static void
enum_waypt_cb(const waypoint *ref)
{
	waypoint *wpt;
	char *str;
	queue *elem, *tmp;

	QUEUE_FOR_EACH(&wdata->Q, elem, tmp) {
		waypoint *cmp = (waypoint *) elem;
		
		/* sort out nearly equal waypoints */
		if ((compare_strings(cmp->shortname, ref->shortname) == 0) &&
		    (cmp->latitude == ref->latitude) &&
		    (cmp->longitude == ref->longitude) &&
		    (compare_strings(cmp->description, ref->description) == 0) &&
		    (compare_strings(cmp->notes, ref->notes) == 0)) return;
	}
	
	wpt = waypt_dupe(ref);

	str = mkshort(short_h, wpt->shortname);
	xfree(wpt->shortname);
	wpt->shortname = str;

	wdata_add_wpt(wdata, wpt);
}


static void
load_bitmap_from_file(const char *fname, char **data, int *data_sz)
{
	gbfile *f;
	int i, sz;
	int dest_bpp;
	int src_line_sz, dest_line_sz;
	bmp_header_t src_h;
	int *color_table = NULL;
	gpi_bitmap_header_t *dest_h;
	void *ptr;
	
	f = gbfopen_le(fname, "rb", MYNAME);
	is_fatal(gbfgetint16(f) != 0x4d42, MYNAME ": No BMP image.");

	/* read a standard bmp file header */
	src_h.size = gbfgetint32(f);
	src_h.res1 = gbfgetint16(f);
	src_h.res2 = gbfgetint16(f);
	src_h.image_offset = gbfgetint32(f);
	src_h.header_size = gbfgetint32(f);
	src_h.width = gbfgetint32(f);
	src_h.height = gbfgetint32(f);
	src_h.planes = gbfgetint16(f);
	src_h.bpp = gbfgetint16(f);
	src_h.compression_type = gbfgetint32(f);
	src_h.image_data_size = gbfgetint32(f);
	src_h.resolution_h = gbfgetint32(f);
	src_h.resolution_v = gbfgetint32(f);
	src_h.used_colors = gbfgetint32(f);
	src_h.important_colors = gbfgetint32(f);

#ifdef GPI_DBG
	printf("data size:             0x%1$x (%1$d)\n", src_h.size);
	printf("image data offset:     0x%1$x (%1$d)\n", src_h.image_offset);
	printf("header size:           0x%1$x (%1$d)\n", src_h.header_size);
	printf("image width:           0x%1$x (%1$d)\n", src_h.width);
	printf("image height:          0x%1$x (%1$d)\n", src_h.height);
	printf("number of planes:      0x%1$x (%1$d)\n", src_h.planes);
	printf("bits per pixel:        0x%1$x (%1$d)\n", src_h.bpp);
	printf("compression type:      0x%1$x (%1$d)\n", src_h.compression_type);
	printf("image size:            0x%1$x (%1$d)\n", src_h.image_data_size);
	printf("horizontal resolution: 0x%1$x (%1$d)\n", src_h.resolution_h);
	printf("vertical resolution:   0x%1$x (%1$d)\n", src_h.resolution_v);
	printf("number of colors:      0x%1$x (%1$d)\n", src_h.used_colors);
	printf("important colors:      0x%1$x (%1$d)\n", src_h.important_colors);
#endif
	/* sort out unsupported files */
	if (! ((src_h.width <= 24) && (src_h.height <= 24) &&
	       (src_h.width > 0) && (src_h.height > 0)))
		fatal(MYNAME ": Unsupported format (%dx%d)!\n", src_h.width, src_h.height);
	if (! ((src_h.bpp == 8) || (src_h.bpp == 24) || (src_h.bpp == 32)))
		fatal(MYNAME ": Unsupported color depth (%d)!\n", src_h.bpp);
	if (! (src_h.compression_type == 0))
		fatal(MYNAME ": Sorry, we don't support compressed bitmaps.\n");

	if (src_h.used_colors > 0) {
		color_table = xmalloc(4 * src_h.used_colors);
		gbfread(color_table, 1, 4 * src_h.used_colors, f);
		for (i = 0; i < src_h.used_colors; i++) {
			int color = color_table[i];
			/* swap blue and red value */
			color = (color >> 16) | (color << 16) | (color & 0x00ff00);
			color_table[i] = color & 0xffffff;
		}
	}
	
	/* calculate line-size for source and destination */
	src_line_sz = (src_h.width * src_h.bpp) / 8;
	src_line_sz = ((int)((src_line_sz + 3) / 4)) * 4;
	
	if (src_h.bpp == 24) dest_bpp = 32;
	else dest_bpp = src_h.bpp;
	
	dest_line_sz = (src_h.width * dest_bpp) / 8;
	dest_line_sz = ((int)((dest_line_sz + 3) / 4)) * 4;
	
	sz = sizeof(*dest_h) + (src_h.height * dest_line_sz);
	if (src_h.used_colors) sz += src_h.used_colors * 4;

	dest_h = ptr = xmalloc(sz);
	*data = ptr;
	*data_sz = sz;
	
	le_write16(&dest_h->index, 0);
	le_write16(&dest_h->height, src_h.height);
	le_write16(&dest_h->width, src_h.width);
	le_write16(&dest_h->line_sz, dest_line_sz);
	le_write16(&dest_h->bpp, dest_bpp);
	le_write16(&dest_h->fixed_0, 0);		/* seems to be fixed */
	le_write32(&dest_h->image_size, dest_line_sz * src_h.height);
	le_write32(&dest_h->fixed_2c, 0x2c);		/* seems to be fixed */
	le_write32(&dest_h->flag1, (dest_bpp == 8) ? 0x100 : 0);
	le_write32(&dest_h->tr_color, 0xff00ff);	/* magenta = transparent color */
	le_write32(&dest_h->flag2, 0x1);		/* ? enable transparent mode ? */
	le_write32(&dest_h->size_2c, (dest_line_sz * src_h.height) + 0x2c);

	/* copy and revert order of BMP lines */
	ptr = dest_h;
	ptr += sizeof(*dest_h) + (dest_line_sz * (src_h.height - 1));

	if (src_h.bpp == 24) {
		/* 24 bpp seems to be not supported, convert to 32 bpp */
		for (i = 0; i < src_h.height; i++) {
			int j;
			void *p = ptr;
			
			for (j = 0; j < src_h.width; j++) {
				int color;
				color = (gbint32)gbfgetint16(f) | (gbfgetc(f) << 16);
				le_write32(p, color);
				p += 4;
			}
			ptr -= dest_line_sz;
		}
	}
	else for (i = 0; i < src_h.height; i++) {
		gbfread(ptr, 1, src_line_sz, f);
		ptr -= dest_line_sz;
	}

	if (src_h.used_colors > 0) {
		ptr = dest_h;
		ptr += sizeof(*dest_h) + (src_h.height * src_line_sz);

		for (i = 0; i < src_h.used_colors; i++) {
			le_write32(ptr, color_table[i]);
			ptr += 4;
		}
	}

	if (color_table) xfree(color_table);
	gbfclose(f);
}

/*******************************************************************************
* %%%        global callbacks called by gpsbabel main process              %%% *
*******************************************************************************/

static void
garmin_gpi_rd_init(const char *fname)
{
	char cp[8];
	
	fin = gbfopen_le(fname, "rb", MYNAME);
	rdata = xcalloc(1, sizeof(*rdata));

	read_header();
	
	if ((codepage >= 1250) && (codepage <= 1257)) {
		snprintf(cp, sizeof(cp), "CP%d", codepage);
		cet_convert_init(cp, 1);
	}
	else warning(MYNAME ": Unsupported code page (%d).\n", codepage);
}


static void
garmin_gpi_wr_init(const char *fname)
{
	char cp[8];
	cet_cs_vec_t *vec;
	int i;
	
	fout = gbfopen_le(fname, "wb", MYNAME);
	
	short_h = mkshort_new_handle();
	
	setshort_length(short_h, 1024);
	setshort_badchars(short_h, "\r\n");
	setshort_mustupper(short_h, 0);
	setshort_mustuniq(short_h, 1);
	setshort_whitespace_ok(short_h, 1);
	setshort_repeating_whitespace_ok(short_h, 0);
	setshort_defname(short_h, "POI");
	
	codepage = 0;
	
	for (i = 1250; i <= 1257; i++) {
		snprintf(cp, sizeof(cp), "CP%d", i);
		vec = cet_find_cs_by_name(cp);
		if (vec == global_opts.charset) {
			codepage = i;
			break;
		}
	}

	if (! codepage) {
		warning(MYNAME ": Unsupported character set (%s)!\n", global_opts.charset_name);
		fatal(MYNAME ": Valid values are CP1250 to CP1257.\n");
	}

	wdata = wdata_alloc();
}


static void 
garmin_gpi_rd_deinit(void)
{
	if (rdata->category) xfree(rdata->category);
	if (rdata->group) xfree(rdata->group);
	xfree(rdata);
	gbfclose(fin);
}


static void 
garmin_gpi_wr_deinit(void)
{
	wdata_free(wdata);
	mkshort_del_handle(&short_h);
	gbfclose(fout);
}


static void
garmin_gpi_read(void)
{
	while (1) {
		int tag = gbfgetint32(fin);
		if (tag == 0xffff) return;
		if (! read_tag("garmin_gpi_read", tag, NULL)) return;
	};
}


static void
garmin_gpi_write(void)
{
	char *image;
	int image_sz;
	
	if (strlen(opt_cat) == 0) fatal(MYNAME ": Can't write empty category!\n");
	
	if (opt_hide_bitmap) {
		image = NULL;
		image_sz = 0;
	}
	else if (opt_bitmap && *opt_bitmap)
		load_bitmap_from_file(opt_bitmap, &image, &image_sz);
	else {
		image = gpi_bitmap;	/* embedded image in gpi format */
		image_sz = GPI_BITMAP_SIZE;
	}
	waypt_disp_all(enum_waypt_cb);

	wdata_check(wdata);
	write_header();
	write_category(opt_cat, image, image_sz);

	gbfputint32(0xffff, fout);	/* final tag */
	gbfputint32(0, fout);		/* ? dummy size ? */
	
	if (image != gpi_bitmap) xfree(image);
}

/**************************************************************************/

ff_vecs_t garmin_gpi_vecs = {
	ff_type_file,
	{ 
		ff_cap_read | ff_cap_write 	/* waypoints */, 
	  	ff_cap_none 			/* tracks */, 
	  	ff_cap_none 			/* routes */
	},
	garmin_gpi_rd_init,	
	garmin_gpi_wr_init,
	garmin_gpi_rd_deinit,	
	garmin_gpi_wr_deinit,
	garmin_gpi_read,
	garmin_gpi_write,
	NULL,
	garmin_gpi_args,
	CET_CHARSET_MS_ANSI, 0		/* WIN-CP1252 */
};

/**************************************************************************/
