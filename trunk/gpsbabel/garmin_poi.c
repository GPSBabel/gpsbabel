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

	ToDo:
	
	* Display mode ("Symbol & Name")
	* decode speed/proximity
	* support category from GMSD "Garmin Special Data"
*/

#include "defs.h"
#include "cet_util.h"
#include "jeeps/gpsmath.h"
#include "garmin_poi.h"
#include <ctype.h>
#include <stdio.h>
#include <string.h>
#include <time.h>

#define MYNAME "garmin_poi"

#define GPI_DBG 1
#undef GPI_DBG

#define DEFAULT_ICON	"Waypoint"

static char *opt_cat, *opt_pos, *opt_notes, *opt_hide, *opt_descr;

static arglist_t garmin_poi_args[] = {
	{"category", &opt_cat, "Default category on output", 
		"My points", ARGTYPE_STRING, ARG_NOMINMAX},
	{"hide", &opt_hide, "Don't show gpi bitmap on device", 
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
} gpi_data_t;

static gbfile *fin, *fout;
static gbint32 codepage;	/* code-page, i.e. 1252 */
static gpi_data_t *dt;
static queue wptq;
static int all_points_sz;
static bounds bds;
static short_handle short_h;

#ifdef GPI_DBG
# define PP printf("@%1$6x (%1$8d): ", gbftell(fin))
#else
# define PP
#endif

#ifdef GPI_DBG
static void
store_bitmap(const char *filename, int sz)
{
	gbfile *fout;
	
	fout = gbfopen_le(filename, "wb", MYNAME);
	
	while (! gbfeof(fin)) {
		char buff[256];
		int len;
		
		len = sizeof(buff);
		if (len > sz) len = sz;
		sz -= len;
		len = gbfread(buff, 1, len, fin);
		gbfwrite(buff, 1, len, fout);
		if (sz <= 0) break;
	}
	gbfclose(fout);
}
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
	dt->D2 = gbfgetint32(fin);

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
			
		case 0x5:	/* group bitmap (BMP: <= 24x24) */
#ifdef GPI_DBG
			// store_bitmap("gpi_bitmap", sz);
#endif
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
			
		case 0xa:	/* description */
			len = gbfgetint32(fin);
			PP;
			wpt->description = read_string(&len);
#ifdef GPI_DBG
			printf("Description: \"%s\"\n", wpt->description);
#endif
			break;
			
		case 0xe:	/* ? notes ? */
			(void) gbfgetc(fin);
			if (dt->D2 == 0x1D) {
				len = gbfgetint32(fin);
				PP;
				str = read_string(NULL);
			}
			else {
				len = gbfgetint16(fin);
				str = xmalloc(len + 1);
				str[len] = '\0';
				PP;
				gbfread(str, 1, len, fin);
			}
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
			gbfseek(fin, sz, SEEK_CUR);
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
	int len = strlen(str);
	
	gbfwrite("EN", 1, 2, fout);
	gbfputint16(len, fout);
	gbfwrite(str, 1, len, fout);
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


static void
write_header(void)
{
	struct tm tm;
	time_t time;
	
	tm = *gmtime(&gpsbabel_now);
	tm.tm_year -= 20;
	time = mkgmtime(&tm);
	time += SECONDS_PER_DAY;
	
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

	QUEUE_FOR_EACH(&wptq, elem, tmp) {
		waypoint *cmp = (waypoint *) elem;
		
		if ((compare_strings(cmp->shortname, ref->shortname) == 0) &&
		    (cmp->latitude == ref->latitude) &&
		    (cmp->longitude == ref->longitude) &&
		    (compare_strings(cmp->description, ref->description) == 0)) return;
	}
	
	wpt = waypt_dupe(ref);

	ENQUEUE_TAIL(&wptq, &wpt->Q);

	str = mkshort(short_h, wpt->shortname);
	xfree(wpt->shortname);
	wpt->shortname = str;
	
	all_points_sz += 12;		/* tag/sz/sub-sz */
	all_points_sz += 19;		/* poi fixed size */
	all_points_sz += strlen(wpt->shortname);
	all_points_sz += 10;		/* tag(4) */

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
		all_points_sz += 22 + strlen(str);
		wpt->extra_data = str;
	}
	
	str = wpt->description;
	if (! str) str = wpt->notes;
	if (str) all_points_sz += (16 + strlen(str));
		
	waypt_add_to_bounds(&bds, wpt);
}


static int
compare_wpt_cb(const queue *a, const queue *b)
{
	const waypoint *wa = (waypoint *) a;
	const waypoint *wb = (waypoint *) b;
	
	return strcmp(wa->shortname, wb->shortname);
}


static void
write_data(const char *group)
{
	queue *elem, *tmp;
	int size;
	
	size = all_points_sz;
	size += 8;
	size += strlen(group);
	
	size += 12 + 23;
	
	/* --- group header --- */
	
	gbfputint32(0x80009, fout);
	gbfputint32(size + BMP_SIZE + 8, fout);
	gbfputint32(size, fout);
	
	gbfputint32(strlen(group) + 4, fout);
	write_string(group);
	
	/* --- list header --- */
	
	gbfputint32(0x80008, fout);
	gbfputint32(all_points_sz + 23, fout);
	gbfputint32(23, fout);

	gbfputint32(GPS_Math_Deg_To_Semi(bds.max_lat), fout);
	gbfputint32(GPS_Math_Deg_To_Semi(bds.max_lon), fout);
	gbfputint32(GPS_Math_Deg_To_Semi(bds.min_lat), fout);
	gbfputint32(GPS_Math_Deg_To_Semi(bds.min_lon), fout);
	
	gbfputc(0, fout);		/* three unknown bytes */
	gbfputc(0, fout);		/* ? should be zero ? */
	gbfputc(0, fout);
	
	gbfputint32(0x1000100, fout);	/* ? const 0x1000100 ? */
	
	QUEUE_FOR_EACH(&wptq, elem, tmp) {
		char *str;
		int s0, s1;
		waypoint *wpt = (waypoint *)elem;
		
		str = wpt->description;
		if (! str) str = wpt->notes;

		/* --- poi header --- */
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
		gbfputc(1, fout);	/* seems to be 1 when extra options present */
					/* because we always write tag(4) this should be okay */
		
		gbfputint32(strlen(wpt->shortname) + 4, fout);
		write_string(wpt->shortname);
		
		gbfputint32(4, fout);	/* tag(4) */
		gbfputint32(2, fout);
		if (opt_hide) gbfputint16(1, fout);	/* values != 0 hides the bitmap */
		else gbfputint16(0, fout);
		
		if (str) {
			gbfputint32(0xa, fout);
			gbfputint32(strlen(str) + 8, fout);
			gbfputint32(strlen(str) + 4, fout);
			write_string(str);
		}
		
		str = (char *)wpt->extra_data;
		if (str) {
			gbfputint32(0x8000b, fout);
			gbfputint32(strlen(str) + 10, fout);
			gbfputint32(0x2, fout);
			gbfputint16(0x10, fout);	/* 0x10 = StreetAddress */
			gbfputint32(strlen(str) + 4, fout);
			write_string(str);
			xfree(str);
		}
	}
	
	if (BMP_SIZE) {
		gbfputint32(5, fout);	/* simple bitmap / GPSBabel logo */
		gbfputint32(BMP_SIZE, fout);
		gbfwrite(gpi_bitmap, 1, BMP_SIZE, fout);
	}

	gbfputc(-1, fout);		/* end of group */
	gbfputint32(0xff, fout);	/* final tag */

	gbfputint16(0, fout);
	gbfputc(0, fout);
}


/*******************************************************************************
* %%%        global callbacks called by gpsbabel main process              %%% *
*******************************************************************************/

static void
garmin_poi_rd_init(const char *fname)
{
	char cp[8];
	
	fin = gbfopen_le(fname, "rb", MYNAME);
	dt = xcalloc(1, sizeof(*dt));

	read_header();
	
	if ((codepage >= 1250) && (codepage <= 1257)) {
		snprintf(cp, sizeof(cp), "CP%d", codepage);
		cet_convert_init(cp, 1);
	}
	else warning(MYNAME ": Unsupported code page (%d).\n", codepage);
}


static void
garmin_poi_wr_init(const char *fname)
{
	char cp[8];
	cet_cs_vec_t *vec;
	int i;
	
	fout = gbfopen_le(fname, "wb", MYNAME);
	
	QUEUE_INIT(&wptq);
	all_points_sz = 0;
	waypt_init_bounds(&bds);

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
garmin_poi_wr_deinit(void)
{
	queue *elem, *tmp;
	
	mkshort_del_handle(&short_h);
	QUEUE_FOR_EACH(&wptq, elem, tmp) {
		dequeue(elem);
		waypt_free((waypoint *) elem);
	}
	gbfclose(fout);
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


static void
garmin_poi_write(void)
{
	if (strlen(opt_cat) == 0) fatal(MYNAME ": Can't write empty category!\n");
	
	write_header();
	waypt_disp_all(enum_waypt_cb);
	sortqueue(&wptq, compare_wpt_cb);

	write_data(opt_cat);
}

/**************************************************************************/

ff_vecs_t garmin_poi_vecs = {
	ff_type_file,
	{ 
		ff_cap_read | ff_cap_write 	/* waypoints */, 
	  	ff_cap_none 			/* tracks */, 
	  	ff_cap_none 			/* routes */
	},
	garmin_poi_rd_init,	
	garmin_poi_wr_init,
	garmin_poi_rd_deinit,	
	garmin_poi_wr_deinit,
	garmin_poi_read,
	garmin_poi_write,
	NULL,
	garmin_poi_args,
	CET_CHARSET_MS_ANSI, 0		/* WIN-CP1252 */
};

/**************************************************************************/
