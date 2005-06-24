/* 
	Support for PathAway Palm Database, 
	Copyright (C) 2005 Olaf Klein, o.b.klein@t-online.de

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
	remarks:
	
	The german release 3.0 of PathAway violates the PathAway standards:
	* N.. .... O.. .... instead of N.. .... E.. ....
	* date is formatted in DDMMYYYY instead of YYYYMMDD
*/

#include <ctype.h>
#include "defs.h"
#include "coldsync/palm.h"
#include "coldsync/pdb.h"
#include "csv_util.h"

#define MYNAME "pathaway"

#define PPDB_MAGIC_TRK	0x55735472		/* UsTr */
#define PPDB_MAGIC_WPT  0x506f4c69		/* PoLi */
#define PPDB_MAGIC	0x4b6e5772 		/* KwNr */

FILE *fd_in, *fd_out;
struct pdb *pdb_in, *pdb_out;
char *fname_in, *fname_out;
static void *mkshort_handle;
static gpsdata_type ppdb_type;
unsigned char german_release = 0;

typedef struct ppdb_appdata
{
	unsigned char reservedA[274];		/* all 0 */
	unsigned char dirtyFlag;
	unsigned char dataBaseSubType; 		/* 0 = Track, 1 = Route */
	short int dbAttributes;			/* 0 */
	char vehicleStr[100];
	unsigned char reservedB[100];           /* all 0 */
} ppdb_appdata_t;

#define PPDB_APPINFO_SIZE sizeof(struct ppdb_appdata)

static char *dbname = NULL;
static char *deficon = NULL;
static char *snlen_opt = NULL;

static arglist_t ppdb_args[] = 
{
	{"dbname", &dbname, "Database name", NULL, ARGTYPE_STRING},
	{"deficon", &deficon, "Default icon name", NULL, ARGTYPE_STRING},
	{"snlen", &snlen_opt, "Length of generated shortnames", NULL, ARGTYPE_INT },
	{0, 0, 0, 0 }
};

static void 
is_fatal(int is, const char *msg, ... )
{
    if (is) fatal(MYNAME ": %s\n", msg);
}

#define PPDB_DEBUG 1

#if PPDB_DEBUG
static void
internal_debug(const char *filename, int fileline, const char *format, ... )
{
	va_list args;
	char *buff;
	static int ct;
	
	buff = (char *) xmalloc(1024);
	va_start(args, format);
	vsnprintf(buff, 1023, format, args);
	printf("DBG(%d): %s in file %s, line %d\n", ct++, buff, filename, fileline);
	va_end(args);
	xfree(buff);
}
#define DBG(fmt, args...) internal_debug (__FILE__, __LINE__, fmt, ## args )
#else
#define DBG(fmt, args...)
#endif


#define CHECK_INP(i, j) is_fatal((i != j), "Error in data structure.")

/*
 * utilities
 */

char *ppdb_strcat(char *dest, char *src, char *def, int *size)
{
	int len;
	char *res, *tmp;
	
	tmp = src;
	if (tmp == NULL)
	{
	    tmp = def;
	    if (tmp == NULL) return dest;
	}
	if (*tmp == '\0') return dest;
	
	len = strlen(dest) + strlen(tmp) + 1;
	if (len > *size)
	{
	    *size = len;
	    res = xrealloc(dest, *size);    
	}
	else
	    res = dest;
	strcat(res, tmp);
	return res;
}

#define STR_POOL_SIZE 16	/* !!! any power of 2 !!! */

static char *str_pool[STR_POOL_SIZE] = {};
static size_t str_pool_s[STR_POOL_SIZE] = {};
static int str_poolp = -1;

void str_pool_init(void)
{
	int i;
	for (i = 0; i < STR_POOL_SIZE; i++)
	{
	    str_pool[i] = NULL;
	    str_pool_s[i] = 0;
	}
}

void str_pool_deinit(void)
{
	int i;
	
	for (i = 0; i < STR_POOL_SIZE; i++)
	    if ( str_pool_s[i] != 0 )
	    {
		xfree(str_pool[i]);
		str_pool[i] = NULL;
		str_pool_s[i] = 0;
	    }
}

char *str_pool_get(size_t size)
{
	char *tmp;
	
	str_poolp = ((str_poolp + 1) & (STR_POOL_SIZE - 1));
	tmp = str_pool[str_poolp];
	
	if (str_pool_s[str_poolp] == 0)
	    tmp = xmalloc(size);
	else if (str_pool_s[str_poolp] < size)
	    tmp = xrealloc(tmp, size);
	else
	    return tmp;
	    
	str_pool[str_poolp] = tmp;
	str_pool_s[str_poolp] = size;
	
	return tmp;
}

char *str_pool_getcpy(char *src, char *def)
{
	char *res;

	if (src == NULL)
	{
	    src = def;
	    if (src == NULL) src = "";
	}
	res = str_pool_get(strlen(src) + 1);
	strcpy(res, src);

	return res;
}

/*
 * decoding/formatting functions
 */
 
char *ppdb_fmt_float(const double val)
{
	char *str = str_pool_get(32);
	char *c;
	snprintf(str, 32, "%.8f", val);
	c = str + strlen(str) - 1;
	while ((c > str) && (*c == '0'))
	{
	    *c = '\0';
	    c--;
	    if (*c == '.')
	    {
		c++;
		*c = '0';
		break;
	    }
	}
	return str;
}

char *ppdb_fmt_degrees(char dir, double val)
{
	char *str = str_pool_get(32);
	int deg = abs(val);
	double min = 60.0 * (fabs(val) - deg);
	int power = 0;
	double fx = min;
	char *tmp;

	while (fx > 1.0)
	{
	    fx = fx / 10.0;
	    power++;
	}
	snprintf(str, 31, "%c%02d 000", dir, deg);
	snprintf(str + 6 - power, 24, "%.8f", min);
	
	tmp = str + strlen(str) - 1;	/* trim trailing nulls */
	while ((tmp > str) && (*tmp == '0'))
	{
	    *tmp = '\0';
	    tmp--;
	    if (*tmp == '.')
	    {
		tmp++;
		*tmp = '0';
		break;
	    }
	}
	return str;
}

double ppdb_decode_coord(const char *str)
{
	double val;
	int deg;
	char dir;
	
	if (*str < 'A') 	/* only numeric */
	{
	    CHECK_INP(1, sscanf(str,"%lf", &val));
	    return val;
	}
	else
	{
	    char *tmp;

	    if (*str == 'O') german_release = 1;
	    
	    *tmp = strchr(str, ' ');
	    if ((tmp) && (tmp - str < 4))
	    {
		CHECK_INP(3, sscanf(str,"%c%d %lf", &dir, &deg, &val));
		val = deg + (val / 60.0);
	    }
	    else
	    {
		CHECK_INP(2, sscanf(str,"%c%lf", &dir, &val));
	    }
	    if ((dir == 'S') || (dir == 'W'))
		val = -val;
	}
	return val;
}

int ppdb_decode_tm(char *str, struct tm *tm)
{
	int msec, d1, d2, d3, d4;
	time_t tnow;
	struct tm now;
	int year;
    
	if (*str == '\0') return 0;	/* empty date and time */

	if (strchr(str, '.'))		/* time in hhmmss.ms */
	{
	    CHECK_INP(8, sscanf(str, "%02d%02d%02d.%d %02d%02d%02d%02d",
		&tm->tm_hour, &tm->tm_min, &tm->tm_sec,
		&msec, &d1, &d2, &d3, &d4));
	}
	else
	{
	    CHECK_INP(7, sscanf(str, "%02d%02d%02d %02d%02d%02d%02d",
		&tm->tm_hour, &tm->tm_min, &tm->tm_sec,
		&d1, &d2, &d3, &d4));
	}

	tnow = current_time();
	now = *localtime(&tnow);
	now.tm_year += 1900;
	now.tm_mon++;
	
	year = (d1 * 100) + d2;
	
	/* the coordinates comes before date and time in
	   the dataset, so the flag "german_release" is set yet. */

	/* next code works for most, except for 19. and 20. of month */
	
	if ((german_release != 0) || (year < 1980) || (year > now.tm_year))	/* YYYYMMDD or DDMMYYY ????? */
	{
	    tm->tm_year = (d3 * 100) + d4;
	    tm->tm_mon = d2;
	    tm->tm_mday = d1;
	}
	else
	{
	    tm->tm_year = (d1 * 100) + d2;
	    tm->tm_mon = d3;
	    tm->tm_mday = d4;
	}

	return 1;
}

static int ppdb_read_wpt(const struct pdb *pdb_in, const struct pdb_record *pdb_rec, route_head *head)
{
	char *data, *str;
	double altfeet;
	struct tm tm;
	
	for (pdb_rec = pdb_in->rec_index.rec; pdb_rec; pdb_rec=pdb_rec->next) 
	{
		waypoint *wpt_tmp = waypt_new();
		int line = 0;

		data = (char *) pdb_rec->data;
		str = csv_lineparse(data, ",", """", line++);

		while (str != NULL)
		{
		    switch(line)
		    {
			case 1:
			    wpt_tmp->latitude = ppdb_decode_coord(str);
			    break;
			case 2:
			    wpt_tmp->longitude = ppdb_decode_coord(str);
			    break;
			case 3:
			    if (*str != '\0')
			    {
				CHECK_INP(1, sscanf(str, "%lf", &altfeet));
				if (altfeet != -9999) 
				    wpt_tmp->altitude = altfeet / 3.2808;
			    }
			    break;
			case 4:
			    memset(&tm, 0, sizeof(tm));
			    if (ppdb_decode_tm(str, &tm))
			    {
				tm.tm_year -= 1900;
				tm.tm_mon--;
				wpt_tmp->creation_time = mktime(&tm) + get_tz_offset();
			    }
			    break;
			case 5:
			    if (*str != '\0')
				wpt_tmp->shortname = xstrdup(str);
			    break;
			case 6:		/* icon, ignore */
			    break;
			case 7:
			    if (*str != '\0')
				wpt_tmp->notes = xstrdup(str);
			    break;
			    
		    }
		    str = csv_lineparse(NULL, ",", """", line++);
		}
		
		if (head)
		    route_add_wpt(head, wpt_tmp);
		else
		    waypt_add(wpt_tmp);

	} 
	return 0;
}

/* ============================================================================================
 * &&& gobal callbacks &&&
 * ----------------------------------------------------------------------------------------- */

static void ppdb_rd_init(const char *fname)
{
	fname_in = xstrdup(fname);
	str_pool_init();
	fd_in = xfopen(fname, "rb", MYNAME);
	
}

static void ppdb_rd_deinit(void)
{
	fclose(fd_in);
	str_pool_deinit();
	xfree(fname_in);
}

static void ppdb_read(void)
{
	struct pdb_record *pdb_rec = NULL;
	ppdb_appdata_t *info = NULL;
	route_head *track_head, *route_head;
	const char *descr = NULL;

	if (NULL == (pdb_in = pdb_Read(fileno(fd_in))))
	    fatal(MYNAME ": pdb_Read failed.\n");
	    
	if (pdb_in->creator != PPDB_MAGIC)	/* identify the database */
	    fatal(MYNAME ": Not a PathAway pdb file.\n");

	if (pdb_in->version != 3)	/* Currently we support only version 3 */
	    fatal(MYNAME ": This file is from an untested version (%d) of PathAway and is unsupported.\n", pdb_in->version);

	if ((pdb_in->appinfo_len > 0) && (pdb_in->appinfo != NULL))
	{
	    info = (ppdb_appdata_t *) pdb_in->appinfo;
	    descr = info->vehicleStr;
	}
	
	switch(pdb_in->type)
	{
	    case PPDB_MAGIC_TRK:
		ppdb_type = trkdata; /* as default */
		if (info != NULL)
		{
		    switch(info->dataBaseSubType)
		    {
			case 0: 
			    ppdb_type = trkdata;
			    break;
			case 1: 
			    ppdb_type = rtedata;
			    break;
			default:
			    fatal(MYNAME": Invalid database subtype.\n");
		    }
		}
		break;
		
	    case PPDB_MAGIC_WPT:
		ppdb_type = wptdata;
		break;
		
	    default:
		fatal(MYNAME ": It looks like a PathAway pdb, but has no gps magic.\n");
	}

	switch(ppdb_type)
	{
	    case trkdata:
		track_head = route_head_alloc();
		track_add_head(track_head);
		track_head->rte_name = xstrdup(pdb_in->name);
		ppdb_read_wpt(pdb_in, pdb_rec, track_head);
		break;
	    case rtedata:
		route_head = route_head_alloc();
		route_add_head(route_head);
		route_head->rte_name = xstrdup(pdb_in->name);
		ppdb_read_wpt(pdb_in, pdb_rec, route_head);
		break;
	    case wptdata:
		ppdb_read_wpt(pdb_in, pdb_rec, NULL);
		break;
	}
	
	free_pdb(pdb_in);
}

/* ============================================================================================
 *   PPDB: Write support
 * -------------------------------------------------------------------------------------------*/

static void ppdb_wr_init(const char *fname)
{
	int len;

	fname_out = xstrdup(fname);
	str_pool_init();
	fd_out = xfopen(fname, "wb", MYNAME);
	mkshort_handle = mkshort_new_handle();
	
	if (global_opts.synthesize_shortnames != 0)
	{
	    if (snlen_opt != NULL)
		len = atoi(snlen_opt);
	    else
		len = 10;
	    setshort_length(mkshort_handle, len);
	    setshort_mustupper(mkshort_handle, 1);
	    setshort_badchars(mkshort_handle, ",");
	    setshort_whitespace_ok(mkshort_handle, 0);
	}
}

static void ppdb_wr_deinit(void)
{
	mkshort_del_handle(mkshort_handle);
	fclose(fd_out);
	str_pool_deinit();
	xfree(fname_out);
}

/*
 * ppdb_write_wpt: callback for waypoint output
 */ 
 
#define REC_SIZE 128

static void ppdb_write_wpt(const waypoint *wpt)
{
	char *buff, *tmp;
	char latdir, longdir;
	int len;
	struct pdb_record *rec;
	static int ct;
	struct tm tm;
	
	buff = xmalloc(REC_SIZE);
	memset(buff, 0, REC_SIZE);

	if (wpt->latitude < 0)
	    latdir = 'S';
	else
	    latdir = 'N';
	if (wpt->longitude < 0)
	    longdir = 'W';
	else
	    longdir = 'E';

	snprintf(buff, REC_SIZE, "%s,%s,", 
	    ppdb_fmt_degrees(latdir, wpt->latitude),
	    ppdb_fmt_degrees(longdir, wpt->longitude)
	);
	
	len = REC_SIZE;		/* we have coordinates in buff, now optional stuff */
	
	if (fabs(wpt->altitude) < 9999.0)	
	{
	    tmp = str_pool_get(32);
	    snprintf(tmp, 32, ppdb_fmt_float(wpt->altitude * 3.2808));
	    buff = ppdb_strcat(buff, tmp, NULL, &len);
	}
	buff = ppdb_strcat(buff, ",", NULL, &len);
	if ( wpt->creation_time != 0)
	{
	    tmp = str_pool_get(20);
	    tm = *gmtime(&wpt->creation_time);
	    strftime(tmp, 20, "%H%M%S %Y%m%d", &tm);
	    buff = ppdb_strcat(buff, tmp, NULL, &len);
	}
	buff = ppdb_strcat(buff, ",", NULL, &len);
	
	if (global_opts.synthesize_shortnames != 0)
	{
	    tmp = mkshort_from_wpt(mkshort_handle, wpt);
	    DBG("shortname %s from %s", tmp, wpt->shortname);
	}
	else
	{
	    tmp = str_pool_getcpy(wpt->shortname, "");
	    while (strchr(tmp, ',') != NULL)
		*strchr(tmp, ',') = '.';
	}
	buff = ppdb_strcat(buff, tmp, "", &len);
	
	buff = ppdb_strcat(buff, ",", NULL, &len);
	buff = ppdb_strcat(buff, deficon, "0", &len);
	buff = ppdb_strcat(buff, ",", NULL, &len);

	tmp = str_pool_getcpy(wpt->description, "");
	if (strchr(tmp, ',') != NULL )
	{
	    buff = ppdb_strcat(buff, "\"", NULL, &len);
	    while (strchr(tmp, '"') != NULL)
		*strchr(tmp, '"') = '\'';
	    buff = ppdb_strcat(buff, tmp,  NULL, &len);
	    buff = ppdb_strcat(buff, "\"", NULL, &len);	
	}
	else
	    buff = ppdb_strcat(buff, tmp, "", &len);

	len = strlen(buff) + 1;
	rec = new_Record(0, 0, ct++, len, (const ubyte *) buff);

	if (rec == NULL) 
	    fatal(MYNAME ": libpdb couldn't create record\n");

	if (pdb_AppendRecord(pdb_out, rec)) 
	    fatal(MYNAME ": libpdb couldn't append record\n");
	    
	xfree(buff);
}

/*
 * track and route write callbacks
 */
 
static void ppdb_track_header(const route_head *rte)
{
}

static void ppdb_track_trailer(const route_head *rte)
{
}


static void ppdb_write(void)
{
	ppdb_appdata_t *appinfo = NULL;
	
	if (NULL == (pdb_out = new_pdb()))
	    fatal(MYNAME ": new_pdb failed\n");
	if (dbname)
	    strncpy(pdb_out->name, dbname, PDB_DBNAMELEN);
	    
	pdb_out->name[PDB_DBNAMELEN-1] = 0;
	pdb_out->attributes = PDB_ATTR_BACKUP;
	pdb_out->ctime = pdb_out->mtime = current_time() + 2082844800U;
	pdb_out->creator = PPDB_MAGIC;
	pdb_out->version = 3;
	
	if (global_opts.objective != wptdata)	/* Waypoint target do not need appinfo block */
	{
	    appinfo = xmalloc(PPDB_APPINFO_SIZE);
	    memset(appinfo, 0, PPDB_APPINFO_SIZE);
	    
	    pdb_out->appinfo = (void *)appinfo;
	    pdb_out->appinfo_len = PPDB_APPINFO_SIZE;
	}
	
	switch(global_opts.objective)		/* Only one target is possible */
	{
	    case wptdata:
		if (dbname == NULL) strncpy(pdb_out->name, "PathAway Waypoints", PDB_DBNAMELEN);
		pdb_out->type = PPDB_MAGIC_WPT;
		waypt_disp_all(ppdb_write_wpt);
		break;
	    case trkdata:
		if (dbname == NULL) strncpy(pdb_out->name, "PathAway Track", PDB_DBNAMELEN);
		pdb_out->type = PPDB_MAGIC_TRK;
		appinfo->dataBaseSubType = 0;
		track_disp_all(ppdb_track_header, ppdb_track_trailer, ppdb_write_wpt);
		break;
	    case rtedata:
		if (dbname == NULL) strncpy(pdb_out->name, "PathAway Route", PDB_DBNAMELEN);
		pdb_out->type = PPDB_MAGIC_TRK;
		appinfo->dataBaseSubType = 1;
		route_disp_all(ppdb_track_header, ppdb_track_trailer, ppdb_write_wpt);
		break;
	}

	pdb_Write(pdb_out, fileno(fd_out));
	
	if (appinfo != NULL) xfree(appinfo);
}


ff_vecs_t ppdb_vecs = {
	ff_type_file,
	FF_CAP_RW_ALL,
	ppdb_rd_init,	
	ppdb_wr_init,	
	ppdb_rd_deinit,
	ppdb_wr_deinit,
	ppdb_read,
	ppdb_write,
	NULL, 
	ppdb_args
};
