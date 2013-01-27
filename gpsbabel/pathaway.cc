/*
	Support for PathAway Palm Database,
	Copyright (C) 2005-2006 Olaf Klein, o.b.klein@gpsbabel.org

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

	Release 4.x store only numeric coordinates and uses a six-number date.

	Modified by by Andrei Boros <slackware@andrix.ro> 2008-11-07
	* added information about database vehicle icon
	* Pathaway 4.x can handle invalid date/time and date format apparently
	has changed slightly between revisions :
	131502.29 26102008 = HHMMSS.MS DDMMYYYY
	* work around errors reading date/time information
	(real life data collected by Pathaway sometimes has the date/time field
	contain some missing/invalid data. This information can be safely
	ignored most of the time. So far gpsbabel stopped processing files
	when encountering such invalid data)
	    - date/time field may contain one or more spaces between fields
	    - date/time field may start with one or more spaces
	    - date/time field may contain invalid characters -> ignore
	    - invalid or missing date/time -> ignore
	    - only time may be present (some older versions of Pathaway 4)

	(this is still incomplete, but solved most of my problems when converting
	pathaway .pdb files)

*/

#include <ctype.h>
#include "defs.h"
#if PDBFMTS_ENABLED
#include "csv_util.h"
#include "pdbfile.h"
#include "strptime.h"

#define MYNAME "pathaway"

#define PPDB_MAGIC_TRK	0x55735472		/* UsTr */
#define PPDB_MAGIC_WPT  0x506f4c69		/* PoLi */
#define PPDB_MAGIC	0x4b6e5772 		/* KwNr */

#define VEHICLE_LEN	100

static pdbfile *file_in, *file_out;
static char *fname_out;
static short_handle mkshort_handle;
static gpsdata_type ppdb_type;
static unsigned char german_release = 0;
static char *datefmt;
static int ct;
static int warn_ = 0;

typedef struct ppdb_appdata {
  unsigned char reservedA[274];		/* all 0 */
  unsigned char dirtyFlag;
  unsigned char dataBaseSubType; 		/* 0 = Track, 1 = Route */
  short int dbAttributes;			/* 0 */
  char vehicleStr[VEHICLE_LEN];
  unsigned char reservedB[100];           /* all 0 */
} ppdb_appdata_t;

#define PPDB_APPINFO_SIZE sizeof(struct ppdb_appdata)
static ppdb_appdata_t *appinfo;

static char *opt_dbname = NULL;
static char *opt_dbicon = NULL;
static char *opt_deficon = NULL;
static char *opt_snlen = NULL;
static char *opt_date = NULL;

static arglist_t ppdb_args[] = {
  {"date",    &opt_date, "Read/Write date format (i.e. DDMMYYYY)", NULL, ARGTYPE_STRING, ARG_NOMINMAX},
  {"dbname",  &opt_dbname, "Database name", NULL, ARGTYPE_STRING, ARG_NOMINMAX},
  {"dbicon",  &opt_dbicon, "Database vehicle icon name", NULL, ARGTYPE_STRING, ARG_NOMINMAX},
  {"deficon", &opt_deficon, "Default icon name", NULL, ARGTYPE_STRING, ARG_NOMINMAX},
  {"snlen",   &opt_snlen, "Length of generated shortnames", "10", ARGTYPE_INT, "1", NULL },
  ARG_TERMINATOR
};

/*#undef PPDB_DEBUG*/
#define PPDB_DEBUG 1

#if PPDB_DEBUG
static void
internal_debug1(const char *filename, int fileline)
{
  static int ct=1;
  printf("DBG(%d): file %s, line %d: ", ct++, filename, fileline);
}
static void
internal_debug2(const char *format, ...)
{
  va_list args;

  va_start(args, format);
  vprintf(format, args);
  puts("");
  va_end(args);
}
#define DBG(args)	internal_debug1(__FILE__, __LINE__);internal_debug2 args
#else
#define DBG(args)	;
#endif


#define CHECK_INP(i, j, k, l) is_fatal((i != j), "Error in data structure (in %s? Value is : %s).", (k), (l))

/*
 * utilities
 */

static
char *ppdb_strcat(char *dest, const char *src, char *def, int *size)
{
  int len;
  char *res;
  const char *tmp;

  tmp = src;
  if (tmp == NULL) {
    tmp = def;
    if (tmp == NULL) {
      return dest;
    }
  }
  if (*tmp == '\0') {
    return dest;
  }

  len = strlen(dest) + strlen(tmp) + 1;
  if (len > *size) {
    *size = len;
    res = (char*) xrealloc(dest, *size);
  } else {
    res = dest;
  }
  strcat(res, tmp);
  return res;
}

#define STR_POOL_SIZE 16	/* !!! any power of 2 !!! */

static char *str_pool[STR_POOL_SIZE];
static size_t str_pool_s[STR_POOL_SIZE];
static int str_poolp = -1;

static
void str_pool_init(void)
{
  int i;
  for (i = 0; i < STR_POOL_SIZE; i++) {
    str_pool[i] = NULL;
    str_pool_s[i] = 0;
  }
}

static
void str_pool_deinit(void)
{
  int i;

  for (i = 0; i < STR_POOL_SIZE; i++)
    if (str_pool_s[i] != 0) {
      xfree(str_pool[i]);
      str_pool[i] = NULL;
      str_pool_s[i] = 0;
    }
}

static
char *str_pool_get(size_t size)
{
  char *tmp;

  str_poolp = ((str_poolp + 1) & (STR_POOL_SIZE - 1));
  tmp = str_pool[str_poolp];

  if (str_pool_s[str_poolp] == 0) {
    tmp = (char*) xmalloc(size);
  } else if (str_pool_s[str_poolp] < size) {
    tmp = (char*) xrealloc(tmp, size);
  } else {
    return tmp;
  }

  str_pool[str_poolp] = tmp;
  str_pool_s[str_poolp] = size;

  return tmp;
}

static
char *str_pool_getcpy(const char *src, char *def)
{
  char *res;

  if (src == NULL) {
    src = def;
    if (src == NULL) {
      src = "";
    }
  }
  res = str_pool_get(strlen(src) + 1);
  strcpy(res, src);

  return res;
}

/*
 * decoding/formatting functions
 */

static
char *ppdb_fmt_float(const double val)
{
  char *str = str_pool_get(32);
  char *c;
  snprintf(str, 32, "%.8f", val);
  c = str + strlen(str) - 1;
  while ((c > str) && (*c == '0')) {
    *c = '\0';
    c--;
    if (*c == '.') {
      c++;
      *c = '0';
      break;
    }
  }
  return str;
}

static
char *ppdb_fmt_degrees(char dir, double val)
{
  char *str = str_pool_get(32);
  int deg = fabs(val);
  double min = 60.0 * (fabs(val) - deg);
  char *tmp;

  snprintf(str, 31, "%c%0*d %.8f", dir, (deg > 99) ? 3 : 2, deg, min);

  tmp = str + strlen(str) - 1;	/* trim trailing nulls */
  while ((tmp > str) && (*tmp == '0')) {
    *tmp = '\0';
    tmp--;
    if (*tmp == '.') {
      tmp++;
      *tmp = '0';
      break;
    }
  }
  return str;
}

static
double ppdb_decode_coord(const char *str)
{
  double val;
  int deg;
  char dir;

  if (*str < 'A') {	/* only numeric */
    CHECK_INP(1, sscanf(str,"%lf", &val), "decode_coord(1) DD.dddd", str);
    return val;
  } else {
    const char *tmp;

    if (*str == 'O') {
      german_release = 1;
    }

    tmp = strchr(str, ' ');
    if ((tmp) && (tmp - str < 5)) {
      CHECK_INP(3, sscanf(str,"%c%d %lf", &dir, &deg, &val), "decode_coord(2) DD MM.mmm", str);
      val = deg + (val / 60.0);
    } else {
      CHECK_INP(2, sscanf(str,"%c%lf", &dir, &val), "decode_coord(3) DD.dddd", str);
    }
    if ((dir == 'S') || (dir == 'W')) {
      val = -val;
    }
  }
  return val;
}

static
int ppdb_decode_tm(char *str, struct tm *tm)
{
  int msec, d1, d2, d3, d4;
  int year;
  int temp=0;
  char *cx;

  str = lrtrim(str);              /* time field may start/end with spaces, drop them */

  if (*str == '\0') {
    if (global_opts.debug_level > 0) {
      warning(MYNAME ": Time value missing, reseting to 0\n");
      warn_ = 1;
    }
    return 0;                       /* empty time field */
  }

  if (strchr(str, '.')) {	/* time in hhmmss.ms */
    CHECK_INP(4, sscanf(str, "%02d%02d%02d.%d",
                        &tm->tm_hour, &tm->tm_min, &tm->tm_sec, &msec),
              "decode_tm(1) hhmmss.ss", str);
  } else if (sscanf(str,"%06d",&temp)==1)
    /* WORKAROUND read time info only if a valid 6 digit string found */
  {
    CHECK_INP(3, sscanf(str, "%02d%02d%02d",
                        &tm->tm_hour, &tm->tm_min, &tm->tm_sec),
              "decode_tm(2) hhmmss", str);
  } else {
    if (global_opts.debug_level > 0) {
      warning(MYNAME ": Invalid time value, reseting to 0\n");
      warn_ = 1;
    }
    return 0;		/* WORKAROUND maybe invalid time, just ignore it and continue */
  }
  cx = strchr(str, ' ');

  if (cx == NULL) {
    if (global_opts.debug_level > 0) {
      warning(MYNAME ": Date value missing, reseting to 0\n");
      warn_ = 1;
    }
    return 0;       /* empty date field */
  }

  cx = lrtrim(cx);
  if (*cx == '\0') {
    if (global_opts.debug_level > 0) {
      warning(MYNAME ": Date value missing, found only spaces, reseting to 0\n");
      warn_ = 1;
    }
    return 0;       /* empty date field */
  }

  if (datefmt) {
    struct tm tm2;

    if (NULL == strptime(cx, datefmt, &tm2)) {
      fatal(MYNAME ": Unable to convert date '%s' using format '%s' (%s)!\n", cx, datefmt, opt_date);
    }

    tm->tm_year = tm2.tm_year + 1900;
    tm->tm_mon = tm2.tm_mon + 1;
    tm->tm_mday = tm2.tm_mday;
  } else {
    time_t tnow;
    struct tm now;


    tnow = current_time();
    now = *localtime(&tnow);
    now.tm_year += 1900;
    now.tm_mon++;

    if (strlen(cx) == 8) {
      CHECK_INP(4, sscanf(cx, "%02d%02d%02d%02d", &d1, &d2, &d3, &d4), "decode_tm(3) invalid date (YYYYMMDD)", cx);

      year = (d1 * 100) + d2;
      /* the coordinates comes before date and time in
         the dataset, so the flag "german_release" is set yet. */

      /* next code works for most, except for 19. and 20. of month */

      if ((german_release != 0) || (year < 1980) || (year > now.tm_year)) {	/* YYYYMMDD or DDMMYYYY ????? */
        tm->tm_year = (d3 * 100) + d4;
        tm->tm_mon = d2;
        tm->tm_mday = d1;
      } else {
        tm->tm_year = (d1 * 100) + d2;
        tm->tm_mon = d3;
        tm->tm_mday = d4;
      }
    } else if (strlen(cx) == 6) {
      CHECK_INP(3, sscanf(cx, "%02d%02d%02d", &d1, &d2, &d3), "decode_tm(3) invalid date (DDMMYY)", cx);
      if (d3 < 1970) {		/* Usual Y2K interpretation */
        year = d3 + 2000;
      } else {
        year = d3 + 1900;
      }

      /* I don't know how a german release handles this
       * so for now I will assume only DDMMYY if date has 6 digits
       */
      tm->tm_year = year;
      tm->tm_mon = d2;
      tm->tm_mday = d1;
    } else {		/* date string is neither 8 nor 6 digits */
      printf(MYNAME ": Date from first record is %s.\n", cx);
      printf(MYNAME ": Please use option 'date' to specify how this is formatted.\n");
      fatal(MYNAME  ": (... -i pathaway,date=DDMMYY ...)\n");
    }
  }
  return 1;
}

static
int ppdb_read_wpt(route_head *head, int isRoute)
{
  char *data, *str;
  double altfeet;
  struct tm tm;

  while (pdb_read_rec(file_in, NULL, NULL, NULL, (void **)&data) >= 0) {
    waypoint *wpt_tmp = waypt_new();
    int line = 0;
    char *tmp = data;

    /* Print the whole input record. All input records are printed before processing. */
    if (global_opts.debug_level >= 5) {
      DBG(("\n\
--- BEGIN Input data record -----------------------------------------------\n\
%s\n\
--- END Input data record -------------------------------------------------\n",data));
    }

    while ((str = csv_lineparse(tmp, ",", "\"", line++))) {
      tmp = NULL;
      switch (line) {
      case 1:		/* latitude */
        wpt_tmp->latitude = ppdb_decode_coord(str);
        break;
      case 2:		/* longitude */
        wpt_tmp->longitude = ppdb_decode_coord(str);
        break;
      case 3:		/* altitude */
        if (*str != '\0') {
          CHECK_INP(1, sscanf(str, "%lf", &altfeet), "altitude", str);
          if (altfeet != -9999) {
            wpt_tmp->altitude = FEET_TO_METERS(altfeet);
          }
        }
        break;
      case 4:		/* time and date (optional) */
        memset(&tm, 0, sizeof(tm));
        if (ppdb_decode_tm(str, &tm)) {
          tm.tm_year -= 1900;
          tm.tm_mon--;
          wpt_tmp->creation_time = mkgmtime(&tm);
        }
        break;
      case 5:		/* name */
        if (*str != '\0') {
          wpt_tmp->shortname = xstrdup(str);
        }
        break;
      case 6:		/* icon */
        if (*str != '\0') {
          wpt_tmp->icon_descr = xstrdup(str);
        }
        wpt_tmp->wpt_flags.icon_descr_is_dynamic = 1;
        break;
      case 7:		/* notes */
        if (*str != '\0') {
          wpt_tmp->notes = xstrdup(str);
        }
        break;

      }
    }

    /* Print the whole input record, should a warning be triggered.
     * Use warning() here instead of DBG() to print the data record
     * right after the warning is issued.
     */
    if (warn_ && (global_opts.debug_level > 1) && (global_opts.debug_level < 5)) {
      warning("Faulty input data record : %s\n",data);
      warn_ = 0;
    }

    if (head && isRoute) {
      route_add_wpt(head, wpt_tmp);
    } else if (head) {
      track_add_wpt(head, wpt_tmp);
    } else {
      waypt_add(wpt_tmp);
    }

  }
  return 0;
}

/* ============================================================================================
 * &&& gobal callbacks &&&
 * ----------------------------------------------------------------------------------------- */

static void ppdb_rd_init(const char *fname)
{
  str_pool_init();
  file_in = pdb_open(fname, MYNAME);
  ct = 0;

  if (opt_date) {
    datefmt = convert_human_date_format(opt_date);
  } else {
    datefmt = NULL;
  }
}

static void ppdb_rd_deinit(void)
{
  pdb_close(file_in);
  str_pool_deinit();
  if (datefmt) {
    xfree(datefmt);
  }
}

static void ppdb_read(void)
{
  ppdb_appdata_t *info = NULL;
  route_head *track_head, *route_head;
  const char *descr = NULL;

  if (file_in->creator != PPDB_MAGIC) {	/* identify the database */
    fatal(MYNAME ": Not a PathAway pdb file.\n");
  }

  if (file_in->version != 3) {	/* Currently we support only version 3 */
    fatal(MYNAME ": This file is from an untested version (%d) of PathAway and is unsupported.\n", file_in->version);
  }

  if ((file_in->appinfo_len > 0) && (file_in->appinfo != NULL)) {
    info = (ppdb_appdata_t *) file_in->appinfo;
    descr = info->vehicleStr;
  }
  switch (file_in->type) {
  case PPDB_MAGIC_TRK:
    ppdb_type = trkdata; /* as default */
    if (info != NULL) {
      switch (info->dataBaseSubType) {
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

  switch (ppdb_type) {
  case trkdata:
    track_head = route_head_alloc();
    track_add_head(track_head);
    track_head->rte_name = xstrdup(file_in->name);
    ppdb_read_wpt(track_head, 0);
    break;
  case rtedata:
    route_head = route_head_alloc();
    route_add_head(route_head);
    route_head->rte_name = xstrdup(file_in->name);
    ppdb_read_wpt(route_head, 1);
    break;
  case wptdata:
  case unknown_gpsdata:
    ppdb_read_wpt(NULL, 0);
    break;
  case posndata:
    fatal(MYNAME ": Realtime positioning not supported.\n");
    break;
  }
}

/* ============================================================================================
 *   PPDB: Write support
 * -------------------------------------------------------------------------------------------*/

static void ppdb_wr_init(const char *fname)
{
  int len;

  fname_out = xstrdup(fname);
  str_pool_init();
  file_out = pdb_create(fname, MYNAME);
  mkshort_handle = mkshort_new_handle();
  ct = 0;
  appinfo = NULL;

  if (global_opts.synthesize_shortnames != 0) {
    len = atoi(opt_snlen);
    setshort_length(mkshort_handle, len);
    setshort_mustupper(mkshort_handle, 1);
    setshort_badchars(mkshort_handle, ",");
    setshort_whitespace_ok(mkshort_handle, 0);
  }
  if (opt_date) {
    char *c = convert_human_date_format(opt_date);
    xasprintf(&datefmt, "%s %s", "%H%M%S", c);
    xfree(c);
  } else {
    datefmt = xstrdup("%H%M%S %Y%m%d");
  }
}

static void ppdb_wr_deinit(void)
{
  mkshort_del_handle(&mkshort_handle);
  pdb_close(file_out);
  str_pool_deinit();
  xfree(fname_out);
  if (datefmt) {
    xfree(datefmt);
  }
  if (appinfo) {
    xfree(appinfo);
  }
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
  struct tm tm;

  buff = (char *) xcalloc(REC_SIZE, 1);

  if (wpt->latitude < 0) {
    latdir = 'S';
  } else {
    latdir = 'N';
  }
  if (wpt->longitude < 0) {
    longdir = 'W';
  } else {
    longdir = 'E';
  }
  /* 1 latitude,
     2 longitude */

  snprintf(buff, REC_SIZE, "%s,%s,",
           ppdb_fmt_degrees(latdir, wpt->latitude),
           ppdb_fmt_degrees(longdir, wpt->longitude)
          );

  len = REC_SIZE;		/* we have coordinates in buff, now optional stuff */
  /* 3 altitude */

  if (fabs(wpt->altitude) < 9999.0) {
    tmp = str_pool_get(32);
    snprintf(tmp, 32, "%s", ppdb_fmt_float(METERS_TO_FEET(wpt->altitude)));
    buff = ppdb_strcat(buff, tmp, NULL, &len);
  }
  buff = ppdb_strcat(buff, ",", NULL, &len);
  /* 4 time, date */

  if (wpt->creation_time != 0) {
    tmp = str_pool_get(20);
    const time_t tt = wpt->creation_time;
    tm = *gmtime(&tt);
    strftime(tmp, 20, datefmt, &tm);
    buff = ppdb_strcat(buff, tmp, NULL, &len);
  }
  buff = ppdb_strcat(buff, ",", NULL, &len);
  /* 5 name */

  if (global_opts.synthesize_shortnames != 0) {
    tmp = mkshort_from_wpt(mkshort_handle, wpt);
    DBG(("shortname %s from %s", tmp, wpt->shortname));
  } else {
    tmp = str_pool_getcpy(wpt->shortname, "");
    while (strchr(tmp, ',') != NULL) {
      *strchr(tmp, ',') = '.';
    }
  }
  buff = ppdb_strcat(buff, tmp, "", &len);

  buff = ppdb_strcat(buff, ",", NULL, &len);
  /* 6 icon */

  tmp = str_pool_getcpy(wpt->icon_descr, opt_deficon);	/* point icon or deficon from options */
  buff = ppdb_strcat(buff, tmp, NULL, &len);
  buff = ppdb_strcat(buff, ",", NULL, &len);
  /* 7 description */

  tmp = str_pool_getcpy(wpt->description, "");
  if (strchr(tmp, ',') != NULL) {
    buff = ppdb_strcat(buff, "\"", NULL, &len);
    while (strchr(tmp, '"') != NULL) {
      *strchr(tmp, '"') = '\'';
    }
    buff = ppdb_strcat(buff, tmp,  NULL, &len);
    buff = ppdb_strcat(buff, "\"", NULL, &len);
  } else {
    buff = ppdb_strcat(buff, tmp, "", &len);
  }

  len = strlen(buff) + 1;
  pdb_write_rec(file_out, 0, 0, ct++, buff, len);

  xfree(buff);
}

/*
 * track and route write callbacks
 */

static void ppdb_write(void)
{

  if (opt_dbname) {
    strncpy(file_out->name, opt_dbname, PDB_DBNAMELEN);
  }

  file_out->attr = PDB_FLAG_BACKUP;
  file_out->ctime = file_out->mtime = current_time() + 2082844800U;
  file_out->creator = PPDB_MAGIC;
  file_out->version = 3;

  /*	Waypoint target does use vehicleStr from appinfo block
   *	Actually, all 3 types have vehicle information.
   *	if (global_opts.objective != wptdata)	/ * Waypoint target do not need appinfo block * /
   *	{
   */
  appinfo = (ppdb_appdata_t *) xcalloc(1, sizeof(*appinfo));
  file_out->appinfo = (void *)appinfo;
  file_out->appinfo_len = PPDB_APPINFO_SIZE;
  /*	}
   */
  if (opt_dbicon != NULL) {
    strncpy(appinfo->vehicleStr, opt_dbicon, VEHICLE_LEN);
  }

  switch (global_opts.objective) {	/* Only one target is possible */
  case wptdata:
  case unknown_gpsdata:
    if (opt_dbname == NULL) {
      strncpy(file_out->name, "PathAway Waypoints", PDB_DBNAMELEN);
    }
    file_out->type = PPDB_MAGIC_WPT;
    waypt_disp_all(ppdb_write_wpt);
    break;
  case trkdata:
    if (opt_dbname == NULL) {
      strncpy(file_out->name, "PathAway Track", PDB_DBNAMELEN);
    }
    file_out->type = PPDB_MAGIC_TRK;
    appinfo->dataBaseSubType = 0;
    track_disp_all(NULL, NULL, ppdb_write_wpt);
    break;
  case rtedata:
    if (opt_dbname == NULL) {
      strncpy(file_out->name, "PathAway Route", PDB_DBNAMELEN);
    }
    file_out->type = PPDB_MAGIC_TRK;
    appinfo->dataBaseSubType = 1;
    route_disp_all(NULL, NULL, ppdb_write_wpt);
    break;
  case posndata:
    fatal(MYNAME ": Realtime positioning not supported.\n");
    break;
  }
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
  ppdb_args,
  CET_CHARSET_ASCII, 0	/* CET-REVIEW */
};
#endif
