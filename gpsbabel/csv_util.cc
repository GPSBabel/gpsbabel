/*
    Utilities for parsing Character Separated Value files (CSV)

    Copyright (C) 2002 Alex Mottram (geo_alexm at cox-internet.com)
    Copyright (C) 2002-2007 Robert Lipe

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

#include <ctype.h>
#include <math.h>
#include <stdlib.h>
#include "defs.h"
#include "csv_util.h"
#include "grtcirc.h"
#include "strptime.h"
#include "jeeps/gpsmath.h"
#include "garmin_fs.h"

#define MYNAME "CSV_UTIL"

/* macros */
#define LAT_DIR(a) a < 0.0 ? 'S' : 'N'
#define LON_DIR(a) a < 0.0 ? 'W' : 'E'
#define NONULL(a) a.isNull() ? "" : a.toLatin1().data()
#define ISWHITESPACE(a) ((a == ' ') || (a == '\t'))

/* convert excel time (days since 1900) to time_t and back again */
#define EXCEL_TO_TIMET(a) ((a - 25569.0) * 86400.0)
#define TIMET_TO_EXCEL(a) ((a / 86400.0) + 25569.0)

#define GPS_DATUM_WGS84		118


/*
 * Internal numeric value to associate with each keyword in a style file.
 * To add new keywords, just add an entry here, handle it in the switch
 * statements below, add it to xcsv_tokens.in, and rebuild on a system
 * that has GNU gperf on it.
 */
typedef enum {
  XT_unused = 0,
  XT_ALT_FEET,
  XT_ALT_METERS,
  XT_ANYNAME,
  XT_CADENCE,
  XT_CITY,
  XT_CONSTANT,
  XT_COUNTRY,
  XT_DESCRIPTION,
  XT_EXCEL_TIME,
  XT_FACILITY,
  XT_FILENAME,
  XT_FORMAT,
  XT_GEOCACHE_CONTAINER,
  XT_GEOCACHE_DIFF,
  XT_GEOCACHE_HINT,
  XT_GEOCACHE_LAST_FOUND,
  XT_GEOCACHE_PLACER,
  XT_GEOCACHE_TERR,
  XT_GEOCACHE_TYPE,
  XT_GEOCACHE_ISAVAILABLE,
  XT_GEOCACHE_ISARCHIVED,
  XT_GMT_TIME,
  XT_GPS_FIX,
  XT_GPS_HDOP,
  XT_GPS_PDOP,
  XT_GPS_SAT,
  XT_GPS_VDOP,
  XT_HEART_RATE,
  XT_HMSG_TIME,
  XT_HMSL_TIME,
  XT_ICON_DESCR,
  XT_IGNORE,
  XT_INDEX,
  XT_ISO_TIME,
  XT_ISO_TIME_MS,
  XT_LATLON_HUMAN_READABLE,
  XT_LAT_DECIMAL,
  XT_LAT_DECIMALDIR,
  XT_LAT_DIR,
  XT_LAT_DIRDECIMAL,
  XT_LAT_HUMAN_READABLE,
  XT_LAT_INT32DEG,
  XT_LAT_DDMMDIR,
  XT_LAT_NMEA,
  XT_LOCAL_TIME,
  XT_LON_DECIMAL,
  XT_LON_DECIMALDIR,
  XT_LON_DIR,
  XT_LON_DIRDECIMAL,
  XT_LON_HUMAN_READABLE,
  XT_LON_INT32DEG,
  XT_LON_DDMMDIR,
  XT_LON_NMEA,
  XT_MAP_EN_BNG,
  XT_NOTES,
  XT_NET_TIME,
  XT_PATH_COURSE,
  XT_PATH_DISTANCE_KM,
  XT_PATH_DISTANCE_METERS,
  XT_PATH_DISTANCE_MILES,
  XT_PATH_SPEED,
  XT_PATH_SPEED_KNOTS,
  XT_PATH_SPEED_KPH,
  XT_PATH_SPEED_MPH,
  XT_PHONE_NR,
  XT_POSTAL_CODE,
  XT_POWER,
  XT_ROUTE_NAME,
  XT_SHORTNAME,
  XT_STATE,
  XT_STREET_ADDR,
  XT_TEMPERATURE,
  XT_TEMPERATURE_F,
  XT_TIMET_TIME,
  XT_TIMET_TIME_MS,
  XT_TRACK_NAME,
  XT_TRACK_NEW,
  XT_URL,
  XT_UTM,
  XT_UTM_ZONE,
  XT_UTM_ZONEC,
  XT_UTM_ZONEF,
  XT_UTM_EASTING,
  XT_UTM_NORTHING,
  XT_URL_LINK_TEXT,
  XT_YYYYMMDD_TIME
} xcsv_token;

// Static definition of in_word_set to meet C99 rules as used by Clang.
static struct xt_mapping*
in_word_set(register const char* str, register unsigned int len);

#include "xcsv_tokens.gperf"

/****************************************************************************/
/* obligatory global struct                                                 */
/****************************************************************************/
xcsv_file_t xcsv_file;

extern char* xcsv_urlbase;
extern char* prefer_shortnames;

#if CSVFMTS_ENABLED
static double pathdist = 0;
static double oldlon = 999;
static double oldlat = 999;

static int waypt_out_count;
static route_head* csv_track, *csv_route;
static double utm_northing, utm_easting, utm_zone = 0;
static char utm_zonec;
static UrlLink* link_;
#endif // CSVFMTS_ENABLED


/*********************************************************************/
/* csv_stringclean() - remove any unwanted characters from string.   */
/*                     returns copy of string.                       */
/*     usage: p = csv_stringclean(stringtoclean, "&,\"")             */
/*            (strip out ampersands, commas, and quotes.             */
/*********************************************************************/
char*
#ifdef DEBUG_MEM
CSV_STRINGCLEAN(const char* string, const char* chararray, DEBUG_PARAMS)
#else
csv_stringclean(const char* string, const char* chararray)
#endif
{
  char* p1;
  char* p2;
  const char* cp;
  char* tmp = xxstrdup(string,file,line);

  if ((! string) || (! chararray)) {
    return (tmp);
  }

  /* p2 - end of the original string */
  p2 = tmp + strlen(tmp);

  cp = chararray;

  while (*cp) {
    p1 = tmp;
    while (*p1) {
      if (*cp == *p1) {
        /* we don't want this character! */
        memmove(p1, p1 + 1, (p2 - p1));
        p1[p2 - p1] = '\0';
        p2--;
      } else {
        p1++;
      }
    }
    cp++;
  }
  return (tmp);
}

char*
csv_stringclean(const QString& string_in, const QString& chararray_in)
{
  // TODO(robertl): replace this with a much more Qt-ish implementation.
  // 
  const char* string = xstrdup(CSTR(string_in));
  const char* chararray = xstrdup(CSTR(chararray_in));

  char* r = csv_stringclean(string, chararray);

//  xfree(string);
//  xfree(chararray);
  return r;
}

/***********************************************************************************/
/* csv_stringtrim() - trim whitespace and leading and trailing enclosures (quotes) */
/*                    returns a copy of the modified string                        */
/*    usage: p = csv_stringtrim(string, "\"", 0)                                   */
/***********************************************************************************/
char*
#ifdef DEBUG_MEM
CSV_STRINGTRIM(const char* string, const char* enclosure, int strip_max, DEBUG_PARAMS)
#else
csv_stringtrim(const char* string, const char* enclosure, int strip_max)
#endif
{
  static const char* p1 = NULL;
  char* p2 = NULL;
  char* tmp = xxstrdup(string,file,line);
  size_t elen;
  int stripped = 0;

  if (!strlen(string)) {
    return (tmp);
  }

  if (!enclosure) {
    elen = 0;
  } else {
    elen = strlen(enclosure);
  }

  p2 = tmp + strlen(tmp) - 1;
  p1 = tmp;

  /* trim off trailing whitespace */
  while ((p2 > p1) && isspace(*p2)) {
    p2--;
  }

  /* advance p1 past any leading whitespace */
  while ((p1 < p2) && (isspace(*p1))) {
    p1++;
  }

  /* if no maximum strippage, assign a reasonable value to max */
  strip_max = strip_max ? strip_max : 9999;

  /* if we have enclosures, skip past them in pairs */
  if (elen) {
    while (
      (stripped < strip_max) &&
      ((size_t)(p2 - p1 + 1) >= (elen * 2)) &&
      (strncmp(p1, enclosure, elen) == 0) &&
      (strncmp((p2 - elen + 1), enclosure, elen) == 0)) {
      p2 -= elen;
      p1 += elen;
      stripped++;
    }
  }

  /* copy what's left over back into tmp. */
  memmove(tmp, p1, (p2 - p1) + 1);

  tmp[(p2 - p1) + 1] = '\0';

  return (tmp);
}

/*****************************************************************************/
/* csv_lineparse() - extract data fields from a delimited string. designed   */
/*                   to handle quoted and delimited data within quotes.      */
/*                   returns temporary COPY of delimited data field (use it  */
/*                   or lose it on the next call).                           */
/*    usage: p = csv_lineparse(string, ",", "\"", line)  [initial call]      */
/*           p = csv_lineparse(NULL, ",", "\"", line)    [subsequent calls]  */
/*****************************************************************************/
char*
csv_lineparse(const char* stringstart, const char* delimited_by,
              const char* enclosed_in, const int line_no)
{
  const char* sp;
  static const char* p = NULL;
  static char* tmp = NULL;
  size_t dlen = 0, elen = 0, efound = 0;
  int enclosedepth = 0;
  short int dfound;
  short int hyper_whitespace_delimiter = 0;

  if (tmp) {
    xfree(tmp);
    tmp = NULL;
  }

  if (strcmp(delimited_by, "\\w") == 0) {
    hyper_whitespace_delimiter = 1;
  }

  /*
   * This is tacky.  Our "csv" format is actually "commaspace" format.
   * Changing that causes unwanted churn, but it also makes "real"
   * comma separated data (such as likely to be produced by Excel, etc.)
   * unreadable.   So we silently change it here on a read and let the
   * whitespace eater consume the space.
   */
  if (strcmp(delimited_by, ", ") == 0) {
    delimited_by = ",";
  }

  if (!p) {
    /* first pass thru */
    p =  stringstart;

    if (!p) {
      /* last pass out */
      return (NULL);
    }
  }

  /* the beginning of the string we start with (this pass) */
  sp = p;

  /* length of delimiters and enclosures */
  if ((delimited_by) && (!hyper_whitespace_delimiter)) {
    dlen = strlen(delimited_by);
  }
  if (enclosed_in) {
    elen = strlen(enclosed_in);
  }
  dfound = 0;

  while ((*p) && (!dfound)) {
    if ((elen) && (strncmp(p, enclosed_in, elen) == 0)) {
      efound = 1;
      p+=elen;
      if (enclosedepth) {
        enclosedepth--;
      } else {
        enclosedepth++;
      }
      continue;
    }

    if (!enclosedepth) {
      if ((dlen) && (strncmp(p, delimited_by, dlen) == 0)) {
        dfound = 1;
      } else if ((hyper_whitespace_delimiter) && (ISWHITESPACE(*p))) {
        dfound = 1;
        while (ISWHITESPACE(*p)) {
          p++;
        }
      } else {
        p++;
      }
    } else {
      p++;
    }
  }

  /* allocate enough space for this data field */
  tmp = (char*) xcalloc((p - sp) + 1, sizeof(char));

  strncpy(tmp, sp, (p - sp));
  tmp[p - sp] = '\0';

  if (elen && efound) {
    char* c = csv_stringtrim(tmp, enclosed_in, 0);
    xfree(tmp);
    tmp = c;
  }

  if (dfound) {
    /* skip over the delimited_by */
    p += dlen;
  } else {
    /* end of the line */
    p = NULL;
  }

  if (enclosedepth != 0) {
    warning(MYNAME
            ": Warning- Unbalanced Field Enclosures (%s) on line %d\n",
            enclosed_in, line_no);
  }
  return (tmp);
}

#if CSVFMTS_ENABLED
/*****************************************************************************/
/* dec_to_intdeg() - convert decimal degrees to integer degreees             */
/*    usage: i = dec_to_intdeg(31.1234);                                     */
/*****************************************************************************/
static int
dec_to_intdeg(const double d)
{
  int ideg = 0;

  if (d >= 0) {
    ideg = (2147483647) - (d * 8388608);
  } else {
    ideg = (2147483647) - (fabs(d) * 8388608) + 1;
  }

  return(ideg);
}

/*****************************************************************************/
/* intdeg_to_dec() - convert integer degrees to decimal degreees             */
/*    usage: lat = dec_to_intdeg(ilat);                                      */
/*****************************************************************************/
static double
intdeg_to_dec(const int ideg)
{
  double d;

  if (ideg >= 0) {
    d = ((2147483647) - ideg) / (double)8388608;
  } else {
    d = ((-2147483647-1) + ideg) / (double)8388608;
  }

  return(d);
}

/*****************************************************************************/
/* decdir_to_dec() - convert a decimal/direction value into pure decimal.    */
/* usage: lat = decdir_to_dec("W90.1234");                                   */
/*        lat = decdir_to_dec("30.1234N");                                  */
/*****************************************************************************/
static double
decdir_to_dec(const char* decdir)
{
  char* p;
  const char* cp;
  double rval;
  int sign = 0;

  cp = &decdir[0];

  if ((*cp == 'W') || (*cp == 'S')) {
    sign = -1;
  } else if ((*cp == 'N') || (*cp == 'E')) {
    sign = 1;
  }

  rval = sign ? strtod(&decdir[1], &p) : strtod(&decdir[0], &p);

  if (sign == 0) {
    if ((*p == 'W') || (*p == 'S')) {
      sign = -1;
    } else if ((*p == 'N') || (*p == 'E')) {
      sign = 1;
    }
  }

  return(rval * sign);
}

/*****************************************************************************/
/* ddmmdir_to_degrees() - convert ddmm/direction value into degrees          */
/* usage: lat = ddmmdir_to_degrees("W90.1234");                              */
/*        lat = ddmmdir_to_degrees("30.1234N");                              */
/*****************************************************************************/
static double
ddmmdir_to_degrees(const char* ddmmdir)
{
  // if not N or E, prepend a '-' to ddmm2degrees input
  // see XT_LAT_NMEA which handles ddmm directly
  if (strchr(ddmmdir, 'W') || strchr(ddmmdir, 'S')) {
    return ddmm2degrees(- atof(ddmmdir));
  }
  return ddmm2degrees(atof(ddmmdir));

}
#endif

/*****************************************************************************
 * human_to_dec() - convert a "human-readable" lat and/or lon to decimal
 * usage: human_to_dec( "N 41° 09.12' W 085° 09.36'", &lat, &lon );
 *        human_to_dec( "41 9 5.652 N", &lat, &lon );
 *
 *        which: 0-no preference    1-prefer lat    2-prefer lon
 *****************************************************************************/

void
human_to_dec(const char* instr, double* outlat, double* outlon, int which)
{
  double unk[3] = {999,999,999};
  double lat[3] = {999,999,999};
  double lon[3] = {999,999,999};
  int    latsign = 0;
  int    lonsign = 0;
  int    unksign = 1;

  const char* cur;
  double* numres = unk;
  int numind = 0;
  char* buff;

  if (strchr(instr, ',') != NULL) {
    char* c;
    buff = xstrdup(instr);
    while ((c = strchr(buff, ','))) {
      *c = '.';
    }
  } else {
    buff = (char*)instr;
  }

  cur = buff;

  while (cur && *cur) {
    switch (*cur) {
    case 'n':
    case 's':
    case 'N':
    case 'S':
      if (unk[0] != 999) {
        numind = 0;
        numres = unk;
        lat[0] = unk[0];
        lat[1] = unk[1];
        lat[2] = unk[2];
        unk[0] = unk[1] = unk[2] = 999;
      } else {
        numres = lat;
        numind = 0;
        lat[0] = lat[1] = lat[2] = 999;
      }

      if (*cur == 'n' || *cur == 'N') {
        latsign = 1;
      } else {
        latsign = -1;
      }
      cur++;
      break;
    case 'w':
    case 'e':
    case 'W':
    case 'E':
      if (unk[0] != 999) {
        numind = 0;
        numres = unk;
        lon[0] = unk[0];
        lon[1] = unk[1];
        lon[2] = unk[2];
        unk[0] = unk[1] = unk[2] = 999;
      } else {
        numres = lon;
        numind = 0;
        lon[0] = lon[1] = lon[2] = 999;
      }

      if (*cur == 'e' || *cur == 'E') {
        lonsign = 1;
      } else {
        lonsign = -1;
      }
      cur++;
      break;
    case '1':
    case '2':
    case '3':
    case '4':
    case '5':
    case '6':
    case '7':
    case '8':
    case '9':
    case '0':
    case '.':
    case ',':
      numres[numind] = atof(cur);
      while (cur && *cur && strchr("1234567890.,",*cur)) {
        cur++;
      }
      break;
    case '-':
      unksign = -1;
      cur++;
      break;
    default:
      if (numres[numind] != 999) {
        numind++;
        if (numind > 2) {
          numres = unk;
          numind = 0;
        }
      }
      cur++;
      break;
    }
  }

  if (lat[0] == 999 && lon[0] == 999) {
    if (which == 1) {
      lat[0] = unk[0];
      lat[1] = unk[1];
      lat[2] = unk[2];
      latsign = unksign;
    } else if (which == 2) {
      lon[0] = unk[0];
      lon[1] = unk[1];
      lon[2] = unk[2];
      lonsign = unksign;
    }
  }

  if (outlat) {
    if (lat[0] != 999) {
      *outlat = lat[0];
    }
    if (lat[1] != 999) {
      *outlat += lat[1]/60.0;
    }
    if (lat[2] != 999) {
      *outlat += lat[2]/3600.0;
    }
    if (*outlat > 360) {
      *outlat = ddmm2degrees(*outlat);  /* NMEA style */
    }
    if (latsign) {
      *outlat *= latsign;
    }
  }
  if (outlon) {
    if (lon[0] != 999) {
      *outlon = lon[0];
    }
    if (lon[1] != 999) {
      *outlon += lon[1]/60.0;
    }
    if (lon[2] != 999) {
      *outlon += lon[2]/3600.0;
    }
    if (*outlon > 360) {
      *outlon = ddmm2degrees(*outlon);  /* NMEA style */
    }
    if (lonsign) {
      *outlon *= lonsign;
    }
  }
  if (buff != instr) {
    xfree(buff);
  }
}

#if CSVFMTS_ENABLED
/*
 * dec_to_human - convert decimal degrees to human readable
 */

void
dec_to_human(char* buff, const char* format, const char* dirs, double val)
{
  char* subformat = NULL;
  const char* formatptr = NULL;
  char* percent = NULL;
  char* type = NULL;

  int  index = 0;
  int  intvals[3] = {0,0,0};
  double  dblvals[3] = {0,0,0};
  int  sign = 0;

  sign = (val < 0) ? 0 : 1;

  dblvals[0] = fabs(val);
  intvals[0] = (int)dblvals[0];
  dblvals[1] = 60*(dblvals[0]-intvals[0]);
  intvals[1] = (int)dblvals[1];
  dblvals[2] = 60*(dblvals[1]-intvals[1]);
  intvals[2] = (int)dblvals[2];

  subformat = (char*) xmalloc(strlen(format)+2);
  formatptr = format;

  buff[0] = '\0';

  while (formatptr && *formatptr) {
    strcpy(subformat, formatptr);
    percent = strchr(subformat, '%');
    if (percent) {
      type = percent+1+strcspn(percent+1, "cdiouxXeEfgG%");
      *(type+1) = '\0';
      switch (*type) {
      case 'c':
        sprintf(buff+strlen(buff), subformat, dirs[sign]);
        break;
      case 'd':
      case 'i':
      case 'o':
      case 'u':
      case 'x':
      case 'X':
        if (index>2) {
          fatal(MYNAME ": too many format specifiers\n");
        }
        sprintf(buff+strlen(buff), subformat, intvals[index]);
        index++;
        break;
      case 'e':
      case 'E':
      case 'f':
      case 'g':
      case 'G':
        if (index>2) {
          fatal(MYNAME ": too many format specifiers\n");
        }
        sprintf(buff+strlen(buff), subformat, dblvals[index]);
        index++;
        break;
      case '%':
        sprintf(buff+strlen(buff), "%s", subformat);
        break;
      default:
        fatal(MYNAME ": invalid format specifier\n");
        break;

      }
    } else {
      sprintf(buff+strlen(buff), "%s", subformat);
    }
    formatptr += strlen(subformat);
  }

  xfree(subformat);
}

/*****************************************************************************/
/* xcsv_file_init() - prepare xcsv_file for first use.                       */
/*****************************************************************************/
void
xcsv_file_init(void)
{
  memset(&xcsv_file, '\0', sizeof(xcsv_file_t));

  QUEUE_INIT(&xcsv_file.prologue);
  QUEUE_INIT(&xcsv_file.epilogue);

  QUEUE_INIT(&xcsv_file.ifield);
  /* ofield is alloced to allow pointing back at ifields
   * where applicable.
   */
  xcsv_file.ofield = (queue*) xcalloc(sizeof(queue), 1);
  QUEUE_INIT(xcsv_file.ofield);
  /*
   * Provide a sane default for CSV _files_.
   */
  xcsv_file.type = ff_type_file;

  xcsv_file.mkshort_handle = mkshort_new_handle();
  xcsv_file.gps_datum = GPS_DATUM_WGS84;
}

/*****************************************************************************/
/* xcsv_ifield_add() - add input field to ifield queue.                      */
/* usage: xcsv_ifield_add("DESCRIPTION", "", "%s")                           */
/*****************************************************************************/
void
xcsv_ifield_add(char* key, char* val, char* pfc)
{
  field_map_t* fmp = (field_map_t*) xcalloc(sizeof(*fmp), 1);
  struct xt_mapping* xm = in_word_set(key, strlen(key));

  fmp->key = key;
  fmp->hashed_key = xm ? xm->xt_token : -1;
  fmp->val = val;
  fmp->printfc = pfc;

  ENQUEUE_TAIL(&xcsv_file.ifield, &fmp->Q);
  xcsv_file.ifield_ct++;
}

/*****************************************************************************/
/* xcsv_ofield_add() - add output field to ofield queue.                     */
/* usage: xcsv_ofield_add("LAT_DECIMAL", "", "%08.5lf")                      */
/*****************************************************************************/
void
xcsv_ofield_add(char* key, char* val, char* pfc, int options)
{
  field_map_t* fmp = (field_map_t*) xcalloc(sizeof(*fmp), 1);
  struct xt_mapping* xm = in_word_set(key, strlen(key));

  fmp->key = key;
  fmp->hashed_key = xm ? xm->xt_token : -1;
  fmp->val = val;
  fmp->printfc = pfc;
  fmp->options = options;

  ENQUEUE_TAIL(xcsv_file.ofield, &fmp->Q);
  xcsv_file.ofield_ct++;
}

/*****************************************************************************/
/* xcsv_prologue_add() - add prologue line to prologue queue                 */
/* usage: xcsv_prologue_add("Four score and seven years ago today,")         */
/*****************************************************************************/
void
xcsv_prologue_add(char* prologue)
{
  ogue_t* ogp = (ogue_t*) xcalloc(sizeof(*ogp), 1);

  ogp->val = prologue;
  ENQUEUE_TAIL(&xcsv_file.prologue, &ogp->Q);
  xcsv_file.prologue_lines++;
}

/*****************************************************************************/
/* xcsv_epilogue_add() - add epilogue line to epilogue queue                 */
/* usage: xcsv_epilogue_add("shall not perish from the earth.")              */
/*****************************************************************************/
void
xcsv_epilogue_add(char* epilogue)
{
  ogue_t* ogp = (ogue_t*) xcalloc(sizeof(*ogp), 1);

  ogp->val = epilogue;
  ENQUEUE_TAIL(&xcsv_file.epilogue, &ogp->Q);
  xcsv_file.epilogue_lines++;
}

static
QDateTime
yyyymmdd_to_time(const char* s)
{
  QDate d = QDate::fromString(s, "yyyyMMdd");
  return QDateTime(d);
}


/*
 * sscanftime - Parse a date buffer using strftime format
 */
static
time_t
sscanftime(const char* s, const char* format, const int gmt)
{
  struct tm stm;
  memset(&stm, 0, sizeof(stm));

  if (strptime(s, format, &stm)) {
    if ((stm.tm_mday == 0) && (stm.tm_mon == 0) && (stm.tm_year == 0)) {
      stm.tm_mday = 1;
      stm.tm_mon = 0;
      stm.tm_year = 70;
    }
    stm.tm_isdst = -1;
    if (gmt) {
      return mkgmtime(&stm);
    } else {
      return mktime(&stm);
    }
  }
  // Don't fuss for empty strings.
  if (*s) {
    warning("date parse of string '%s' with format '%s' failed.\n",
            s, format);
  }
  return 0;
}

static
time_t
addhms(const char* s, const char* format)
{
  time_t tt =0;
  int  hour =0;
  int  min  =0;
  int  sec  =0;
  char* ampm = NULL;
  int ac;

  ampm = (char*) xmalloc(strlen(s));
  ac = sscanf(s, format, &hour, &min, &sec, &ampm);
  /* If no time format in arg string, assume AM */
  if (ac < 4) {
    ampm[0] = 0;
  }
  if (ac) {
    tt = ((tolower(ampm[0])=='P')?43200:0)+3600*hour+60*min+sec;
  }
  xfree(ampm);

  return tt;
}

static
int
writetime(char* buff, size_t bufsize, const char* format, time_t t, bool gmt)
{
  static struct tm* stmp;

  if (gmt) {
    stmp = gmtime(&t);
  } else {
    stmp = localtime(&t);
  }
  return strftime(buff, bufsize, format, stmp);
}

static
int
writetime(char* buff, size_t bufsize, const char* format, const gpsbabel::DateTime& t, bool gmt)
{
  return writetime(buff, bufsize, format, t.toTime_t(), gmt);
}

static
int
writehms(char* buff, size_t bufsize, const char* format, time_t t, int gmt)
{
  static struct tm no_time = {0};
  static struct tm* stmp = &no_time;

  if (gmt) {
    stmp = gmtime(&t);
  } else {
    stmp = localtime(&t);
  }

  if (stmp == NULL) {
    stmp = &no_time;
  }

  return snprintf(buff, bufsize, format,
                  stmp->tm_hour, stmp->tm_min, stmp->tm_sec,
                  (stmp->tm_hour>=12?"PM":"AM"));
}

static
int
writehms(char* buff, size_t bufsize, const char* format, const gpsbabel::DateTime& t, int gmt)
{
  return writehms(buff, bufsize, format, t.toTime_t(), gmt);
}

static
long
time_to_yyyymmdd(QDateTime t)
{
  QDate d = t.date();
  return d.year() * 10000 + d.month() * 100 + d.day();
}

static garmin_fs_t*
gmsd_init(waypoint* wpt)
{
  garmin_fs_t* gmsd = GMSD_FIND(wpt);
  if (gmsd == NULL) {
    gmsd = garmin_fs_alloc(-1);
    fs_chain_add(&wpt->fs, (format_specific_data*) gmsd);
  }
  return gmsd;
}

/*****************************************************************************/
/* xcsv_parse_val() - parse incoming data into the waypt structure.          */
/* usage: xcsv_parse_val("-123.34", *waypt, *field_map)                      */
/*****************************************************************************/
static void
xcsv_parse_val(const char* s, waypoint* wpt, const field_map_t* fmp,
               route_head** trk)
{
  const char* enclosure = "";
  geocache_data* gc_data = NULL;

  if (!fmp->printfc) {
    fatal(MYNAME ": xcsv style '%s' is missing format specifier", fmp->key);
  }

  if (0 == strcmp(fmp->printfc, "\"%s\"")) {
    enclosure = "\"";
  }
  switch (fmp->hashed_key) {
  case XT_IGNORE:
    /* IGNORE -- Categorically ignore this... */
    break;
  case XT_CONSTANT:
    /* CONSTANT -- Ignore on Input... */
    break;
  case XT_ANYNAME:
    /* ANYNAME -- Ignore -- this is output magic. */
    break;
  case XT_INDEX:
    /* IGNORE -- Calculated Sequence # For Ouput*/
    break;
  case XT_SHORTNAME:
    wpt->shortname = csv_stringtrim(s, enclosure, 0);
    break;
  case XT_DESCRIPTION:
    wpt->description = csv_stringtrim(s, enclosure, 0);
    break;
  case XT_NOTES:
    wpt->notes = csv_stringtrim(s, "", 0);
    break;
  case XT_URL:
    if (!link_) {
      link_ = new UrlLink;
    }
    link_->url_ = QString(s).trimmed();
    break;
  case XT_URL_LINK_TEXT:
    if (!link_) {
      link_ = new UrlLink;
    }
    link_->url_link_text_ = QString(s).trimmed();
    break;
  case XT_ICON_DESCR:
    wpt->icon_descr = QString(s).trimmed();
    break;

    /* LATITUDE CONVERSIONS**************************************************/
  case XT_LAT_DECIMAL:
    /* latitude as a pure decimal value */
    wpt->latitude = atof(s);
    break;
  case XT_LAT_DECIMALDIR:
  case XT_LAT_DIRDECIMAL:
    /* latitude as a decimal with N/S in it. */
    wpt->latitude = decdir_to_dec(s);
    break;
  case XT_LAT_INT32DEG:
    /* latitude as a 32 bit integer offset */
    wpt->latitude = intdeg_to_dec((int) atof(s));
    break;
  case XT_LAT_HUMAN_READABLE:
    human_to_dec(s, &wpt->latitude, &wpt->longitude, 1);
    break;
  case XT_LAT_DDMMDIR:
    wpt->latitude = ddmmdir_to_degrees(s);
    break;
  case XT_LAT_NMEA:
    wpt->latitude = ddmm2degrees(atof(s));
    break;
    // XT_LAT_10E is handled outside the switch.
    /* LONGITUDE CONVERSIONS ***********************************************/
  case XT_LON_DECIMAL:
    /* longitude as a pure decimal value */
    wpt->longitude = atof(s);
    break;
  case XT_LON_DECIMALDIR:
  case XT_LON_DIRDECIMAL:
    /* longitude as a decimal with N/S in it. */
    wpt->longitude = decdir_to_dec(s);
    break;
  case XT_LON_INT32DEG:
    /* longitude as a 32 bit integer offset  */
    wpt->longitude = intdeg_to_dec((int) atof(s));
    break;
  case XT_LON_HUMAN_READABLE:
    human_to_dec(s, &wpt->latitude, &wpt->longitude, 2);
    break;
  case XT_LON_DDMMDIR:
    wpt->longitude = ddmmdir_to_degrees(s);
    break;
  case XT_LON_NMEA:
    wpt->longitude = ddmm2degrees(atof(s));
    break;
    // case XT_LON_10E is handled outside the switch.
    /* LAT AND LON CONVERSIONS ********************************************/
  case XT_LATLON_HUMAN_READABLE:
    human_to_dec(s, &wpt->latitude, &wpt->longitude, 0);
    break;
    /* DIRECTIONS **********************************************************/
  case XT_LAT_DIR:
    /* latitude N/S.  Ignore on input for now */
    break;
  case XT_LON_DIR:
    /* longitude E/W. Ingore on input for now */
    break;
    /* SPECIAL COORDINATES/GRID */
  case XT_MAP_EN_BNG:
    parse_coordinates(s, DATUM_OSGB36, grid_bng,
                      &wpt->latitude, &wpt->longitude, MYNAME);
    break;
  case XT_UTM_ZONE:
    utm_zone = atoi(s);
    break;
  case XT_UTM_ZONEC:
    utm_zonec = atoi(s);
    break;
  case XT_UTM_ZONEF:
    utm_zone = atoi(s);
    utm_zonec = s[strlen(s) - 1];
    break;
  case XT_UTM_EASTING:
    utm_easting = atof(s);
    break;
  case XT_UTM_NORTHING:
    utm_northing = atof(s);
    break;
  case XT_UTM: {
    char* ss;
    int i = 0;;

    utm_zone = strtod(s, &ss);
    utm_zonec = ss[i];
    ss++;
    utm_easting = strtod(ss, &ss);
    while (*ss && !isdigit(*ss)) {
      ss++;
    }
    utm_northing = strtod(ss, NULL);
  }
  break;
  /* ALTITUDE CONVERSIONS ************************************************/
  case XT_ALT_FEET:
    /* altitude in feet as a decimal value */
    wpt->altitude = FEET_TO_METERS(atof(s));
    if (wpt->altitude < unknown_alt + 1) {
      wpt->altitude = unknown_alt;
    }
    break;
  case XT_ALT_METERS:
    /* altitude in meters as a decimal value */
    wpt->altitude = atof(s);
    if (wpt->altitude < unknown_alt + 1) {
      wpt->altitude = unknown_alt;
    }
    break;

    /* PATH CONVERSIONS ************************************************/
  case XT_PATH_SPEED:
    WAYPT_SET(wpt, speed, atof(s));
    break;
  case XT_PATH_SPEED_KPH:
    WAYPT_SET(wpt, speed, KPH_TO_MPS(atof(s)));
    break;
  case XT_PATH_SPEED_MPH:
    WAYPT_SET(wpt, speed, MPH_TO_MPS(atof(s)));
    break;
  case XT_PATH_SPEED_KNOTS:
    WAYPT_SET(wpt, speed, KNOTS_TO_MPS(atof(s)));
    break;
  case XT_PATH_COURSE:
    WAYPT_SET(wpt, course, atof(s));
    break;

    /* TIME CONVERSIONS ***************************************************/
  case XT_EXCEL_TIME:
    /* Time as Excel Time  */
    wpt->SetCreationTime(EXCEL_TO_TIMET(atof(s)));
    break;
  case XT_TIMET_TIME:
    /* Time as time_t */
    wpt->SetCreationTime((time_t) atol(s));
    break;
  case XT_TIMET_TIME_MS: {
    /* Time as time_t in milliseconds */
    int s_len = strlen(s);
    if (s_len < 4) {
      /* less than 1 epochsecond, an unusual case */
      wpt->SetCreationTime(0, atoi(s));
    } else {
      char buff[32];
      int off = s_len - 3;
      strncpy(buff, s, off);
      buff[off] = '\0';
      time_t t = (time_t) atol(buff);
      s += off;
      strncpy(buff, s, 3);
      buff[3] = '\0';
      wpt->SetCreationTime(t, atoi(buff));
    }
  }
  break;
  case XT_YYYYMMDD_TIME:
    wpt->SetCreationTime(yyyymmdd_to_time(s));
    break;
  case XT_GMT_TIME:
    wpt->SetCreationTime(sscanftime(s, fmp->printfc, 1));
    break;
  case XT_LOCAL_TIME:
    if (getenv("GPSBABEL_FREEZE_TIME")) {
      /* Force constant time zone for test */
      wpt->creation_time += sscanftime(s, fmp->printfc, 1);
    } else {
      wpt->creation_time += sscanftime(s, fmp->printfc, 0);
    }
    break;
    /* Useful when time and date are in separate fields
    	GMT / Local offset is handled by the two cases above */
  case XT_HMSG_TIME:
  case XT_HMSL_TIME:
    wpt->creation_time += addhms(s, fmp->printfc);
    break;
  case XT_ISO_TIME:
  case XT_ISO_TIME_MS:
    wpt->SetCreationTime(xml_parse_time(s));
    break;
  case XT_NET_TIME: {
    fatal("XT_NET_TIME can't have possibly ever worked.");
//    time_t tt = wpt->GetCreationTime();
//    dotnet_time_to_time_t(atof(s), &tt, &wpt->microseconds);
  }
  break;
  case XT_GEOCACHE_LAST_FOUND:
    waypt_alloc_gc_data(wpt)->last_found = yyyymmdd_to_time(s);
    break;

    /* GEOCACHING STUFF ***************************************************/
  case XT_GEOCACHE_DIFF:
    /* Geocache Difficulty as an int */
    waypt_alloc_gc_data(wpt)->diff = atof(s) * 10;
    break;
  case XT_GEOCACHE_TERR:
    /* Geocache Terrain as an int */
    waypt_alloc_gc_data(wpt)->terr = atof(s) * 10;
    break;
  case XT_GEOCACHE_TYPE:
    /* Geocache Type */
    waypt_alloc_gc_data(wpt)->type = gs_mktype(s);
    break;
  case XT_GEOCACHE_CONTAINER:
    waypt_alloc_gc_data(wpt)->container = gs_mkcont(s);
    break;
  case XT_GEOCACHE_HINT:
    waypt_alloc_gc_data(wpt)->hint = QString(s).trimmed();
    break;
  case XT_GEOCACHE_PLACER:
    waypt_alloc_gc_data(wpt)->placer = QString(s).trimmed();
    break;
  case XT_GEOCACHE_ISAVAILABLE:
    gc_data = waypt_alloc_gc_data(wpt);
    if (case_ignore_strcmp(csv_stringtrim(s, "", 0), "False") == 0) {
      gc_data->is_available = status_false;
    } else if (case_ignore_strcmp(csv_stringtrim(s, "", 0), "True") == 0) {
      gc_data->is_available = status_true;
    } else {
      gc_data->is_available = status_unknown;
    }
    break;
  case XT_GEOCACHE_ISARCHIVED:
    gc_data = waypt_alloc_gc_data(wpt);
    if (case_ignore_strcmp(csv_stringtrim(s, "", 0), "False") == 0) {
      gc_data->is_archived = status_false;
    } else if (case_ignore_strcmp(csv_stringtrim(s, "", 0), "True") == 0) {
      gc_data->is_archived = status_true;
    } else {
      gc_data->is_archived = status_unknown;
    }
    break;

    /* GPS STUFF *******************************************************/
  case XT_GPS_HDOP:
    wpt->hdop = atof(s);
    break;
  case XT_GPS_VDOP:
    wpt->vdop = atof(s);
    break;
  case XT_GPS_PDOP:
    wpt->pdop = atof(s);
    break;
  case XT_GPS_SAT:
    wpt->sat = atoi(s);
    break;
  case XT_GPS_FIX:
    wpt->fix = (fix_type)(atoi(s)-(fix_type)1);
    if (wpt->fix < fix_2d) {
      if (!case_ignore_strcmp(s, "none")) {
        wpt->fix = fix_none;
      } else if (!case_ignore_strcmp(s, "dgps")) {
        wpt->fix = fix_dgps;
      } else if (!case_ignore_strcmp(s, "pps")) {
        wpt->fix = fix_pps;
      } else {
        wpt->fix = fix_unknown;
      }
    }
    break;
    /* Tracks and routes *********************************************/
  case XT_ROUTE_NAME:
    if (csv_route) {
      csv_route->rte_name = csv_stringtrim(s, enclosure, 0);
    }
    break;
  case XT_TRACK_NEW:
    if (atoi(s) && csv_track && !QUEUE_EMPTY(&csv_track->Q)) {
      *trk = route_head_alloc();
      csv_track = *trk;

      track_add_head(*trk);
    }
    break;
  case XT_TRACK_NAME:
    if (!csv_track) {
      csv_track = route_head_alloc();
    }
    csv_track->rte_name = csv_stringtrim(s, enclosure, 0);
    break;

    /* OTHER STUFF ***************************************************/
  case XT_PATH_DISTANCE_METERS:
    wpt->odometer_distance = atof(s);
    break;
  case XT_PATH_DISTANCE_KM:
    wpt->odometer_distance = atof(s) * 1000.0;
    break;
  case XT_PATH_DISTANCE_MILES:
    wpt->odometer_distance = MILES_TO_METERS(atof(s));
    break;
  case XT_HEART_RATE:
    wpt->heartrate = atoi(s);
    break;
  case XT_CADENCE:
    wpt->cadence = atoi(s);
    break;
  case XT_POWER:
    wpt->power = atof(s);
    break;
  case XT_TEMPERATURE:
    wpt->temperature = atof(s);
    break;
  case XT_TEMPERATURE_F:
    wpt->temperature = (FAHRENHEIT_TO_CELSIUS(atof(s)));
    break;
    /* GMSD ****************************************************************/
  case XT_COUNTRY: {
    garmin_fs_t* gmsd = gmsd_init(wpt);
    GMSD_SET(country, csv_stringtrim(s, enclosure, 0));
  }
  break;
  case XT_STATE: {
    garmin_fs_t* gmsd = gmsd_init(wpt);
    GMSD_SET(state, csv_stringtrim(s, enclosure, 0));
  }
  break;
  case XT_CITY: {
    garmin_fs_t* gmsd = gmsd_init(wpt);
    GMSD_SET(city, csv_stringtrim(s, enclosure, 0));
  }
  break;
  case XT_STREET_ADDR: {
    garmin_fs_t* gmsd = gmsd_init(wpt);
    GMSD_SET(addr, csv_stringtrim(s, enclosure, 0));
  }
  break;
  case XT_POSTAL_CODE: {
    garmin_fs_t* gmsd = gmsd_init(wpt);
    GMSD_SET(postal_code, csv_stringtrim(s, enclosure, 0));
  }
  break;
  case XT_PHONE_NR: {
    garmin_fs_t* gmsd = gmsd_init(wpt);
    GMSD_SET(phone_nr, csv_stringtrim(s, enclosure, 0));
  }
  break;
  case XT_FACILITY: {
    garmin_fs_t* gmsd = gmsd_init(wpt);
    GMSD_SET(facility, csv_stringtrim(s, enclosure, 0));
  }
  break;
  case -1:
    if (strncmp(fmp->key, "LON_10E", 7) == 0) {
      wpt->longitude = atof(s) / pow((double)10, atof(fmp->key+7));
    } else if (strncmp(fmp->key, "LAT_10E", 7) == 0) {
      wpt->latitude = atof(s) / pow((double)10, atof(fmp->key+7));
    } else {
      warning(MYNAME ": Unknown style directive: %s\n", fmp->key);
    }
    break;

  default:
    fatal("This can't happen\n");
    break;
  }
}

/*****************************************************************************/
/* xcsv_data_read() - read input file, parsing lines, fields and handling    */
/*                   any data conversion (the input meat)                    */
/*****************************************************************************/
void
xcsv_data_read(void)
{
  char* buff;
  char* s;
  waypoint* wpt_tmp;
  int linecount = 0;
  queue* elem, *tmp;
  field_map_t* fmp;
  ogue_t* ogp;
  route_head* rte = NULL;
  route_head* trk = NULL;
  utm_northing = 0;
  utm_easting = 0;
  utm_zone = 0;
  utm_zonec = 'N';

  csv_route = csv_track = NULL;
  if (xcsv_file.datatype == trkdata) {
    csv_track = trk;
  } else if (xcsv_file.datatype == rtedata) {
    csv_route = rte;
  }

  while ((buff = gbfgetstr(xcsv_file.xcsvfp))) {
    if ((linecount == 0) && xcsv_file.xcsvfp->unicode) {
      cet_convert_init(CET_CHARSET_UTF8, 1);
    }

    linecount++;
    /* Whack trailing space; leading space may matter if our field sep
     * is whitespace and we have leading whitespace.
     */
    rtrim(buff);

    /* skip over x many lines on the top for the prologue... */
    if ((xcsv_file.prologue_lines) && ((linecount - 1) <
                                       xcsv_file.prologue_lines)) {
      continue;
    }

    /* We should skip over epilogue lines also.  Since we don't want to
     * pre-read the file to know how many data lines we should be seeing,
     * we take this cheap shot at the data and cross our fingers.
     */

    QUEUE_FOR_EACH(&xcsv_file.epilogue, elem, tmp) {
      ogp = (ogue_t*) elem;
      if (strncmp(buff, ogp->val, strlen(ogp->val)) == 0) {
        buff[0] = '\0';
        break;
      }
    }

    if (strlen(buff)) {
      wpt_tmp = waypt_new();

      s = buff;
      s = csv_lineparse(s, xcsv_file.field_delimiter,
                        xcsv_file.field_encloser, linecount);

      if (QUEUE_EMPTY(&xcsv_file.ifield)) {
        fatal(MYNAME ": attempt to read, but style '%s' has no IFIELDs in it.\n", xcsv_file.description? xcsv_file.description : "unknown");
      }

      /* reset the ifield queue */
      elem = QUEUE_FIRST(&xcsv_file.ifield);

      /* now rip the line apart, advancing the queue for each tear
       * off the beginning of buff since there's no index into queue.
       */
      while (s) {
        fmp = (field_map_t*) elem;
        xcsv_parse_val(s, wpt_tmp, fmp, &trk);

        elem = QUEUE_NEXT(elem);

        if (elem == &xcsv_file.ifield) {
          /* we've wrapped the queue. so stop parsing! */
          while (s) {
            s=csv_lineparse(NULL, "\xff","",linecount);
          }
          break;
        }

        s = csv_lineparse(NULL, xcsv_file.field_delimiter,
                          xcsv_file.field_encloser, linecount);
      }

      if ((xcsv_file.gps_datum > -1) && (xcsv_file.gps_datum != GPS_DATUM_WGS84)) {
        double alt;
        GPS_Math_Known_Datum_To_WGS84_M(wpt_tmp->latitude, wpt_tmp->longitude, 0.0,
                                        &wpt_tmp->latitude, &wpt_tmp->longitude, &alt, xcsv_file.gps_datum);
      }

      if (utm_easting || utm_northing) {
        GPS_Math_UTM_EN_To_Known_Datum(&wpt_tmp->latitude,
                                       &wpt_tmp->longitude,
                                       utm_easting, utm_northing,
                                       utm_zone, utm_zonec,
                                       DATUM_WGS84);
      }

      if (link_) {
        wpt_tmp->AddUrlLink(*link_);
        delete link_;
        link_ = NULL;
      }

      switch (xcsv_file.datatype) {
      case unknown_gpsdata:
      case wptdata:
        waypt_add(wpt_tmp);
        break;
      case trkdata:
        if (trk == NULL) {
          trk = route_head_alloc();
          csv_track = trk;
          track_add_head(trk);
        }
        track_add_wpt(trk, wpt_tmp);
        break;
      case rtedata:
        if (rte == NULL) {
          rte = route_head_alloc();
          csv_route = rte;
          route_add_head(rte);
        }
        route_add_wpt(rte, wpt_tmp);
        break;
      default:
        ;
      }
    }

  }
}

static void
xcsv_resetpathlen(const route_head* head)
{
  pathdist = 0;
  oldlat = 999;
  oldlon = 999;
  csv_route = csv_track = NULL;
  switch (xcsv_file.datatype) {
  case trkdata:
    csv_track = (route_head*) head;
    break;
  case rtedata:
    csv_route = (route_head*) head;
    break;
  default:
    break;
  }
}

/*****************************************************************************/
/* xcsv_waypt_pr() - write output file, handling output conversions          */
/*                  (the output meat)                                        */
/*****************************************************************************/
static void
xcsv_waypt_pr(const waypoint* wpt)
{
  char buff[1024];
  char* shortname = NULL;
  char* description = NULL;
  char* anyname = NULL;
  const char* write_delimiter;
  int i;
  field_map_t* fmp;
  queue* elem, *tmp;
  double latitude, longitude;
  int32 utmz;
  double utme, utmn;
  char utmzc;

  buff[0] = '\0';

  if (oldlon < 900) {
    pathdist += radtomiles(gcdist(RAD(oldlat),RAD(oldlon),
                                  RAD(wpt->latitude),RAD(wpt->longitude)));
  }
  longitude = oldlon = wpt->longitude;
  latitude = oldlat = wpt->latitude;

  if (xcsv_file.field_delimiter && strcmp(xcsv_file.field_delimiter, "\\w") == 0) {
    write_delimiter = " ";
  } else {
    write_delimiter = xcsv_file.field_delimiter;
  }

  if ((! wpt->shortname) || (global_opts.synthesize_shortnames)) {
    if (wpt->description) {
      if (global_opts.synthesize_shortnames) {
        shortname = mkshort_from_wpt(xcsv_file.mkshort_handle, wpt);
      } else {
        shortname = csv_stringclean(wpt->description, xcsv_file.badchars);
      }
    } else {
      /* no shortname available -- let shortname default on output */
    }
  } else {
    shortname = csv_stringclean(wpt->shortname, xcsv_file.badchars);
  }

  if (! wpt->description) {
    if (shortname) {
      description = csv_stringclean(shortname, xcsv_file.badchars);
    } else {
      /* no description -- let description default on output */
    }
  } else {
    description = csv_stringclean(wpt->description, xcsv_file.badchars);
  }

  if (prefer_shortnames) {
    if (description) {
      xfree(description);
    }
    description = shortname;
  } else if (description) {
    char* odesc = description;
    description = xstrdup(odesc);
    xfree(odesc);
  }
  if ((xcsv_file.gps_datum > -1) && (xcsv_file.gps_datum != GPS_DATUM_WGS84)) {
    double alt;
    GPS_Math_WGS84_To_Known_Datum_M(latitude, longitude, 0.0,
                                    &latitude, &longitude, &alt, xcsv_file.gps_datum);
  }

  i = 0;
  QUEUE_FOR_EACH(xcsv_file.ofield, elem, tmp) {
    char* obuff;
    double lat = latitude;
    double lon = longitude;
    /*
     * A klunky concept.   This should evaluate to true for any
     * field if we think we don't have realistic value for it.
     * This is used by the 'optional' attribute for suppressing
     * fields on output.
     */
    int field_is_unknown = 0;

    fmp = (field_map_t*) elem;

    if ((i != 0) && !(fmp->options & OPTIONS_NODELIM)) {
      gbfprintf(xcsv_file.xcsvfp, write_delimiter);
    }

    if (fmp->options & OPTIONS_ABSOLUTE) {
      lat = fabs(lat);
      lon = fabs(lon);
    }

    i++;
#define writebuff(b, fmt, data) snprintf(b, sizeof(b), fmt, data)
    switch (fmp->hashed_key) {
    case XT_IGNORE:
      /* IGNORE -- Write the char printf conversion */
      writebuff(buff, fmp->printfc, "");
      break;
    case XT_INDEX:
      writebuff(buff, fmp->printfc, waypt_out_count + atoi(fmp->val));
      break;
    case XT_CONSTANT: {
      const char* cp = xcsv_get_char_from_constant_table(fmp->val);
      if (cp) {
        writebuff(buff, fmp->printfc, cp);
      } else {
        writebuff(buff, fmp->printfc, fmp->val);
      }
    }
    break;
    case XT_SHORTNAME:
      writebuff(buff, fmp->printfc,
                (shortname && *shortname) ? shortname : fmp->val);
      break;
    case XT_ANYNAME:
      if (wpt->shortname) {
        anyname = xstrdup(wpt->shortname);
      } else if (wpt->description) {
        anyname = mkshort(xcsv_file.mkshort_handle, wpt->description);
      } else if (wpt->notes) {
        anyname = xstrdup(wpt->notes);
      } else {
        anyname = xstrdup(fmp->val);
      }

      if ((anyname) && (global_opts.synthesize_shortnames)) {
        anyname = xstrdup(shortname);
      }

      writebuff(buff, fmp->printfc, anyname);

      xfree(anyname);
      break;
    case XT_DESCRIPTION:
      writebuff(buff, fmp->printfc,
                (description && *description) ? description : fmp->val);
      break;
    case XT_NOTES:
      writebuff(buff, fmp->printfc,
                (wpt->notes && *wpt->notes) ? wpt->notes : fmp->val);
      break;
    case XT_URL: {
      int off = 0;
      if (xcsv_urlbase) {
        strcpy(buff, xcsv_urlbase);
        off = strlen(xcsv_urlbase);
      }
      if (wpt->HasUrlLink()) {
        UrlLink l = wpt->GetUrlLink();
        snprintf(buff + off, sizeof(buff) - off, fmp->printfc, l.url_.toUtf8().data());
      } else {
        strcpy(buff, (fmp->val && *fmp->val) ? fmp->val : "\"\"");
      }
    }
    break;
    case XT_URL_LINK_TEXT:
      if (wpt->HasUrlLink()) {
        UrlLink l = wpt->GetUrlLink();
        snprintf(buff, sizeof(buff), fmp->printfc,
                 !l.url_link_text_.isEmpty() ? l.url_link_text_.toUtf8().data() : fmp->val);
      }
      break;
    case XT_ICON_DESCR:
      writebuff(buff, fmp->printfc,
                (!wpt->icon_descr.isNull()) ?
                wpt->icon_descr.toUtf8().data() : fmp->val);
      break;

      /* LATITUDE CONVERSION***********************************************/
    case XT_LAT_DECIMAL:
      /* latitude as a pure decimal value */
      writebuff(buff, fmp->printfc, lat);
      break;
    case XT_LAT_DECIMALDIR:
      /* latitude as a decimal value with N/S after it */
      snprintf(buff, sizeof(buff), fmp->printfc, fabs(lat),
               LAT_DIR(lat));
      break;
    case XT_LAT_DIRDECIMAL:
      /* latitude as a decimal value with N/S before it */
      snprintf(buff, sizeof(buff), fmp->printfc,
               LAT_DIR(lat),
               fabs(lat));
      break;
    case XT_LAT_INT32DEG:
      /* latitude as an integer offset from 0 degrees */
      writebuff(buff, fmp->printfc,
                dec_to_intdeg(lat));
      break;
    case XT_LAT_DDMMDIR:
      /*latitude as (degrees * 100) + decimal minutes, with N/S after it */
      dec_to_human(buff, fmp->printfc, "SN", degrees2ddmm(lat));
      break;
    case XT_LAT_HUMAN_READABLE:
      dec_to_human(buff, fmp->printfc, "SN", lat);
      break;
    case XT_LAT_NMEA:
      writebuff(buff, fmp->printfc, degrees2ddmm(lat));
      break;
      // case XT_LAT_10E is handled outside the switch.
      /* LONGITUDE CONVERSIONS*********************************************/
    case XT_LON_DECIMAL:
      /* longitude as a pure decimal value */
      writebuff(buff, fmp->printfc, lon);
      break;
    case XT_LON_DECIMALDIR:
      /* latitude as a decimal value with N/S after it */
      snprintf(buff, sizeof(buff),  fmp->printfc,
               fabs(lon),
               LON_DIR(lon));
      break;
    case XT_LON_DIRDECIMAL:
      /* latitude as a decimal value with N/S before it */
      snprintf(buff, sizeof(buff), fmp->printfc,
               LON_DIR(lon),
               fabs(lon));
      break;
    case XT_LON_INT32DEG:
      /* longitudee as an integer offset from 0 degrees */
      writebuff(buff, fmp->printfc,
                dec_to_intdeg(lon));
      break;
    case XT_LON_DDMMDIR:
      /* longidute as (degrees * 100) + decimal minutes, with W/E after it*/
      dec_to_human(buff, fmp->printfc, "WE", degrees2ddmm(lon));
      break;
    case XT_LON_HUMAN_READABLE:
      dec_to_human(buff, fmp->printfc, "WE", lon);
      break;
    case XT_LATLON_HUMAN_READABLE:
      dec_to_human(buff, fmp->printfc, "SN", lat);
      if (!isspace(buff[strlen(buff)])) {
        strcat(buff, " ");
      }
      dec_to_human(buff+strlen(buff), fmp->printfc, "WE",
                   lon);
      break;
    case XT_LON_NMEA:
      writebuff(buff, fmp->printfc, degrees2ddmm(lon));
      break;
      // case XT_LON_10E is handled outside the switch.
      /* DIRECTIONS *******************************************************/
    case XT_LAT_DIR:
      /* latitude N/S as a char */
      writebuff(buff, fmp->printfc,
                LAT_DIR(lat));
      break;
    case XT_LON_DIR:
      /* longitude E/W as a char */
      writebuff(buff, fmp->printfc,
                LON_DIR(lon));
      break;

      /* SPECIAL COORDINATES */
    case XT_MAP_EN_BNG: {
      char map[3];
      double north, east;
      if (! GPS_Math_WGS84_To_UKOSMap_M(wpt->latitude, wpt->longitude, &east, &north, map))
        fatal(MYNAME ": Position (%.5f/%.5f) outside of BNG.\n",
              wpt->latitude, wpt->longitude);
      snprintf(buff, sizeof(buff), fmp->printfc, map, (int)(east + 0.5), (int)(north + 0.5));
    }
    break;
    case XT_UTM: {
      char tbuf[100];
      GPS_Math_WGS84_To_UTM_EN(wpt->latitude, wpt->longitude,
                               &utme, &utmn, &utmz, &utmzc);
      snprintf(tbuf, sizeof(tbuf), "%d%c %6.0f %7.0f",
               utmz, utmzc, utme, utmn);
      writebuff(buff, fmp->printfc, tbuf);
    }
    break;
    case XT_UTM_ZONE:
      GPS_Math_WGS84_To_UTM_EN(wpt->latitude, wpt->longitude,
                               &utme, &utmn, &utmz, &utmzc);
      writebuff(buff, fmp->printfc, utmz);
      break;
    case XT_UTM_ZONEC:
      GPS_Math_WGS84_To_UTM_EN(wpt->latitude, wpt->longitude,
                               &utme, &utmn, &utmz, &utmzc);
      writebuff(buff, fmp->printfc, utmzc);
      break;
    case XT_UTM_ZONEF: {
      char tbuf[10];
      GPS_Math_WGS84_To_UTM_EN(wpt->latitude, wpt->longitude,
                               &utme, &utmn, &utmz, &utmzc);
      tbuf[0] = 0;
      snprintf(tbuf, sizeof(tbuf), "%d%c", utmz, utmzc);
      writebuff(buff, fmp->printfc, tbuf);
    }
    break;
    case XT_UTM_NORTHING:
      GPS_Math_WGS84_To_UTM_EN(wpt->latitude, wpt->longitude,
                               &utme, &utmn, &utmz, &utmzc);
      writebuff(buff, fmp->printfc, utmn);
      break;
    case XT_UTM_EASTING:
      GPS_Math_WGS84_To_UTM_EN(wpt->latitude, wpt->longitude,
                               &utme, &utmn, &utmz, &utmzc);
      writebuff(buff, fmp->printfc, utme);
      break;

      /* ALTITUDE CONVERSIONS**********************************************/
    case XT_ALT_FEET:
      /* altitude in feet as a decimal value */
      writebuff(buff, fmp->printfc,
                METERS_TO_FEET(wpt->altitude));
      break;
    case XT_ALT_METERS:
      /* altitude in meters as a decimal value */
      writebuff(buff, fmp->printfc,
                wpt->altitude);
      break;

      /* DISTANCE CONVERSIONS**********************************************/
      /* prefer odometer distance. */
      /* if not available, use calculated distance from positions */
    case XT_PATH_DISTANCE_MILES:
      /* path (route/track) distance in miles */
      if (wpt->odometer_distance) {
        writebuff(buff, fmp->printfc, METERS_TO_MILES(wpt->odometer_distance));
      } else {
        writebuff(buff, fmp->printfc, pathdist);
      }
      break;
    case XT_PATH_DISTANCE_METERS:
      /* path (route/track) distance in meters */
      if (wpt->odometer_distance) {
        writebuff(buff, fmp->printfc, wpt->odometer_distance);
      } else {
        writebuff(buff, fmp->printfc, MILES_TO_METERS(pathdist));
      }
      break;
    case XT_PATH_DISTANCE_KM:
      /* path (route/track) distance in kilometers */
      if (wpt->odometer_distance) {
        writebuff(buff, fmp->printfc, wpt->odometer_distance / 1000.0);
      } else {
        writebuff(buff, fmp->printfc, MILES_TO_METERS(pathdist) / 1000.0);
      }
      break;
    case XT_PATH_SPEED:
      writebuff(buff, fmp->printfc, wpt->speed);
      break;
    case XT_PATH_SPEED_KPH:
      writebuff(buff, fmp->printfc, MPS_TO_KPH(wpt->speed));
      break;
    case XT_PATH_SPEED_MPH:
      writebuff(buff, fmp->printfc, MPS_TO_MPH(wpt->speed));
      break;
    case XT_PATH_SPEED_KNOTS:
      writebuff(buff, fmp->printfc, MPS_TO_KNOTS(wpt->speed));
      break;
    case XT_PATH_COURSE:
      writebuff(buff, fmp->printfc, wpt->course);
      break;

      /* HEART RATE CONVERSION***********************************************/
    case XT_HEART_RATE:
      writebuff(buff, fmp->printfc, wpt->heartrate);
      break;
      /* CADENCE CONVERSION***********************************************/
    case XT_CADENCE:
      writebuff(buff, fmp->printfc, wpt->cadence);
      break;
      /* POWER CONVERSION***********************************************/
    case XT_POWER:
      writebuff(buff, fmp->printfc, wpt->power);
      break;
    case XT_TEMPERATURE:
      writebuff(buff, fmp->printfc, wpt->temperature);
      break;
    case XT_TEMPERATURE_F:
      writebuff(buff, fmp->printfc, CELSIUS_TO_FAHRENHEIT(wpt->temperature));
      break;
      /* TIME CONVERSIONS**************************************************/
    case XT_EXCEL_TIME:
      /* creation time as an excel (double) time */
      writebuff(buff, fmp->printfc, TIMET_TO_EXCEL(wpt->GetCreationTime().toTime_t()));
      break;
    case XT_TIMET_TIME:
      /* time as a time_t variable */
    {
      time_t tt = wpt->GetCreationTime().toTime_t();
      writebuff(buff, fmp->printfc, tt);
    }
    break;
    case XT_TIMET_TIME_MS: {
      /* time as a time_t variable in milliseconds */
      char tbuf[24];
      writetime(tbuf, sizeof(tbuf), "%s", wpt->GetCreationTime().toTime_t(), false);
      char mbuf[32];
      snprintf(mbuf, sizeof(mbuf), "%s%03d", tbuf, wpt->GetCreationTime().time().msec());
      writebuff(buff, "%s", mbuf);
    }
    break;
    case XT_YYYYMMDD_TIME:
      writebuff(buff, fmp->printfc, time_to_yyyymmdd(wpt->GetCreationTime()));
      break;
    case XT_GMT_TIME:
      writetime(buff, sizeof buff, fmp->printfc, wpt->GetCreationTime(), true);
      break;
    case XT_LOCAL_TIME:
      writetime(buff, sizeof buff, fmp->printfc, wpt->GetCreationTime(), false);
      break;
    case XT_HMSG_TIME:
      writehms(buff, sizeof buff, fmp->printfc, wpt->GetCreationTime(), 1);
      break;
    case XT_HMSL_TIME:
      writehms(buff, sizeof buff, fmp->printfc, wpt->GetCreationTime(), 0);
      break;
    case XT_ISO_TIME:
      writetime(buff, sizeof buff, "%Y-%m-%dT%H:%M:%SZ", wpt->GetCreationTime(), true);
      break;
    case XT_ISO_TIME_MS:
      strcpy(buff, wpt->GetCreationTime().toPrettyString().toUtf8().data());
      break;
    case XT_GEOCACHE_LAST_FOUND:
      writebuff(buff, fmp->printfc, time_to_yyyymmdd(wpt->gc_data->last_found));
      break;

      /* GEOCACHE STUFF **************************************************/
    case XT_GEOCACHE_DIFF:
      /* Geocache Difficulty as a double */
      writebuff(buff, fmp->printfc, wpt->gc_data->diff / 10.0);
      field_is_unknown = !wpt->gc_data->diff;
      break;
    case XT_GEOCACHE_TERR:
      /* Geocache Terrain as a double */
      writebuff(buff, fmp->printfc, wpt->gc_data->terr / 10.0);
      field_is_unknown = !wpt->gc_data->terr;
      break;
    case XT_GEOCACHE_CONTAINER:
      /* Geocache Container */
      writebuff(buff, fmp->printfc, gs_get_container(wpt->gc_data->container));
      field_is_unknown = wpt->gc_data->container == gc_unknown;
      break;
    case XT_GEOCACHE_TYPE:
      /* Geocache Type */
      writebuff(buff, fmp->printfc, gs_get_cachetype(wpt->gc_data->type));
      field_is_unknown = wpt->gc_data->type == gt_unknown;
      break;
    case XT_GEOCACHE_HINT:
      writebuff(buff, fmp->printfc, wpt->gc_data->hint.toUtf8().data());
      field_is_unknown = !wpt->gc_data->hint.isEmpty();
      break;
    case XT_GEOCACHE_PLACER:
      writebuff(buff, fmp->printfc, wpt->gc_data->placer.toUtf8().data());
      field_is_unknown = !wpt->gc_data->placer.isEmpty();
      break;
    case XT_GEOCACHE_ISAVAILABLE:
      if (wpt->gc_data->is_available == status_false) {
        writebuff(buff, fmp->printfc, "False");
      } else if (wpt->gc_data->is_available == status_true) {
        writebuff(buff, fmp->printfc, "True");
      } else {
        writebuff(buff, fmp->printfc, "Unknown");
      }
      break;
    case XT_GEOCACHE_ISARCHIVED:
      if (wpt->gc_data->is_archived == status_false) {
        writebuff(buff, fmp->printfc, "False");
      } else if (wpt->gc_data->is_archived == status_true) {
        writebuff(buff, fmp->printfc, "True");
      } else {
        writebuff(buff, fmp->printfc, "Unknown");
      }
      break;
      /* Tracks and Routes ***********************************************/
    case XT_TRACK_NEW:
      if (csv_track) {
        if (WAYPT_HAS(wpt,new_trkseg)) {
          writebuff(buff, fmp->printfc, 1);
        } else {
          writebuff(buff, fmp->printfc, 0);
        }
      }
      break;
    case XT_TRACK_NAME:
      if (csv_track) {
        QString r = csv_track->rte_name;
        writebuff(buff, fmp->printfc, NONULL(r));
      }
      break;
    case XT_ROUTE_NAME:
      if (csv_route) {
        QString r = csv_route->rte_name;
        writebuff(buff, fmp->printfc, NONULL(r));
      }
      break;

      /* GPS STUFF *******************************************************/
    case XT_GPS_HDOP:
      writebuff(buff, fmp->printfc, wpt->hdop);
      field_is_unknown = !wpt->hdop;
      break;
    case XT_GPS_VDOP:
      writebuff(buff, fmp->printfc, wpt->vdop);
      field_is_unknown = !wpt->vdop;
      break;
    case XT_GPS_PDOP:
      writebuff(buff, fmp->printfc, wpt->pdop);
      field_is_unknown = !wpt->pdop;
      break;
    case XT_GPS_SAT:
      writebuff(buff, fmp->printfc, wpt->sat);
      field_is_unknown = !wpt->sat;
      break;
    case XT_GPS_FIX: {
      const char* fix = NULL;
      switch (wpt->fix) {
      case fix_unknown:
        field_is_unknown = 1;
        fix = "Unknown";
        break;
      case fix_none:
        fix = "None";
        break;
      case fix_2d:
        fix = "2d";
        break;
      case fix_3d:
        fix = "3d";
        break;
      case fix_dgps:
        fix = "dgps";
        break;
      case fix_pps:
        fix = "pps";
        break;
      }
      writebuff(buff, fmp->printfc, fix);
    }
    break;
    /* GMSD ************************************************************/
    case XT_COUNTRY: {
      garmin_fs_t* gmsd = GMSD_FIND(wpt);
      writebuff(buff, fmp->printfc, GMSD_GET(country, ""));
    }
    break;
    case XT_STATE: {
      garmin_fs_t* gmsd = GMSD_FIND(wpt);
      writebuff(buff, fmp->printfc, GMSD_GET(state, ""));
    }
    break;
    case XT_CITY: {
      garmin_fs_t* gmsd = GMSD_FIND(wpt);
      writebuff(buff, fmp->printfc, GMSD_GET(city, ""));
    }
    break;
    case XT_POSTAL_CODE: {
      garmin_fs_t* gmsd = GMSD_FIND(wpt);
      writebuff(buff, fmp->printfc, GMSD_GET(postal_code, ""));
    }
    break;
    case XT_STREET_ADDR: {
      garmin_fs_t* gmsd = GMSD_FIND(wpt);
      writebuff(buff, fmp->printfc, GMSD_GET(addr, ""));
    }
    break;
    case XT_PHONE_NR: {
      garmin_fs_t* gmsd = GMSD_FIND(wpt);
      writebuff(buff, fmp->printfc, GMSD_GET(phone_nr, ""));
    }
    break;
    case XT_FACILITY: {
      garmin_fs_t* gmsd = GMSD_FIND(wpt);
      writebuff(buff, fmp->printfc, GMSD_GET(facility, ""));
    }
    break;
    /* specials */
    case XT_FILENAME:
      writebuff(buff, fmp->printfc, wpt->session->filename);
      break;
    case XT_FORMAT:
      writebuff(buff, fmp->printfc, wpt->session->name);
      break;
    case -1:
      if (strncmp(fmp->key, "LON_10E", 7) == 0) {
        writebuff(buff, fmp->printfc, lon * pow((double)10, atof(fmp->key+7)));
      } else if (strncmp(fmp->key, "LAT_10E", 7) == 0) {
        writebuff(buff, fmp->printfc, lat * pow((double)10, atof(fmp->key+7)));
      }
      break;
    default:
      warning(MYNAME ": Unknown style directive: %s\n", fmp->key);
      break;
    }
    obuff = csv_stringclean(buff, xcsv_file.badchars);

    if (field_is_unknown && fmp->options & OPTIONS_OPTIONAL) {
      goto next;
    }

    if (xcsv_file.field_encloser) {
      /* print the enclosing character(s) */
      gbfprintf(xcsv_file.xcsvfp, "%s", xcsv_file.field_encloser);
    }

    /* As a special case (pronounced "horrible hack") we allow
     * ""%s"" to smuggle bad characters through.
     */
    if (0 == strcmp(fmp->printfc, "\"%s\"")) {
      gbfprintf(xcsv_file.xcsvfp, "\"%s\"", obuff);
    } else {
      gbfprintf(xcsv_file.xcsvfp, "%s", obuff);
    }

    if (xcsv_file.field_encloser) {
      /* print the enclosing character(s) */
      gbfprintf(xcsv_file.xcsvfp, "%s", xcsv_file.field_encloser);
    }

next:
    xfree(obuff);
  }

  gbfprintf(xcsv_file.xcsvfp, "%s", xcsv_file.record_delimiter);

  if (description && description != shortname) {
    xfree(description);
  }

  if (shortname) {
    xfree(shortname);
  }

  /* increment the index counter */
  waypt_out_count++;
}

static void
xcsv_noop(const route_head* wp)
{
  /* no-op */
}

/*****************************************************************************/
/* xcsv_data_write(void) - write prologues, spawn the output loop, and write */
/*                         epilogues.                                        */
/*****************************************************************************/
void
xcsv_data_write(void)
{
  queue* elem, *tmp;
  ogue_t* ogp;
  time_t time;
  struct tm tm;
  char tbuf[32];

  /* reset the index counter */
  waypt_out_count = 0;

  time = gpsbabel_time;
  if (time == 0) {	/* testo script ? */
    tm = *gmtime(&time);
  } else {
    tm = *localtime(&time);
  }

  /* output prologue lines, if any. */
  QUEUE_FOR_EACH(&xcsv_file.prologue, elem, tmp) {
    char* cout, *ctmp;
    ogp = (ogue_t*) elem;

    cout = xstrdup((ogp->val) ? ogp->val : "");

    while ((ctmp = strsub(cout, "__FILE__", xcsv_file.fname))) {
      xfree(cout);
      cout = ctmp;
    }

    while ((ctmp = strsub(cout, "__VERSION__", (time == 0) ? "" : gpsbabel_version))) {
      xfree(cout);
      cout = ctmp;
    }

    while (strstr(cout, "__DATE__")) {
      strftime(tbuf, sizeof(tbuf), "%m/%d/%Y", &tm);
      ctmp = strsub(cout, "__DATE__", tbuf);
      xfree(cout);
      cout = ctmp;
    }

    while (strstr(cout, "__TIME__")) {
      strftime(tbuf, sizeof(tbuf), "%H:%S:%M", &tm);
      ctmp = strsub(cout, "__TIME__", tbuf);
      xfree(cout);
      cout = ctmp;
    }

    while (strstr(cout, "__DATE_AND_TIME__")) {
      strftime(tbuf, sizeof(tbuf), "%a %b %d %H:%M:%S %Y", &tm);
      ctmp = strsub(cout, "__DATE_AND_TIME__", tbuf);
      xfree(cout);
      cout = ctmp;
    }

    gbfprintf(xcsv_file.xcsvfp, "%s", cout);
    xfree(cout);
    gbfprintf(xcsv_file.xcsvfp, "%s", xcsv_file.record_delimiter);
  }

  if ((xcsv_file.datatype == 0) || (xcsv_file.datatype == wptdata)) {
    waypt_disp_all(xcsv_waypt_pr);
  }
  if ((xcsv_file.datatype == 0) || (xcsv_file.datatype == rtedata)) {
    route_disp_all(xcsv_resetpathlen,xcsv_noop,xcsv_waypt_pr);
  }
  if ((xcsv_file.datatype == 0) || (xcsv_file.datatype == trkdata)) {
    track_disp_all(xcsv_resetpathlen,xcsv_noop,xcsv_waypt_pr);
  }

  /* output epilogue lines, if any. */
  QUEUE_FOR_EACH(&xcsv_file.epilogue, elem, tmp) {
    ogp = (ogue_t*) elem;
    gbfprintf(xcsv_file.xcsvfp, "%s%s", ogp->val, xcsv_file.record_delimiter);
  }
}
#endif
