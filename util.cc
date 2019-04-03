/*
    Misc utilities.

    Copyright (C) 2002-2014 Robert Lipe, robertlipe+source@gpsbabel.org

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

#include <cctype>                  // for isspace, isalpha, ispunct, tolower, toupper
#include <cerrno>                  // for errno
#include <cmath>                   // for fabs, floor
#include <cstdarg>                 // for va_list, va_end, va_start, va_copy
#include <cstdint>                 // for uint32_t
#include <cstdio>                  // for size_t, vsnprintf, FILE, fopen, printf, sprintf, stderr, stdin, stdout
#include <cstdlib>                 // for abs, getenv, calloc, free, malloc, realloc
#include <cstring>                 // for strlen, strcat, strstr, memcpy, strcmp, strcpy, strdup, strchr, strerror
#include <ctime>                   // for mktime, localtime

#include <QtCore/QByteArray>       // for QByteArray
#include <QtCore/QChar>            // for QChar, operator<=, operator>=
#include <QtCore/QCharRef>         // for QCharRef
#include <QtCore/QDateTime>        // for QDateTime
#include <QtCore/QFileInfo>        // for QFileInfo
#include <QtCore/QList>            // for QList
#include <QtCore/QString>          // for QString, operator+
#include <QtCore/QTextCodec>       // for QTextCodec
#include <QtCore/QTextStream>      // for operator<<, QTextStream, qSetFieldWidth, endl, QTextStream::AlignLeft
#include <QtCore/Qt>               // for CaseInsensitive
#include <QtCore/QtGlobal>         // for qPrintable

#include "defs.h"
#include "cet.h"                   // for cet_utf8_to_ucs4
#include "src/core/datetime.h"     // for DateTime
#include "src/core/xmltag.h"


// First test Apple's clever macro that's really a runtime test so
// that our universal binaries work right.
#if defined __BIG_ENDIAN__
#define i_am_little_endian !__BIG_ENDIAN__
#else
#if defined WORDS_BIGENDIAN
# define i_am_little_endian 0
#else
# define i_am_little_endian 1
#endif
#endif

void*
xmalloc(size_t size)
{
  void* obj = malloc(size);

  if (!obj) {
    fatal("gpsbabel: Unable to allocate %ld bytes of memory.\n", (unsigned long) size);
  }

  return obj;
}

void*
xcalloc(size_t nmemb, size_t size)
{
  void* obj = calloc(nmemb, size);

  if (!obj) {
    fatal("gpsbabel: Unable to allocate %ld units of %ld bytes of memory.\n", (unsigned long) nmemb, (unsigned long) size);
  }

  return obj;
}

void
xfree(const void* mem)
{
  free(const_cast<void*>(mem));
}

char*
xstrdup(const char* s)
{
  char* o = s ? strdup(s) : strdup("");

  if (!o) {
    fatal("gpsbabel: Unable to allocate %ld bytes of memory.\n", (unsigned long) strlen(s));
  }

  return o;
}

char* xstrdup(const QString& s)
{
  return xstrdup(CSTR(s));
}

/*
 * Duplicate at most sz bytes in str.
 */
char*
xstrndup(const char* str, size_t sz)
{
  size_t newlen = 0;
  const char* cin = str;

  while ((newlen < sz) && (*cin != '\0')) {
    newlen++;
    cin++;
  }

  char* newstr = (char*) xmalloc(newlen + 1);
  memcpy(newstr, str, newlen);
  newstr[newlen] = 0;

  return newstr;
}

void*
xrealloc(void* p, size_t s)
{
  char* o = (char*) realloc(p, s);

  if (!o) {
    fatal("gpsbabel: Unable to realloc %ld bytes of memory.\n", (unsigned long) s);
  }

  return o;
}

/*
* For an allocated string, realloc it and append 's'
*/
char*
xstrappend(char* src, const char* newd)
{
  if (!src) {
    return xstrdup(newd);
  }
  if (!newd) {
    return xstrdup(src);
  }

  size_t newsz = strlen(src) + strlen(newd) + 1;
  src = (char*) xrealloc(src, newsz);
  strcat(src, newd);

  return src;
}

/*
 * Wrapper for open that honours - for stdin, stdout, unifies error text.
 */
FILE*
xfopen(const char* fname, const char* type, const char* errtxt)
{
  int am_writing = strchr(type, 'w') != nullptr;

  if (fname == nullptr) {
    fatal("%s must have a filename specified for %s.\n",
          errtxt, am_writing ? "write" : "read");
  }

  if (0 == strcmp(fname, "-")) {
    return am_writing ? stdout : stdin;
  }
  FILE* f = ufopen(QString::fromUtf8(fname), type);
  if (nullptr == f) {
    fatal("%s cannot open '%s' for %s.  Error was '%s'.\n",
          errtxt, fname,
          am_writing ? "write" : "read",
          strerror(errno));
  }
  return f;
}

/*
 * Thin wrapper around fopen() that supports UTF-8 fname on all platforms.
 */
FILE*
ufopen(const QString& fname, const char* mode)
{
#if __WIN32__
  // On Windows standard fopen() doesn't support UTF-8, so we have to convert
  // to wchar_t* (UTF-16) and use the wide-char version of fopen(), _wfopen().
  return _wfopen((const wchar_t*) fname.utf16(),
                 (const wchar_t*) QString(mode).utf16());
#else
  // On other platforms, convert to native locale (UTF-8 or other 8-bit).
  return fopen(qPrintable(fname), mode);
#endif
}

/*
 * OS-abstracting wrapper for getting Unicode environment variables.
 */
QString ugetenv(const char* env_var) {
#ifdef __WIN32__
  // Use QString to convert 8-bit env_var argument to wchar_t* for _wgetenv().
  return QString::fromWCharArray(
      _wgetenv((const wchar_t*) QString(env_var).utf16()));
#else
  // Everyone else uses UTF-8 or some other locale-specific 8-bit encoding.
  return QString::fromLocal8Bit(std::getenv(env_var));
#endif
}

/*
 * Allocate a string using a format list with optional arguments
 * Returns -1 on error.
 * If return value is anything else, *strp will be populated with an
 * allocated string containging the formatted buffer.
 *
 * Freeing that is the responsbility of the caller.
 */

int
xasprintf(char** strp, const char* fmt, ...)
{
  va_list args;

  va_start(args, fmt);
  int res = xvasprintf(strp, fmt, args);
  va_end(args);

  return res;
}

int
xasprintf(QString* strp, const char* fmt, ...)
{
  va_list args;
  va_start(args, fmt);
  char* cstrp;
  int res = xvasprintf(&cstrp, fmt, args);
  *strp = cstrp;
  xfree(cstrp);
  va_end(args);

  return res;
}

int
xvasprintf(char** strp, const char* fmt, va_list ap)
{
  /* From http://perfec.to/vsnprintf/pasprintf.c */
  /* size of first buffer malloc; start small to exercise grow routines */
# define	FIRSTSIZE	1
  char* buf = nullptr;
  char* newbuf;
  size_t nextsize = 0;
  int outsize;
  va_list args;

  int bufsize = 0;
  for (;;) {
    if (bufsize == 0) {
      if ((buf = (char*) xmalloc(FIRSTSIZE)) == nullptr) {
        *strp = nullptr;
        return -1;
      }
      bufsize = FIRSTSIZE;
    } else if ((newbuf = (char*) xrealloc(buf, nextsize)) != nullptr) {
      buf = newbuf;
      bufsize = nextsize;
    } else {
      xfree(buf);
      *strp = nullptr;
      return -1;
    }

    va_copy(args, ap);
    outsize = vsnprintf(buf, bufsize, fmt, args);
    va_end(args);

    if (outsize == -1) {
      /* Clear indication that output was truncated, but no
       * clear indication of how big buffer needs to be, so
       * simply double existing buffer size for next time.
       */
      nextsize = bufsize * 2;

    } else if (outsize == bufsize) {
      /* Output was truncated (since at least the \0 could
       * not fit), but no indication of how big the buffer
       * needs to be, so just double existing buffer size
       * for next time.
       */
      nextsize = bufsize * 2;

    } else if (outsize > bufsize) {
      /* Output was truncated, but we were told exactly how
       * big the buffer needs to be next time. Add two chars
       * to the returned size. One for the \0, and one to
       * prevent ambiguity in the next case below.
       */
      nextsize = outsize + 2;

    } else if (outsize == bufsize - 1) {
      /* This is ambiguous. May mean that the output string
       * exactly fits, but on some systems the output string
       * may have been trucated. We can't tell.
       * Just double the buffer size for next time.
       */
      nextsize = bufsize * 2;

    } else {
      /* Output was not truncated */
      break;
    }
  }
  /* Prevent us from allocating millions of unused bytes. */
  /* O.K.: I think this is not the final solution. */
  if (bufsize > outsize + 1) {
    const unsigned ptrsz = sizeof(buf);
    if (((bufsize + ptrsz + 1) / ptrsz) > ((outsize + ptrsz + 1) / ptrsz)) {
      buf = (char*) xrealloc(buf, outsize + 1);
    }

  }
  *strp = buf;
  return outsize;
}

void
rtrim(char* s)
{
  char* t = s;

  if (!s || !*s) {
    return;
  }

  while (*s) {
    s++;
  }

  s--;
  while ((s >= t) && isspace(*s)) {
    *s = 0;
    s--;
  }
}

/*
 * Like trim, but trims whitespace from both beginning and end.
 */
char*
lrtrim(char* buff)
{
  if (buff[0] == '\0') {
    return buff;
  }

  char* c = buff + strlen(buff);
  while ((c >= buff) && ((unsigned char)*c <= ' ')) {
    *c-- = '\0';
  }

  c = buff;
  while ((*c != '\0') && ((unsigned char)*c <= ' ')) {
    c++;
  }

  if (c != buff) {
    char* src = c;
    char* dst = buff;

    while (*src) {
      *dst++ = *src++;
    }
    *dst = '\0';
  }

  return buff;
}

/*
 * compare str with match
 * match may contain wildcards "*" and "?"
 *
 * examples:
 *		str_match("ABCDE", "*BC*") ->	1
 *		str_match("ABCDE", "A*C*E") ->	1
 *		str_match("?ABCDE", "\\?A*") ->	1
 *		str_match("", "*A") -> 		0
 */

int
str_match(const char* str, const char* match)
{
  const char* s = str;
  const char* m = match;

  while (*m || *s) {
    switch (*m) {

    case '\0':
      /* there is something left in s, FAIL */
      return 0;

    case '*':
      /* skip all wildcards */
      while ((*m == '*') || (*m == '?')) {
        m++;
      }
      if (*m == '\0') {
        return 1;
      }

      if (*m == '\\') {			/* ? escaped ? */
        m++;
        if (*m == '\0') {
          return 0;
        }
      }

      do {
        while (*s && (*s != *m)) {
          s++;
        }
        if (*s == '\0') {
          return 0;
        }

        const char* sx = s + 1;
        const char* mx = m + 1;

        while (*sx) {
          if (*mx == '\\') {	/* ? escaped ? */
            mx++;
            if (*mx == '\0') {
              return 0;
            }

          }
          if (*sx == *mx) {
            sx++;
            mx++;
          } else {
            break;
          }
        }
        if (*mx == '\0') {	/* end of match */
          if (*sx == '\0') {
            return 1;
          }
          s++;
        } else if ((*mx == '?') || (*mx == '*')) {
          s = sx;
          m = mx;
          break;
        } else {
          s++;
        }
      } while (*s);
      break;

    case '?':
      if (*s == '\0') {
        return 0;  /* no character left */
      }
      m++;
      s++;
      break;

    case '\\':
      m++;
      if (*m == '\0') {
        return 0;  /* incomplete escape sequence */
      }
    /* pass-through next character */

    default:
      if (*m != *s) {
        return 0;
      }
      m++;
      s++;
    }
  }
  return ((*s == '\0') && (*m == '\0'));
}

void
printposn(const double c, int is_lat)
{
  char d;
  if (is_lat) {
    if (c < 0) {
      d = 'S';
    } else {
      d = 'N';
    }
  } else {
    if (c < 0) {
      d = 'W';
    } else {
      d = 'E';
    }
  }
  printf("%f%c ", fabs(c), d);
}

void
is_fatal(const int condition, const char* fmt, ...)
{
  va_list args;
  char buff[128];

  if (condition == 0) {
    return;
  }

  va_start(args, fmt);
  vsnprintf(buff, sizeof(buff), fmt, args);
  va_end(args);

  fatal("%s\n", buff);
}

/*
 * Read 4 bytes in big-endian.   Return as "int" in native endianness.
 */
signed int
be_read32(const void* ptr)
{
  const unsigned char* i = (const unsigned char*) ptr;
  return i[0] << 24 | i[1] << 16  | i[2] << 8 | i[3];
}

signed int
be_read16(const void* ptr)
{
  const unsigned char* i = (const unsigned char*) ptr;
  return i[0] << 8 | i[1];
}

unsigned int
be_readu16(const void* ptr)
{
  const unsigned char* i = (const unsigned char*) ptr;
  return i[0] << 8 | i[1];
}

void
be_write16(void* ptr, const unsigned value)
{
  unsigned char* p = (unsigned char*) ptr;
  p[0] = value >> 8;
  p[1] = value;
}

void
be_write32(void* ptr, const unsigned value)
{
  unsigned char* p = (unsigned char*) ptr;

  p[0] = value >> 24;
  p[1] = value >> 16;
  p[2] = value >> 8;
  p[3] = value;
}

signed int
le_read16(const void* ptr)
{
  const unsigned char* p = (const unsigned char*) ptr;
  return p[0] | (p[1] << 8);
}

unsigned int
le_readu16(const void* ptr)
{
  const unsigned char* p = (const unsigned char*) ptr;
  return p[0] | (p[1] << 8);
}

signed int
le_read32(const void* ptr)
{
  const unsigned char* p = (const unsigned char*) ptr;
  return p[0] | (p[1] << 8) | (p[2] << 16) | (p[3] << 24);
}

unsigned int
le_readu32(const void* ptr)
{
  const unsigned char* p = (const unsigned char*) ptr;
  return p[0] | (p[1] << 8) | (p[2] << 16) | (p[3] << 24);
}

/*
 *  Read a little-endian 64-bit value from 'src' and return it in 'dest'
 *  in host endianness.
 */
void
le_read64(void* dest, const void* src)
{
  char* cdest = (char*) dest;
  const char* csrc = (const char*) src;

  if (i_am_little_endian) {
    memcpy(dest, src, 8);
  } else {
    int i;
    for (i = 0; i < 8; i++) {
      cdest[i] = csrc[7-i];
    }
  }
}

void
le_write16(void* ptr, const unsigned value)
{
  unsigned char* p = (unsigned char*) ptr;
  p[0] = value;
  p[1] = value >> 8;
}

void
le_write32(void* ptr, const unsigned value)
{
  unsigned char* p = (unsigned char*) ptr;
  p[0] = value;
  p[1] = value >> 8;
  p[2] = value >> 16;
  p[3] = value >> 24;
}

signed int
si_round(double d)
{
  if (d < 0) {
    return (signed int)(d-0.5);
  } else {
    return (signed int)(d+0.5);
  }
}

/*
	mkgmtime -- convert tm struct in UTC to time_t

	works just like mktime but without all the mucking
	around with timezones and daylight savings

	obsoletes get_tz_offset()

	Borrowed from lynx GPL source code
	http://lynx.isc.org/release/lynx2-8-5/src/mktime.c

	Written by Philippe De Muyter <phdm@macqel.be>.
*/

time_t
mkgmtime(struct tm* t)
{
  static int      m_to_d[12] =
  {0, 31, 59, 90, 120, 151, 181, 212, 243, 273, 304, 334};

  short month = t->tm_mon;
  short year = t->tm_year + month / 12 + 1900;
  month %= 12;
  if (month < 0) {
    year -= 1;
    month += 12;
  }
  time_t result = (year - 1970) * 365 + m_to_d[month];
  if (month <= 1) {
    year -= 1;
  }
  result += (year - 1968) / 4;
  result -= (year - 1900) / 100;
  result += (year - 1600) / 400;
  result += t->tm_mday;
  result -= 1;
  result *= 24;
  result += t->tm_hour;
  result *= 60;
  result += t->tm_min;
  result *= 60;
  result += t->tm_sec;
  return (result);
}

/*
 * mklocaltime: same as mktime, but try to recover the "Summer time flag",
 *              which is evaluated by mktime
 */
time_t
mklocaltime(struct tm* t)
{
  time_t result;
  struct tm check = *t;

  check.tm_isdst = 0;
  result = mktime(&check);
  check = *localtime(&result);
  if (check.tm_isdst == 1) {	/* DST is in effect */
    check = *t;
    check.tm_isdst = 1;
    result = mktime(&check);
  }
  return result;
}

/*
 * Historically, when we were C, this was A wrapper for time(2) that
 * allowed us to "freeze" time for testing. The UNIX epoch
 * (1970-1-1-00:00:00UTC) was a convenient value for that.  Now in the
 * world of Qt, sub-second time is convenient, but regenerating all the
 * reference files would be tedious, so we uphold that convention.
 */
gpsbabel::DateTime
current_time()
{
  if (getenv("GPSBABEL_FREEZE_TIME")) {
    return QDateTime::fromTime_t(0);
  }

  return QDateTime::currentDateTime();
}

/*
 * Return the (zero based) month number of the year or -1 for failure.
 */
signed int
month_lookup(const char* m)
{
  static const char* months[] = {
    "JAN", "FEB", "MAR", "APR", "MAY", "JUN",
    "JUL", "AUG", "SEP", "OCT", "NOV", "DEC", nullptr
  };

  for (const char** mp = months; *mp; mp++) {
    if (0 == case_ignore_strcmp(*mp, m)) {
      return mp - months;
    }
  }
  return -1;
}

/*
 * Microsoft dot net's time format is the number of 100 nanosecond intervals
 * since midnight Jan 1, 0001.   We have time_t deeply ingrained into our
 * internals and since we're in the GPS biz, timestamps before 1/1/1970 aren't
 * that interesting to us anyway.
 */
#define EPOCH_TICKS 621355968000000000.0
void dotnet_time_to_time_t(double dotnet, time_t* t, int* millisecs)
{
  // TODO: replace this with better interface with normal return values
  // and called via a QDateTime.
  *t = (dotnet - EPOCH_TICKS) / 10000000.;
#if LATER
  // TODO: work out fractional seconds.
  if (millisecs) {
    *millisecs = dotnet % 10000;
  }
#else
  (void)millisecs;
#endif
}


/*
 * Return a pointer to a constant string that is suitable for icon lookup
 * based on geocache attributes.   The strings used are those present in
 * a GPX file from geocaching.com.  Thus we sort of make all the other
 * formats do lookups based on these strings.
 */
const char*
get_cache_icon(const Waypoint* waypointp)
{
  if (!global_opts.smart_icons) {
    return nullptr;
  }

  /*
   * For icons, type overwrites container.  So a multi-micro will
   * get the icons for "multi".
   */
  switch (waypointp->gc_data->type) {
  case gt_virtual:
    return "Virtual cache";
  case gt_multi:
    return "Multi-Cache";
  case gt_event:
    return "Event Cache";
  case gt_suprise:
    return "Unknown Cache";
  case gt_webcam:
    return "Webcam Cache";
  default:
    break;
  }

  switch (waypointp->gc_data->container) {
  case gc_micro:
    return "Micro-Cache";
    break;
  default:
    break;
  }

  if (waypointp->gc_data->diff > 1) {
    return "Geocache";
  }

  return nullptr;
}

double
endian_read_double(const void* ptr, int read_le)
{
  double ret;
  char r[8];
  const void* p;

  if (i_am_little_endian == read_le) {
    p = ptr;
  } else {
    for (int i = 0; i < 8; i++) {
      r[i] = ((char*)ptr)[7-i];
    }
    p = r;
  }

// Word order is different on arm, but not on arm-eabi.
#if defined(__arm__) && !defined(__ARM_EABI__)
  memcpy(&ret, p + 4, 4);
  memcpy(((void*)&ret) + 4, p, 4);
#else
  memcpy(&ret, p, 8);
#endif

  return ret;
}

float
endian_read_float(const void* ptr, int read_le)
{
  float ret;
  char r[4];
  const void* p;

  if (i_am_little_endian == read_le) {
    p = ptr;
  } else {
    for (int i = 0; i < 4; i++) {
      r[i] = ((char*)ptr)[3-i];
    }
    p = r;
  }

  memcpy(&ret, p, 4);
  return ret;
}

void
endian_write_double(void* ptr, double value, int write_le)
{
  char* optr = (char*) ptr;
// Word order is different on arm, but not on arm-eabi.
#if defined(__arm__) && !defined(__ARM_EABI__)
  char r[8];
  memcpy(r + 4, &value, 4);
  memcpy(r, ((void*)&value) + 4, 4);
#else
  char* r = (char*)(void*)&value;
#endif


  if (i_am_little_endian == write_le) {
    memcpy(ptr, r, 8);
  } else {
    for (int i = 0; i < 8; i++) {
      *optr++ = r[7-i];
    }
  }
}

void
endian_write_float(void* ptr, float value, int write_le)
{
  char* r = (char*)(void*)&value;
  char* optr = (char*) ptr;

  if (i_am_little_endian == write_le) {
    memcpy(ptr, &value, 4);
  } else {
    for (int i = 0; i < 4; i++) {
      *optr++ = r[3-i];
    }
  }
}

float
le_read_float(const void* ptr)
{
  return endian_read_float(ptr, 1);
}

void
le_write_float(void* ptr, float value)
{
  endian_write_float(ptr, value, 1);
}

float
be_read_float(void* ptr)
{
  return endian_read_float(ptr, 0);
}

void
be_write_float(void* ptr, float value)
{
  endian_write_float(ptr, value, 0);
}

double
le_read_double(const void* ptr)
{
  return endian_read_double(ptr, 1);
}

void
le_write_double(void* ptr, double value)
{
  endian_write_double(ptr, value, 1);
}

double
be_read_double(void* ptr)
{
  return endian_read_double(ptr, 0);
}

void
be_write_double(void* ptr, double value)
{
  endian_write_double(ptr, value, 0);
}


/* Magellan and PCX formats use this DDMM.mm format */
double ddmm2degrees(double pcx_val)
{
  signed int deg = (signed int)(pcx_val / 100.0);
  double minutes = (((pcx_val / 100.0) - deg) * 100.0) / 60.0;
  return (double) deg + minutes;
}

double degrees2ddmm(double deg_val)
{
  signed int deg = (signed int) deg_val;
  return (deg * 100.0) + ((deg_val - deg) * 60.0);
}

/*
 * replace a single occurrence of "search" in  "s" with "replace".
 * Returns an allocated copy if substitution was made, otherwise returns NULL.
 * Doesn't try to make an optimally sized dest buffer.
 */
char*
strsub(const char* s, const char* search, const char* replace)
{
  int len = strlen(s);
  int slen = strlen(search);
  int rlen = strlen(replace);

  const char* p = strstr(s, search);
  if (!slen || !p) {
    return nullptr;
  }

  char* d = (char*) xmalloc(len + rlen + 1);

  /* Copy first part */
  len = p - s;
  memcpy(d, s, len);
  d[len] = 0;

  /* Copy replacement */
  strcat(d, replace);

  /* Copy last part */
  strcat(d, p + slen);
  return d;
}

/*
 *  As strsub, but do it globally.
 */
char*
gstrsub(const char* s, const char* search, const char* replace)
{
  int ooffs = 0;
  const char* c;
  const char* src = s;
  int olen = strlen(src);
  int slen = strlen(search);
  int rlen = strlen(replace);

  char* o = (char*) xmalloc(olen + 1);

  while ((c = strstr(src, search))) {
    olen += (rlen - slen);
    o = (char*) xrealloc(o, olen + 1);
    memcpy(o + ooffs, src, c - src);
    ooffs += (c - src);
    src = c + slen;
    if (rlen) {
      memcpy(o + ooffs, replace, rlen);
      ooffs += rlen;
    }
  }

  if (ooffs < olen) {
    memcpy(o + ooffs, src, olen - ooffs);
  }
  o[olen] = '\0';
  return o;
}

/*
 *
 */
char*
strupper(char* src)
{
  for (char* c = src; *c; c++) {
    *c = toupper(*c);
  }
  return src;
}

/*
 *
 */
char*
strlower(char* src)
{
  for (char* c = src; *c; c++) {
    *c = tolower(*c);
  }
  return src;
}

QString
rot13(const QString& s)
{
  static const QChar A('A');
  static const QChar M('M');
  static const QChar N('N');
  static const QChar Z('Z');
  QString r = s;
  int i = r.length();
  while (i--) {
    QChar letter = r[i].toUpper();
    if (letter >= A && letter <= M) {
      r[i] = QChar(r[i].toLatin1() + 13);
    } else if (letter >= N && letter <= Z) {
      r[i] = QChar(r[i].toLatin1() - 13);
    }
  }
  return r;
}

/*
 * Convert a human readable date format (i.e. "YYYY/MM/DD") into
 * a format usable for strftime and others
 */

char*
convert_human_date_format(const char* human_datef)
{
  char* result = (char*) xcalloc((2*strlen(human_datef)) + 1, 1);
  char* cout = result;
  char prev = '\0';
  int ylen = 0;

  for (const char* cin = human_datef; *cin; cin++) {
    char okay = 1;

    if (toupper(*cin) != 'Y') {
      ylen = 0;
    }
    if (isalpha(*cin)) {
      switch (*cin) {
      case 'y':
      case 'Y':
        if (prev != 'Y') {
          strcat(cout, "%y");
          cout += 2;
          prev = 'Y';
        }
        ylen++;
        if (ylen > 2) {
          *(cout-1) = 'Y';
        }
        break;
      case 'm':
      case 'M':
        if (prev != 'M') {
          strcat(cout, "%m");
          cout += 2;
          prev = 'M';
        }
        break;
      case 'd':
      case 'D':
        if (prev != 'D') {
          strcat(cout, "%d");
          cout += 2;
          prev = 'D';
        }
        break;
      default:
        okay = 0;
      }
    } else if (ispunct(*cin)) {
      *cout++ = *cin;
      prev = '\0';
    } else {
      okay = 0;
    }

    is_fatal(okay == 0, "Invalid character \"%c\" in date format!", *cin);
  }
  return result;
}

/*
 * Convert a human readable time format (i.e. "HH:mm:ss") into
 * a format usable for strftime and others
 */

char*
convert_human_time_format(const char* human_timef)
{
  char* result = (char*) xcalloc((2*strlen(human_timef)) + 1, 1);
  char* cout = result;
  char prev = '\0';

  for (const char* cin = human_timef; *cin; cin++) {
    int okay = 1;

    if (isalpha(*cin)) {
      switch (*cin) {
      case 'S':
      case 's':
        if (prev != 'S') {
          strcat(cout, "%S");
          cout += 2;
          prev = 'S';
        }
        break;

      case 'M':
      case 'm':
        if (prev != 'M') {
          strcat(cout, "%M");
          cout += 2;
          prev = 'M';
        }
        break;

      case 'h':				/* 12-hour-clock */
        if (prev != 'H') {
          strcat(cout, "%l");	/* 1 .. 12 */
          cout += 2;
          prev = 'H';
        } else {
          *(cout-1) = 'I';  /* 01 .. 12 */
        }
        break;

      case 'H':				/* 24-hour-clock */
        if (prev != 'H') {
          strcat(cout, "%k");
          cout += 2;
          prev = 'H';
        } else {
          *(cout-1) = 'H';
        }
        break;

      case 'x':
        if (prev != 'X') {
          strcat(cout, "%P");
          cout += 2;
          prev = 'X';
        } else {
          *(cout-1) = 'P';
        }
        break;

      case 'X':
        if (prev != 'X') {
          strcat(cout, "%p");
          cout += 2;
          prev = 'X';
        } else {
          *(cout-1) = 'p';
        }
        break;

      default:
        okay = 0;
      }
    } else if (ispunct(*cin) || isspace(*cin)) {
      *cout++ = *cin;
      prev = '\0';
    } else {
      okay = 0;
    }

    is_fatal(okay == 0, "Invalid character \"%c\" in time format!", *cin);
  }
  return result;
}


/*
 * Return a decimal degree pair as
 * DD.DDDDD  DD MM.MMM or DD MM SS.S
 * fmt = ['d', 'm', 's']
 * sep = string between lat and lon (separator)
 * html = 1 for html output otherwise text
 */
char*
pretty_deg_format(double lat, double lon, char fmt, const char* sep, int html)
{
  char*	result;
  char latsig = lat < 0 ? 'S':'N';
  char lonsig = lon < 0 ? 'W':'E';
  int latint = abs((int) lat);
  int lonint = abs((int) lon);
  double latmin = 60.0 * (fabs(lat) - latint);
  double lonmin = 60.0 * (fabs(lon) - lonint);
  double latsec = 60.0 * (latmin - floor(latmin));
  double lonsec = 60.0 * (lonmin - floor(lonmin));
  if (sep == nullptr) {
    sep = " ";  /* default " " */
  }
  if (fmt == 'd') { /* ddd */
    xasprintf(&result, "%c%6.5f%s%s%c%6.5f%s",
              latsig, fabs(lat), html?"&deg;":"", sep,
              lonsig, fabs(lon), html?"&deg;":"");
  } else if (fmt == 's') { /* dms */
    xasprintf(&result, "%c%d%s%02d'%04.1f\"%s%c%d%s%02d'%04.1f\"",
              latsig, latint, html?"&deg;":" ", (int)latmin, latsec, sep,
              lonsig, lonint, html?"&deg;":" ", (int)lonmin, lonsec);
  } else { /* default dmm */
    xasprintf(&result,  "%c%d%s%06.3f%s%c%d%s%06.3f",
              latsig, latint, html?"&deg;":" ", latmin, sep,
              lonsig, lonint, html?"&deg;":" ", lonmin);
  }
  return result;
}



/*
 * Get rid of potentially nasty HTML that would influence another record
 * that includes;
 * <body> - to stop backgrounds/background colours from being loaded
 * </body> and </html>- stop processing altogether
 * <style> </style> - stop overriding styles for everything
 */
char*
strip_nastyhtml(const QString& in)
{
  char* returnstr;
  char* lcstr;

  char* sp = returnstr = xstrdup(in);
  char* lcp = lcstr = strlower(xstrdup(in));

  while (lcp = strstr(lcstr, "<body>"), nullptr != lcp) {
    sp = returnstr + (lcp - lcstr) ; /* becomes <!   > */
    sp++;
    *sp++ = '!';
    *sp++ = ' ';
    *sp++ = ' ';
    *sp++ = ' ';
    *lcp = '*';         /* so we wont find it again */
  }
  while (lcp = strstr(lcstr, "<body"), lcp != nullptr) {   /* becomes <!--        --> */
    sp = returnstr + (lcp - lcstr) ;
    sp++;
    *sp++ = '!';
    *sp++ = '-';
    *sp++ = '-';
    while ((*sp) && (*sp != '>')) {
      sp++;
    }
    *--sp = '-';
    *--sp = '-';
    *lcp = '*';         /* so we wont find it again */
  }
  while (lcp = strstr(lcstr, "</body>"), nullptr != lcp) {
    sp = returnstr + (lcp - lcstr) ; /* becomes <!---- */
    sp++;
    *sp++ = '!';
    *sp++ = '-';
    *sp++ = '-';
    *sp++ = '-';
    *sp++ = '-';
    *lcp = '*';         /* so we wont find it again */
  }
  while (lcp = strstr(lcstr, "</html>"), nullptr != lcp) {
    sp = returnstr + (lcp - lcstr) ; /* becomes </---- */
    sp++;
    *sp++ = '!';
    *sp++ = '-';
    *sp++ = '-';
    *sp++ = '-';
    *sp++ = '-';
    *lcp = '*';         /* so we wont find it again */
  }
  while (lcp = strstr(lcstr, "<style"), nullptr != lcp) {
    sp = returnstr + (lcp - lcstr) ; /* becomes <!--   */
    sp++;
    *sp++ = '!';
    *sp++ = '-';
    *sp++ = '-';
    *sp++ = ' ';
    *sp++ = ' ';
    *sp = ' ';
    *lcp = '*';         /* so we wont find it again */
  }
  while (lcp = strstr(lcstr, "</style>"), nullptr != lcp) {
    sp = returnstr + (lcp - lcstr) ; /* becomes    --> */
    *sp++ = ' ';
    *sp++ = ' ';
    *sp++ = ' ';
    *sp++ = ' ';
    *sp++ = ' ';
    *sp++ = '-';
    *sp++ = '-';
    *lcp = '*';         /* so we wont find it again */
  }
  while (lcp = strstr(lcstr, "<image"), nullptr != lcp) {
    sp = returnstr + (lcp - lcstr) ; /* becomes <img */
    sp+=3;
    *sp++ = 'g';
    *sp++ = ' ';
    *sp++ = ' ';
    *lcp = '*';
  }
  xfree(lcstr);
  return (returnstr);
}

/*
 *  Without getting into all the complexity of technically legal HTML,
 *  this function tries to strip "ugly" parts of it to make it more
 *  pleasant for a human reader.   Yes, this falls down in all kinds of
 *  ways such as spaces within the tags, etc.
 */
char*
strip_html(const utf_string* in)
{
#if 0
  // If we were willing to link core against QtGui (not out of the question)
  // we could just do...and either decide whether to add handling for [IMG]
  // or just say we don't do that any more.
  QTextDocument doc;
  doc.setHtml(in->utfstring);
  return xstrdup(CSTR(doc.toPlainText().simplified()));
#else
  char* out;
  char* instr;
  char tag[8];
  unsigned short int taglen = 0;

  char* incopy = instr = xstrdup(in->utfstring);
  if (!in->is_html) {
    return instr;
  }
  /*
   * We only shorten, so just dupe the input buf for space.
   */
  char* outstring = out = xstrdup(in->utfstring);

  tag[0] = 0;
  while (*instr) {
    if ((*instr == '<') || (*instr == '&')) {
      tag[0] = *instr;
      taglen = 0;
    }

    if (! tag[0]) {
      if (*instr == '\n') {
        *out++ = ' ';
        do {
          instr++;
        } while (isspace(*instr));
        continue;
      } else {
        *out++ = *instr;
      }
    } else {
      if (taglen < (sizeof(tag)-1)) {
        tag[taglen++] = tolower(*instr);
        tag[taglen] = 0;
      }
    }

    if (((tag[0] == '<') && (*instr == '>')) ||
        ((tag[0] == '&') && (*instr == ';'))) {
      if (! strcmp(tag, "&amp;")) {
        *out++ = '&';
      } else if (! strcmp(tag, "&lt;")) {
        *out++ = '<';
      } else if (! strcmp(tag, "&gt;")) {
        *out++ = '>';
      } else if (! strcmp(tag, "&quot;")) {
        *out++ = '"';
      } else if (! strcmp(tag, "&nbsp;")) {
        *out++ = ' ';
      } else if (! strcmp(tag, "&deg;")) {
        *out++ = 'd';
        *out++ = 'e';
        *out++ = 'g';
      } else if ((tag[0]=='<') && (tag[1]=='p')) {
        *out++ = '\n';
      } else if ((tag[0]=='<') && (tag[1]=='b') && (tag[2]=='r')) {
        *out++ = '\n';
      } else if ((tag[0]=='<') && (tag[1]=='/') && (tag[2]=='t') && (tag[3]=='r')) {
        *out++ = '\n';
      } else if ((tag[0]=='<') && (tag[1]=='/') && (tag[2]=='t') && (tag[3]=='d')) {
        *out++ = ' ';
      } else if ((tag[0]=='<') && (tag[1]=='i') && (tag[2]=='m') && (tag[3]=='g')) {
        *out++ = '[';
        *out++ = 'I';
        *out++ = 'M';
        *out++ = 'G';
        *out++ = ']';
      }

      tag[0] = 0;
    }
    instr++;
  }
  *out++ = 0;
  if (incopy) {
    xfree(incopy);
  }
  return (outstring);
#endif
}

typedef struct {
  const char* text;
  const char* entity;
  int  not_html;
} entity_types;

static
entity_types stdentities[] =  {
  { "&",	"&amp;", 0 },
  { "'",	"&apos;", 1 },
  { "<",	"&lt;", 0 },
  { ">",	"&gt;", 0 },
  { "\"",	"&quot;", 0 },
  { "\x01",	" ", 1 }, // illegal xml 1.0 character
  { "\x02",	" ", 1 }, // illegal xml 1.0 character
  { "\x03",	" ", 1 }, // illegal xml 1.0 character
  { "\x04",	" ", 1 }, // illegal xml 1.0 character
  { "\x05",	" ", 1 }, // illegal xml 1.0 character
  { "\x06",	" ", 1 }, // illegal xml 1.0 character
  { "\x07",	" ", 1 }, // illegal xml 1.0 character
  { "\x08",	" ", 1 }, // illegal xml 1.0 character
  // { "\x09",	" ", 1 },  legal xml 1.0 character
  // { "\x0a",	" ", 1 },  legal xml 1.0 character
  { "\x0b",	" ", 1 }, // illegal xml 1.0 character
  { "\x0c",	" ", 1 }, // illegal xml 1.0 character
  // { "\x0d",	" ", 1 },  legal xml 1.0 character
  { "\x0e",	" ", 1 }, // illegal xml 1.0 character
  { "\x0f",	" ", 1 }, // illegal xml 1.0 character
  { "\x10",	" ", 1 }, // illegal xml 1.0 character
  { "\x11",	" ", 1 }, // illegal xml 1.0 character
  { "\x12",	" ", 1 }, // illegal xml 1.0 character
  { "\x13",	" ", 1 }, // illegal xml 1.0 character
  { "\x14",	" ", 1 }, // illegal xml 1.0 character
  { "\x15",	" ", 1 }, // illegal xml 1.0 character
  { "\x16",	" ", 1 }, // illegal xml 1.0 character
  { "\x17",	" ", 1 }, // illegal xml 1.0 character
  { "\x18",	" ", 1 }, // illegal xml 1.0 character
  { "\x19",	" ", 1 }, // illegal xml 1.0 character
  { "\x1a",	" ", 1 }, // illegal xml 1.0 character
  { "\x1b",	" ", 1 }, // illegal xml 1.0 character
  { "\x1c",	" ", 1 }, // illegal xml 1.0 character
  { "\x1d",	" ", 1 }, //illegal xml 1.0 character
  { "\x1e",	" ", 1 }, //illegal xml 1.0 character
  { "\x1f",	" ", 1 }, //illegal xml 1.0 character
  { nullptr,	nullptr, 0 }
};

static
char*
entitize(const char* str, bool is_html)
{
  int ecount;
  int nsecount;
  char* p;
  char* tmp;
  char* xstr;

  int bytes = 0;
  int value = 0;
  entity_types* ep = stdentities;
  int elen = ecount = nsecount = 0;

  /* figure # of entity replacements and additional size. */
  while (ep->text) {
    const char* cp = str;
    while ((cp = strstr(cp, ep->text)) != nullptr) {
      elen += strlen(ep->entity) - strlen(ep->text);
      ecount++;
      cp += strlen(ep->text);
    }
    ep++;
  }

  /* figure the same for other than standard entities (i.e. anything
   * that isn't in the range U+0000 to U+007F */

#if 0
  for (cp = str; *cp; cp++) {
    if (*cp & 0x80) {
      cet_utf8_to_ucs4(cp, &bytes, &value);
      cp += bytes-1;
      elen += sprintf(tmpsub, "&#x%x;", value) - bytes;
      nsecount++;
    }
  }
#endif

  /* enough space for the whole string plus entity replacements, if any */
  tmp = (char*) xcalloc((strlen(str) + elen + 1), 1);
  strcpy(tmp, str);

  /* no entity replacements */
  if (ecount == 0 && nsecount == 0) {
    return (tmp);
  }

  if (ecount != 0) {
    for (ep = stdentities; ep->text; ep++) {
      p = tmp;
      if (is_html && ep->not_html)  {
        continue;
      }
      while ((p = strstr(p, ep->text)) != nullptr) {
        elen = strlen(ep->entity);

        xstr = xstrdup(p + strlen(ep->text));

        strcpy(p, ep->entity);
        strcpy(p + elen, xstr);

        xfree(xstr);

        p += elen;
      }
    }
  }

  if (nsecount != 0) {
    p = tmp;
    while (*p) {
      if (*p & 0x80) {
        cet_utf8_to_ucs4(p, &bytes, &value);
        if (p[bytes]) {
          xstr = xstrdup(p + bytes);
        } else {
          xstr = nullptr;
        }
        sprintf(p, "&#x%x;", value);
        p = p+strlen(p);
        if (xstr) {
          strcpy(p, xstr);
          xfree(xstr);
        }
      } else {
        p++;
      }
    }
  }
  return (tmp);
}

/*
 * Public callers for the above to hide the absence of &apos from HTML
 */

char* xml_entitize(const char* str)
{
  return entitize(str, false);
}

char* html_entitize(const char* str)
{
  return entitize(str, true);
}
char* html_entitize(const QString& str)
{
  return entitize(CSTR(str), true);
}

/*
 * xml_tag utilities
 */

xml_tag* xml_next(xml_tag* root, xml_tag* cur)
{
  if (cur->child) {
    cur = cur->child;
  } else if (cur->sibling) {
    cur = cur->sibling;
  } else {
    cur = cur->parent;
    if (cur == root) {
      cur = nullptr;
    }
    if (cur) {
      cur = cur->sibling;
    }
  }
  return cur;
}

xml_tag* xml_findnext(xml_tag* root, xml_tag* cur, const char* tagname)
{
  xml_tag* result = cur;
  do {
    result = xml_next(root, result);
  } while (result && result->tagname.compare(tagname, Qt::CaseInsensitive));
  return result;
}

xml_tag* xml_findfirst(xml_tag* root, const char* tagname)
{
  return xml_findnext(root, root, tagname);
}

char* xml_attribute(xml_tag* tag, const char* attrname)
{
  char* result = nullptr;
  if (tag->attributes) {
    char** attr = tag->attributes;
    while (attr && *attr) {
      if (0 == case_ignore_strcmp(*attr, attrname)) {
        result = attr[1];
        break;
      }
      attr+=2;
    }
  }
  return result;
}

const QString get_filename(const QString& fname)
{
  return  QFileInfo(fname).fileName();
}

/* bit manipulation functions */

/*
 * setbit: Set bit number [nr] of buffer [buf]
 */
void gb_setbit(void* buf, const uint32_t nr)
{
  unsigned char* bytes = (unsigned char*) buf;
  bytes[nr / 8] |= (1 << (nr % 8));
}

/*
 * setbit: Get state of bit number [nr] of buffer [buf]
 */
char gb_getbit(const void* buf, const uint32_t nr)
{
  const unsigned char* bytes = (const unsigned char*) buf;
  return (bytes[nr / 8] & (1 << (nr % 8)));

}

/*
 * gb_int2ptr: Needed, when sizeof(*void) != sizeof(int) ! compiler warning !
 */
void* gb_int2ptr(const int i)
{
  union {
    void* p;
    int i;
  } x = { nullptr };

  x.i = i;
  return x.p;
}

/*
 * gb_ptr2int: Needed, when sizeof(*void) != sizeof(int) ! compiler warning !
 */
int gb_ptr2int(const void* p)
{
  union {
    const void* p;
    int i;
  } x = { p };

  return x.i;
}

void
list_codecs()
{
  QTextStream info(stderr);
  info.setFieldAlignment(QTextStream::AlignLeft);
  auto mibs = QTextCodec::availableMibs();
  int maxlen = 0;
  for (auto mib : mibs) {
    auto codec = QTextCodec::codecForMib(mib);
    if (codec->name().size() > maxlen) {
      maxlen = codec->name().size();
    }
  }
  info << "Avaialble Codecs:" << endl;
  info << qSetFieldWidth(8) << "MIBenum" << qSetFieldWidth(maxlen+1) << "Name" << qSetFieldWidth(0) << "Aliases" << endl;
  for (auto mib : mibs) {
    auto codec = QTextCodec::codecForMib(mib);
    info << qSetFieldWidth(8) << mib << qSetFieldWidth(maxlen+1) << codec->name() << qSetFieldWidth(0);
    bool first = true;
    const auto aliases = codec->aliases();
    for (const auto& alias : aliases) {
      if (first) {
        first = false;
      } else {
        info << ", ";
      }
      info << alias;
    }
    info << endl;
  }
}

