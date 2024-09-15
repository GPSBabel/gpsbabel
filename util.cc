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
    Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.

 */

#include <algorithm>                    // for sort
#include <cassert>                      // for assert
#include <cctype>                       // for isspace, tolower
#include <cerrno>                       // for errno
#include <climits>                      // for INT_MAX, INT_MIN
#include <cmath>                        // for fabs, floor
#include <cstdio>                       // for size_t, vsnprintf, FILE, fopen, printf, sprintf, stderr, stdin, stdout
#include <cstdlib>                      // for abs, calloc, free, malloc, realloc
#include <cstring>                      // for strlen, strcat, strstr, memcpy, strcmp, strcpy, strdup, strchr, strerror
#include <utility>                      // for as_const

#include <QByteArray>                   // for QByteArray
#include <QChar>                        // for QChar, operator<=, operator>=
#include <QDate>                        // for QDate
#include <QDateTime>                    // for QDateTime
#include <QFileInfo>                    // for QFileInfo
#include <QList>                        // for QList
#include <QRegularExpression>           // for QRegularExpressio
#include <QString>                      // for QString
#include <QTextBoundaryFinder>          // for QTextBoundaryFinder, QTextBoundaryFinder::Grapheme
#include <QTextCodec>                   // for QTextCodec
#include <QTextStream>                  // for operator<<, QTextStream, qSetFieldWidth, endl, QTextStream::AlignLeft
#include <Qt>                           // for CaseInsensitive
#include <QTime>                        // for QTime
#include <QTimeZone>                    // for QTimeZone
#include <QtGlobal>                     // for qEnvironmentVariableIsSet, QAddConst<>::Type, qPrintable

#include "defs.h"
#include "src/core/datetime.h"          // for DateTime
#include "src/core/logging.h"           // for Warning


#if Q_BYTE_ORDER == Q_BIG_ENDIAN
# define i_am_little_endian 0
#elif Q_BYTE_ORDER == Q_LITTLE_ENDIAN
# define i_am_little_endian 1
#else
# error Unhandled Endianness
#endif

void*
xmalloc(size_t size)
{
  void* obj = malloc(size);

  if (!obj) {
    fatal("gpsbabel: Unable to allocate %zu bytes of memory.\n", size);
  }

  return obj;
}

void*
xcalloc(size_t nmemb, size_t size)
{
  void* obj = calloc(nmemb, size);

  if (!obj) {
    fatal("gpsbabel: Unable to allocate %zu units of %zu bytes of memory.\n", nmemb, size);
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
    fatal("gpsbabel: Unable to allocate %zu bytes of memory.\n", strlen(s));
  }

  return o;
}

char* xstrdup(const QString& s)
{
  return xstrdup(CSTR(s));
}

void*
xrealloc(void* p, size_t s)
{
  char* o = (char*) realloc(p, s);

  if (!o) {
    fatal("gpsbabel: Unable to realloc %zu bytes of memory.\n", s);
  }

  return o;
}

/*
 * Wrapper for open that honours - for stdin, stdout, unifies error text.
 */
FILE*
xfopen(const char* fname, const char* type, const char* errtxt)
{
  bool am_writing = strchr(type, 'w') != nullptr;

  if (fname == nullptr) {
    fatal("%s must have a filename specified for %s.\n",
          errtxt, am_writing ? "write" : "read");
  }

  if (0 == strcmp(fname, "-")) {
    return am_writing ? stdout : stdin;
  }
  FILE* f = ufopen(QString::fromUtf8(fname), type);
  if (nullptr == f) {
    // There are some possible vagaries of using Qt for the full pathname
    // vs. the STD C library used for the actual file I/O. It's worth it
    // to get a better error message.
    QFileInfo info(fname);
    fatal("%s cannot open '%s' for %s.  Error was '%s'.\n",
          errtxt, qPrintable(info.absoluteFilePath()),
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

void
printposn(const double c, bool is_lat)
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

/*
 * Read 4 bytes in big-endian.   Return as "int" in native endianness.
 */
signed int
be_read32(const void* ptr)
{
  const auto* i = (const unsigned char*) ptr;
  return i[0] << 24 | i[1] << 16  | i[2] << 8 | i[3];
}

signed int
be_read16(const void* ptr)
{
  const auto* i = (const unsigned char*) ptr;
  return i[0] << 8 | i[1];
}

unsigned int
be_readu16(const void* ptr)
{
  const auto* i = (const unsigned char*) ptr;
  return i[0] << 8 | i[1];
}

void
be_write16(void* ptr, const unsigned value)
{
  auto* p = (unsigned char*) ptr;
  p[0] = value >> 8;
  p[1] = value;
}

void
be_write32(void* ptr, const unsigned value)
{
  auto* p = (unsigned char*) ptr;

  p[0] = value >> 24;
  p[1] = value >> 16;
  p[2] = value >> 8;
  p[3] = value;
}

signed int
le_read16(const void* ptr)
{
  const auto* p = (const unsigned char*) ptr;
  return p[0] | (p[1] << 8);
}

unsigned int
le_readu16(const void* ptr)
{
  const auto* p = (const unsigned char*) ptr;
  return p[0] | (p[1] << 8);
}

signed int
le_read32(const void* ptr)
{
  const auto* p = (const unsigned char*) ptr;
  return p[0] | (p[1] << 8) | (p[2] << 16) | (p[3] << 24);
}

unsigned int
le_readu32(const void* ptr)
{
  const auto* p = (const unsigned char*) ptr;
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

  if constexpr(i_am_little_endian) {
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
  auto* p = (unsigned char*) ptr;
  p[0] = value;
  p[1] = value >> 8;
}

void
le_write32(void* ptr, const unsigned value)
{
  auto* p = (unsigned char*) ptr;
  p[0] = value;
  p[1] = value >> 8;
  p[2] = value >> 16;
  p[3] = value >> 24;
}

QDateTime
make_datetime(QDate date, QTime time, bool is_localtime, bool force_utc, int utc_offset)
{
  QDateTime result;
  Qt::TimeSpec timespec;
  int offset = 0;

  if (is_localtime) {
    if (force_utc) { // override with passed option value
      if (utc_offset == 0) {
        // Qt 6.5.0 QDate::startOfDay(Qt::OffsetFromUTC, 0) returns an invalid QDateTime.
        timespec = Qt::UTC;
      } else {
        timespec = Qt::OffsetFromUTC;
        // Avoid Qt 6.5.0 warnings with non-zero offsets when not using Qt::OffsetFromUTC.
        offset = utc_offset;
      }
    } else {
      timespec = Qt::LocalTime;
    }
  } else {
    timespec = Qt::UTC;
  }

  if (date.isValid() && time.isValid()) {
    result = QDateTime(date, time, timespec, offset);
  } else if (time.isValid()) {
    // TODO: Wouldn't it be better to return an invalid QDateTime
    // that contained an invalid QDate, a valid QTime and a valid
    // Qt::TimeSpec?
    result = QDateTime(QDate(1970, 1, 1), time, timespec, offset);
  } else if (date.isValid()) {
    //  no time, use start of day in the given Qt::TimeSpec.
    result = date.startOfDay(timespec, offset);
  }

  return result;
}

bool
gpsbabel_testmode()
{
  static bool testmode = qEnvironmentVariableIsSet("GPSBABEL_FREEZE_TIME");
  return testmode;
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
  if (gpsbabel_testmode()) {
    return QDateTime::fromMSecsSinceEpoch(0, Qt::UTC);
  }

  return QDateTime::currentDateTimeUtc();
}

/*
 * Microsoft dot net's time format is the number of 100 nanosecond intervals
 * since midnight Jan 1, 0001.   We have time_t deeply ingrained into our
 * internals and since we're in the GPS biz, timestamps before 1/1/1970 aren't
 * that interesting to us anyway.
 */
QDateTime dotnet_time_to_qdatetime(long long dotnet)
{
  QDateTime epoch = QDateTime(QDate(1, 1, 1), QTime(0, 0, 0), Qt::UTC);
  qint64 millisecs = (dotnet + 5000)/ 10000;
  return epoch.addMSecs(millisecs);
}

long long qdatetime_to_dotnet_time(const QDateTime& dt)
{
  QDateTime epoch = QDateTime(QDate(1, 1, 1), QTime(0, 0, 0), Qt::UTC);
  qint64 millisecs = epoch.msecsTo(dt);
  return millisecs * 10000;
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
      r[i] = static_cast<const char*>(ptr)[7-i];
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
      r[i] = static_cast<const char*>(ptr)[3-i];
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
  auto deg = (signed int)(pcx_val / 100.0);
  double minutes = (((pcx_val / 100.0) - deg) * 100.0) / 60.0;
  return (double) deg + minutes;
}

double degrees2ddmm(double deg_val)
{
  auto deg = (signed int) deg_val;
  return (deg * 100.0) + ((deg_val - deg) * 60.0);
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

QString
convert_human_date_format(const QString& human_datef)
{
  QString result;
  QChar prev = '\0';
  int ylen = 0;

  for (const QChar cin : human_datef) {
    bool okay = true;

    if (cin.toUpper() != 'Y') {
      ylen = 0;
    }
    if (cin.isLetter()) {
      switch (cin.unicode()) {
      case 'y':
      case 'Y':
        if (prev != 'Y') {
          result.append("%y");
          prev = 'Y';
        }
        ylen++;
        if (ylen > 2) {
          result.back() = 'Y';
        }
        break;
      case 'm':
      case 'M':
        if (prev != 'M') {
          result.append("%m");
          prev = 'M';
        }
        break;
      case 'd':
      case 'D':
        if (prev != 'D') {
          result.append("%d");
          prev = 'D';
        }
        break;
      default:
        okay = false;
      }
    } else if (cin.isPunct()) {
      result.append(cin);
      prev = '\0';
    } else {
      okay = false;
    }

    if (!okay) {
      fatal(FatalMsg().nospace() << "Invalid character " << cin << " in date format " << human_datef << "!");
    }
  }
  return result;
}

/*
 * Convert a human readable time format (i.e. "HH:mm:ss") into
 * a format usable for strftime and others
 */

QString
convert_human_time_format(const QString& human_timef)
{
  QString result;
  QChar prev = '\0';

  for (const QChar cin : human_timef) {
    bool okay = true;

    if (cin.isLetter()) {
      switch (cin.unicode()) {
      case 'S':
      case 's':
        if (prev != 'S') {
          result.append("%S");
          prev = 'S';
        }
        break;

      case 'M':
      case 'm':
        if (prev != 'M') {
          result.append("%M");
          prev = 'M';
        }
        break;

      case 'h':				/* 12-hour-clock */
        if (prev != 'H') {
          result.append("%l");	/* 1 .. 12 */
          prev = 'H';
        } else {
          result.back() = 'I';  /* 01 .. 12 */
        }
        break;

      case 'H':				/* 24-hour-clock */
        if (prev != 'H') {
          result.append("%k");
          prev = 'H';
        } else {
          result.back() = 'H';
        }
        break;

      case 'x':
        if (prev != 'X') {
          result.append("%P");
          prev = 'X';
        } else {
          result.back() = 'P';
        }
        break;

      case 'X':
        if (prev != 'X') {
          result.append("%p");
          prev = 'X';
        } else {
          result.back() = 'p';
        }
        break;

      default:
        okay = false;
      }
    } else if (cin.isPunct() || cin.isSpace()) {
      result.append(cin);
      prev = '\0';
    } else {
      okay = false;
    }

    if (!okay) {
      fatal(FatalMsg().nospace() << "Invalid character " << cin << " in time format " << human_timef << "!");
    }
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
QString
pretty_deg_format(double lat, double lon, char fmt, const char* sep, bool html)
{
  QString	result;
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
    result = QStringLiteral("%1%2%3%4%5%6%7")
             .arg(latsig).arg(fabs(lat), 6, 'f', 5).arg(html ? "&deg;" : "", sep)
             .arg(lonsig).arg(fabs(lon), 6, 'f', 5).arg(html ? "&deg;" : "");
  } else if (fmt == 's') { /* dms */
    result = QStringLiteral("%1%2%3%4'%5\"%6%7%8%9%10'%11\"")
             .arg(latsig).arg(latint).arg(html ? "&deg;" : " ").arg((int)latmin, 2, 10, QChar('0')).arg(latsec, 4, 'f', 1, QChar('0')).arg(sep)
             .arg(lonsig).arg(lonint).arg(html ? "&deg;" : " ").arg((int)lonmin, 2, 10, QChar('0')).arg(lonsec, 4, 'f', 1, QChar('0'));
  } else { /* default dmm */
    result = QStringLiteral("%1%2%3%4%5%6%7%8%9")
             .arg(latsig).arg(latint).arg(html ? "&deg;" : " ").arg(latmin, 6, 'f', 3, QChar('0')).arg(sep)
             .arg(lonsig).arg(lonint).arg(html ? "&deg;" : " ").arg(lonmin, 6, 'f', 3, QChar('0'));
  }
  return result;
}



/*
 * Get rid of potentially nasty HTML that would influence another record
 * that includes;
 * <body> - to stop backgrounds/background colors from being loaded
 * </body> and </html>- stop processing altogether
 * <style> </style> - stop overriding styles for everything
 */
QString
strip_nastyhtml(const QString& in)
{
  static const QRegularExpression htmlre("<html.*?>", QRegularExpression::CaseInsensitiveOption | QRegularExpression::DotMatchesEverythingOption);
  assert(htmlre.isValid());
  static const QRegularExpression bodyre("<body.*?>", QRegularExpression::CaseInsensitiveOption | QRegularExpression::DotMatchesEverythingOption);
  assert(bodyre.isValid());
  static const QRegularExpression stylere("<style.*?>.*?</style>", QRegularExpression::CaseInsensitiveOption | QRegularExpression::DotMatchesEverythingOption);
  assert(stylere.isValid());
  QString out(in);
  
  out.replace(bodyre, "");
  out.replace("</body>", "", Qt::CaseInsensitive);
  out.replace(htmlre, "");
  out.replace("</html>", "", Qt::CaseInsensitive);
  out.replace(stylere, "");
  out.replace("<image", "<img", Qt::CaseInsensitive);

  return out;
}

/*
 *  Without getting into all the complexity of technically legal HTML,
 *  this function tries to strip "ugly" parts of it to make it more
 *  pleasant for a human reader.   Yes, this falls down in all kinds of
 *  ways such as spaces within the tags, etc.
 */
QString strip_html(const QString& utfstring)
{
#if 0
  // If we were willing to link core against QtGui (not out of the question)
  // we could just do...and either decide whether to add handling for [IMG]
  // or just say we don't do that any more.
  QTextDocument doc;
  doc.setHtml(utfstring);
  return doc.toPlainText().simplified();
#else
  static const QRegularExpression pre("<p.*?>", QRegularExpression::CaseInsensitiveOption | QRegularExpression::DotMatchesEverythingOption);
  assert(pre.isValid());
  static const QRegularExpression brre("<br.*?>", QRegularExpression::CaseInsensitiveOption | QRegularExpression::DotMatchesEverythingOption);
  assert(brre.isValid());
  static const QRegularExpression trre("<tr.*?>", QRegularExpression::CaseInsensitiveOption | QRegularExpression::DotMatchesEverythingOption);
  assert(trre.isValid());
  static const QRegularExpression tdre("<td.*?>", QRegularExpression::CaseInsensitiveOption | QRegularExpression::DotMatchesEverythingOption);
  assert(tdre.isValid());

  QString out(utfstring);

  // Tag replacement first
  out.replace(pre, "\n");
  out.replace(brre, "\n");
  out.replace(trre, "\n");
  out.replace(tdre, " ");
  out.replace("<img", "[IMG]", Qt::CaseInsensitive);

  // Then entity replacement (entities are case sensitive)
  out.replace("&amp;","&");
  out.replace("&lt;", "<");
  out.replace("&gt;", ">");
  out.replace("&quot;", "\"");
  out.replace("&nbsp", " ");
  out.replace("&deg;", "deg");

  return out;
#endif
}

QString get_filename(const QString& fname)
{
  return QFileInfo(fname).fileName();
}

QTextCodec* get_codec(const QByteArray& cs_name)
{
  QTextCodec* codec = QTextCodec::codecForName(cs_name);
  if (codec == nullptr) {
    fatal(FatalMsg().nospace() << "Unsupported character set " << cs_name << ".");
  }
  return codec;
}

void
list_codecs()
{
  QTextStream info(stderr);
  info.setFieldAlignment(QTextStream::AlignLeft);
  const auto mibs = QTextCodec::availableMibs();
  int maxlen = 0;
  for (auto mib : mibs) {
    auto* codec = QTextCodec::codecForMib(mib);
    if (codec->name().size() > maxlen) {
      maxlen = codec->name().size();
    }
  }
  info << "Available Codecs:" << Qt::endl;
  info << qSetFieldWidth(8) << "MIBenum" << qSetFieldWidth(maxlen+1) << "Name" << qSetFieldWidth(0) << "Aliases" << Qt::endl;
  for (auto mib : mibs) {
    auto* codec = QTextCodec::codecForMib(mib);
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
    info << Qt::endl;
  }
}

void list_timezones()
{
  QList<QByteArray> zoneids = QTimeZone::availableTimeZoneIds();
  auto alpha = [](const QByteArray& a, const QByteArray& b)->bool {
    return QString::compare(a, b, Qt::CaseInsensitive) < 0;
  };
  std::sort(zoneids.begin(), zoneids.end(), alpha);
  Warning() << "Available timezones are:";
  for (const auto& id : std::as_const(zoneids)) {
    Warning() << id;
  }
}

QString grapheme_truncate(const QString& input, unsigned int count)
{
  QString output(input);
  QTextBoundaryFinder boundary(QTextBoundaryFinder::Grapheme, input);
  boundary.toStart();
  unsigned int grapheme_cnt = 0;
  QList<int> boundaries{0};
  while (boundary.toNextBoundary() >= 0) {
    ++grapheme_cnt;
    boundaries.append(boundary.position());
  }
  if (grapheme_cnt > count) {
    output.truncate(boundaries.at(count));
  }
  if constexpr(false) {
    qDebug() << input << "->" << output <<  boundaries << ", limit:" <<
             count << ", input QChars:" << input.size() << ",input graphemes:" << grapheme_cnt <<
             ", output QChars:" << output.size();
  }
  return output;
}

int xstrtoi(const char* str, char** str_end, int base)
{

  long value = strtol(str, str_end, base);
  if (value > INT_MAX) {
    errno = ERANGE;
    return INT_MAX;
  }
  if (value < INT_MIN) {
    errno = ERANGE;
    return INT_MIN;
  }
  return value;
}
