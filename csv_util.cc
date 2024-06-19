/*
    Utilities for parsing Character Separated Value files (CSV)

    Copyright (C) 2002 Alex Mottram (geo_alexm at cox-internet.com)
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

#include <cmath>               // for fabs
#include <cstdlib>             // for strtod
#include <cstring>             // for strlen, strchr, strncmp, strcmp, memmove, strcpy, strcspn, strncpy

#include <QByteArray>          // for QByteArray
#include <QChar>               // for QChar
#include <QDebug>              // for QDebug
#include <QList>               // for QList
#include <QString>             // for QString, operator+

#include "defs.h"
#include "csv_util.h"
#include "src/core/logging.h"  // for Warning


#define MYNAME "CSV_UTIL"

/*********************************************************************/
/* csv_stringclean() - remove any unwanted characters from string.   */
/*                     returns copy of string.                       */
/*     usage: p = csv_stringclean(stringtoclean, "&,\"")             */
/*            (strip out ampersands, commas, and quotes.             */
/*********************************************************************/
QString
csv_stringclean(const QString& source, const QString& to_nuke)
{
  QString r = source;
  if (!to_nuke.isEmpty()) {
    auto isNukeable = [&to_nuke](const QChar &ch)->bool {
        return to_nuke.contains(ch);
    };
    r.removeIf(isNukeable);
  }
  return r;
}

// csv_stringtrim() - trim whitespace and leading and trailing
//                    enclosures (quotes)
//                    returns a copy of the modified string
//    usage: p = csv_stringtrim(string, "\"", 0)
QString
csv_stringtrim(const QString& string, const QString& enclosure, int strip_max)
{
  if (string.isEmpty()) {
    return string;
  }

  int elen = enclosure.size();

  /* trim off leading and trailing whitespace */
  QString retval = string.trimmed();

  /* if no maximum strippage, assign a reasonable value to max */
  if (strip_max == 0) {
    strip_max = 9999;
  }

  /* if we have enclosures, skip past them in pairs */
  if (elen > 0) {
    int stripped = 0;
    while (
      (stripped < strip_max) &&
      (retval.size() >= (elen * 2)) &&
      (retval.startsWith(enclosure)) &&
      (retval.endsWith(enclosure))) {
      retval = retval.mid(elen, retval.size() - (elen * 2));
      stripped++;
    }
  }

  return retval;
}

// RFC4180 method, but we don't handle line breaks within a field.
// for enclosure = "
// make str = blank into nothing
// make str = foo into "foo"
// make str = foo"bar into "foo""bar"
// No, that doesn't seem obvious to me, either...

QString
csv_enquote(const QString& str, const QString& enclosure)
{
  QString retval = str;
  if (enclosure.size() > 0) {
    retval = enclosure + retval.replace(enclosure, enclosure + enclosure) + enclosure;
  }
  return retval;
}

// RFC4180 method, but we don't handle line breaks within a field,
// and we strip spaces from the ends.
// csv_dequote() - trim whitespace, leading and trailing
//                 enclosures (quotes), and de-escape
//                 internal enclosures.
//    usage: p = csv_dequote(string, "\"")
QString
csv_dequote(const QString& string, const QString& enclosure)
{
  if (string.isEmpty()) {
    return string;
  }

  int elen = enclosure.size();

  /* trim off leading and trailing whitespace */
  QString retval = string.trimmed();

  if (elen > 0) {
    /* If the string is enclosed in enclosures */
    if (retval.startsWith(enclosure) && retval.endsWith(enclosure)) {
      /* strip the enclosures */
      retval = retval.mid(elen, retval.size() - (elen * 2));
      /* replace any contained escaped enclosures */
      retval = retval.replace(enclosure + enclosure, enclosure);
    }
  }

  return retval;
}

/*****************************************************************************/
/* csv_linesplit() - extract data fields from a delimited string. designed   */
/*                   to handle quoted and delimited data within quotes.      */
/*    usage: p = csv_lineparse(string, ",", "\"", line)                      */
/*****************************************************************************/
QStringList
csv_linesplit(const QString& string, const QString& delimited_by,
              const QString& enclosed_in, const int line_no, CsvQuoteMethod method)
{
  QStringList retval;

  const bool hyper_whitespace_delimiter = delimited_by == "\\w";

  /*
   * This is tacky.  Our "csv" format is actually "commaspace" format.
   * Changing that causes unwanted churn, but it also makes "real"
   * comma separated data (such as likely to be produced by Excel, etc.)
   * unreadable.   So we silently change it here on a read and let the
   * whitespace eater consume the space.
   */
  QString delimiter = delimited_by;
  if (delimited_by == ", ") {
    delimiter = ",";
  }

  /* length of delimiters and enclosures */
  int dlen = 0;
  if ((!delimiter.isEmpty()) && (!hyper_whitespace_delimiter)) {
    dlen = delimiter.size();
  }
  int elen = enclosed_in.size();

  int p = 0;
  bool endofline = false;
  while (!endofline) {
    bool efound = false;
    bool dfound = false;
    bool enclosed = false;

    /* the beginning of the string we start with (this pass) */
    const int sp = p;

    while (p < string.size() && !dfound) {
      if ((elen > 0) && string.mid(p).startsWith(enclosed_in)) {
        efound = true;
        p += elen;
        enclosed = !enclosed;
        continue;
      }

      if (!enclosed) {
        if ((dlen > 0) && string.mid(p).startsWith(delimiter)) {
          dfound = true;
        } else if (hyper_whitespace_delimiter && string.at(p).isSpace()) {
          dfound = true;
          while ((p < string.size()) && string.at(p).isSpace()) {
            p++;
          }
        } else {
          p++;
        }
      } else {
        p++;
      }
    }

    QString value = string.mid(sp, p - sp);

    if (efound) {
      if (method == CsvQuoteMethod::rfc4180) {
        value = csv_dequote(value, enclosed_in);
      } else {
        value = csv_stringtrim(value, enclosed_in, 0);
      }
    }

    if (dfound) {
      /* skip over the delimiter */
      p += dlen;
    } else {
      endofline = true;
    }

    if (enclosed) {
      Warning() << MYNAME":" <<
              "Warning- Unbalanced Field Enclosures" <<
              enclosed_in <<
              "on line" <<
              line_no;
    }

    retval.append(value);

  }
  return retval;
}
/*****************************************************************************/
/* dec_to_intdeg() - convert decimal degrees to integer degreees             */
/*    usage: i = dec_to_intdeg(31.1234);                                     */
/*****************************************************************************/
int
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
double
intdeg_to_dec(const int ideg)
{
  double d;

  if (ideg >= 0) {
    d = ((2147483647) - ideg) / 8388608.0;
  } else {
    d = ((-2147483647-1) + ideg) / 8388608.0;
  }

  return(d);
}

/*****************************************************************************/
/* decdir_to_dec() - convert a decimal/direction value into pure decimal.    */
/* usage: lat = decdir_to_dec("W90.1234");                                   */
/*        lat = decdir_to_dec("30.1234N");                                  */
/*****************************************************************************/
double
decdir_to_dec(const char* decdir)
{
  char* p;
  int sign = 0;

  const char* cp = &decdir[0];

  if ((*cp == 'W') || (*cp == 'S')) {
    sign = -1;
  } else if ((*cp == 'N') || (*cp == 'E')) {
    sign = 1;
  }

  double rval = sign ? strtod(&decdir[1], &p) : strtod(&decdir[0], &p);

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
double
ddmmdir_to_degrees(const char* ddmmdir)
{
  // if not N or E, prepend a '-' to ddmm2degrees input
  // see XT_LAT_NMEA which handles ddmm directly
  if (strchr(ddmmdir, 'W') || strchr(ddmmdir, 'S')) {
    return ddmm2degrees(- strtod(ddmmdir, nullptr));
  }
  return ddmm2degrees(strtod(ddmmdir, nullptr));

}

/*****************************************************************************
 * human_to_dec() - convert a "human-readable" lat and/or lon to decimal
 * usage: human_to_dec( "N 41° 09.12′ W 085° 09.36′", &lat, &lon );
 *        human_to_dec( "41 9 5.652 N", &lat, &lon );
 *
 *        which: 0-no preference    1-prefer lat    2-prefer lon
 *****************************************************************************/

void
human_to_dec(const QString& instr, double* outlat, double* outlon, int which)
{
  double unk[3] = {999,999,999};
  double lat[3] = {999,999,999};
  double lon[3] = {999,999,999};
  int    latsign = 0;
  int    lonsign = 0;
  int    unksign = 1;

  double* numres = unk;
  int numind = 0;

  // Allow comma as decimal separator.
  const QByteArray inbytes = instr.toUtf8().replace(',', '.');
  const char* cur = inbytes.constData();

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
      char* end;
      numres[numind] = strtod(cur, &end);
      cur = end;
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
}

/*
 * dec_to_human - convert decimal degrees to human readable
 */

QString
dec_to_human(const char* format, const char* dirs, double val)
{
  int  index = 0;
  int  intvals[3] = {0,0,0};
  double  dblvals[3] = {0,0,0};

  int sign = (val < 0) ? 0 : 1;

  dblvals[0] = fabs(val);
  intvals[0] = (int)dblvals[0];
  dblvals[1] = 60*(dblvals[0]-intvals[0]);
  intvals[1] = (int)dblvals[1];
  dblvals[2] = 60*(dblvals[1]-intvals[1]);
  intvals[2] = (int)dblvals[2];

  char* subformat = (char*) xmalloc(strlen(format)+2);
  const char* formatptr = format;

  QString buff;

  while (formatptr && *formatptr) {
    strcpy(subformat, formatptr);
    char* percent = strchr(subformat, '%');
    if (percent) {
      char* type = percent+1+strcspn(percent+1, "cdiouxXeEfgG%");
      *(type+1) = '\0';
      switch (*type) {
      case 'c':
        buff += QString::asprintf(subformat, dirs[sign]);
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
        buff += QString::asprintf(subformat, intvals[index]);
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
        buff += QString::asprintf(subformat, dblvals[index]);
        index++;
        break;
      case '%':
        buff += subformat;
        break;
      default:
        fatal(MYNAME ": invalid format specifier\n");
        break;

      }
    } else {
      buff += subformat;
    }
    formatptr += strlen(subformat);
  }

  xfree(subformat);
  return buff;
}
