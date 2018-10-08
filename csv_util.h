/*
    Copyright (C) 2002 Alex Mottram (geo_alexm at cox-internet.com)
    Copyright (C) 2002-2014 Robert Lipe

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

#include "queue.h"
#include "defs.h"
#include <QtCore/QStringList>
class QTextStream;

/* function prototypes */

char*
#ifndef DEBUG_MEM
csv_stringtrim(const char* string, const char* enclosure, int strip_max);
#else
CSV_STRINGTRIM(const char* string, const char* enclosure, int strip_max, DEBUG_PARAMS);
#define csv_stringtrim( s, e,m ) CSV_STRINGTRIM( s, e, m, __FILE__, __LINE__)
#endif
QString csv_stringtrim(const QString& source, const QString& enclosure);

char*
csv_lineparse(const char* stringstart, const char* delimited_by, const char* enclosed_in, const int line_no);

void
human_to_dec(const char* instr, double* outlat, double* outlon, int which);
inline void
human_to_dec(const QString& instr, double* outlat, double* outlon, int which) {
  human_to_dec(CSTR(instr), outlat, outlon, which);
}

char*
#ifndef DEBUG_MEM
csv_stringclean(const char* string, const char* chararray);
#else
CSV_STRINGCLEAN(const char* string, const char* chararray,DEBUG_PARAMS);
#define csv_stringclean(s,c) CSV_STRINGCLEAN(s,c,__FILE__,__LINE__)
#endif
QString csv_stringclean(const QString& string, const QString& chararray);

void
xcsv_data_read(void);

void
xcsv_data_write(void);

void
xcsv_file_init(void);

void
xcsv_prologue_add(QString);

void
xcsv_epilogue_add(QString);

void
xcsv_ifield_add(const char*, const char*, const char*);

void
xcsv_ofield_add(const char*, const char*, const char*, int options);

void
xcsv_destroy_style(void);

QString
xcsv_get_char_from_constant_table(QString key);

/****************************************************************************/
/* types required for various xcsv functions                                */
/****************************************************************************/

/* something to map fields to waypts */
#define OPTIONS_NODELIM 1
#define OPTIONS_ABSOLUTE 2
#define OPTIONS_OPTIONAL 3
typedef struct field_map {
  queue Q;
  const char* key;
  const char* val;
  const char* printfc;
  int hashed_key;
  int options;
} field_map_t;

/* something to map config file constants to chars */
typedef struct char_map {
  const QString key;
  const QString chars;
} char_map_t;

namespace gpsbabel
{
    class File;
}
/*
 * a Class describing all the wonderful elements of xcsv files, in a
 * nutshell.
 * It completely shows that this began life as a C struct...baby steps.
 */
class XcsvFile {
 public:
  XcsvFile();

  bool is_internal;		/* bool - is internal (1) or parsed (0) */

  /* header lines for writing at the top of the file. */
  QStringList prologue;

  /* footer lines for writing at the bottom of the file. */
  QStringList epilogue;

  QString field_delimiter; 	/* comma, quote, etc... */
  QString field_encloser; 	/* doublequote, etc... */
  QString record_delimiter;	/* newline, c/r, etc... */

  QString badchars;		/* characters we never write to output */

  queue ifield;		/* input field mapping */
  queue* ofield;    		/* output field mapping */
  QList<field_map> ifields;
  QList<field_map> ofields;

  int ifield_ct;		/* actual # of ifields */
  int ofield_ct;		/* actual # of ofields */

  gpsbabel::File* file;
  QTextStream* stream;
  QTextCodec* codec;
  QString fname;                 /* ptr to filename of above. */

  QString description;		/* Description for help text */
  QString extension;		/* preferred filename extension (for wrappers)*/

  short_handle mkshort_handle;/* handle for mkshort() */
  ff_type type;		/* format type for GUI wrappers. */

  int gps_datum;		/* result of GPS_Lookup_Datum_Index */
  gpsdata_type datatype;	/* can be wptdata, rtedata or trkdata */
  /* ... or ZERO to keep the old behaviour */

};


/****************************************************************************/
/* obligatory global struct                                                 */
/****************************************************************************/
extern XcsvFile xcsv_file;
