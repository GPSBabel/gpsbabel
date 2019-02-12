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

#ifndef XCSV_H_INCLUDED_
#define XCSV_H_INCLUDED_

#include <utility>             // for move

#include <QtCore/QByteArray>   // for QByteArray
#include <QtCore/QList>        // for QList
#include <QtCore/QString>      // for QString
#include <QtCore/QStringList>  // for QStringList
#include <QtCore/QTextCodec>   // for QTextCodec
#include <QtCore/QTextStream>  // for QTextStream

#include "defs.h"
#include "src/core/file.h"     // for File

/* function prototypes */

void xcsv_setup_internal_style(const char* style_buf);
void xcsv_read_internal_style(const char* style_buf);

/****************************************************************************/
/* types required for various xcsv functions                                */
/****************************************************************************/

/* something to map fields to waypts */
#define OPTIONS_NODELIM 1U
#define OPTIONS_ABSOLUTE 2U
#define OPTIONS_OPTIONAL 4U
struct field_map {
public:
  // We use QByteArrays because consumers want char* data and QByteArrays supply this through constData().
  // If we used QStrings, then we would have to convert to QByteArrays to get the char* data.
  // If we use char* then we have to manage memory allocation/deallocation.
  // TODO: when consumers use QStrings then we can store QStrings instead of QByteArrays.
  QByteArray key;
  QByteArray val;
  QByteArray printfc;
  int hashed_key{0};
  unsigned options{0};

  field_map() = default;
  field_map(QByteArray k, QByteArray v, QByteArray p, int hk) : key{std::move(k)},val{std::move(v)},printfc{std::move(p)},hashed_key{hk} {}
  field_map(QByteArray k, QByteArray v, QByteArray p, int hk, unsigned o) : key{std::move(k)},val{std::move(v)},printfc{
          std::move(p)},hashed_key{hk},options{o} {}
};

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
  QString field_encloser;		/* doublequote, etc... */
  QString record_delimiter;	/* newline, c/r, etc... */

  QString badchars;		/* characters we never write to output */

  QList<field_map> ifields;	/* input field mapping */
  QList<field_map> ofields;	/* output field mapping */

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

#endif  // XCSV_H_INCLUDED_
