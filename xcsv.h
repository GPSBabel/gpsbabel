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
    Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.

 */

#ifndef XCSV_H_INCLUDED_
#define XCSV_H_INCLUDED_

#include <utility>                // for move

#include <QtCore/QByteArray>      // for QByteArray
#include <QtCore/QList>           // for QList
#include <QtCore/QString>         // for QString
#include <QtCore/QStringList>     // for QStringList

#include "defs.h"
#include "src/core/optional.h"    // for optional
#include "src/core/textstream.h"  // for TextStream

#if CSVFMTS_ENABLED

/****************************************************************************/
/* types required for various xcsv functions                                */
/****************************************************************************/

class XcsvFile {
public:
  XcsvFile() : mkshort_handle(mkshort_new_handle()) {}
  // delete copy and move constructors and assignment operators.
  // The defaults are not appropriate, and we haven't implemented proper ones.
  XcsvFile(const XcsvFile&) = delete;
  XcsvFile& operator=(const XcsvFile&) = delete;
  XcsvFile(XcsvFile&&) = delete;
  XcsvFile& operator=(XcsvFile&&) = delete;
  ~XcsvFile() {
    if (mkshort_handle != nullptr) {
      mkshort_del_handle(&mkshort_handle);
    }
  }

  gpsbabel::TextStream stream;
  QString fname;
  int gps_datum_idx{-1};		/* result of GPS_Lookup_Datum_Index */
  short_handle mkshort_handle{nullptr};
};

/* something to map fields to waypts */
constexpr unsigned options_nodelim = 1;
constexpr unsigned options_absolute = 2;
constexpr unsigned options_optional = 4;

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
 * Class describing an xcsv format.
 */
struct XcsvStyle {
  /* PROLOGUE from style file */
  /* header lines for writing at the top of the file. */
  QStringList prologue;

  /* EPILOGUE from style file */
  /* footer lines for writing at the bottom of the file. */
  QStringList epilogue;

  /* FIELD_DELIMITER from style file */
  /* comma, quote, etc... */
  QString field_delimiter;

  /* FIELD_ENCLOSER from style file */
  /* doublequote, etc... */
  QString field_encloser;

  /* RECORD_DELIMITER from style file */
  /* newline, c/r, etc... */
  QString record_delimiter;

  /* BADCHARS from style file */
  /* characters we never write to output */
  QString badchars;

  /* IFIELDS from style file */
  /* input field mapping */
  QList<field_map> ifields;

  /* OFIELDS from style file */
  /* output field mapping */
  QList<field_map> ofields;

  /* ENCODING from style file */
  QString codecname;

  /* DESCRIPTION from style file */
  /* for help text */
  QString description;

  /* EXTENSION from style file */
  /* preferred filename extension (for wrappers)*/
  QString extension;

  /* FORMAT_TYPE from style file */
  /* format type for GUI wrappers. */
  ff_type type{ff_type_file};

  /* DATUM from style file */
  QString gps_datum_name;

  /* DATATYPE from style file */
  /* can be wptdata, rtedata or trkdata */
  /* ... or ZERO to keep the old behaviour */
  gpsdata_type datatype{unknown_gpsdata};

  /* SHORTLEN from style file */
  gpsbabel_optional::optional<int> shortlen;

  /* SHORTWHITE from style file */
  gpsbabel_optional::optional<int> whitespace_ok;
};

/* public function prototypes */

void xcsv_setup_internal_style(const char* style_buf);
XcsvStyle xcsv_read_internal_style(const char* style_buf);

#endif // CSVFMTS_ENABLED
#endif // XCSV_H_INCLUDED_
