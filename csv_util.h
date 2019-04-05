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

#ifndef CSV_UTIL_H_INCLUDED_
#define CSV_UTIL_H_INCLUDED_

#include <QtCore/QString>      // for QString

#include "defs.h"

/* function prototypes */

QString
csv_stringclean(const QString& source, const QString& to_nuke);

char*
csv_stringtrim(const char* string, const char* enclosure, int strip_max);
QString
csv_stringtrim(const QString& source, const QString& enclosure);
QString
csv_stringtrim(const QString& string, const QString& enclosure, int strip_max);
QString
csv_enquote(const QString& str, const QString& enclosure);
QString
csv_dequote(const QString& string, const QString& enclosure);

enum class CsvQuoteMethod {historic, rfc4180};

char*
csv_lineparse(const char* stringstart, const char* delimited_by, const char* enclosed_in, int line_no);
QStringList
csv_linesplit(const QString& string, const QString& delimited_by,
              const QString& enclosed_in, const int line_no, CsvQuoteMethod method = CsvQuoteMethod::historic);

int
dec_to_intdeg(const double d);

double
intdeg_to_dec(const int ideg);

double
decdir_to_dec(const char* decdir);

double
ddmmdir_to_degrees(const char* ddmmdir);

void
human_to_dec(const char* instr, double* outlat, double* outlon, int which);
inline void
human_to_dec(const QString& instr, double* outlat, double* outlon, int which) {
  human_to_dec(CSTR(instr), outlat, outlon, which);
}

QString
dec_to_human(const char* format, const char* dirs, double val);

#endif // CSV_UTIL_H_INCLUDED_
