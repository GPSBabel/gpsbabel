/*
	Support for Columbus/Visiontac V900 csv format
        This format pads fields with NULL up to a fixed per field length.
        Because of that, and because xcsv does not allows a regex as a field delimiter,
        a special c module is required.

	Copyright (C) 2009 Tal Benavidor

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
#ifndef V900_H_INCLUDED_
#define V900_H_INCLUDED_

#include <cstdio>     // for FILE

#include <QDateTime>  // for QDateTime
#include <QList>      // for QList
#include <QString>    // for QString
#include <QVector>    // for QVector

#include "defs.h"     // for ff_cap, ff_cap_read, ff_cap_none, arglist_t, ff_type, ff_type_file
#include "format.h"   // for Format


class V900Format : public Format
{
public:
  using Format::Format;

  QVector<arglist_t>* get_args() override
  {
    return nullptr;
  }

  ff_type get_type() const override
  {
    return ff_type_file;
  }

  QVector<ff_cap> get_cap() const override
  {
    /* Read only format. May only read trackpoints and waypoints. */
    return {ff_cap_read, ff_cap_read, ff_cap_none};
  }

  void rd_init(const QString& fname) override;
  void read() override;
  void rd_deinit() override;

private:
  /* Types */

  /* the start of each record (line) is common to both advanced and basic mode.
     it will be parsed by a single common code. hence, it will be easier and clearer
     to have a common structure for it.
   */
  struct one_line_common_start {
    char index[6];          /* record number */
    char comma1;            /* ',' */
    char tag;               /* tag type. T=trackpoint. TODO: more options??? */
    char comma2;            /* ',' */
    char date[6];           /* YYMMDD. YY=09 is 2009. */
    char comma3;            /* ',' */
    char time[6];           /* HHMMSS */
    char comma4;            /* ',' */
    char latitude_num[9];   /* example: "31.768380" */
    char latitude_NS;       /* 'N' or 'S' */
    char comma5;            /* ',' */
    char longitude_num[10]; /* example: "035.209656" */
    char longitude_EW;      /* 'E' or 'W' */
    char comma6;            /* ',' */
    char height[5];         /* Altitude in meters.
                                 * (not corrected to WGS84 ??) */
    char comma7;            /* ',' */
    char speed[4];          /* speed in km/h. no decimal point. */
    char comma8;            /* ',' */
    char heading[3];        /* heading in degrees */
    char comma9;            /* ',' */
  };

  /* this structure holds one record (line) in advanced logging mode.
     advanced mode lines looks like this ('*' means NULL):
  1717**,T,090204,062634,31.765528N,035.207730E,772**,0***,0**,2D,SPS ,2.1**,1.9**,1.0**,*********
  */
  struct one_line_advanced_mode {
    one_line_common_start common;
    char fixmode[2]; /* "2D" or "3D" */
    char comma10;    /* ',' */
    char valid[4];   /* "SPS " or "DGPS" */
    char comma11;    /* ',' */
    char pdop[5];
    char comma12;    /* ',' */
    char hdop[5];
    char comma13;    /* ',' */
    char vdop[5];
    char comma14;    /* ',' */
    char vox[9];     /* voicetag recorded */
    char cr;         /* '\r' */
    char lf;         /* '\n' */
  };

  /* this structure holds one record (line) in basic logging mode.
     basic mode lines looks like this ('*' means NULL):
  1*****,T,090404,063401,31.765931N,035.206969E,821**,0***,0**,*********
  */
  struct one_line_basic_mode {
    one_line_common_start common;
    char vox[9];    /* voicetag recorded */
    char cr;        /* '\r' */
    char lf;        /* '\n' */
  };

  /* Member Functions */

  [[gnu::format(printf, 1, 2)]] static void v900_log(const char* fmt, ...);
  static QDateTime bintime2utc(int date, int time);

  /* Data Members */

  FILE* fin = nullptr;
};

#endif // V900_H_INCLUDED_
