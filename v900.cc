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

	TODO:
		- QUESTION: course = heading ??
                - HEIGHT: Altitude in meters (not corrected to WGS84...) ??
 */

/******************************************************************************
 FILE FORMAT INFO
=================

File has csv extension, and is somewhat csv like creature...
All lines end with \r\n
First line is a header line. It contains no nulls.
Following lines are record lines. They are comma separated, but fields always
have the exact same length (per field), and therefore, the commas are always
at the exact same position on the line. Fields are padded with nulls, in case
they have shorter value then the fixed field length.
Two modes are available: basic and advanced.

The following two examples show "*" where null appears.

------basic mode - start-------------------------
INDEX,TAG,DATE,TIME,LATITUDE N/S,LONGITUDE E/W,HEIGHT,SPEED,HEADING,VOX
1*****,T,090404,063401,31.765931N,035.206969E,821**,0***,0**,*********
2*****,T,090404,063402,31.765931N,035.206969E,821**,0***,0**,*********
3*****,T,090404,063403,31.765933N,035.206971E,821**,0***,0**,*********
4*****,T,090404,063404,31.765933N,035.206971E,822**,0***,0**,*********
5*****,T,090404,063407,31.765934N,035.206971E,824**,0***,0**,*********
------basic mode - end---------------------------


------advanced mode - start-------------------------
INDEX,TAG,DATE,TIME,LATITUDE N/S,LONGITUDE E/W,HEIGHT,SPEED,HEADING,FIX MODE,VALID,PDOP,HDOP,VDOP,VOX
1*****,T,090204,055722,31.768380N,035.209656E,149**,0***,0**,3D,SPS ,2.6**,2.4**,1.0**,*********
2*****,T,090204,055723,31.768380N,035.209656E,149**,0***,0**,3D,SPS ,2.5**,2.3**,0.9**,*********
3*****,T,090204,055724,31.768378N,035.209658E,149**,0***,0**,3D,SPS ,2.5**,2.3**,0.9**,*********
4*****,T,090204,055725,31.768378N,035.209658E,149**,0***,0**,3D,SPS ,2.5**,2.3**,0.9**,*********
5*****,T,090204,055728,31.768376N,035.209660E,150**,0***,0**,3D,SPS ,2.5**,2.3**,0.9**,*********
6*****,T,090204,055729,31.768376N,035.209660E,150**,0***,0**,3D,SPS ,4.0**,2.8**,2.9**,*********
7*****,T,090204,055730,31.768376N,035.209661E,150**,0***,0**,3D,SPS ,2.5**,2.3**,0.9**,*********
8*****,T,090204,055731,31.768376N,035.209661E,150**,0***,0**,3D,SPS ,2.5**,2.3**,0.9**,*********
9*****,T,090204,055737,31.768326N,035.209993E,150**,0***,0**,3D,SPS ,2.5**,2.3**,0.9**,*********
10****,T,090204,055738,31.768339N,035.209976E,153**,0***,0**,3D,SPS ,2.5**,2.3**,0.9**,*********
11****,T,090204,055739,31.768338N,035.209991E,155**,0***,0**,3D,SPS ,2.5**,2.3**,0.9**,*********
42****,C,090724,162320,31.763841N,035.205461E,788**,9***,344,3D,SPS ,1.2**,0.9**,0.8**,*********
121***,V,090724,162502,31.769619N,035.208964E,786**,16**,306,3D,SPS ,1.1**,0.8**,0.8**,VOX00003*
------advanced mode - end---------------------------

for a little more info, see structures:
	one_line_advanced_mode, one_line_basic_mode, one_line_common_start.
******************************************************************************/

#include "v900.h"

#include <cassert>     // for assert
#include <cstdarg>     // for va_end, va_start
#include <cstdio>      // for fclose, fgets, fread, va_list
#include <cstdlib>     // for strtod
#include <cstring>     // for strncmp, strcat, strcpy, strstr

#include <QByteArray>  // for QByteArray
#include <QDate>       // for QDate
#include <QTime>       // for QTime
#include <QtCore>      // for qPrintable, UTC

#include "defs.h"


/* copied from dg-100.cpp */
void
V900Format::v900_log(const char* fmt, ...)
{
  if (global_opts.debug_level >= 1) {
    va_list ap;
    va_start(ap, fmt);
    db.vlog(fmt, ap);
    va_end(ap);
  }
}

void
V900Format::rd_init(const QString& fname)
{
  v900_log("%s(%s)\n",__func__,qPrintable(fname));
  /* note: file is opened in binary mode, since lines end with \r\n, and in windows text mode
     that will be translated to a single \n, making the line len one character shorter than
     on linux machines.
   */
  fin = ufopen(fname, "rb");
  if (!fin) {
    fatal("v900: could not open '%s'.\n", qPrintable(fname));
  }
}

void
V900Format::rd_deinit()
{
  v900_log("%s\n",__func__);
  if (fin) {
    fclose(fin);
  }
}

/* copied from dg-100.c - slight (incompatible) modification to how the date parameter is used */
QDateTime
V900Format::bintime2utc(int date, int time) {
  int secs = time % 100;
  time /= 100;
  int mins = time % 100;
  time /= 100;
  // What's left in 'time' is hours, ranged 0-23.
  QTime tm(time, mins, secs);

  // 'date' starts at 2000 and is YYMMDD
  int day = date % 100;
  date /= 100;
  int month = date % 100;
  date /= 100;
  // What's left in 'date' is year.
  QDate dt(date + 2000, month, day);

  return QDateTime(dt, tm, QtUTC);
}

void
V900Format::read()
{
  /* use line buffer large enough to hold either basic or advanced mode lines. */
  union {
    one_line_basic_mode    bas;
    one_line_advanced_mode adv;
    char text[200]; /* used to read the header line, which is normal text */
  } line;
  int lc = 0;

  v900_log("%s\n",__func__);

  /*
  Basic mode:    INDEX,TAG,DATE,TIME,LATITUDE N/S,LONGITUDE E/W,HEIGHT,SPEED,HEADING,VOX
  Advanced mode: INDEX,TAG,DATE,TIME,LATITUDE N/S,LONGITUDE E/W,HEIGHT,SPEED,HEADING,FIX MODE,VALID,PDOP,HDOP,VDOP,VOX
  */
  /* first, determine if this is advanced mode by reading the first line.
           since the first line does not contain any nulls, it can be safely read by fgets(). */
  if (!fgets(line.text, sizeof(line), fin)) {
    fatal("v900: error reading header (first) line from input file\n");
  }
  int is_advanced_mode = (nullptr != strstr(line.text,"PDOP")); /* PDOP field appears only in advanced mode */

  v900_log("header line: %s",line.text);
  v900_log("is_advance_mode=%d\n",is_advanced_mode);

  auto* track = new route_head;
  track->rte_name = "V900 tracklog";
  track->rte_desc = "V900 GPS tracklog data";
  track_add_head(track);

  while (true) {
    int bad = 0;
    int record_len = is_advanced_mode ? sizeof(line.adv) : sizeof(line.bas);
    if (fread(&line, record_len, 1, fin) != 1) {
      break;
    }
    lc++;

    /* change all "," characters to NULLs.
                   so every field is null terminated.
                 */
    bad |= (line.bas.common.comma1 != ',');
    bad |= (line.bas.common.comma2 != ',');
    bad |= (line.bas.common.comma3 != ',');
    bad |= (line.bas.common.comma4 != ',');
    bad |= (line.bas.common.comma5 != ',');
    bad |= (line.bas.common.comma6 != ',');
    bad |= (line.bas.common.comma7 != ',');
    bad |= (line.bas.common.comma8 != ',');
    bad |= (line.bas.common.comma9 != ',');

    if (bad) {
      warning("v900: skipping malformed record at line %d\n", lc);
    }

    line.bas.common.comma1 = 0;
    line.bas.common.comma2 = 0;
    line.bas.common.comma3 = 0;
    line.bas.common.comma4 = 0;
    line.bas.common.comma5 = 0;
    line.bas.common.comma6 = 0;
    line.bas.common.comma7 = 0;
    line.bas.common.comma8 = 0;
    line.bas.common.comma9 = 0;
    if (is_advanced_mode) {
      /* change all "," characters to NULLs.
               so every field is null terminated.
             */
      assert(line.adv.comma10==','); // TODO: abort with fatal()
      assert(line.adv.comma11==',');
      assert(line.adv.comma12==',');
      assert(line.adv.comma13==',');
      assert(line.adv.comma14==',');
      assert(line.adv.cr=='\r');
      assert(line.adv.lf=='\n');
      line.adv.comma10 = 0;
      line.adv.comma11 = 0;
      line.adv.comma12 = 0;
      line.adv.comma13 = 0;
      line.adv.comma14 = 0;
      line.adv.cr = 0;	/* null terminate vox field */

    } else {
      assert(line.bas.cr=='\r');
      assert(line.bas.lf=='\n');
      line.bas.cr = 0;	/* null terminate vox field */
    }

    auto* wpt = new Waypoint;

    /* lat is a string in the form: 31.768380N */
    char c = line.bas.common.latitude_NS;	/* N/S */
    assert(c == 'N' || c == 'S');
    wpt->latitude = strtod(line.bas.common.latitude_num, nullptr);
    if (c == 'S') {
      wpt->latitude = -wpt->latitude;
    }

    /* lon is a string in the form: 035.209656E */
    c = line.bas.common.longitude_EW; /* get E/W */
    assert(c == 'E' || c == 'W');
    line.bas.common.longitude_EW = 0; /* the E will confuse strtod(), if not removed */
    wpt->longitude = strtod(line.bas.common.longitude_num, nullptr);
    if (c == 'W') {
      wpt->longitude = -wpt->longitude;
    }

    wpt->altitude = xstrtoi(line.bas.common.height, nullptr, 10);

    /* handle date/time fields */
    {
      int date = xstrtoi(line.bas.common.date, nullptr, 10);
      int time = xstrtoi(line.bas.common.time, nullptr, 10);
      wpt->SetCreationTime(bintime2utc(date, time));
    }

    wpt->set_speed(KPH_TO_MPS(xstrtoi(line.bas.common.speed, nullptr, 10)));

    wpt->set_course(xstrtoi(line.bas.common.heading, nullptr, 10));

    if (is_advanced_mode) {
      wpt->hdop = strtod(line.adv.hdop, nullptr);
      wpt->vdop = strtod(line.adv.vdop, nullptr);
      wpt->pdop = strtod(line.adv.pdop, nullptr);

      /* handle fix mode (2d, 3d, etc.) */
      if (!strncmp(line.adv.valid,"DGPS", sizeof line.adv.valid)) {
        wpt->fix = fix_dgps;
      } else if (!strncmp(line.adv.fixmode,"3D", sizeof line.adv.fixmode)) {
        wpt->fix = fix_3d;
      } else if (!strncmp(line.adv.fixmode,"2D", sizeof line.adv.fixmode)) {
        wpt->fix = fix_2d;
      } else
        /* possible values: fix_unknown,fix_none,fix_2d,fix_3d,fix_dgps,fix_pps */
      {
        wpt->fix = fix_unknown;
      }
    }

    track_add_wpt(track, wpt);
    if (line.bas.common.tag != 'T') {
      // A 'G' tag appears to be a 'T' tag, but generated on the trailing
      // edge of a DGPS fix as it decays to an SPS fix.  See 1/13/13 email
      // thread on gpsbabel-misc with Jamie Robertson.
      assert(line.bas.common.tag == 'C' || line.bas.common.tag == 'G' ||
             line.bas.common.tag == 'V');
      auto* wpt2 = new Waypoint(*wpt);
      if (line.bas.common.tag == 'V') {	// waypoint with voice recording?
        char vox_file_name[sizeof(line.adv.vox)+5];
        const char* vox = is_advanced_mode ? line.adv.vox : line.bas.vox;
        assert(vox[0] != '\0');
        strcpy(vox_file_name,vox);
        strcat(vox_file_name,".WAV");
        wpt2->shortname = vox_file_name;
        wpt2->description = vox_file_name;
        waypt_add_url(wpt2, vox_file_name, vox_file_name);
      }
      waypt_add(wpt2);
    }
  }
}
