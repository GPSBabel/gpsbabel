/*


	Copyright (C) 2008  Dr. J�rgen Neumann, Juergen.Neumann@online.de
	Copyright (C) 2005  Robert Lipe, robertlipe+source@gpsbabel.org (based on nmea.c)
	Copyright (C) 20XX  probably many others from the gpsbabel development team ;-)

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

	=====================================================================================

	This file allows gpsbabel to read and write the internal track log format used by
	GoPal navigation systems. They produce a simple line-oriented format with one point per
	second. Unfortunately the the data does not contain a valid date, only some kind of timetick,
	together with each point (perhaps by mistake ??). So we have to parse the filename for a valid starting
	date and add the timeoffset. Second problem (at least to me) was that irregularly stupid errors were
	in the data, i.e. only one data point shows a totally wrong longitude or latitude. Everything else in
	the dataset seems ok, so I needed a way to sort out these errors. My solution is to calculate the speed
	between successive points and drop points not between minspeed and maxspeed. This way I can sort out most
	of this annoying bugs, a side effect is that if a minimum speed > 0 is set points with the same coordinates are also
	dropped.

	Fileformat GoPal
	TICK;   TIME UTC; LONG;    LAT;       HEIGHT; SPEED km/h;  FIX; HDOP;     SAT
	3801444, 080558, 2.944362, 43.262117, 295.28, 0.12964, 2, 2.900000, 3
	Filenames:
		trackYYYYMMDD_HHMMSS.trk
		A_YYYYMMDD_HHMMSS.trk
	with HHMMSS local time (not UTC)

	History
	2008-07-18 initial release of Version 0.1
	2008-07-26 bugfix: filenamehandling linux, format specification in write statement

	ToDo:
	- check for midnight & adjust
*/

#include "defs.h"
#include "cet_util.h"
#include "csv_util.h"
#include "grtcirc.h"
#include "jeeps/gpsmath.h"
#include "strptime.h"
#include <cmath>
#include <cstdio>
#include <cstdlib>
#include <QtCore/QDateTime>
#include <QtCore/QString>

#define MYNAME "gopal"

static gbfile* fin, *fout;

static struct tm tm,filenamedate, trackdate;
static time_t		tx;
static char* optdate=nullptr;
static char* optmaxspeed=nullptr;
static char* optminspeed=nullptr;
static char* optclean= nullptr;
static double minspeed,maxspeed;
static struct tm opt_tm;	/* converted "date" parameter */
static
QVector<arglist_t> gopal_args = {
  {"date", &optdate, "Complete date-free tracks with given date (YYYYMMDD).", nullptr, ARGTYPE_INT, ARG_NOMINMAX, nullptr },
  {"maxspeed", &optmaxspeed, "The maximum speed (km/h) traveling from waypoint to waypoint.", "200", ARGTYPE_INT, "1", "1000", nullptr },
  {"minspeed", &optminspeed, "The minimum speed (km/h) traveling from waypoint to waypoint. Set >0 to remove duplicate waypoints", "0", ARGTYPE_INT, "0", "999", nullptr },
  {"clean", &optclean, "Cleanup common errors in trackdata", "1", ARGTYPE_BOOL, ARG_NOMINMAX, nullptr },
};

#define CHECK_BOOL(a) if (a && (*a == '0')) a = NULL

static int gopal_check_line(char* line)
{
  char* c = line;
  int i = 0;
  while ((c = strchr(c, ','))) {
    c++;
    i++;
  }
  return i;
}



/*******************************************************************************
* %%%        global callbacks called by gpsbabel main process              %%% *
*******************************************************************************/

static void
gopal_rd_init(const QString& fname)
{
  CHECK_BOOL(optclean);
  if (optminspeed) {
    minspeed=atof(optminspeed);
    if (global_opts.debug_level > 1) {
      fprintf(stderr,"options from command line : gopal minspeed = %s\n",optminspeed);
    }
  } else {
    minspeed=0;
  }
  if (optmaxspeed) {
    maxspeed=atof(optmaxspeed);
    if (global_opts.debug_level > 1) {
      fprintf(stderr,"options from command line : gopal maxspeed = %s\n",optmaxspeed);
    }
  } else {
    maxspeed=200;
  }
  if (global_opts.debug_level > 1) {
    fprintf(stderr,"setting minspeed to %5.1lf km/h and maxspeed to %5.1lf km/h\n",minspeed,maxspeed);
  }

  fin = gbfopen(fname, "r", MYNAME);

  if (optdate) {
    memset(&opt_tm, 0, sizeof(opt_tm));

    char* ck = strptime(optdate, "%Y%m%d", &opt_tm);
    if ((ck == nullptr) || (*ck != '\0') || (strlen(optdate) != 8)) {
      fatal(MYNAME ": Invalid date \"%s\"!\n", optdate);
    } else if (opt_tm.tm_year < 70) {
      fatal(MYNAME ": Date \"%s\" is out of range (have to be 19700101 or later)!\n", optdate);
    }
    tx = mkgmtime(&opt_tm);

  } else {
    /* remove path */
    QString filename = get_filename(fname);
	QString datestr;

    if (filename.startsWith("track")&&(filename.length()>13)) { // we need at least 13 letters: trackYYYYMMDD...
      datestr = filename.mid(5,8);
    } else if (filename.startsWith("A_")&&(filename.length()>10)) { // here we expect at least 10 letters: A_YYYYMMDD...
      datestr = filename.mid(2,8);
    }
    // in buff we should now have something which looks like a valid date starting with YYYYMMDD
    /*ck = (char*)*/strptime(qPrintable(datestr), "%Y%m%d", &filenamedate);
    // if (((ck == NULL) || (*ck != '\0') )&&!(optdate))
    // fatal(MYNAME ": Invalid date in filename \"%s\", try to set manually using \"date\" switch!\n", buff);
    // /* else */ if (filenamedate.tm_year < 70)
    // fatal(MYNAME ": Date \"%s\" is out of range (have to be 19700101 or later)!\n", buff);
    // tx= mkgmtime(&filenamedate);
  }
}

static void
gopal_rd_deinit()
{
  gbfclose(fin);
}

static void
gopal_read()
{

  char* buff;
  double hmsd;
  int fix;
  int hms;
  Waypoint* lastwpt=nullptr;
  char tbuffer[64];
  struct tm tm2;
  double lat_old = 0;
  

  auto* route = new route_head;
  QDateTime qtx = QDateTime::fromSecsSinceEpoch(tx, Qt::UTC);
  route->rte_name = "Tracklog ";
  route->rte_name += qtx.toString(Qt::ISODate);
  route_add_head(route);

  long line = 0;
  while ((buff = gbfgetstr(fin))) {
    if ((line == 0) && fin->unicode) {
      cet_convert_init(CET_CHARSET_UTF8, 1);
    }

    char* str = buff = lrtrim(buff);
    if (*buff == '\0') {
      continue;
    }
    int nfields = gopal_check_line(buff);
    if ((nfields != 8) && (nfields != 11)) {
      continue;
    }
    // Old format.  Hassle for date.
    if ((nfields == 8) && (tx == 0)) {
      // fatal(MYNAME ": Invalid date in filename \"%s\", try to set manually using \"date\" switch!\n", buff);
    }
    auto* wpt = new Waypoint;

    int column = -1;
    // the format of gopal is quite simple. Unfortunately the developers forgot the date as the first element...
    //TICK;    TIME;   LONG;     LAT;       HEIGHT; SPEED;  Fix; HDOP;    SAT
    //3801444, 080558, 2.944362, 43.262117, 295.28, 0.12964, 2, 2.900000, 3
    char* c = csv_lineparse(str, ",", "", column++);
    int millisecs = 0;
    while (c != nullptr) {
      switch (column) {
      case  0: /* "-" */	/* unknown fields for the moment */
        unsigned long microsecs;
        sscanf(c, "%lu", &microsecs);
        // Just save this; we'll use it on the next field.
        millisecs = lround((microsecs % 1000000) / 1000.0);
        break;
      case  1:				/* Time UTC */
        sscanf(c,"%lf",&hmsd);
        hms = (int) hmsd;
        tm.tm_sec = hms % 100;
        hms = hms / 100;
        tm.tm_min = hms % 100;
        hms = hms / 100;
        tm.tm_hour = hms % 100;
        tm.tm_year=trackdate.tm_year;
        tm.tm_mon=trackdate.tm_mon;
        tm.tm_mday=trackdate.tm_mday;
        // This will probably be overwritten by field 9...if we have 9 fields.
        wpt->SetCreationTime(tx+((((time_t)tm.tm_hour * 60) + tm.tm_min) * 60) + tm.tm_sec);
        wpt->creation_time = wpt->creation_time.addMSecs(millisecs);
        if (global_opts.debug_level > 1) {
          time_t t = wpt->GetCreationTime().toTime_t();
          strftime(tbuffer, sizeof(tbuffer), "%c", gmtime(&t));
          printf("parsed timestamp: %s\n",tbuffer);
        }
        break;

      case  2: 				/* longitude */
        sscanf(c, "%lf", &wpt->longitude);
        break;

      case  3: 				/* latitude */
        sscanf(c, "%lf", &wpt->latitude);
        break;
      case  4: 				/* altitude */
        sscanf(c, "%lf", &wpt->altitude);
        break;
      case  5: 				/* speed */
        //sscanf(c, "%lf", &wpt->speed);
        wpt->speed=atof(c);
        if (global_opts.debug_level > 1) {
          printf("parsed speed: %8.5f\n",wpt->speed);
        }
        break;
      case  6: 				/* type of fix */
        sscanf(c, "%d", &fix);
        //my device shows only 0 or 2
        //should i guess from no of sats if 2d or 3d?
        switch (fix) {
        case 0:
          wpt->fix = fix_none;
          break;
        case 2:
          wpt->fix = fix_2d;
          break;
          //case 3: wpt->fix = fix_3d;break;
          //case 4: wpt->fix = fix_dgps;break; /* 2D_diff */
          //case 5: wpt->fix = fix_dgps;break; /* 3D_diff */
        default:
          wpt->fix = fix_unknown;
          break;
        }
        break;
      case  7: 				/* hdop */
        wpt->hdop = atof(c);
        //sscanf(c, "%lf", &wpt->hdop); does not work ???
        //wpt->vdop=0;wpt->hdop=0;
        break;
      case  8: 				/* number of sats */
        sscanf(c, "%d", &wpt->sat);
        break;
        // Somewhere around mid/late 2009, these files started
        // seeing 11 fields.
      case  9:
        memset(&tm2, 0, sizeof(tm2));
        if (!strptime(c, "%Y%m%d", &tm2)) {
          fatal("Bad date '%s'.\n", c);
        }
        wpt->creation_time = wpt->creation_time.addSecs(mkgmtime(&tm2));
        break;
      case 10:  // Unknown.  Ignored.
      case 11:  // Bearing.  Ignored.
        break;
      }
      c = csv_lineparse(nullptr, ",", "", column++);
    }
    line++;

    if ((wpt->fix != fix_none)&&(lat_old==0)) { //first-time init
      lat_old=wpt->latitude;
      //route_add_wpt(route, wpt);
      lastwpt=wpt;
    }
    //calculate the speed to reach this waypoint from the last. This way I try to sort out invalid waypoints
    double speed = 0;
    if (lastwpt !=nullptr) {
      speed=3.6*radtometers(gcdist(RAD(lastwpt->latitude), RAD(lastwpt->longitude), RAD(wpt->latitude), RAD(wpt->longitude))) /
            abs((int)(wpt->creation_time.toTime_t() - lastwpt->GetCreationTime().toTime_t()));
      //printf("speed line %d %lf \n",line,speed);
    }
    /* Error handling: in the tracklog of my device sometimes "jump" waypoints ;-) */
    if	((optclean) &&
         (((wpt->longitude==0.0)|| (wpt->latitude==0.0)||(std::abs(wpt->latitude)>90)||(std::abs(wpt->longitude)>180))||
          ((speed>maxspeed)||(speed<minspeed)))
       ) {
      if (global_opts.debug_level > 1) {
        fprintf(stderr,"Problem in or around line %5lu: \"%s\" %lf km/h\n",line,buff,speed);
      }
    } else {
      if (global_opts.debug_level > 1) {
        fprintf(stderr,"valid                line %5lu: \"%s\" %lf km/h\n",line,buff,speed);
      }
      lastwpt=wpt;
      lat_old=wpt->latitude;
      route_add_wpt(route,wpt);
      waypt_add(new Waypoint(*wpt));
    }
  }
}

static void
gopal_write_waypt(const Waypoint* wpt)
{
  int fix=fix_unknown;
  //TICK;    TIME;   LONG;     LAT;       HEIGHT; SPEED;  UN; HDOP;     SAT
  //3801444, 080558, 2.944362, 43.262117, 295.28, 0.12964, 2, 2.900000, 3
  QString tbuffer = wpt->creation_time.toString("HHmmss");
  if (wpt->fix!=fix_unknown) {
    switch (wpt->fix) {
    case fix_none:
      fix = 0;
      break;
    case fix_2d:
      fix = 2;
      break;
    default:
      fix = 0;
      break;
    }
  }
  //MSVC handles time_t as int64, gcc and mac only int32, so convert it:
  unsigned long timestamp = (unsigned long)wpt->GetCreationTime().toTime_t();
  gbfprintf(fout, "%lu, %s, %lf, %lf, %5.1lf, %8.5lf, %d, %lf, %d\n",timestamp, CSTR(tbuffer),  wpt->longitude, wpt->latitude,wpt->altitude,
            wpt->speed,fix,wpt->hdop,wpt->sat);
}


static void
gopal_wr_init(const QString& fname)
{
  fout = gbfopen(fname, "w", MYNAME);
}

static void
gopal_wr_deinit()
{
  gbfclose(fout);
}

static void
gopal_write()
{
  route_disp_all(nullptr, nullptr, gopal_write_waypt);
}

/**************************************************************************/

// capabilities below means: we can only read and write waypoints
//

ff_vecs_t gopal_vecs = {
  ff_type_file,
  {
    ff_cap_none 	 	/* waypoints */,
    (ff_cap)(ff_cap_read | ff_cap_write)	/* tracks */,
    ff_cap_none  	/* routes */
  },
  gopal_rd_init,
  gopal_wr_init,
  gopal_rd_deinit,
  gopal_wr_deinit,
  gopal_read,
  gopal_write,
  nullptr,
  &gopal_args,
  CET_CHARSET_ASCII, 0	/* ascii is the expected character set */
  /* not fixed, can be changed through command line parameter */
  , NULL_POS_OPS,
  nullptr
};
/**************************************************************************/
