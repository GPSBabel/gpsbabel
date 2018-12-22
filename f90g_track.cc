/*
  Map file reader for F90G Automobile DVR.

    Copyright (C) 2014 Jim Keeler, James.L.Keeler@gmail.com
    Copyright (C) 2001-2013 Robert Lipe, robertlipe+source@gpsbabel.org

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

    Read the map file contents picking out the defined record types.

    The map file contains a constant 30 byte header record followed by a variable number of
    TT records.  The TT records start with the two characters "TT" and are 251 bytes long.
    The TT records conatain values for time, position and velocity.

 */

#include "defs.h"
#include "gbfile.h"
#include <QtCore/QDebug>

#define MYNAME "f90g_track"
#define TTRECORDSIZE      249
#define HEADERRECORDSIZE  30
#define FLIPEDBITS        0xaa



static gbfile* fin = nullptr;
static route_head* track = nullptr;


static
arglist_t f90g_track_args[] = {
  ARG_TERMINATOR
};

/*******************************************************************************
* %%%        global callbacks called by gpsbabel main process              %%% *
*******************************************************************************/
#define VALIDHEADER "MEDIA 1."
static void
f90g_track_rd_init(const QString& fname)
{
  char header[HEADERRECORDSIZE];

  fin = gbfopen(fname, "r", MYNAME);
  gbfseek(fin, 0, SEEK_SET);
  if (gbfread(header, 1, HEADERRECORDSIZE, fin) != HEADERRECORDSIZE) {
    fatal(MYNAME ": read error");
  } else {
    // flip bits and check for valid header
    for (char &i : header) {
        i ^= FLIPEDBITS;
    }
    if (memcmp(header, VALIDHEADER, sizeof(VALIDHEADER)-1)) {
      fatal(MYNAME ": bad header");
    }
    // start the track list
    track = route_head_alloc();
    is_fatal((track == nullptr), MYNAME ": memory non-enough");
    track->rte_name = fname;
    track_add_head(track);
  }
}

static void
f90g_track_rd_deinit()
{
  gbfclose(fin);
}

// needed conversion factors
static const double MIN_PER_DEGREE  = 600000.0f;
static const float  SPEED_CONVERSION = (10.0f)/(36.0f); // convert KPH to meters per second

static void
f90g_track_read()
{
  char northSouth, eastWest, velocityMark, ttRec[TTRECORDSIZE], tempBuf[20];
  int year, mon, mday, hour, min, sec, latitudeDeg, latitudeMin, longitudeDeg, longitudeMin, velocity;

  is_fatal((track == nullptr), MYNAME "Track setup error");
  for (;;) {
    if ((gbfread((void*)ttRec, 1, 2, fin) != 2)
        || (memcmp(ttRec,"TT",2))) {
      break;
    }
    if (gbfread((void*)ttRec, 1, TTRECORDSIZE, fin) != TTRECORDSIZE) {
      break;
    }
    for (char &i : ttRec) {
        i ^= FLIPEDBITS;
    }

    // Pick the TT record apart and if it is good, fill in a new Waypoint
    year = mon = mday = hour = min = sec = latitudeDeg = latitudeMin = longitudeDeg = longitudeMin = velocity= 0;
    // Get the time stamp
    sscanf(&ttRec[15],"%4d%2d%2d%2d%2d%2d" ,&year, &mon, &mday, &hour, &min, &sec);
    // Get latitude and longitude
    sscanf(&ttRec[30],"%1c%2d%6d%1c%3d%6d", &northSouth, &latitudeDeg, &latitudeMin,
           &eastWest, &longitudeDeg, &longitudeMin);
    // Get velocity (KPH)
    sscanf(&ttRec[53],"%1c%3d", &velocityMark, &velocity);

    // sanity check the data before committing to the Waypoint
    if (year != 0 && (northSouth == 'N' || northSouth == 'S') && (eastWest == 'E' || eastWest == 'W')
        && velocityMark == 'M') {

      // create the Waypoint and fill it in
      Waypoint* readWaypoint = new Waypoint;
      QDateTime dt = QDateTime(QDate(year, mon, mday), QTime(hour, min, sec), Qt::UTC);

      readWaypoint->SetCreationTime(dt);
      readWaypoint->latitude = (double(latitudeDeg) + double(latitudeMin)/MIN_PER_DEGREE)
                               * ((northSouth == 'N')? 1.0f : -1.0f);
      readWaypoint->longitude = (double(longitudeDeg) + double(longitudeMin)/MIN_PER_DEGREE)
                                * ((eastWest == 'E')? 1.0f : -1.0f);
//       qDebug() << dt.toString() << latitudeDeg << latitudeMin << readWaypoint->latitude;
      readWaypoint->speed = float(velocity) * SPEED_CONVERSION;
      // Name the Waypoint
      snprintf(tempBuf, sizeof(tempBuf), "%2.2dM%2.2dS-%3.3dKPH", min, sec, velocity);
      readWaypoint->shortname = QString(tempBuf);

      // Add the Waypoint to the current track
      track_add_wpt(track, readWaypoint);
    }
  }
}

// capabilities below means: we can only read trace file.

ff_vecs_t f90g_track_vecs = {
  ff_type_file,
  { ff_cap_none, (ff_cap)(ff_cap_read), ff_cap_none },
  f90g_track_rd_init,
  nullptr,
  f90g_track_rd_deinit,
  nullptr,
  f90g_track_read,
  nullptr,
  nullptr,
  f90g_track_args,
  CET_CHARSET_UTF8, 0			/* ascii is the expected character set */
  /* not fixed, can be changed through command line parameter */
  , NULL_POS_OPS,
  nullptr
};
