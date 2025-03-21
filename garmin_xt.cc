/*

   Copyright (C) 2010  Eriks Zelenka, isindir@users.sourceforge.net
   Copyright (C) 2009  jekaeff,
   GMXT2GPX ( http://www.geocaching.hu/users.geo?id=9508 ; http://sites.google.com/site/jekaeff/eng-1 )
   The original code written in Pascal and does not include specific License, however on the project
   webpage it is said to be OpenSource/Libre software
   Copyright (C) 2005  Robert Lipe, robertlipe+source@gpsbabel.org

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

#include "garmin_xt.h"

#include <cstdint>   // for uint8_t, uint32_t, uint16_t, int32_t
#include <cstdio>    // for SEEK_CUR, SEEK_SET
#include <cstring>   // for strcmp, strcpy

#include <QString>   // for QString

#include "defs.h"
#include "gbfile.h"  // for gbfread, gbfgetuint16, gbfseek, gbfgetc, gbfgetuint32, gbfclose, gbfeof, gbfopen


#define GARMIN_XT_ELE 31500/65536
#define DATABLOCKSIZE 1
#define STRK_BLOCK_SIZE 97

/*******************************************************************************
 * %%%        global callbacks called by gpsbabel main process              %%% *
 *******************************************************************************/

/*******************************************************************************
 * %%%        Reader callbacks                                              %%% *
 *******************************************************************************/
void
GarminXTFormat::rd_init(const QString& fname)
{
  fin = gbfopen(fname, "rb");
}

void
GarminXTFormat::rd_deinit()
{
  gbfclose(fin);
}

uint16_t
GarminXTFormat::format_garmin_xt_rd_st_attrs(char* p_trk_name, uint8_t* p_track_color)
{
  int		method = 0;
  uint8_t	spam = 0;
  int32_t		 TrackMaxLat = 0;
  int32_t		 TrackMaxLon = 0;
  int32_t		 TrackMinLat = 0;
  int32_t		 TrackMinLon = 0;
  char		trk_name[30]="";
  // TODO: SHIFT - can't test behaviour, do not have appropriate files
  //int		ii;

  // get the option for the processing the track name
  if (opt_trk_header) {
    method = opt_trk_header.get_result();
    // if method is out of range set to default
    if ((method < 0) || (method > 1)) {
      method = 0;
    }
  }
  // set to RED if not specified
  *p_track_color=9;

  uint16_t trackbytes = gbfgetuint16(fin);
  uint16_t TrackPoints = gbfgetuint16(fin);
  (void) TrackPoints;

  switch (method) {
  case 1:
    break; // IGNORE
    /* TODO: SHIFT - can't test behaviour, do not have appropriate files
       case 2: { // SHIFTED method
       spam = gbfgetc(fin);
       gbfread(&trk_name, 30, DATABLOCKSIZE, fin);
       gbfseek(fin, -1, SEEK_CUR);
       for (ii = 0; ii<29; ii++)
       {
       trk_name[ii] = (trk_name[ii] >> 2) + ( trk_name[ii+1] % 4 ) * 64;
       }
       }
       break;
       */
  default: { // NORMAL
    spam = gbfgetc(fin);
    gbfread(&trk_name, 30, DATABLOCKSIZE, fin);
    gbfseek(fin, -1, SEEK_CUR);
  }
  break;
  }
  spam = gbfgetc(fin);

  gbfread(&TrackMaxLat, 3, DATABLOCKSIZE, fin);
  gbfread(&spam, 1, DATABLOCKSIZE, fin);
  gbfread(&TrackMaxLon, 3, DATABLOCKSIZE, fin);
  gbfread(&spam, 1, DATABLOCKSIZE, fin);
  gbfread(&TrackMinLat, 3, DATABLOCKSIZE, fin);
  gbfread(&spam, 1, DATABLOCKSIZE, fin);
  gbfread(&TrackMinLon, 3, DATABLOCKSIZE, fin);
  gbfread(p_track_color, 1, DATABLOCKSIZE, fin);
  gbfread(&spam, 1, DATABLOCKSIZE, fin);

  strcpy(p_trk_name, trk_name);
  return trackbytes;
}

/*
 * Function to decrypt track block in saved read from saved tracks file
 */
void
GarminXTFormat::format_garmin_xt_decrypt_trk_blk(int Count, uint8_t TrackBlock[])
{
  int j = 12;
  while (j<(Count-1)) {
    for (int i = j; i < Count; i++) {
      TrackBlock[i] = TrackBlock[i] >> 1;
      if (i<(Count-1)) {
        TrackBlock[i] = TrackBlock[i] + (TrackBlock[i+1] % 2) * 128;
      }
    }
    j+=12;
  }
}

/*
 * Function to Decompose track block of STRK_BLOCK_SIZE bytes
 */
void
GarminXTFormat::format_garmin_xt_decomp_trk_blk(uint8_t ii, const uint8_t TrackBlock[], double* Ele, double* Lat, double* Lon, uint32_t* Time)
{
  //printf("%d %d %d %d %d %d\n", TrackBlock[0], TrackBlock[1], TrackBlock[2], TrackBlock[3], TrackBlock[4], TrackBlock[5]);
  uint16_t PrevEleW = TrackBlock[(ii - 1) * 12 + 1 ];
  PrevEleW = PrevEleW << 8;
  PrevEleW = PrevEleW + TrackBlock[(ii - 1) * 12 ];
  *Ele = (double)PrevEleW * GARMIN_XT_ELE - 1500;

  uint32_t LatLW = TrackBlock[(ii - 1) * 12 + 4];
  LatLW = LatLW << 8;
  LatLW = LatLW + TrackBlock[(ii - 1) * 12 + 3];
  LatLW = LatLW << 8;
  LatLW = LatLW + TrackBlock[(ii - 1) * 12 + 2];
  auto LatF = (double)LatLW;
  if (LatF > 8388608) {
    LatF = LatF - 16777216;
  }
  *Lat = LatF * 360 / 16777216;

  uint32_t LonLW = TrackBlock[(ii-1)*12+7];
  LonLW = LonLW << 8;
  LonLW = LonLW+TrackBlock[(ii-1)*12+6];
  LonLW = LonLW << 8;
  LonLW = LonLW+TrackBlock[(ii-1)*12+5];
  auto LonF = (double)LonLW;
  if (LonF>8388608) {
    LonF = LonF - 16777216;
  }
  *Lon = LonF * 360 / 16777216;

  uint32_t TimeLW = TrackBlock[(ii - 1) * 12 + 11];
  TimeLW = TimeLW << 8;
  TimeLW = TimeLW+TrackBlock[(ii - 1) * 12 + 10];
  TimeLW = TimeLW << 8;
  TimeLW = TimeLW+TrackBlock[(ii - 1) * 12 + 9];
  TimeLW = TimeLW << 8;
  TimeLW = TimeLW + TrackBlock[(ii - 1) * 12 + 8];
  *Time = TimeLW + 631065600;
}

/*
 * Decompose Last Waypoint Elevation
 */
void
GarminXTFormat::format_garmin_xt_decomp_last_ele(uint8_t ii, double* PrevEle, const uint8_t TrackBlock[])
{
  uint16_t PrevEleW = TrackBlock[ii - 1];
  PrevEleW = PrevEleW << 8;
  PrevEleW = PrevEleW + TrackBlock[ii - 2];
  *PrevEle = (double)PrevEleW * GARMIN_XT_ELE - 1500;
}

/*
 * Main Function to process Saved tracks file
 */
void
GarminXTFormat::format_garmin_xt_proc_strk()
{
  int 		Count = 0; // Used to obtain number of read bytes
  int TracksCompleted = 0; // Number of processed tracks
  uint8_t	TrackBlock[STRK_BLOCK_SIZE]; // File Block
  double		 Lat = 0;
  double		 Lon = 0; // wpt data
  double		 PrevLat = 0;
  double		 PrevLon = 0;
  double		 PrevEle = 0; // wpt data
  uint32_t	 Time = 0;
  uint32_t	 PrevTime = 0; // wpt data
  uint8_t	trk_color = 0xff;

  // Skip 12 bytes from the BOF
  gbfseek(fin, 12, SEEK_SET);

  // read # of tracks
  int NumberOfTracks = gbfgetuint16(fin); // Number of tracks in the file

  // Skip 2 bytes
  gbfseek(fin, 2, SEEK_CUR);

  // Process all tracks one by one
  while ((TracksCompleted < NumberOfTracks) && (!gbfeof(fin))) {
    Waypoint*	wpt;
    char* trk_name = (char*) xmalloc(30);

    // Generate Track Header
    uint16_t trackbytes = format_garmin_xt_rd_st_attrs(trk_name, &trk_color) - 50; // Bytes in track

    auto* tmp_track = new route_head;
    // update track color
    tmp_track->line_color.bbggrr = colors[trk_color];
    tmp_track->line_color.opacity = 255;
    // update track name
    tmp_track->rte_name = trk_name;
    xfree(trk_name);
    track_add_head(tmp_track);

    // This is the 1st coordinate of the track
    bool FirstCoo = true;
    while (trackbytes>0) {
      if (trackbytes>=STRK_BLOCK_SIZE) {
        Count = gbfread(&TrackBlock, DATABLOCKSIZE, STRK_BLOCK_SIZE, fin);
        trackbytes -= STRK_BLOCK_SIZE;
      } else {
        Count = gbfread(&TrackBlock, DATABLOCKSIZE, trackbytes, fin);
        trackbytes = 0;
      }

      // decrypt loaded track block (Count - size of loaded TrackBlock)
      format_garmin_xt_decrypt_trk_blk(Count, TrackBlock);

      // process each track point in the loaded TrackBlock
      for (auto ii = 1; ii <= ((Count-1) / 12); ii++) {
        // decompose loaded track block part (track point)
        format_garmin_xt_decomp_trk_blk(ii, TrackBlock, &PrevEle, &Lat, &Lon, &Time);

        // Add point to the track if not the first point
        if (!FirstCoo) {
          //create new waypoint
          wpt = new Waypoint;

          //populate wpt;
          wpt->latitude = PrevLat;	/* Degrees */
          wpt->longitude = PrevLon; 	/* Degrees */
          wpt->altitude = PrevEle; 			/* Meters. */
          wpt->SetCreationTime(PrevTime);  		/* Unix Time adjusted to Garmin time */

          // add way point to the track
          track_add_wpt(tmp_track, wpt);
        } else {
          FirstCoo = false;
        }
        PrevLat = Lat;
        PrevLon = Lon;
        PrevTime = Time;
      }
    }

    // decompose elevation for the last point
    if (Count > 12) {
      Count--;
    }
    format_garmin_xt_decomp_last_ele(Count, &PrevEle, TrackBlock);

    //create new waypoint
    wpt = new Waypoint;

    //populate wpt;
    wpt->latitude = PrevLat;	/* Degrees */
    wpt->longitude = PrevLon; 	/* Degrees */
    wpt->altitude = PrevEle; 			/* Meters. */
    wpt->SetCreationTime(PrevTime);  		/* Unix Time adjusted to Garmin time */

    // add way point to the track
    track_add_wpt(tmp_track, wpt);

    // update completed tracks counter
    TracksCompleted++;
  }
}

void
GarminXTFormat::format_garmin_xt_proc_atrk()
{
  int		method = 0;
  unsigned char 	buf[3];

  // get the option for the processing the track name
  if (opt_trk_header) {
    method = opt_trk_header.get_result();
  }

  if (! track) {
    track = new route_head;
    // header option was not set to ignore
    if (method !=1) {
      track->rte_name = "ATRK XT";
    }
    track_add_head(track);
  }

  // We think the word at offset 0xc is the trackpoint count.
  gbfseek(fin, 12, SEEK_SET);
  int32_t num_trackpoints = gbfgetuint32(fin);

  while (num_trackpoints--) {
    uint16_t block = gbfgetuint16(fin);
    if (block != 0x0c) {
      break;
    }

    gbfread(&buf, 3, DATABLOCKSIZE, fin); //1. Lat
    uint32_t Lat = buf[0] | (buf[1] << 8) | (buf[2] << 16);
    gbfread(&buf, 3, DATABLOCKSIZE, fin); //2. Lon
    uint32_t Lon = buf[0] | (buf[1] << 8) | (buf[2] << 16);

    uint16_t uu = gbfgetuint16(fin);
    uint32_t Tim = gbfgetuint32(fin);

    Tim += 631065600; // adjustment to UnixTime
    double LatF = Lat;
    if (LatF>8388608) {
      LatF -= 16777216;
    }
    double LonF = Lon;
    if (LonF>8388608) {
      LonF -= 16777216;
    }
    double AltF = (double)uu * GARMIN_XT_ELE - 1500;

    //create new waypoint
    auto* wpt = new Waypoint;

    //populate wpt;
    wpt->latitude = LatF*180/16777216;	/* Degrees */
    wpt->longitude = LonF*360/16777216; 	/* Degrees */
    wpt->altitude = AltF; 			/* Meters. */
    wpt->SetCreationTime(Tim);  		/* Unix Time adjusted to Garmin time */

    track_add_wpt(track, wpt);
  }
}

void
GarminXTFormat::read()
{
  // Saved Tracks file
  if (opt_xt_ftype.get() == "STRK") {
    format_garmin_xt_proc_strk();
  } else { // Active Track file
    format_garmin_xt_proc_atrk();
  }
}
