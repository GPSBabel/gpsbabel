/*
    National Geographic Topo! TPO file support.
    2.x support contributed to gpsbabel by Steve Chamberlin.
    3.x support contributed to gpsbabel by Curt Mills.
    4.x files read properly when treated as 3.x (final release was 4.5)
    track parsing bugs fixed by Steve Eckert in 2012 and 2020

    Topo! version 2.x:      Tracks are implemented.
    Topo! version 3.x/4.x:  Reading of Tracks/Waypoints/Routes is
                            implemented.  Also extracts Map Notes/
                            Symbols/Text Labels as Waypoints.

    Copyright (C) 2005 Steve Chamberlin, slc at alum.mit.edu
    Portions Copyright (C) 2006 Curtis E. Mills, archer at eskimo dot com

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

/*
 TPO format notes:
 -----------------
 Most of the ASCII strings embedded in the text will have a
 byte-count prepended to the string.  Unknown yet whether other
 fields have this same byte-count, but so far it doesn't look like
 it.

 New format (3.x and later) files begin with a string byte-count
 byte and then a string starting with "TOPO! Ver. 3.", like "TOPO!
 Ver.  3.3.4".  Can contain routes/tracks/waypoints, embedded
 images, Map Notes, Symbols, Text Labels, Compass symbols, and
 several others.

 Older (pre-3.0) format does not have the above string.  Contains
 only tracks.  Waypoints are saved in a separate .TPG file.

 Track parsing has been problematic and may still not be right!
 See further notes and clues in tpo_process_tracks()
 (can #define Tracks2012 to revert to pre-2020 code in tpo_process_tracks)

 May contain these strings:
    Frmt:   String:
    -----   --------------------------------
    2.x     "CTopoAzimuth"
    2.x     "CTopoBookmark"
    2.x     "CTopoGpsRoute".  Saved in .tpg files (see tpg.c)
    2.x     "CTopoRoute".  The actual tracks we parse here.
    2.x     "CTopoSymbol"
    2.x/3.x "CTopoText"
    2.x     "CTopoWaypoint".  Saved in .tpg files (see tpg.c)
    3.x     "Notes"
    3.x     "PNG."  Embedded PNG image containing 2 rows of 40
              symbols each.  Starts with signature: 89 50 4e 47 0d
              0a 1a 0a, ends with signature 49 45 4e 44 ae 42 60 82.
    3.x     "shapes"
    3.x     "arrows"
    3.x     "recreation"
*/

#include "tpo.h"

#include <cstdint>              // for uint8_t
#include <cstdio>               // for printf, SEEK_CUR, SEEK_SET
#include <cmath>                // for abs
#include <cstring>              // for strlen, strncmp
#include <vector>               // for vector

#include <QByteArray>           // for QByteArray
#include <QScopedArrayPointer>  // for QScopedArrayPointer
#include <QString>              // for QString
#include <QStringLiteral>       // for qMakeStringPrivate, QStringLiteral
#include <QtGlobal>             // for qPrintable, Q_UNUSED

#include "defs.h"               // for Waypoint, fatal, route_head, le_read32, waypt_add, track_add_wpt, track_add_head, doing_rtes, doing_wpts, gb_color, route_add_head, route_add_wpt, unknown_alt, doing_trks
#include "gbfile.h"             // for gbfread, gbfgetc, gbfgetint32, gbfreadbuf, gbfseek, gbfgetdbl, gbfgetint16, gbfclose, gbfgetnativecstr, gbfgetuint16, gbfopen_le
#include "jeeps/gpsmath.h"      // for GPS_Math_Known_Datum_To_WGS84_M


/*******************************************************************************/
/*                                      READ                                   */
/*******************************************************************************/

/* tpo_check_version_string()
   Check the first bytes of the file for a version 3.0 header. */
void
TpoFormatBase::tpo_check_version_string()
{

  unsigned char string_size;
  const char* v3_id_string = "TOPO! Ver";

  /* read the id string */
  gbfread(&string_size, 1, 1, tpo_file_in);
  QByteArray string_buffer = gbfgetnativecstr(tpo_file_in);

  /* check for the presence of a 3.0-style id string */
  /* Note this check also finds version 4 id strings, e.g. "TOPO! Ver. 4.5.0" */
  if (strncmp(v3_id_string, string_buffer, strlen(v3_id_string)) == 0) {
    /*		fatal("gpsbabel can only read TPO version 2.7.7 or below; this file is %s\n", string_buffer); */
//fprintf(stderr,"gpsbabel can only read TPO version 2.7.7 or below; this file is %s\n", string_buffer);

    gbfseek(tpo_file_in, -(string_size+1), SEEK_CUR);
    tpo_version = 3.0;  /* Really any 3.x version */
    return;

  } else {
    /* We found a version 1.x or 2.x file */
    /* seek back to the beginning of the file */
    gbfseek(tpo_file_in, -(string_size+1), SEEK_CUR);
    tpo_version = 2.0;  /* Really any 1.x or 2.x version */
    return;
  }
}

/* tpo_dump_header_bytes(int header_size)
   Write the first header_size bytes of the file to standard output
   as a C array definition. */
void
TpoFormatBase::tpo_dump_header_bytes(int header_size)
{
  QByteArray buffer = gbfreadbuf(header_size, tpo_file_in);

  printf("unsigned char header_bytes[] = {\n");

  for (int i = 0; i < header_size; i++) {
    if (i%8 == 0) {
      printf("    ");
    }
    printf("0x%02X", buffer.at(i));
    if (i != header_size-1) {
      printf(", ");
    }
    if (i%8 == 7) {
      printf("\n");
    }
  }

  printf("};\n");
}

/* tpo_read_until_section()
   Keep reading bytes from the file until the section name is encountered,
   then go seek_bytes forwards (+) or backwards (-) to the start of
   the section data. */
void
TpoFormatBase::tpo_read_until_section(const char* section_name, int seek_bytes)
{
  char byte;
  unsigned int match_index = 0;
  int header_size = 0;

  while (true) {
    if (gbfread(&byte, 1, 1, tpo_file_in) < 1) {
      fatal("malformed input file - attempt to read past end");
    }
    header_size++;

    if (byte == section_name[match_index]) {
      match_index++;
      if (match_index == strlen(section_name)) {
        /*fprintf(stderr,"Found %s\n", section_name);*/
        gbfseek(tpo_file_in, seek_bytes, SEEK_CUR);
        header_size += seek_bytes;

        if (dumpheader && dumpheader[0] == '1') {
          gbfseek(tpo_file_in, -header_size, SEEK_CUR);
          tpo_dump_header_bytes(header_size);
        }
        return;

      }
    } else {
      match_index = 0;
    }
  }
}





//-------------------------------------------------------------------
//-------------------------------------------------------------------





// Decoder for version 2.x files.  This one does tracks only, as
// that is the only type of data available in the version 2.x TPO
// files.
//
void TpoFormatBase::tpo_read_2_x()
{
  char buff[16];

  /* track count */
  short track_count = gbfgetint16(tpo_file_in);

  /*fprintf(stderr,"track_count:%d\n", track_count);*/

  /* 4 unknown bytes */
  gbfread(&buff[0], 1, 4, tpo_file_in);

  /* chunk name: "CTopoRoute" */
  gbfread(&buff[0], 1, 12, tpo_file_in);

  for (int i = 0; i < track_count; i++) {

    auto* track_temp = new route_head;
    track_add_head(track_temp);

    /* generate a generic track name */
    track_temp->rte_name = QStringLiteral("Track %1").arg(i+1);

    /* zoom level 1-5 visibility flags */
    gbfread(&buff[0], 1, 10, tpo_file_in);

    /* 8 bytes of zeros, meaning unknown */
    gbfread(&buff[0], 1, 8, tpo_file_in);

    /* 4 more unknown bytes, possibly sign flags for the longitude and latitude? */
    gbfread(&buff[0], 1, 4, tpo_file_in);

    /* read the position of the initial track point */
    /* for some very odd reason, signs on longitude are swapped */
    /* coordinates are in NAD27/CONUS datum                     */

    /* 8 bytes - longitude, sign swapped  */
    double first_lon = gbfgetdbl(tpo_file_in);

    /* 8 bytes - latitude */
    double first_lat = gbfgetdbl(tpo_file_in);

    /* swap sign before we do datum conversions */
    first_lon *= -1.0;

    /* 8 unknown bytes: seems to be some kind of bounding box info */
    gbfread(&buff[0], 1, 8, tpo_file_in);

    /* number of route points */
    short waypoint_count = gbfgetint16(tpo_file_in);

    /* allocate temporary memory for the waypoint deltas */
    std::vector<short> lat_delta(waypoint_count);
    std::vector<short> lon_delta(waypoint_count);

    for (int j = 0; j < waypoint_count; j++) {

      /* get this point's longitude delta from the first waypoint */
      lon_delta[j] = gbfgetint16(tpo_file_in);

      /* get this point's latitude delta from the first waypoint */
      lat_delta[j] = gbfgetint16(tpo_file_in);
    }

    /* 8 bytes - longitude delta to degrees scale  */
    double lon_scale = gbfgetdbl(tpo_file_in);

    /* 8 bytes - latitude delta to degrees scale */
    double lat_scale = gbfgetdbl(tpo_file_in);

    /* 4 bytes: the total length of the route in feet*/
    gbfread(&buff[0], 1, 4, tpo_file_in);

    /* 2 unknown bytes */
    gbfread(&buff[0], 1, 2, tpo_file_in);

    /* 2 bytes: continuation marker */
    gbfread(&buff[0], 1, 2, tpo_file_in);

    /* multiply all the deltas by the scaling factors to determine the waypoint positions */
    for (int j = 0; j < waypoint_count; j++) {

      auto* waypoint_temp = new Waypoint;
      double amt;
      /* convert incoming NAD27/CONUS coordinates to WGS84 */
      GPS_Math_Known_Datum_To_WGS84_M(
        first_lat-lat_delta[j] * lat_scale,
        first_lon+lon_delta[j] * lon_scale,
        0.0,
        &waypoint_temp->latitude,
        &waypoint_temp->longitude,
        &amt,
        78);

      /* there is no elevation data for the waypoints */
      /* this is unnecessary, the constructor will do this anyway. */
      waypoint_temp->altitude = unknown_alt;

      track_add_wpt(track_temp, waypoint_temp);
    }
  }
}





//-------------------------------------------------------------------
//-------------------------------------------------------------------

// This will read 8/16/32 bits in little-endian format depending
// upon the value of the first byte.
//
// For version 3.x files.
//
int TpoFormatBase::tpo_read_int()
{
  constexpr int debug = 0;

  auto val = (unsigned char) gbfgetc(tpo_file_in);

  switch (val) {

  case 0xff:  // 32-bit value
    if constexpr(debug) {
      db.log("Found 32-bit value indicator: %x\n", val);
    }
    return (gbfgetint32(tpo_file_in));
    break;

  case 0xfe:  // 16-bit value
    if constexpr(debug) {
      db.log("Found 16-bit value indicator: %x\n", val);
    }
    return (gbfgetuint16(tpo_file_in));
    break;

  default:    // 8-bit value
    if constexpr(debug) {
      db.log("Found 8-bit value: %x\n", val);
    }
    return ((int)val);
    break;
  }
}





// Find variable block of interest in the file.  Leaves the file
// pointer pointing after the two 4-byte type/pointer bytes.  The
// file pointer should be pointing at the first data byte of the
// block after calling this, which is normally an 8/16/32-bit value
// specifying the number of elements in the block.
//
// Returns -1 if block not found
//          0 if block found
//
// For version 3.x/4.x files.
//
int TpoFormatBase::tpo_find_block(unsigned int block_desired)
{
  unsigned int block_type;
  constexpr int debug = 0;

  // Skip 512 byte fixed-length header
  unsigned int block_offset = 512;

  do {

    // Seek to offset from start of file
    gbfseek(tpo_file_in, block_offset, SEEK_SET);

    // Read record type
    block_type = gbfgetint32(tpo_file_in);
    if constexpr(debug) {
      db.log("Block: %08x\tat offset: %08x\n", block_type, block_offset);
    }

    // Read offset to next record
    block_offset = gbfgetint32(tpo_file_in);
  } while (block_type != block_desired && block_offset != 0);

  if (block_type == block_desired) {
    return (0);
  } else {
    return (-1);
  }
}





// Convert lat/long to normal values, save in waypoint struct
//
// For version 3.x files.
//
Waypoint* TpoFormatBase::tpo_convert_ll(int lat, int lon)
{
  auto* waypoint_temp = new Waypoint;

  double latitude = (double)lat / 0x800000;
  double longitude = (double)lon / 0x800000;

//printf("lat: %f\tlon: %f\n", latitude, longitude);

  /*
      // Note:  We shouldn't need this section of code as the version
      // 3.x files are already in WGS84 datum.
      //
      // Convert incoming NAD27/CONUS coordinates to WGS84, Molodensky
      // transform.
      //
      GPS_Math_Known_Datum_To_WGS84_M(
          latitude,                   // Source latitude
          longitude,                  // Source longitude
          0.0,                        // Source height (meters)
          &waypoint_temp->latitude,   // Dest latitude
          &waypoint_temp->longitude,  // Dest longitude
          &height,                    // Dest height (meters)
          78);
  */

  waypoint_temp->latitude = latitude;
  waypoint_temp->longitude = longitude;

//printf("lat: %f\tlon: %f\tNew Height: %f\n", waypoint_temp->latitude, waypoint_temp->longitude, height);

  return (waypoint_temp);
}

// Track decoder for version 3.x/4.x files.
// This block contains tracks (called "freehand routes" in Topo).
void TpoFormatBase::tpo_process_tracks()
{
  constexpr int debug = 0; // 0-4 for increasingly verbose output in this subroutine)

  if constexpr(debug) {
    db.log("Processing Track Styles... (added in 2012 by SRE)\n");
  }
  // Find block 0x050000 (definitions of styles for free-hand routes)
  if (tpo_find_block(0x050000)) {
    if constexpr(debug) {
      db.log("Found no track styles, skipping tracks entirely\n");
    }
    return;
  }
  // Read the number of track styles.
  unsigned int track_style_count = tpo_read_int(); // 8 bit value

  if constexpr(debug) {
    db.log("Unpacking %u track styles...\n",track_style_count);
  }

  QScopedArrayPointer<StyleInfo> styles(new StyleInfo[track_style_count]);

  for (unsigned ii = 0; ii < track_style_count; ii++) {

    // clumsy way to skip two undefined bytes (compiler should unwind this)
    for (unsigned xx = 0; xx < 2; xx++) {
      unsigned int skipped = (unsigned char) gbfgetc(tpo_file_in);
      Q_UNUSED(skipped)
      if constexpr(debug > 1) {
        db.log("Skipping unknown byte 0x%x (? per-zoom-level visibility ?)\n", skipped);
      }
    }

    // next three bytes are RGB color, fourth is unknown
    // Topo and web uses rrggbb, also need line_color.bbggrr for KML
    for (unsigned char& xx : styles[ii].color) {
      int col = gbfgetc(tpo_file_in);
      if ((col < 0) || (col >255)) {
        col = 0; // assign black if out of range (0x00 to 0xff)
      }
      xx = (uint8_t)col;
    }

    unsigned char tmp = gbfgetc(tpo_file_in);
    Q_UNUSED(tmp)
    if constexpr(debug > 2) {
      db.log("Skipping unknown byte 0x%x after color (? always zero ?)\n",tmp);
    }

    // byte for track style name length, then name itself
    tmp = gbfgetc(tpo_file_in);
    // wrong byte order?? tmp = tpo_read_int(); // 16 bit value
    if constexpr(debug > 1) {
      db.log("Track style %u has %d-byte (0x%x) name\n", ii, tmp, tmp);
    }
    if (tmp >= TRACKNAMELENGTH) {
      warning("ERROR! Found track style name over %d chars, skipping all tracks!\n",TRACKNAMELENGTH);
      return;
    }
    if (tmp) {
      styles[ii].name = gbfreadbuf(tmp, tpo_file_in);
    } else { // Assign a generic style name
      styles[ii].name = QStringLiteral("STYLE %1").arg(ii);
    }
#ifdef Tracks2012
    //TBD: Should this be TRACKNAMELENGTH?
    for (unsigned xx = 0; xx < 3; xx++) {
      if (styles[ii].name[xx] == ',') {
        styles[ii].name[xx] = '_';
      }
      if (styles[ii].name[xx] == '=') {
        styles[ii].name[xx] = '_';
      }
    }
#else
    // Should limit be TRACKNAMELENGTH? No! But also should not be '3' like it was.
    styles[ii].name.replace(',', '_');
    styles[ii].name.replace('=', '_');
#endif

    // one byte for line width (value 1-4), one byte for 'dashed' boolean
    styles[ii].wide = (uint8_t) gbfgetc(tpo_file_in);
    styles[ii].dash = (uint8_t) gbfgetc(tpo_file_in);

    // clumsy way to skip two undefined bytes
    for (unsigned xx = 0; xx < 2; xx++) {
      tmp = gbfgetc(tpo_file_in);
      if constexpr(debug > 2) {
        db.log("Skipping trailing line style byte 0x%x (? always zero ?)\n", tmp);
      }
    }

    if constexpr(debug) {
      db.log("Track style %u: color=#%02x%02x%02x, width=%d, dashed=%d, name=%s\n",
             ii, styles[ii].color[0], styles[ii].color[1], styles[ii].color[2], styles[ii].wide, styles[ii].dash, qPrintable(styles[ii].name));
    }
  }

  if constexpr(debug) {
    db.log("Done Processing Track Styles... found %u styles\n", track_style_count);
  }

  // Find block 0x060000 (free-hand routes) (original track code, pre-2012, without styles)
  if (tpo_find_block(0x060000)) {
    if constexpr(debug) {
      db.log("Found no track data block, skipping all tracks!\n");
    }
    return;
  }

  // Read the number of tracks.  Can be 8/16/32-bit value.
  unsigned int track_count = tpo_read_int();

  if constexpr(debug) {
    db.log("Number of tracks in file: %u\n", track_count);
  }

  if (track_count == 0) {
    if constexpr(debug) {
      db.log("Found no track data, even though there was a track data block!\n");
    }
    return;
  }

  // Read/process each track in the file
  //
  for (unsigned ii = 0; ii < track_count; ii++) {
    if constexpr(debug > 1) {
      db.log("\nStarting Track %u",ii+1);
    }
    int lat = 0;
    int lon = 0;

    // Allocate the track struct
    auto* track_temp = new route_head;
    track_add_head(track_temp);

//UNKNOWN DATA LENGTH
    unsigned int line_type = tpo_read_int(); // always zero??

    // Can be 8/16/32-bit value (defined in 2012, ignored before then)
    unsigned int track_style = tpo_read_int(); // index into freehand route styles defined in this .tpo file
    track_style -= 1;  // STARTS AT 1, whereas style arrays start at 0

    // Can be 8/16/32-bit value - never used? length in meters?
    double track_length = tpo_read_int();

//UNKNOWN DATA LENGTH
    unsigned int name_length = tpo_read_int();
    QString track_name;
    if (name_length) {
      gbfread(track_name, 1, name_length, tpo_file_in);
      if constexpr(debug > 2) {
        db.log(", length %.0fm?, named %s\n", track_length, qPrintable(track_name));
      }
    } else { // Assign a generic track name
      track_name = "TRK ";
      track_name += QString::number(ii + 1);
      if constexpr(debug > 2) {
        db.log(", length %.0fm?, inventing name %s\n", track_length, qPrintable(track_name));
      }
    }
    track_temp->rte_name = track_name;

    // RGB line_color expressed for html=rrggbb and kml=bbggrr - not assigned before 2012
    auto rgb = QString::asprintf("%02x%02x%02x",
                                 styles[track_style].color[0],
                                 styles[track_style].color[1],
                                 styles[track_style].color[2]);
    int bbggrr = styles[track_style].color[2] << 16 |
                 styles[track_style].color[1] << 8 |
                 styles[track_style].color[0];
    track_temp->line_color.bbggrr = bbggrr;

    // track texture (dashed=1, solid=0) mapped into opacity - not assigned before 2012
    track_temp->line_color.opacity = 0xff;   // 255
    if (styles[track_style].dash) {
      track_temp->line_color.opacity = 0x50;
    }

    // track width, from 1=hairline to 4=thick in Topo - not assigned before 2012
    //  (what are correct values for KML or other outputs??)
    track_temp->line_width = styles[track_style].wide;

    if constexpr(debug) {
      db.log("Track Name: %s, ?Type?: %u, Style Name: %s, Width: %d, Dashed: %d, Color: #%s\n",
             qPrintable(track_name), line_type,
             qPrintable(styles[track_style].name),
             styles[track_style].wide, styles[track_style].dash,
             qPrintable(rgb));
    }

    // Track description
    track_temp->rte_desc = QString("Style=%1, Width=%2, Dashed=%3, Color=#%4")
                           .arg(styles[track_style].name)
                           .arg(styles[track_style].wide)
                           .arg(styles[track_style].dash)
                           .arg(rgb);

    // Route number
    track_temp->rte_num = ii + 1;

//UNKNOWN DATA LENGTH
    unsigned int track_byte_count = tpo_read_int();

    // Read the number of bytes specified for the track.  These
    // contain scaling factors, lon/lat's, and offsets from
    // those long/lat's.  First will be a lon/lat (8 bytes).
    // Keep track of the bytes as we go so that we know how many
    // we've read.  We need to do this so that we start at the
    // proper place for the next track.

    /* track points vs. hex data - best guess as of 2020
     - must start with full coords for track point, 8 bytes total (4 lon, then 4 lat)
     - note that most significant byte is at the right end below
     - double zero bytes in the next 4-byte segment implies scale factor (possible future bug)
     - WARNING: scale factor appears to be either lon or lon-then-lat (may be 4 or 8 bytes!)
     - after read scale factors, 1 or 2, next data is a single byte per scaled trackpoint
     - zero single byte implies end to scaled points, full point OR scaling follows
     - full point is followed by scaled points UNLESS next byte is 0x88 tag
     - scale factor(s) remain(s) the same until re-specified

    SAMPLE HEX DATA:
    lon tpt 1 = byte 0: 44 7e a7 c4 (most  significant bytes can NOT be zero for points - REALLY??)
    lat tpt 2 = byte 4: f1 22 4d 12 (least significant bytes can NOT be 0x88 for points - REALLY??)
        + after full point, can have scaling or 0x88 for another full point or single byte to be scaled
    lonscale =  byte 8: fe 00 00 00 (look-ahead in segments of 4 bytes, 2 zeros implies scaling)
    latscale =  byte12: cd 00 00 00
        + after scaling (one or two), can have 0x88 for full point or more scaling or single byte to be scaled
    adj tpt 2     byte 16: f4
    adj tpt 3     byte 17: 04
    adj tpt 4     byte 18: 04
    zero found at byte 19: 00 (next will be full wpt, because next does not have two zeros)
        + list of single bytes to be scaled can only end with 0x00, can then have full point or scaling
    lon tpt 5 = byte 20: f2 7c a7 c4
    lat tpt 5 = byte 24: 16 30 4d 12
        + after full point, can have scaling or 0x88 for another full point or single byte to be scaled
    adj tpt 6     byte 28: 20
    adj tpt 7     byte 29: 13
    adj tpt 8     byte 30: dc
    zero found at byte 31: 00 (but next is NOT full wpt) two zeros case not handled until 2020
        + list of single bytes to be scaled can only end with 0x00, can then have full point or scaling
    lonscale =  byte 32: 66 00 00 00 (apparently OK to skip latscale, and to specify scale but not use it!)
        + after scaling (one or two), can have 0x88 for full point or more scaling or single byte to be scaled
    0x88 at byte 36: 88 (next will be full wpt)
    lon: f6 7a a7 c4
    lat: 7d 36 4d 12
        + after full point, can have scaling or 0x88 for another full point or single byte to be scaled
    0x88 at byte 45: 88 (next will be full wpt)
    lon: fe 76 a7 c4
    lat: e3 3c 4d 12
    (etc)
    */

    // Read the track bytes into a buffer
    QScopedArrayPointer<unsigned char> buf(new unsigned char[track_byte_count]);
    gbfread(buf.get(), 1, track_byte_count, tpo_file_in);

    // these can be set repeatedly, and retain their value between settings
    //  (even if not used for every trackpoint)
    int latscale = 0;
    int lonscale = 0;

#ifdef Tracks2012
    int llvalid = 0; // boolean has been replaced with multiple modes
#else
#define EndScalePoints 0
#define CheckLonScale  1
#define CheckLatScale  2
#define Check0x88Tag   3
#define ScaleOneByte   4
#define CheckZeroTag   5
#define GetFullPoint   6
    const char* tpmodeshow[7] = { "EndScalePoints", "CheckLonScale", "CheckLatScale", "Check0x88Tag", "ScaleOneByte", "CheckZeroTag", "GetFullPoint" };
    int tpmode = GetFullPoint; // prior to 2020 we used "llvalid" (boolean), which did not provide enough flow control
#define EndScaleTag    0x00
#define FullPointTag   0x88
#endif

    // Process the track bytes - ugly flow control due to many special cases in file structure
    int cnttp = 0; // just for debug stroking
    float lastlat = 0.0; // to catch discontinuities we can't seem to fix
    float lastlon = 0.0;
    for (unsigned int jj = 0; jj < track_byte_count;) { // NO INCREMENT - advance "jj" in the loop
      Waypoint* waypoint_temp;
#ifdef Tracks2012
      if constexpr(debug > 3) {
        db.log("%02x %02x %02x %02x - byte %u, track %u, llvallid=%d\n",
               buf[jj], buf[jj+1], buf[jj+2], buf[jj+3], jj, ii+1, llvalid);
      }
      // Time to read a new latlong?
      if (!llvalid) {

        lon = le_read32(&buf[jj]);
        if constexpr(debug > 3) {
          db.log("%02x %02x %02x %02x - raw lon = %d (byte %u)\n", buf[jj], buf[jj+1], buf[jj+2], buf[jj+3], lon,jj);
        }
        jj+=4;

        lat = le_read32(&buf[jj]);
        if constexpr(debug > 3) {
          db.log("%02x %02x %02x %02x - raw lat = %d (byte %u)\n", buf[jj], buf[jj+1], buf[jj+2], buf[jj+3], lat,jj);
        }
        jj+=4;

//printf("L");

        // Peek to see if next is a lonscale.  Note that it
        // can begin with 0x88, which is confusing.  Here we
        // allow up to 16-bits of offset, so two of the
        // bytes must be 0x00 for us to recognize it.
        if (jj+3<track_byte_count
            && !buf[jj+3]
            && !buf[jj+2]) {

          lonscale = le_read32(&buf[jj]);
          if constexpr(debug > 3) {
            db.log("%02x %02x %02x %02x - raw lon scale = %d (byte %u)\n", buf[jj], buf[jj+1], buf[jj+2], buf[jj+3], lonscale, jj);
          }
//printf(" LONSCALE:");
//printf("%02x%02x%02x%02x", buf[jj], buf[jj+1], buf[jj+2], buf[jj+3]);
          jj+=4;
        }
        // Peek to see if next is a latscale.  Note that it
        // can begin with 0x88, which is confusing.  Here we
        // allow up to 16-bits of offset, so two of the
        // bytes must be 0x00 for us to recognize it.
        if (jj+3<track_byte_count
            && !buf[jj+3]
            && !buf[jj+2]) {

          latscale = le_read32(&buf[jj]);
          if constexpr(debug > 3) {
            db.log("%02x %02x %02x %02x - raw lat scale = %d (byte %u)\n", buf[jj], buf[jj+1], buf[jj+2], buf[jj+3], latscale, jj);
          }
//printf(" LATSCALE:");
//printf("%02x%02x%02x%02x ", buf[jj], buf[jj+1], buf[jj+2], buf[jj+3]);
          jj+=4;
        }
        llvalid = 1;

        waypoint_temp = tpo_convert_ll(lat, lon);
        track_add_wpt(track_temp, waypoint_temp);
        cnttp++;
        if constexpr(debug > 3) {
          db.log("Adding BASIC trackpoint #%i: lat=%.5f, lon=%.5f\n", cnttp, waypoint_temp->latitude, waypoint_temp->longitude);
        }
      }
#else
      if constexpr(debug > 3) {
        db.log("%02x %02x %02x %02x = bytes %u-%u (track %u, mode now %s)\n",
               buf[jj], buf[jj+1], buf[jj+2], buf[jj+3], jj, jj+3, ii+1, tpmodeshow[tpmode]);
      }

      // read 8-byte lon+lat, required at start of track or after 0x88 tag
      if (tpmode == GetFullPoint) {
        lon = le_read32(&buf[jj]);
        if constexpr(debug > 3) {
          db.log("%02x %02x %02x %02x - raw lon = %d (byte %u)\n", buf[jj], buf[jj+1], buf[jj+2], buf[jj+3], lon,jj);
        }
        jj+=4;

        lat = le_read32(&buf[jj]);
        if constexpr(debug > 3) {
          db.log("%02x %02x %02x %02x - raw lat = %d (byte %u)\n", buf[jj], buf[jj+1], buf[jj+2], buf[jj+3], lat,jj);
        }
        jj+=4;

        waypoint_temp = tpo_convert_ll(lat, lon);
        track_add_wpt(track_temp, waypoint_temp);
        cnttp++;
        if (((abs(waypoint_temp->latitude - lastlat) > 1.0) && lastlat) || ((abs(waypoint_temp->longitude - lastlon) > 1.0) && lastlon)) {
          warning("WARNING! Track '%s' point #%d is more than 1 degree from the last track point!\n  (probably corrupt - try splitting in two at sharp corners)\n", qPrintable(track_name), cnttp);
        }
        lastlat = waypoint_temp->latitude;
        lastlon = waypoint_temp->longitude;

        if constexpr(debug > 3) {
          db.log("Adding BASIC trackpoint #%i: lat=%.5f, lon=%.5f\n", cnttp, waypoint_temp->latitude, waypoint_temp->longitude);
        }

        // after full point, can have scaling or 0x88 for another full point or single byte to be scaled
        if ((jj+3<track_byte_count) && !(buf[jj+3]) && !(buf[jj+2])) {
          tpmode = CheckLonScale;
        } else {
          tpmode = Check0x88Tag;
        }
        continue; // for jj
      }

      // look ahead to identify 4 bytes of scale factor (lon first, then lat optional)
      // last two bytes must be zero, allow 16 bits of "offset" to calculate trackpoints from deltas
      // Note that lonscale can begin with 0x88, which should not be confused with GetFullPoint tags.
      if (tpmode == CheckLonScale) {
        if ((jj+3<track_byte_count) && !(buf[jj+3]) && !(buf[jj+2])) {
          lonscale = le_read32(&buf[jj]);
          if constexpr(debug > 3) {
            db.log("%02x %02x %02x %02x - raw lon scale = %d (byte %u)\n", buf[jj], buf[jj+1], buf[jj+2], buf[jj+3], lonscale, jj);
          }
          jj+=4;
          tpmode = CheckLatScale;
          continue; // for jj
        }

        // after scaling (one or two), can have 0x88 for full point or more scaling (above) or single byte to be scaled
        tpmode = Check0x88Tag;
        continue; // for jj
      }

      // look ahead to identify 4 bytes of scale factor (lat is optional)
      // last two bytes must be zero, allow 16 bits of "offset" to calculate trackpoints from deltas
      // Note that latscale can begin with 0x88, which should not be confused with GetFullPoint tags.
      if (tpmode == CheckLatScale) {
        if ((jj+3<track_byte_count) && !(buf[jj+3]) && !(buf[jj+2])) {
          latscale = le_read32(&buf[jj]);
          if constexpr(debug > 3) {
            db.log("%02x %02x %02x %02x - raw lat scale = %d (byte %u)\n", buf[jj], buf[jj+1], buf[jj+2], buf[jj+3], latscale, jj);
          }
          jj+=4;
        }

        // after scaling (two), can have 0x88 for full point or single byte to be scaled
        tpmode = Check0x88Tag;
        continue; // for jj
      }
#endif

#ifdef Tracks2012
      // Check whether there's a lonlat coming up instead of
      // offsets.
      else if (buf[jj] == 0x88) {
        if constexpr(debug > 3) {
          db.log("%02x should mean full lat/lon comes next (byte %u)\n",buf[jj],jj);
        }
        jj++;
        llvalid = 0;
      }
#else
      // Check whether 8 bytes of lon+lat are next, instead of offsets or another scaling spec.
      // 0x88 is a tag that signals a full trackpoint will follow
      if (tpmode == Check0x88Tag) {
        if (buf[jj] == FullPointTag) {
          if constexpr(debug > 3) {
            db.log("%02x should mean full lat/lon comes next (byte %u)\n",buf[jj],jj);
          }
          jj++;
          tpmode = GetFullPoint;
          continue;
        }
        tpmode = ScaleOneByte; // only if no 0x88 tag
        continue; // for jj
      }
#endif

#ifdef Tracks2012
      // Check whether there's a lonlat + lonscale/latscale
      // combo embedded in this track next.
      else if (buf[jj] == 0x00) {
        if constexpr(debug > 3) {
          db.log("%02x should mean full lat/lon or lonscale/latscale comes next (at byte %u)\n",buf[jj],jj);
        }
//printf(" ZERO ");
        jj++;
        llvalid = 0;
      }

      // Process the delta
      else {
        static const int scarray[] = {0,1,2,3,4,5,6,7,-8,-7,-6,-5,-4,-3,-2,-1};
        if constexpr(debug) {
          db.log("%02x - lat mult = %d, lon mult=%d, byte %u\n", buf[jj], scarray[buf[jj] & 0xf], scarray[buf[jj] >> 4], jj);
        }


        if (buf[jj] == 0) {
          fatal("Found unexpected ZERO\n");
        }

        if (latscale == 0 || lonscale == 0) {
          fatal("Found bad scales lonscale=0x%x latscale=0x%x\n", lonscale, latscale);
        }


        if constexpr(debug > 3) {
          db.log("%02x - adjusting prev lat/lon from %i/%i", buf[jj], lat, lon);
        }
        lon += lonscale * scarray[buf[jj] >> 4];
        lat += latscale * scarray[(buf[jj] & 0xf)];
        if constexpr(debug > 3) {
          db.log(" to %i/%i, byte %u\n", lat, lon, jj);
        }
//printf(".");
        jj++;

        waypoint_temp = tpo_convert_ll(lat, lon);
        track_add_wpt(track_temp, waypoint_temp);
        cnttp++;
        if constexpr(debug > 3) {
          db.log("Adding ADJUSTED trackpoint #%i: lat=%.5f, lon=%.5f\n", cnttp, waypoint_temp->latitude, waypoint_temp->longitude);
        }
      }
#else
      // ScaleOneByte applies lonscale and latscale to a single byte to create an 8-byte lat+lon
      // EndScalePoints (0) is a tag that signals an end to adjusted trackpoints (full point or scale may follow)
      if (tpmode == ScaleOneByte) {

        // list of single bytes to be scaled can only end with 0x00, can then have full point or scaling
        if (buf[jj] == EndScaleTag) {
          if constexpr(debug > 3) {
            db.log("%02x should mean full lat/lon or lonscale/latscale comes next (at byte %u)\n",buf[jj],jj);
          }
          jj++;
          tpmode = GetFullPoint;
          if ((jj+3<track_byte_count) && !(buf[jj+3]) && !(buf[jj+2])) {
            tpmode = CheckLonScale;
          }
          continue; // for jj
        }

        if (buf[jj] == FullPointTag) {
          if constexpr(debug > 3) {
            db.log("%02x should mean full lat/lon comes next (at byte %u)\n",buf[jj],jj);
          }
          jj++;
          tpmode = GetFullPoint;
          continue; // for jj
        }

        // Process the delta: stored only one byte per track point, expand into full coords here
        static const int scarray[] = {0,1,2,3,4,5,6,7,-8,-7,-6,-5,-4,-3,-2,-1}; // MAGIC! (no idea where this comes from)

        if constexpr(debug) {
          db.log("%02x - lat mult = %d, lon mult=%d, byte %u\n", buf[jj], scarray[buf[jj] & 0xf], scarray[buf[jj] >> 4], jj);
        }
        if (buf[jj] == 0) {
          fatal("Found unexpected ZERO\n");
        }

        if ((latscale == 0) || (lonscale == 0)) {
          fatal("Found bad scales lonscale=0x%x latscale=0x%x while trying to scale a single byte trackpoint\n", lonscale, latscale);
        }

        if constexpr(debug > 3) {
          db.log("%02x - adjusting prev lat/lon from %i/%i", buf[jj], lat, lon);
        }
        lon += lonscale * scarray[buf[jj] >> 4];
        lat += latscale * scarray[(buf[jj] & 0xf)];
        if constexpr(debug > 3) {
          db.log(" to %i/%i, byte %u\n", lat, lon, jj);
        }
        jj++;

        waypoint_temp = tpo_convert_ll(lat, lon);
        track_add_wpt(track_temp, waypoint_temp);
        cnttp++;
        if constexpr(debug > 3) {
          db.log("Adding ADJUSTED trackpoint #%i: lat=%.5f, lon=%.5f\n", cnttp, waypoint_temp->latitude, waypoint_temp->longitude);
        }

        if (((abs(waypoint_temp->latitude - lastlat) > 1) && lastlat) || ((abs(waypoint_temp->longitude - lastlon) > 1) && lastlon)) {
          warning("WARNING! Track '%s' point #%i is more than 1 degree from the last track point!\n  (probably corrupt - try splitting in two at sharp corners)\n", qPrintable(track_name), cnttp);
        }
        lastlat = waypoint_temp->latitude;
        lastlon = waypoint_temp->longitude;
      } // if ScaleOneByte
#endif

    } // end for jj track_byte_count
  } // end for ii track_count
} // end of tpo_process_tracks


// Waypoint decoder for version 3.x files.
//
void TpoFormatBase::tpo_process_waypoints(QList<Waypoint>& tpo_wp_index)
{
  //printf("Processing Waypoints...\n");

  // Find block 0x0e0000 (GPS-Waypoints)
  if (tpo_find_block(0x0e0000)) {
    return;
  }

  // Read the number of waypoints.  8/16/32-bit value.
  unsigned int waypoint_count = tpo_read_int();

//printf("Total Waypoints: %d\n", waypoint_count);

  // Fetch storage for the waypoint index (needed later for
  // routes)
  tpo_wp_index.clear();
  tpo_wp_index.reserve(waypoint_count);

  if (waypoint_count == 0) {
    return;
  }

  // Read/process each waypoint in the file
  for (unsigned int ii = 0; ii < waypoint_count; ii++) {
    //UNKNOWN DATA LENGTH
    (void)tpo_read_int(); // 0x00

//UNKNOWN DATA LENGTH
    (void)tpo_read_int(); // 0x00

//UNKNOWN DATA LENGTH
    // Fetch name length
    unsigned int name_length = tpo_read_int();
    QString waypoint_name;
    if (name_length) {
      gbfread(waypoint_name, 1, name_length, tpo_file_in);
    } else { // Assign a generic waypoint name
      waypoint_name = "WPT ";
      waypoint_name += QString::number(ii + 1);
    }

//UNKNOWN DATA LENGTH
    (void)tpo_read_int();

    int lon = gbfgetint32(tpo_file_in);
    int lat = gbfgetint32(tpo_file_in);

    // Allocate space for waypoint and store lat/lon
    Waypoint* waypoint_temp = tpo_convert_ll(lat, lon);

    // Assign the waypoint name
    waypoint_temp->shortname = waypoint_name;

    // Grab the altitude in centimeters
    int altitude = gbfgetint32(tpo_file_in);
    // The original untested check for unknown altitude was for 0xfffd000c (-196596 cm),
    // but a test case submitted later used 0xffce0000 (-3276800 cm).
    if (altitude == -3276800) { // Unknown altitude
      /* this is unnecessary, the constructor will do this anyway. */
      waypoint_temp->altitude = unknown_alt;
    } else {
      waypoint_temp->altitude = (double) altitude / 100.0;   // Meters
    }
//printf("\tAltitude: %1.2f meters\n", waypoint_temp->altitude);

//UNKNOWN DATA LENGTH
    // Fetch comment length
    name_length = tpo_read_int();
//printf("\tComment length: %d\n", name_length);
    if (name_length) {
      waypoint_temp->description = gbfreadbuf(name_length, tpo_file_in);
    }

    // For routes (later), we need a duplicate of each waypoint
    // indexed by the order we read them in.
    // Attach a copy to our index.
    tpo_wp_index.append(*waypoint_temp);

    // Add the original waypoint to the chain of waypoints
    waypt_add(waypoint_temp);

//UNKNOWN DATA LENGTH
//        (void)tpo_read_int();
    (void) gbfgetc(tpo_file_in);

//UNKNOWN DATA LENGTH
//        (void)tpo_read_int();
    (void) gbfgetc(tpo_file_in);

//UNKNOWN DATA LENGTH
//        (void)tpo_read_int();
    (void) gbfgetc(tpo_file_in);

//UNKNOWN DATA LENGTH
//        (void)tpo_read_int();
    (void) gbfgetc(tpo_file_in);
  }
}





// Map Notes decoder for version 3.x files.
//
void TpoFormatBase::tpo_process_map_notes()
{
  //printf("Processing Map Notes...\n");

  // Find block 0x090000 (Map Notes)
  if (tpo_find_block(0x090000)) {
    return;
  }

  // Read the number of waypoints.  8/16/32-bit value.
  unsigned int waypoint_count = tpo_read_int();

//printf("Elements: %d\n", waypoint_count);

  if (!waypoint_count) {
    return;
  }

  // Process each waypoint
  for (unsigned int ii = 0; ii < waypoint_count; ii++) {
    //UNKNOWN DATA LENGTH
    (void)tpo_read_int();

    int lon = gbfgetint32(tpo_file_in);
    int lat = gbfgetint32(tpo_file_in);

    // Allocate space for waypoint and store lat/lon
    Waypoint* waypoint_temp = tpo_convert_ll(lat, lon);

    // Assign a generic waypoint name
    waypoint_temp->shortname = QStringLiteral("NOTE %1").arg(ii + 1);

//UNKNOWN DATA LENGTH
    (void)tpo_read_int();

//UNKNOWN DATA LENGTH
    (void)tpo_read_int();

//UNKNOWN DATA LENGTH
    (void)tpo_read_int();

//UNKNOWN DATA LENGTH
    // Fetch comment length
    unsigned int name_length = tpo_read_int();
    if (name_length) {
      waypoint_temp->description = gbfreadbuf(name_length, tpo_file_in);
    }

    // Length of text for external path.  If non-zero, skip past
    // the text.
//UNKNOWN DATA LENGTH
    name_length = tpo_read_int();
//printf("name_length: %x\n", name_length);
    if (name_length) {
      QString notes = gbfreadbuf(name_length, tpo_file_in);
      waypoint_temp->AddUrlLink(notes);
    }

    // Length of text for image path.  If non-zero, skip past
    // the text.
//UNKNOWN DATA LENGTH
    name_length = tpo_read_int();
    if (name_length) {
      QString notes = gbfreadbuf(name_length, tpo_file_in);
      waypoint_temp->AddUrlLink(notes);
    }

//UNKNOWN DATA LENGTH
    (void)tpo_read_int();

//UNKNOWN DATA LENGTH
    (void)tpo_read_int();

    // Number of bytes to skip until next element or end of
    // block.  May be 8/16/32 bits.
    unsigned int num_bytes = tpo_read_int();
//printf("num_bytes: %x\n", num_bytes);
    for (unsigned int jj = 0; jj < num_bytes; jj++) {
      (void) gbfgetc(tpo_file_in); // Skip bytes
    }

    // Can be 8/16/32 bits
    (void)tpo_read_int();

//UNKNOWN DATA LENGTH
    (void)tpo_read_int();

    // Add the waypoint to the chain of waypoints
    waypt_add(waypoint_temp);
  }
}





// Symbols decoder for version 3.x files.
//
void TpoFormatBase::tpo_process_symbols()
{
  //printf("Processing Symbols...\n");

  // Find block 0x040000 (Symbols)
  if (tpo_find_block(0x040000)) {
    return;
  }

  // Read the number of waypoints.  8/16/32-bit value.
  unsigned int waypoint_count = tpo_read_int();

//printf("Elements: %d\n", waypoint_count);

  if (!waypoint_count) {
    return;
  }

  // Process each waypoint
  for (unsigned int ii = 0; ii < waypoint_count; ii++) {
    //UNKNOWN DATA LENGTH
    (void)tpo_read_int();

//UNKNOWN DATA LENGTH
    (void)tpo_read_int();

    int lon = gbfgetint32(tpo_file_in);
    int lat = gbfgetint32(tpo_file_in);

    // Allocate space for waypoint and store lat/lon
    Waypoint* waypoint_temp = tpo_convert_ll(lat, lon);

    // Assign a generic waypoint name
    waypoint_temp->shortname = QStringLiteral("SYM %1").arg(ii + 1);

    // Add the waypoint to the chain of waypoints
    waypt_add(waypoint_temp);
  }
}


// Text Labels decoder for version 3.x files.
//
void TpoFormatBase::tpo_process_text_labels()
{
  //printf("Processing Text Labels...\n");

  // Find block 0x080000 (Text Labels)
  if (tpo_find_block(0x080000)) {
    return;
  }

  // Read the number of waypoints.  8/16/32-bit value.
  unsigned int waypoint_count = tpo_read_int();

//printf("Elements: %d\n", waypoint_count);

  if (!waypoint_count) {
    return;
  }

  // Process each waypoint
  for (unsigned int ii = 0; ii < waypoint_count; ii++) {
    //UNKNOWN DATA LENGTH
    (void)tpo_read_int();

//UNKNOWN DATA LENGTH
    (void)tpo_read_int();

    int lon = gbfgetint32(tpo_file_in);
    int lat = gbfgetint32(tpo_file_in);

    // Allocate space for waypoint and store lat/lon
    Waypoint* waypoint_temp = tpo_convert_ll(lat, lon);

    // Assign a generic waypoint name
    waypoint_temp->shortname = QStringLiteral("TXT %1").arg(ii + 1);

    for (int jj = 0; jj < 16; jj++) {
//UNKNOWN DATA LENGTH
      (void) gbfgetc(tpo_file_in);
    }

    // Fetch comment length
//UNKNOWN DATA LENGTH
    unsigned int name_length = tpo_read_int();
    if (name_length) {
      QString comment;
      gbfread(comment, 1, name_length, tpo_file_in);
      waypoint_temp->description = comment;
    }

    // Add the waypoint to the chain of waypoints
    waypt_add(waypoint_temp);
  }
}





// Route decoder for version 3.x files.
//
// We depend on tpo_wp_index having been filled-in
// with waypoint objects by the tpo_process_waypoints()
// function above.
//
void TpoFormatBase::tpo_process_routes(const QList<Waypoint>& tpo_wp_index)
{
  //printf("Processing Routes...\n");

  // Find block 0x0f0000 (GPS-Routes)
  if (tpo_find_block(0x0f0000)) {
    return;
  }

  // Read the number of routes.  8/16/32-bit value
  unsigned int route_count = tpo_read_int();

//printf("Total Routes: %d\n", route_count);

  if (route_count == 0) {
    return;
  }

  // Read/process each route in the file
  //
  for (unsigned int ii = 0; ii < route_count; ii++) {
    // Allocate the route struct
    auto* route_temp = new route_head;
    route_add_head(route_temp);

//UNKNOWN DATA LENGTH
    (void)tpo_read_int();

//UNKNOWN DATA LENGTH
    (void)tpo_read_int();

//UNKNOWN DATA LENGTH
    // Fetch name length
    unsigned int name_length = tpo_read_int();
    QString route_name;
    if (name_length) {
      gbfread(route_name, 1, name_length, tpo_file_in);
    } else { // Assign a generic route name
      route_name = "RTE ";
      route_name += QString::number(ii + 1);
    }
    route_temp->rte_name = route_name;

//printf("Route Name: %s\n", route_name);

//UNKNOWN DATA LENGTH
    // Comment?
    (void)tpo_read_int();
//        gbfgetc(tpo_file_in);
//        route_temp->rte_desc = NULL;

    route_temp->rte_num = ii + 1;

    // Fetch the number of waypoints in this route.  8/16/32-bit
    // value.
    unsigned int waypoint_cnt = tpo_read_int();


    // Run through the list of waypoints, look up each in our
    // index, then add the waypoint to this route.
    //
    for (unsigned int jj = 0; jj < waypoint_cnt; jj++) {
      //UNKNOWN DATA LENGTH
      // Fetch the index to the waypoint
      unsigned int val = tpo_read_int();
//printf("val: %x\t\t", val);

      // Duplicate a waypoint from our index of waypoints.
      auto* waypoint_temp = new Waypoint(tpo_wp_index[val-1]);

      // Add the waypoint to the route
      route_add_wpt(route_temp, waypoint_temp);
    }
//printf("\n");
  }
}





#ifdef DEAD_CODE_IS_REBORN
// Compass decoder for version 3.x files.
//
void TpoFormatBase::tpo_process_compass()
{

  // Not implemented yet
}
#endif





// Decoder for version 3.x files.  These files have "tracks"
// (called "freehand routes" or just "routes" in Topo), "waypoints",
// and "gps-routes".  We intend to read all three types.
//
void TpoFormatBase::tpo_read_3_x()
{
  // Global index to waypoints, needed for routes, filled in by
  // tpo_process_waypoints.
  //
  // For version 3.x files.
  QList<Waypoint> tpo_wp_index;

  if (doing_trks) {
//printf("Processing Tracks\n");
    tpo_process_tracks();
  }

  if (doing_wpts || doing_rtes) {
//printf("Processing Waypoints\n");
    tpo_process_waypoints(tpo_wp_index);
  }

  if (doing_rtes) {
    //
    // Note:  To process routes we _MUST_ process waypoints
    // first!  This creates the index of waypoints that we need
    // for routes.
    //
//printf("Processing Routes\n");
    tpo_process_routes(tpo_wp_index);
  }

  if (doing_wpts) {
    //
    // Other blocks in the file have waypoint-type information
    // in them.  Map Notes, Symbols, and Text Labels.  We
    // process those here and add them to the end of the
    // waypoint list.
    //
//printf("Processing Map Notes\n");
    tpo_process_map_notes();

//printf("Processing Symbols\n");
    tpo_process_symbols();

//printf("Processing Text Labels\n");
    tpo_process_text_labels();

//printf("Processing Compass Symbols\n");
//        tpo_process_compass();

  }
}





//-------------------------------------------------------------------
//-------------------------------------------------------------------





void
TpoFormatBase::tpo_rd_init(const QString& fname)
{
  tpo_file_in = gbfopen_le(fname, "rb");
  tpo_check_version_string();

  if (tpo_version == 2.0) {
    if (doing_wpts || doing_rtes) {
      fatal("this file format only supports tracks, not waypoints or routes.\n");
    }

    /*fprintf(stderr,"Version 2.x, Looking for CTopoRoute\n"); */
    /* Back up 18 bytes if this section found */
    tpo_read_until_section("CTopoRoute", -18);
  } else if (tpo_version == 3.0) {
    /*fprintf(stderr,"Version 3.x, Looking for 'Red Without Arrow'\n"); */
    /* Go forward four more bytes if this section found.  "IEND"
     * plus four bytes is the end of the embedded PNG image */
    tpo_read_until_section("Red Without Arrow", 17);
  } else {
    fatal("gpsbabel can only read TPO versions through 3.x.x\n");
  }
}

void
TpoFormatBase::tpo_rd_deinit()
{
  gbfclose(tpo_file_in);
}

void
TpoFormatBase::tpo_read()
{

  if (tpo_version == 2.0) {
//printf("\nFound a version 2.x file\n");
    tpo_read_2_x();
  } else if (tpo_version == 3.0) {
//printf("\nFound a version 3.x file\n");
    tpo_read_3_x();
  } else {
    fatal("gpsbabel can only read TPO versions through 3.x.x\n");
  }
}
