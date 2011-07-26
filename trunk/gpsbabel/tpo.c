/*
    National Geographic Topo! TPO file support.
    2.x support contributed to gpsbabel by Steve Chamberlin.
    3.x support contributed to gpsbabel by Curt Mills.

    Topo! version 2.x:  Tracks are implemented.
    Topo! version 3.x:  Reading of Tracks/Waypoints/Routes is
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
    Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111 USA

 */


/*
 ***** This format has problems and the author hasn't returned emails
 * on it, so we've set the format type to 'ff_type_internal' to make it
 * disappear from the various lists...
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

#include "defs.h"
#include <string.h>
#include <ctype.h>
#include "jeeps/gpsmath.h" /* for datum conversions */

#define MYNAME	"TPO"

static char *dumpheader = NULL;
static char *output_state = NULL;

/*
static
arglist_t tpo2_args[] = {
	{ "dumpheader", &dumpheader, "Display the file header bytes",
		"0", ARGTYPE_BOOL, ARG_NOMINMAX} ,
	{ "state", &output_state, "State map format to write, default=CA",
	  "CA", ARGTYPE_STRING, ARG_NOMINMAX} ,
	ARG_TERMINATOR
};
*/
//
// Note that we've disabled the write capabilites for the tpo2
// format at present.  The "testo" tests were failing on some
// platforms and there wasn't anyone willing to work on the problem.
// If this is fixed in the future we can go back to the tpo2_args[]
// above.
//
static
arglist_t tpo2_args[] = {
  ARG_TERMINATOR
};

static
arglist_t tpo3_args[] = {
  ARG_TERMINATOR
};


static gbfile *tpo_file_in;
static gbfile *tpo_file_out;
//static short_handle mkshort_handle;

static double output_track_lon_scale;
static double output_track_lat_scale;

static unsigned int track_out_count;
static double first_track_waypoint_lat;
static double first_track_waypoint_lon;
static double track_length;
static double last_waypoint_x;
static double last_waypoint_y;
static double last_waypoint_z;

/*******************************************************************************/
/*                                      READ                                   */
/*******************************************************************************/

/* Define a global here that we can query from multiple places */
float tpo_version = 0.0;

/* tpo_check_version_string()
   Check the first bytes of the file for a version 3.0 header. */
static void
tpo_check_version_string()
{

  unsigned char string_size;
  char* string_buffer;
  char* v3_id_string = "TOPO! Ver";

  /* read the id string */
  gbfread(&string_size, 1, 1, tpo_file_in);
  string_buffer = (char *) xmalloc(string_size+1);
  gbfread(string_buffer, 1, string_size, tpo_file_in);

  /* terminate the string */
  string_buffer[string_size] = 0;

  /* check for the presence of a 3.0-style id string */
  if (strncmp(v3_id_string, string_buffer, strlen(v3_id_string)) == 0) {
    /*		fatal(MYNAME ": gpsbabel can only read TPO version 2.7.7 or below; this file is %s\n", string_buffer); */
//fprintf(stderr,"gpsbabel can only read TPO version 2.7.7 or below; this file is %s\n", string_buffer);

    gbfseek(tpo_file_in, -(string_size+1), SEEK_CUR);
    xfree(string_buffer);
    tpo_version = 3.0;  /* Really any 3.x version */
    return;

  } else {
    /* We found a version 1.x or 2.x file */
    /* seek back to the beginning of the file */
    gbfseek(tpo_file_in, -(string_size+1), SEEK_CUR);
    xfree(string_buffer);
    tpo_version = 2.0;  /* Really any 1.x or 2.x version */
    return;
  }
}

static void
/* tpo_dump_header_bytes(int header_size)
   Write the first header_size bytes of the file to standard output
   as a C array definition. */
tpo_dump_header_bytes(int header_size)
{
  int i;
  unsigned char* buffer = (unsigned char*)(char *) xmalloc(header_size);

  gbfread(buffer, 1, header_size, tpo_file_in);

  printf("unsigned char header_bytes[] = {\n");

  for (i=0; i<header_size; i++) {
    if (i%8 == 0) {
      printf("    ");
    }
    printf("0x%02X", buffer[i]);
    if (i != header_size-1) {
      printf(", ");
    }
    if (i%8 == 7) {
      printf("\n");
    }
  }

  printf("};\n");

  xfree(buffer);
}

/* tpo_read_until_section()
   Keep reading bytes from the file until the section name is encountered,
   then go seek_bytes forwards (+) or backwards (-) to the start of
   the section data. */
static void
tpo_read_until_section(const char* section_name, int seek_bytes)
{
  char byte;
  unsigned int match_index = 0;
  int header_size = 0;

  while (1) {
    if (gbfread(&byte, 1, 1, tpo_file_in) < 1) {
      fatal(MYNAME ": malformed input file - attempt to read past end");
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

static void
tpo_rd_init(const char *fname)
{

  tpo_file_in = gbfopen_le(fname, "rb", MYNAME);
  tpo_check_version_string();

  if (tpo_version == 2.0) {
    if (doing_wpts || doing_rtes) {
      fatal(MYNAME ": this file format only supports tracks, not waypoints or routes.\n");
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
    fatal(MYNAME ": gpsbabel can only read TPO versions through 3.x.x\n");
  }
}

static void
tpo_rd_deinit(void)
{
  gbfclose(tpo_file_in);
}





//-------------------------------------------------------------------
//-------------------------------------------------------------------





// Decoder for version 2.x files.  This one does tracks only, as
// that is the only type of data available in the version 2.x TPO
// files.
//
void tpo_read_2_x(void)
{
  char buff[16];
  short track_count, waypoint_count;
  double first_lat, first_lon, lat_scale, lon_scale, amt;
  short *lon_delta, *lat_delta;
  int i, j;
  route_head* track_temp;
  waypoint* waypoint_temp;

  /* track count */
  track_count = gbfgetint16(tpo_file_in);

  /*fprintf(stderr,"track_count:%d\n", track_count);*/

  /* 4 unknown bytes */
  gbfread(&buff[0], 1, 4, tpo_file_in);

  /* chunk name: "CTopoRoute" */
  gbfread(&buff[0], 1, 12, tpo_file_in);

  for (i=0; i<track_count; i++) {

    track_temp = route_head_alloc();
    track_add_head(track_temp);

    /* generate a generic track name */
    sprintf(buff, "Track %d", i+1);
    track_temp->rte_name = xstrdup(buff);

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
    first_lon = gbfgetdbl(tpo_file_in);

    /* 8 bytes - latitude */
    first_lat = gbfgetdbl(tpo_file_in);

    /* swap sign before we do datum conversions */
    first_lon *= -1.0;

    /* 8 unknown bytes: seems to be some kind of bounding box info */
    gbfread(&buff[0], 1, 8, tpo_file_in);

    /* number of route points */
    waypoint_count = gbfgetint16(tpo_file_in);

    /* allocate temporary memory for the waypoint deltas */
    lon_delta = (short*)(char *) xmalloc(waypoint_count * sizeof(short));
    lat_delta = (short*)(char *) xmalloc(waypoint_count * sizeof(short));

    for (j=0; j<waypoint_count; j++) {

      /* get this point's longitude delta from the first waypoint */
      lon_delta[j] = gbfgetint16(tpo_file_in);

      /* get this point's latitude delta from the first waypoint */
      lat_delta[j] = gbfgetint16(tpo_file_in);
    }

    /* 8 bytes - longitude delta to degrees scale  */
    lon_scale = gbfgetdbl(tpo_file_in);

    /* 8 bytes - latitude delta to degrees scale */
    lat_scale = gbfgetdbl(tpo_file_in);

    /* 4 bytes: the total length of the route in feet*/
    gbfread(&buff[0], 1, 4, tpo_file_in);

    /* 2 unknown bytes */
    gbfread(&buff[0], 1, 2, tpo_file_in);

    /* 2 bytes: continuation marker */
    gbfread(&buff[0], 1, 2, tpo_file_in);

    /* multiply all the deltas by the scaling factors to determine the waypoint positions */
    for (j=0; j<waypoint_count; j++) {

      waypoint_temp = waypt_new();

      /* convert incoming NAD27/CONUS coordinates to WGS84 */
      GPS_Math_Known_Datum_To_WGS84_M(
        first_lat-lat_delta[j]*lat_scale,
        first_lon+lon_delta[j]*lon_scale,
        0.0,
        &waypoint_temp->latitude,
        &waypoint_temp->longitude,
        &amt,
        78);

      /* there is no elevation data for the waypoints */
      waypoint_temp->altitude = 0;

      track_add_wpt(track_temp, waypoint_temp);
    }

    /* free temporary memory */
    xfree(lon_delta);
    xfree(lat_delta);
  }
}





//-------------------------------------------------------------------
//-------------------------------------------------------------------

// This will read 8/16/32 bits in little-endian format depending
// upon the value of the first byte.
//
// For version 3.x files.
//
int tpo_read_int()
{
  unsigned char val;

  val = (unsigned char) gbfgetc(tpo_file_in);

  switch (val) {

  case 0xff:  // 32-bit value
//printf("Found 32-bit value indicator: %x\n", val);
    return(gbfgetint32(tpo_file_in));
    break;

  case 0xfe:  // 16-bit value
//printf("Found 16-bit value indicator: %x\n", val);
    return(gbfgetuint16(tpo_file_in));
    break;

  default:    // 8-bit value
//printf("Found 8-bit value: %x\n", val);
    return((int)val);
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
// For version 3.x files.
//
int tpo_find_block(unsigned int block_desired)
{
  int block_type;
  int block_offset;


  // Skip 512 byte fixed-length header
  block_offset = 512;

  do {

    // Seek to offset from start of file
    gbfseek(tpo_file_in, block_offset, SEEK_SET);

    // Read record type
    block_type = gbfgetint32(tpo_file_in);
//printf("Block: %08x\tat offset: %08x\n", block_type, block_offset);

    // Read offset to next record
    block_offset = gbfgetint32(tpo_file_in);
  } while (block_type != block_desired && block_offset != 0);

  if (block_type == block_desired) {
    return(0);
  } else {
    return(-1);
  }
}





// Convert lat/long to normal values, save in waypoint struct
//
// For version 3.x files.
//
waypoint *tpo_convert_ll(int lat, int lon)
{
  double latitude;
  double longitude;
  waypoint *waypoint_temp;


  waypoint_temp = waypt_new();

  latitude = (double)lat / 0x800000;
  longitude = (double)lon / 0x800000;

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

  return(waypoint_temp);
}





// Track decoder for version 3.x files.  This block contains tracks
// (called "freehand routes" or just "routes" in Topo).
//
void tpo_process_tracks(void)
{
  unsigned int track_count;
  unsigned int ii;


//printf("Processing Tracks...\n");

  // Find block 0x060000 (free-hand routes)
  if (tpo_find_block(0x060000)) {
    return;
  }

  // Read the number of tracks.  Can be 8/16/32-bit value.
  track_count = tpo_read_int();

//printf("Total Tracks: %d\n", track_count);

  if (track_count == 0) {
    return;
  }

  // Read/process each track in the file
  //
  for (ii = 0; ii < track_count; ii++) {
    unsigned int line_type;
    unsigned int track_number;
    unsigned int track_length;
    unsigned int name_length;
    char *track_name;
    unsigned int track_byte_count;
    int llvalid;
    unsigned char *buf;
    int lonscale;
    int latscale;
    int waypoint_count = 0;
    int lat = 0;
    int lon = 0;
    unsigned int jj;
    route_head* track_temp;


    // Allocate the track struct
    track_temp = route_head_alloc();
    track_add_head(track_temp);

//UNKNOWN DATA LENGTH
    line_type = tpo_read_int();

    // Can be 8/16/32-bit value
    track_number = tpo_read_int();

    // Can be 8/16/32-bit value
    track_length = tpo_read_int();

//UNKNOWN DATA LENGTH
    name_length = tpo_read_int();

    if (name_length) {
      track_name = (char *) xmalloc(name_length+1);
      track_name[0] = '\0';
      gbfread(track_name, 1, name_length, tpo_file_in);
      track_name[name_length] = '\0';  // Terminator
    } else { // Assign a generic track name
      track_name = (char *) xmalloc(15);
      sprintf(track_name, "TRK %d", ii+1);
    }
    track_temp->rte_name = track_name;
//printf("\nTrack Name: %s  ", track_name);

    // Route description
//        track_temp->rte_desc = NULL;

    // Route number
    track_temp->rte_num = ii+1;

//UNKNOWN DATA LENGTH
    track_byte_count = tpo_read_int();

    // Read the number of bytes specified for the track.  These
    // contain scaling factors, long/lat's, and offsets from
    // those long/lat's.  First will be a long/lat (8 bytes).
    // Keep track of the bytes as we go so that we know how many
    // we've read.  We need to do this so that we start at the
    // proper place for the next track.

    // Read the track bytes into a buffer
    buf = (unsigned char *) xmalloc(track_byte_count);
    gbfread(buf, 1, track_byte_count, tpo_file_in);

    latscale=0;
    lonscale=0;

    // Process the track bytes
    llvalid = 0;
    for (jj = 0; jj < track_byte_count;) {
      waypoint* waypoint_temp;

      // Time to read a new latlong?
      if (!llvalid) {

        lon = le_read32(buf+jj);
        jj+=4;

        lat = le_read32(buf+jj);
        jj+=4;

//printf("L");

        // Peek to see if next is a lonscale.  Note that it
        // can begin with 0x88, which is confusing.  Here we
        // allow up to 16-bits of offset, so two of the
        // bytes must be 0x00 for us to recognize it.
        if (jj+3<track_byte_count
            && !buf[jj+3]
            && !buf[jj+2]) {

          lonscale = le_read32(buf+jj);
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

          latscale = le_read32(buf+jj);
//printf(" LATSCALE:");
//printf("%02x%02x%02x%02x ", buf[jj], buf[jj+1], buf[jj+2], buf[jj+3]);
          jj+=4;
        }
        llvalid = 1;

        waypoint_temp = tpo_convert_ll(lat, lon);
        track_add_wpt(track_temp, waypoint_temp);
        waypoint_count++;
      }

      // Check whether there's a lonlat coming up instead of
      // offsets.
      else if (buf[jj] == 0x88) {
        jj++;
        llvalid = 0;
      }

      // Check whether there's a lonlat + lonscale/latscale
      // combo embedded in this track next.
      else if (buf[jj] == 0x00) {
//printf(" ZERO ");
        jj++;
        llvalid = 0;
      }

      // Process the delta
      else {
        int scarray[] = {0,1,2,3,4,5,6,7,0,-7,-6,-5,-4,-3,-2,-1};


        if (buf[jj] == 0) {
          printf("Found unexpected ZERO\n");
          exit(1);
        }

        if (latscale == 0 || lonscale == 0) {
          printf("Found bad scales lonscale=0x%x latscale=0x%x\n", lonscale, latscale);
          exit(1);
        }

        lon+=lonscale*scarray[buf[jj]>>4];
        lat+=latscale*scarray[(buf[jj]&0xf)];
//printf(".");
        jj++;

        waypoint_temp = tpo_convert_ll(lat, lon);
        route_add_wpt(track_temp, waypoint_temp);
        waypoint_count++;
      }
    }
    track_temp->rte_waypt_ct = waypoint_count;

    xfree(buf);
  }
//printf("\n");
}





// Global index to waypoints, needed for routes, filled in by
// tpo_process_waypoints.
//
// For version 3.x files.
//
waypoint** tpo_wp_index;
unsigned int tpo_index_ptr = 0;





// Waypoint decoder for version 3.x files.
//
void tpo_process_waypoints(void)
{
  unsigned int waypoint_count;
  unsigned int ii;


//printf("Processing Waypoints...\n");

  // Find block 0x0e0000 (GPS-Waypoints)
  if (tpo_find_block(0x0e0000)) {
    return;
  }

  // Read the number of waypoints.  8/16/32-bit value.
  waypoint_count = tpo_read_int();

//printf("Total Waypoints: %d\n", waypoint_count);

  // Fetch storage for the waypoint index (needed later for
  // routes)
  tpo_wp_index = (waypoint **)(char *) xmalloc(sizeof(waypoint *) * waypoint_count);

  if (waypoint_count == 0) {
    return;
  }

  // Read/process each waypoint in the file
  for (ii = 0; ii < waypoint_count; ii++) {
    waypoint* waypoint_temp;
    waypoint* waypoint_temp2;
    unsigned int name_length;
    char *waypoint_name;
    int lat;
    int lon;
    int altitude;


//UNKNOWN DATA LENGTH
    (void)tpo_read_int(); // 0x00

//UNKNOWN DATA LENGTH
    (void)tpo_read_int(); // 0x00

//UNKNOWN DATA LENGTH
    // Fetch name length
    name_length = tpo_read_int();
//printf("\nName Length: %d\n", name_length);
    if (name_length) {
      waypoint_name = (char *) xmalloc(name_length+1);
      waypoint_name[0] = '\0';
      gbfread(waypoint_name, 1, name_length, tpo_file_in);
      waypoint_name[name_length] = '\0';  // Terminator
    } else { // Assign a generic waypoint name
      waypoint_name = (char *) xmalloc(15);
      sprintf(waypoint_name, "WPT %d", ii+1);
    }
//printf("\tWaypoint Name: %s\n", waypoint_name);

//UNKNOWN DATA LENGTH
    (void)tpo_read_int();

    lon = gbfgetint32(tpo_file_in);
    lat = gbfgetint32(tpo_file_in);

    // Allocate space for waypoint and store lat/lon
    waypoint_temp = tpo_convert_ll(lat, lon);

    // Assign the waypoint name
    waypoint_temp->shortname = waypoint_name;

    // Grab the altitude in meters
    altitude = gbfgetint32(tpo_file_in);
    if (altitude == 0xfffd000c) { // Unknown altitude
      altitude = 0;
    }
    waypoint_temp->altitude = altitude / 100;   // Meters
//printf("\tAltitude: %1.0f meters\n", waypoint_temp->altitude);

//UNKNOWN DATA LENGTH
    // Fetch comment length
    name_length = tpo_read_int();
//printf("\tComment length: %d\n", name_length);
    if (name_length) {
      char *comment;

      comment = (char *) xmalloc(name_length+1);
      comment[0] = '\0';
      gbfread(comment, 1, name_length, tpo_file_in);
      comment[name_length] = '\0';  // Terminator
      waypoint_temp->description = comment;
//printf("\tComment: %s\n", waypoint_name);
    } else {
//            waypoint_temp->description = NULL;
    }

//        waypoint_temp->notes = NULL;
//        waypoint_temp->url = NULL;
//        waypoint_temp->url_link_text = NULL;

    // For routes (later), we need a duplicate of each waypoint
    // indexed by the order we read them in.
    waypoint_temp2 = waypt_dupe(waypoint_temp);

    // Attach the copy to our index
    tpo_wp_index[tpo_index_ptr++] = waypoint_temp2;

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
void tpo_process_map_notes(void)
{
  unsigned int waypoint_count;
  unsigned int ii;


//printf("Processing Map Notes...\n");

  // Find block 0x090000 (Map Notes)
  if (tpo_find_block(0x090000)) {
    return;
  }

  // Read the number of waypoints.  8/16/32-bit value.
  waypoint_count = tpo_read_int();

//printf("Elements: %d\n", waypoint_count);

  if (!waypoint_count) {
    return;
  }

  // Process each waypoint
  for (ii = 0; ii < waypoint_count; ii++) {
    int lat;
    int lon;
    unsigned int name_length;
    char *waypoint_name;
    waypoint* waypoint_temp;
    unsigned int num_bytes;
    unsigned int jj;


//UNKNOWN DATA LENGTH
    (void)tpo_read_int();

    lon = gbfgetint32(tpo_file_in);
    lat = gbfgetint32(tpo_file_in);

    // Allocate space for waypoint and store lat/lon
    waypoint_temp = tpo_convert_ll(lat, lon);

    // Assign a generic waypoint name
    waypoint_name = (char *) xmalloc(15);
    sprintf(waypoint_name, "NOTE %d", ii+1);
//printf("Waypoint Name: %s\t\t", waypoint_name);
    waypoint_temp->shortname = waypoint_name;

//UNKNOWN DATA LENGTH
    (void)tpo_read_int();

//UNKNOWN DATA LENGTH
    (void)tpo_read_int();

//UNKNOWN DATA LENGTH
    (void)tpo_read_int();

//UNKNOWN DATA LENGTH
    // Fetch comment length
    name_length = tpo_read_int();
    if (name_length) {
      char *comment;

      comment = (char *) xmalloc(name_length+1);
      comment[0] = '\0';
      gbfread(comment, 1, name_length, tpo_file_in);
      comment[name_length] = '\0';  // Terminator
      waypoint_temp->description = comment;
//printf("Comment: %s\n", comment);
    } else {
//            waypoint_temp->description = NULL;
    }

//        waypoint_temp->url_link_text = NULL;

    // Length of text for external path.  If non-zero, skip past
    // the text.
//UNKNOWN DATA LENGTH
    name_length = tpo_read_int();
//printf("name_length: %x\n", name_length);
    if (name_length) {
      char *notes;

      notes = (char *) xmalloc(name_length+1);
      notes[0] = '\0';
      gbfread(notes, 1, name_length, tpo_file_in);
      notes[name_length] = '\0';  // Terminator
      waypoint_temp->url = notes;
//printf("Notes: %s\n", notes);
    }

    // Length of text for image path.  If non-zero, skip past
    // the text.
//UNKNOWN DATA LENGTH
    name_length = tpo_read_int();
    if (name_length) {
      char *notes;

      notes = (char *) xmalloc(name_length+1);
      notes[0] = '\0';
      gbfread(notes, 1, name_length, tpo_file_in);
      notes[name_length] = '\0';  // Terminator
      waypoint_temp->url = notes;
//printf("Notes: %s\n", notes);
    }

//UNKNOWN DATA LENGTH
    (void)tpo_read_int();

//UNKNOWN DATA LENGTH
    (void)tpo_read_int();

    // Number of bytes to skip until next element or end of
    // block.  May be 8/16/32 bits.
    num_bytes = tpo_read_int();
//printf("num_bytes: %x\n", num_bytes);
    for (jj = 0; jj < num_bytes; jj++) {
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
void tpo_process_symbols(void)
{
  unsigned int waypoint_count;
  unsigned int ii;


//printf("Processing Symbols...\n");

  // Find block 0x040000 (Symbols)
  if (tpo_find_block(0x040000)) {
    return;
  }

  // Read the number of waypoints.  8/16/32-bit value.
  waypoint_count = tpo_read_int();

//printf("Elements: %d\n", waypoint_count);

  if (!waypoint_count) {
    return;
  }

  // Process each waypoint
  for (ii = 0; ii < waypoint_count; ii++) {
    int lat;
    int lon;
    char *waypoint_name;
    waypoint* waypoint_temp;


//UNKNOWN DATA LENGTH
    (void)tpo_read_int();

//UNKNOWN DATA LENGTH
    (void)tpo_read_int();

    lon = gbfgetint32(tpo_file_in);
    lat = gbfgetint32(tpo_file_in);

    // Allocate space for waypoint and store lat/lon
    waypoint_temp = tpo_convert_ll(lat, lon);

    // Assign a generic waypoint name
    waypoint_name = (char *) xmalloc(15);
    sprintf(waypoint_name, "SYM %d", ii+1);
//printf("Waypoint Name: %s\n", waypoint_name);
    waypoint_temp->shortname = waypoint_name;

//        waypoint_temp->description = NULL;
//        waypoint_temp->notes = NULL;
//        waypoint_temp->url = NULL;
//        waypoint_temp->url_link_text = NULL;

    // Add the waypoint to the chain of waypoints
    waypt_add(waypoint_temp);
  }
}





// Text Labels decoder for version 3.x files.
//
void tpo_process_text_labels(void)
{
  unsigned int waypoint_count;
  unsigned int ii;


//printf("Processing Text Labels...\n");

  // Find block 0x080000 (Text Labels)
  if (tpo_find_block(0x080000)) {
    return;
  }

  // Read the number of waypoints.  8/16/32-bit value.
  waypoint_count = tpo_read_int();

//printf("Elements: %d\n", waypoint_count);

  if (!waypoint_count) {
    return;
  }

  // Process each waypoint
  for (ii = 0; ii < waypoint_count; ii++) {
    int jj;
    int lat;
    int lon;
    unsigned int name_length;
    char *waypoint_name;
    waypoint* waypoint_temp;


//UNKNOWN DATA LENGTH
    (void)tpo_read_int();

//UNKNOWN DATA LENGTH
    (void)tpo_read_int();

    lon = gbfgetint32(tpo_file_in);
    lat = gbfgetint32(tpo_file_in);

    // Allocate space for waypoint and store lat/lon
    waypoint_temp = tpo_convert_ll(lat, lon);

    // Assign a generic waypoint name
    waypoint_name = (char *) xmalloc(15);
    sprintf(waypoint_name, "TXT %d", ii+1);
//printf("Waypoint Name: %s\t\t", waypoint_name);
    waypoint_temp->shortname = waypoint_name;

    for (jj = 0; jj < 16; jj++) {
//UNKNOWN DATA LENGTH
      (void) gbfgetc(tpo_file_in);
    }

    // Fetch comment length
//UNKNOWN DATA LENGTH
    name_length = tpo_read_int();
    if (name_length) {
      char *comment;

      comment = (char *) xmalloc(name_length+1);
      comment[0] = '\0';
      gbfread(comment, 1, name_length, tpo_file_in);
      comment[name_length] = '\0';  // Terminator
      waypoint_temp->description = comment;
//printf("Comment: %s\n", comment);
    } else {
//            waypoint_temp->description = NULL;
    }

//        waypoint_temp->notes = NULL;
//        waypoint_temp->url = NULL;
//        waypoint_temp->url_link_text = NULL;

    // Add the waypoint to the chain of waypoints
    waypt_add(waypoint_temp);
  }
}





// Route decoder for version 3.x files.
//
// We depend on tpo_wp_index[] having been malloc'ed and filled-in
// with pointers to waypoint objects by tpo_process_waypoints()
// function above.
//
void tpo_process_routes(void)
{
  unsigned int route_count;
  unsigned int ii;


//printf("Processing Routes...\n");

  // Find block 0x0f0000 (GPS-Routes)
  if (tpo_find_block(0x0f0000)) {
    return;
  }

  // Read the number of routes.  8/16/32-bit value
  route_count = tpo_read_int();

//printf("Total Routes: %d\n", route_count);

  if (route_count == 0) {
    return;
  }

  // Read/process each route in the file
  //
  for (ii = 0; ii < route_count; ii++) {
    unsigned int name_length = 0;
    char *route_name;
    unsigned int jj;
    unsigned int waypoint_cnt;
    route_head* route_temp;


    // Allocate the route struct
    route_temp = route_head_alloc();
    route_add_head(route_temp);

//UNKNOWN DATA LENGTH
    (void)tpo_read_int();

//UNKNOWN DATA LENGTH
    (void)tpo_read_int();

//UNKNOWN DATA LENGTH
    // Fetch name length
    name_length = tpo_read_int();
    if (name_length) {
      route_name = (char *) xmalloc(name_length+1);
      route_name[0] = '\0';
      gbfread(route_name, 1, name_length, tpo_file_in);
      route_name[name_length] = '\0';  // Terminator
    } else { // Assign a generic route name
      route_name = (char *) xmalloc(15);
      sprintf(route_name, "RTE %d", ii+1);
    }
    route_temp->rte_name = route_name;
//printf("Route Name: %s\n", route_name);

//UNKNOWN DATA LENGTH
    // Comment?
    (void)tpo_read_int();
//        gbfgetc(tpo_file_in);
//        route_temp->rte_desc = NULL;

    route_temp->rte_num = ii+1;

    // Fetch the number of waypoints in this route.  8/16/32-bit
    // value.
    waypoint_cnt = tpo_read_int();

    route_temp->rte_waypt_ct = waypoint_cnt;

    // Run through the list of waypoints, look up each in our
    // index, then add the waypoint to this route.
    //
    for (jj = 0; jj < waypoint_cnt; jj++) {
      waypoint* waypoint_temp;
      unsigned char val;


//UNKNOWN DATA LENGTH
      // Fetch the index to the waypoint
      val = tpo_read_int();
//printf("val: %x\t\t", val);

      // Duplicate a waypoint from our index of waypoints.
      waypoint_temp = waypt_dupe(tpo_wp_index[val-1]);

      // Add the waypoint to the route
      route_add_wpt(route_temp, waypoint_temp);
    }
//printf("\n");
  }

  // Free the waypoint index, we don't need it anymore.
  for (ii = 0; ii < tpo_index_ptr; ii++) {
    if (tpo_wp_index[ii]->shortname) {
      xfree(tpo_wp_index[ii]->shortname);
    }
    if (tpo_wp_index[ii]->description) {
      xfree(tpo_wp_index[ii]->description);
    }
    xfree(tpo_wp_index[ii]);
  }

  // Free the index array itself
  xfree(tpo_wp_index);
}





// Compass decoder for version 3.x files.
//
void tpo_process_compass(void)
{

  // Not implemented yet
}





// Decoder for version 3.x files.  These files have "tracks"
// (called "freehand routes" or just "routes" in Topo), "waypoints",
// and "gps-routes".  We intend to read all three types.
//
void tpo_read_3_x(void)
{

  if (doing_trks) {
//printf("Processing Tracks\n");
    tpo_process_tracks();
  }

  if (doing_wpts || doing_rtes) {
//printf("Processing Waypoints\n");
    tpo_process_waypoints();
  }

  if (doing_rtes) {
    //
    // Note:  To process routes we _MUST_ process waypoints
    // first!  This creates the index of waypoints that we need
    // for routes.
    //
//printf("Processing Routes\n");
    tpo_process_routes();
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





static void
tpo_read(void)
{

  if (tpo_version == 2.0) {
//printf("\nFound a version 2.x file\n");
    tpo_read_2_x();
  } else if (tpo_version == 3.0) {
//printf("\nFound a version 3.x file\n");
    tpo_read_3_x();
  } else {
    fatal(MYNAME ": gpsbabel can only read TPO versions through 3.x.x\n");
  }
}





/*******************************************************************************/
/*                                     WRITE                                   */
/*******************************************************************************/

/* tpo_write_file_header()
   Write the appropriate header for the desired TOPO! state.

   National Geographic sells about 75 different state and regional software
   programs called TOPO! that use the TPO format. Each one uses a different
   header data sequence. The header contains the name of the state maps, as well
   as some map scaling information and other data. In most cases, you can't open
   a TPO file created by a different state/regional version of TOPO! than the one
   you're using yourself. When writing a TPO file, it is therefore necessary to
   specify what TOPO! state product to create the file for. I believe that the
   TOPO! regional products can open TPO files created by the TOPO! state products
   as long as the track data is within the area covered by the regional product.
   As a result, it's only necessary to decide what state product output format to
   use.

   TO ADD SUPPORT FOR ANOTHER STATE:
   1. Obtain an example .tpo file generated by the state product for which you wish
   to add support. National Geographic MapXchange (http://maps.nationalgeographic.com/topo/search.cfm)
   is a good source of .tpo files.
   2. Run gpsbabel using the "dumpheader" option of the TPO format converter, and
   specifying a dummy output file. For example:
     gpsbabel -t -i tpo,dumpheader=1 -f sample_file.tpo -o csv -F dummy.txt
   This will write a snippet of C code containing the header bytes to the shell window.
   3. Add a new if() clause to tpo_write_file_header(). Copy the header bytes definition
   from the previous step.
   4. Recompile gpsbabel.
   5. You should now be able write TPO ouput in the new state's format. For example, if
      you added support for Texas:
     gpsbabel -t -i gpx -f input.gpx -o tpo,state="TX" -F output.tpo */
static void
tpo_write_file_header()
{
  /* force upper-case state name */
  strupper(output_state);

  if (strncmp("CA", output_state, 2) == 0) {

    unsigned char header_bytes[] = {
      0x18, 0x43, 0x61, 0x6C, 0x69, 0x66, 0x6F, 0x72,
      0x6E, 0x69, 0x61, 0x20, 0x53, 0x68, 0x61, 0x64,
      0x65, 0x64, 0x20, 0x52, 0x65, 0x6C, 0x69, 0x65,
      0x66, 0x03, 0x43, 0x41, 0x31, 0x05, 0x00, 0x00,
      0x00, 0x00, 0x00, 0x00, 0x40, 0x5F, 0x40, 0x00,
      0x00, 0x00, 0x00, 0x00, 0x00, 0x45, 0x40, 0x00,
      0x00, 0x00, 0x00, 0x00, 0x80, 0x5C, 0x40, 0x00,
      0x00, 0x00, 0x00, 0x00, 0x00, 0x40, 0x40, 0x00,
      0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
      0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
      0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
      0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
      0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
      0x00, 0x00, 0x00, 0x00, 0x27, 0x43, 0x3A, 0x5C,
      0x50, 0x72, 0x6F, 0x67, 0x72, 0x61, 0x6D, 0x20,
      0x46, 0x69, 0x6C, 0x65, 0x73, 0x5C, 0x54, 0x4F,
      0x50, 0x4F, 0x21, 0x5C, 0x54, 0x50, 0x4F, 0x5F,
      0x44, 0x41, 0x54, 0x41, 0x5C, 0x43, 0x41, 0x5F,
      0x44, 0x30, 0x31, 0x5C, 0x20, 0x43, 0x3A, 0x5C,
      0x50, 0x72, 0x6F, 0x67, 0x72, 0x61, 0x6D, 0x20,
      0x46, 0x69, 0x6C, 0x65, 0x73, 0x5C, 0x54, 0x4F,
      0x50, 0x4F, 0x21, 0x5C, 0x54, 0x50, 0x4F, 0x5F,
      0x44, 0x41, 0x54, 0x41, 0x5C, 0x12, 0x43, 0x3A,
      0x5C, 0x54, 0x4F, 0x50, 0x4F, 0x21, 0x5C, 0x54,
      0x50, 0x4F, 0x5F, 0x44, 0x41, 0x54, 0x41, 0x5C,
      0x00, 0x00, 0x00, 0xDC, 0x30, 0x32, 0x30, 0x32,
      0x30, 0x32, 0x30, 0x33, 0x30, 0x33, 0x30, 0x30,
      0x30, 0x30, 0x30, 0x30, 0x30, 0x30, 0x30, 0x30,
      0x30, 0x30, 0x30, 0x32, 0x30, 0x32, 0x30, 0x32,
      0x30, 0x33, 0x30, 0x33, 0x30, 0x30, 0x30, 0x30,
      0x30, 0x30, 0x30, 0x30, 0x30, 0x30, 0x30, 0x30,
      0x30, 0x34, 0x30, 0x34, 0x30, 0x34, 0x30, 0x34,
      0x30, 0x36, 0x30, 0x36, 0x30, 0x30, 0x30, 0x30,
      0x30, 0x30, 0x30, 0x30, 0x30, 0x30, 0x30, 0x30,
      0x30, 0x35, 0x30, 0x35, 0x30, 0x34, 0x30, 0x36,
      0x30, 0x36, 0x30, 0x36, 0x30, 0x30, 0x30, 0x30,
      0x30, 0x30, 0x30, 0x30, 0x30, 0x30, 0x30, 0x35,
      0x30, 0x35, 0x30, 0x35, 0x30, 0x36, 0x30, 0x37,
      0x30, 0x37, 0x30, 0x38, 0x30, 0x30, 0x30, 0x30,
      0x30, 0x30, 0x30, 0x30, 0x30, 0x30, 0x30, 0x35,
      0x30, 0x35, 0x30, 0x37, 0x30, 0x37, 0x30, 0x37,
      0x30, 0x38, 0x30, 0x38, 0x30, 0x39, 0x30, 0x30,
      0x30, 0x30, 0x30, 0x30, 0x30, 0x30, 0x30, 0x35,
      0x31, 0x30, 0x30, 0x38, 0x30, 0x38, 0x30, 0x38,
      0x30, 0x39, 0x30, 0x39, 0x30, 0x39, 0x30, 0x30,
      0x30, 0x30, 0x30, 0x30, 0x30, 0x30, 0x31, 0x30,
      0x31, 0x30, 0x31, 0x30, 0x31, 0x30, 0x31, 0x31,
      0x30, 0x39, 0x30, 0x39, 0x30, 0x30, 0x30, 0x30,
      0x30, 0x30, 0x30, 0x30, 0x31, 0x30, 0x31, 0x30,
      0x31, 0x31, 0x31, 0x31, 0x31, 0x31, 0x31, 0x31,
      0x30, 0x39, 0x30, 0x30, 0x30, 0x30, 0x30, 0x30,
      0x30, 0x30, 0x30, 0x30, 0x30, 0x30, 0x31, 0x31,
      0x31, 0x31, 0x31, 0x31, 0x30, 0x39, 0x30, 0x39,
      0x0D, 0x55, 0x6E, 0x69, 0x74, 0x65, 0x64, 0x20,
      0x53, 0x74, 0x61, 0x74, 0x65, 0x73, 0x00, 0x00,
      0x00, 0x00, 0x00, 0x40, 0x5F, 0x40, 0xBC, 0x23,
      0x63, 0xB5, 0xF9, 0x3A, 0x50, 0x40, 0x00, 0x00,
      0x00, 0x00, 0x00, 0x80, 0x50, 0x40, 0x22, 0xE2,
      0xE6, 0x54, 0x32, 0x28, 0x22, 0x40, 0x0A, 0x43,
      0x61, 0x6C, 0x69, 0x66, 0x6F, 0x72, 0x6E, 0x69,
      0x61, 0x00, 0x00, 0x00, 0x00, 0x00, 0x40, 0x5F,
      0x40, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x45,
      0x40, 0x00, 0x00, 0x00, 0x00, 0x00, 0x80, 0x5C,
      0x40, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x40,
      0x40, 0x16, 0x2A, 0x47, 0x65, 0x6E, 0x65, 0x72,
      0x61, 0x6C, 0x20, 0x52, 0x65, 0x66, 0x65, 0x72,
      0x65, 0x6E, 0x63, 0x65, 0x20, 0x4D, 0x61, 0x70,
      0x00, 0x09, 0x3D, 0x00, 0x0C, 0x43, 0x41, 0x31,
      0x5F, 0x4D, 0x41, 0x50, 0x31, 0x5C, 0x53, 0x31,
      0x4C, 0xAF, 0x02, 0x15, 0x03, 0x00, 0x00, 0x00,
      0x00, 0x00, 0x00, 0x26, 0x40, 0x00, 0x00, 0x00,
      0x00, 0x00, 0x00, 0x24, 0x40, 0x84, 0x00, 0x78,
      0x00, 0x01, 0x00, 0x00, 0x00, 0x01, 0x00, 0x16,
      0x2A, 0x4E, 0x61, 0x74, 0x69, 0x6F, 0x6E, 0x61,
      0x6C, 0x20, 0x41, 0x74, 0x6C, 0x61, 0x73, 0x20,
      0x53, 0x65, 0x72, 0x69, 0x65, 0x73, 0xE8, 0x32,
      0x0D, 0x00, 0x02, 0x44, 0x41, 0x23, 0x01, 0x6C,
      0x01, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0xF0,
      0x3F, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0xF0,
      0x3F, 0x3C, 0x00, 0x3C, 0x00, 0x01, 0x00, 0x00,
      0x00, 0x01, 0x00, 0x10, 0x2A, 0x35, 0x30, 0x30,
      0x4B, 0x20, 0x4D, 0x61, 0x70, 0x20, 0x53, 0x65,
      0x72, 0x69, 0x65, 0x73, 0xC0, 0xFE, 0x04, 0x00,
      0x02, 0x44, 0x46, 0x00, 0x01, 0x40, 0x01, 0xB5,
      0x2B, 0x4C, 0x55, 0x55, 0x55, 0xD5, 0x3F, 0xB5,
      0x2B, 0x4C, 0x55, 0x55, 0x55, 0xD5, 0x3F, 0x28,
      0x00, 0x28, 0x00, 0x01, 0x00, 0x00, 0x00, 0x02,
      0x00, 0x10, 0x2A, 0x31, 0x30, 0x30, 0x4B, 0x20,
      0x4D, 0x61, 0x70, 0x20, 0x53, 0x65, 0x72, 0x69,
      0x65, 0x73, 0x50, 0xC3, 0x00, 0x00, 0x02, 0x44,
      0x4B, 0x00, 0x00, 0x89, 0x01, 0x00, 0x00, 0x00,
      0x00, 0x00, 0x00, 0xB0, 0x3F, 0x00, 0x00, 0x00,
      0x00, 0x00, 0x00, 0xB0, 0x3F, 0x2D, 0x00, 0x2D,
      0x00, 0x0C, 0x00, 0x01, 0x00, 0x0A, 0x00, 0x10,
      0x2A, 0x37, 0x2E, 0x35, 0x27, 0x20, 0x4D, 0x61,
      0x70, 0x20, 0x53, 0x65, 0x72, 0x69, 0x65, 0x73,
      0x0F, 0x3C, 0x00, 0x00, 0x02, 0x44, 0x51, 0x00,
      0x00, 0x00, 0x01, 0x9A, 0x99, 0x99, 0x99, 0x99,
      0x99, 0x99, 0x3F, 0x9A, 0x99, 0x99, 0x99, 0x99,
      0x99, 0x89, 0x3F, 0x5A, 0x00, 0x2D, 0x00, 0x0D,
      0x00, 0x01, 0x00, 0x28, 0x00
    };

    gbfwrite(header_bytes, sizeof(header_bytes), 1, tpo_file_out);
  } else if (strncmp("CT", output_state, 2) == 0 ||
             strncmp("MA", output_state, 2) == 0 ||
             strncmp("ME", output_state, 2) == 0 ||
             strncmp("NJ", output_state, 2) == 0 ||
             strncmp("NH", output_state, 2) == 0 ||
             strncmp("NY", output_state, 2) == 0 ||
             strncmp("RI", output_state, 2) == 0 ||
             strncmp("VT", output_state, 2) == 0) {
    /* These 8 states are all covered in a single "Northeast" title */

    unsigned char header_bytes[] = {
      0x1E, 0x4E, 0x6F, 0x72, 0x74, 0x68, 0x65, 0x61,
      0x73, 0x74, 0x65, 0x72, 0x6E, 0x20, 0x55, 0x53,
      0x41, 0x20, 0x53, 0x68, 0x61, 0x64, 0x65, 0x64,
      0x20, 0x52, 0x65, 0x6C, 0x69, 0x65, 0x66, 0x03,
      0x4E, 0x45, 0x31, 0x05, 0x00, 0x00, 0x00, 0x00,
      0x00, 0x00, 0x00, 0x54, 0x40, 0x00, 0x00, 0x00,
      0x00, 0x00, 0x00, 0x48, 0x40, 0x00, 0x00, 0x00,
      0x00, 0x00, 0x80, 0x50, 0x40, 0x00, 0x00, 0x00,
      0x00, 0x00, 0x00, 0x43, 0x40, 0x00, 0x00, 0x00,
      0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
      0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
      0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
      0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
      0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
      0x00, 0x00, 0x0B, 0x44, 0x3A, 0x5C, 0x4E, 0x45,
      0x31, 0x5F, 0x44, 0x30, 0x31, 0x5C, 0x12, 0x43,
      0x3A, 0x5C, 0x54, 0x4F, 0x50, 0x4F, 0x21, 0x5C,
      0x54, 0x50, 0x4F, 0x5F, 0x44, 0x41, 0x54, 0x41,
      0x5C, 0x12, 0x45, 0x3A, 0x5C, 0x54, 0x4F, 0x50,
      0x4F, 0x21, 0x5C, 0x54, 0x50, 0x4F, 0x5F, 0x44,
      0x41, 0x54, 0x41, 0x5C, 0x00, 0x00, 0x00, 0xFF,
      0x18, 0x01, 0x30, 0x30, 0x30, 0x30, 0x30, 0x30,
      0x30, 0x30, 0x30, 0x30, 0x30, 0x30, 0x30, 0x30,
      0x30, 0x30, 0x30, 0x30, 0x30, 0x30, 0x30, 0x37,
      0x30, 0x37, 0x30, 0x37, 0x30, 0x30, 0x30, 0x30,
      0x30, 0x30, 0x30, 0x30, 0x30, 0x30, 0x30, 0x30,
      0x30, 0x30, 0x30, 0x30, 0x30, 0x30, 0x30, 0x30,
      0x30, 0x37, 0x30, 0x37, 0x30, 0x37, 0x30, 0x37,
      0x30, 0x30, 0x30, 0x30, 0x30, 0x30, 0x30, 0x30,
      0x30, 0x30, 0x30, 0x30, 0x30, 0x34, 0x30, 0x34,
      0x30, 0x35, 0x30, 0x35, 0x30, 0x36, 0x30, 0x37,
      0x30, 0x37, 0x30, 0x37, 0x30, 0x30, 0x30, 0x30,
      0x30, 0x30, 0x30, 0x30, 0x30, 0x33, 0x30, 0x33,
      0x30, 0x34, 0x30, 0x34, 0x30, 0x35, 0x30, 0x35,
      0x30, 0x36, 0x30, 0x36, 0x30, 0x36, 0x30, 0x36,
      0x30, 0x36, 0x30, 0x32, 0x30, 0x32, 0x30, 0x32,
      0x30, 0x33, 0x30, 0x33, 0x30, 0x34, 0x30, 0x34,
      0x30, 0x35, 0x30, 0x35, 0x30, 0x36, 0x30, 0x36,
      0x30, 0x36, 0x30, 0x30, 0x30, 0x30, 0x30, 0x32,
      0x30, 0x32, 0x30, 0x32, 0x30, 0x33, 0x30, 0x33,
      0x30, 0x38, 0x30, 0x38, 0x30, 0x38, 0x30, 0x39,
      0x30, 0x39, 0x30, 0x30, 0x30, 0x30, 0x30, 0x30,
      0x30, 0x30, 0x30, 0x32, 0x30, 0x32, 0x30, 0x32,
      0x30, 0x33, 0x31, 0x30, 0x31, 0x30, 0x30, 0x38,
      0x30, 0x39, 0x30, 0x39, 0x30, 0x39, 0x30, 0x39,
      0x30, 0x30, 0x30, 0x30, 0x30, 0x30, 0x30, 0x30,
      0x30, 0x30, 0x30, 0x30, 0x30, 0x30, 0x31, 0x30,
      0x31, 0x30, 0x31, 0x30, 0x30, 0x39, 0x30, 0x30,
      0x30, 0x30, 0x30, 0x30, 0x30, 0x30, 0x30, 0x30,
      0x30, 0x30, 0x30, 0x30, 0x30, 0x30, 0x30, 0x30,
      0x30, 0x30, 0x31, 0x30, 0x31, 0x30, 0x30, 0x30,
      0x30, 0x30, 0x30, 0x30, 0x30, 0x30, 0x30, 0x30,
      0x30, 0x30, 0x30, 0x30, 0x30, 0x30, 0x30, 0x30,
      0x30, 0x30, 0x30, 0x30, 0x30, 0x30, 0x30, 0x30,
      0x31, 0x30, 0x30, 0x30, 0x30, 0x30, 0x30, 0x30,
      0x30, 0x30, 0x30, 0x30, 0x30, 0x30, 0x30, 0x30,
      0x30, 0x30, 0x0D, 0x55, 0x6E, 0x69, 0x74, 0x65,
      0x64, 0x20, 0x53, 0x74, 0x61, 0x74, 0x65, 0x73,
      0x00, 0x00, 0x00, 0x00, 0x00, 0x40, 0x5F, 0x40,
      0x00, 0x00, 0x00, 0x00, 0x00, 0xC0, 0x4E, 0x40,
      0x00, 0x00, 0x00, 0x00, 0x00, 0x80, 0x50, 0x40,
      0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x21, 0x40,
      0x10, 0x4E, 0x6F, 0x72, 0x74, 0x68, 0x65, 0x61,
      0x73, 0x74, 0x65, 0x72, 0x6E, 0x20, 0x55, 0x53,
      0x41, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x54,
      0x40, 0x00, 0x00, 0x00, 0x00, 0x00, 0xBE, 0x48,
      0x40, 0x00, 0x00, 0x00, 0x00, 0x00, 0x80, 0x50,
      0x40, 0x00, 0x00, 0x00, 0x00, 0x00, 0x42, 0x42,
      0x40, 0x16, 0x2A, 0x47, 0x65, 0x6E, 0x65, 0x72,
      0x61, 0x6C, 0x20, 0x52, 0x65, 0x66, 0x65, 0x72,
      0x65, 0x6E, 0x63, 0x65, 0x20, 0x4D, 0x61, 0x70,
      0x00, 0x09, 0x3D, 0x00, 0x0C, 0x4E, 0x45, 0x31,
      0x5F, 0x4D, 0x41, 0x50, 0x31, 0x5C, 0x53, 0x31,
      0x4C, 0x68, 0x03, 0x16, 0x03, 0x00, 0x00, 0x00,
      0x00, 0x00, 0x00, 0x2C, 0x40, 0x00, 0x00, 0x00,
      0x00, 0x00, 0x00, 0x24, 0x40, 0x8C, 0x00, 0x64,
      0x00, 0x01, 0x00, 0x00, 0x00, 0x01, 0x00, 0x16,
      0x2A, 0x4E, 0x61, 0x74, 0x69, 0x6F, 0x6E, 0x61,
      0x6C, 0x20, 0x41, 0x74, 0x6C, 0x61, 0x73, 0x20,
      0x53, 0x65, 0x72, 0x69, 0x65, 0x73, 0xE8, 0x32,
      0x0D, 0x00, 0x02, 0x44, 0x41, 0x0B, 0x01, 0x6C,
      0x01, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0xF0,
      0x3F, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0xF0,
      0x3F, 0x3C, 0x00, 0x3C, 0x00, 0x01, 0x00, 0x00,
      0x00, 0x01, 0x00, 0x10, 0x2A, 0x35, 0x30, 0x30,
      0x4B, 0x20, 0x4D, 0x61, 0x70, 0x20, 0x53, 0x65,
      0x72, 0x69, 0x65, 0x73, 0xC0, 0xFE, 0x04, 0x00,
      0x02, 0x44, 0x46, 0xEA, 0x00, 0x40, 0x01, 0xB5,
      0x2B, 0x4C, 0x55, 0x55, 0x55, 0xD5, 0x3F, 0xB5,
      0x2B, 0x4C, 0x55, 0x55, 0x55, 0xD5, 0x3F, 0x28,
      0x00, 0x28, 0x00, 0x01, 0x00, 0x00, 0x00, 0x02,
      0x00, 0x10, 0x2A, 0x31, 0x30, 0x30, 0x4B, 0x20,
      0x4D, 0x61, 0x70, 0x20, 0x53, 0x65, 0x72, 0x69,
      0x65, 0x73, 0x50, 0xC3, 0x00, 0x00, 0x02, 0x44,
      0x4B, 0x00, 0x00, 0x89, 0x01, 0x00, 0x00, 0x00,
      0x00, 0x00, 0x00, 0xB0, 0x3F, 0x00, 0x00, 0x00,
      0x00, 0x00, 0x00, 0xB0, 0x3F, 0x2D, 0x00, 0x2D,
      0x00, 0x0C, 0x00, 0x01, 0x00, 0x0A, 0x00, 0x10,
      0x2A, 0x37, 0x2E, 0x35, 0x27, 0x20, 0x4D, 0x61,
      0x70, 0x20, 0x53, 0x65, 0x72, 0x69, 0x65, 0x73,
      0x0F, 0x3C, 0x00, 0x00, 0x02, 0x44, 0x51, 0x00,
      0x00, 0x00, 0x01, 0x9A, 0x99, 0x99, 0x99, 0x99,
      0x99, 0x99, 0x3F, 0x9A, 0x99, 0x99, 0x99, 0x99,
      0x99, 0x89, 0x3F, 0x5A, 0x00, 0x2D, 0x00, 0x0D,
      0x00, 0x01, 0x00, 0x28, 0x00
    };

    gbfwrite(header_bytes, sizeof(header_bytes), 1, tpo_file_out);
  }

  else {
    fatal(MYNAME ": writing ouput for state \"%s\" is not currently supported.\n", output_state);
  }
}

static void
tpo_track_hdr(const route_head *rte)
{
  double amt;
  unsigned char temp_buffer[8];
  unsigned char visibility_flags[] = { 0x01, 0x00, 0x01, 0x00, 0x01, 0x00, 0x01, 0x00, 0x01, 0x00 };
  unsigned char unknown1[] = { 0xFF, 0x00, 0x00, 0x00 };
  unsigned char bounding_box[8] = { 0x00, 0x80, 0x00, 0x80, 0xFF, 0x7F, 0xFF, 0x7F };

  waypoint* first_track_waypoint = (waypoint*) QUEUE_FIRST(&rte->waypoint_list);

  /* zoom level 1-5 visibility flags */
  gbfwrite(visibility_flags, 1, sizeof(visibility_flags), tpo_file_out);

  /* 8 bytes of zeros, meaning unknown */
  memset(temp_buffer, 0, sizeof(temp_buffer));
  gbfwrite(temp_buffer, 1, sizeof(temp_buffer), tpo_file_out);

  /* 4 more unknown bytes, possibly sign flags for the longitude and latitude? */
  gbfwrite(unknown1, 1, sizeof(unknown1), tpo_file_out);

  /* the starting point of the route */
  /* convert lat/long to NAD27/CONUS datum */
  GPS_Math_WGS84_To_Known_Datum_M(
    first_track_waypoint->latitude,
    first_track_waypoint->longitude,
    first_track_waypoint->altitude,
    &first_track_waypoint_lat,
    &first_track_waypoint_lon,
    &amt,
    78);

  /* swap the sign back *after* the datum conversion */
  first_track_waypoint_lon *= -1.0;

  /* Compute this track's scaling factors: Used for scaling each track point and then
     later written out to the track footer. These are approximately the ratios between
     pixels and degrees when viewing the 1:24000 map in TOPO!. In practice, it doesn't
     appear to be necessary that they be correct, as long as the same values are used
     for doing the scaling and for writing into the track footer data. */
  output_track_lat_scale = 4.8828125e-005; /* TOPO! appears to use a constant lat scale */
  output_track_lon_scale = output_track_lat_scale / cos(GPS_Math_Deg_To_Rad(first_track_waypoint_lat));

  /* 8 bytes - longitude */
  gbfputdbl(first_track_waypoint_lon, tpo_file_out);

  /* 8 bytes - latitude */
  gbfputdbl(first_track_waypoint_lat, tpo_file_out);

  /* 8 bytes: seems to be bounding box info */
  gbfwrite(bounding_box, 1, sizeof(bounding_box), tpo_file_out);

  /* number of route points */
  gbfputint16(rte->rte_waypt_ct, tpo_file_out);

  /* initialize the track length computation */
  track_length = 0;
  GPS_Math_WGS84LatLonH_To_XYZ(
    first_track_waypoint->latitude,
    first_track_waypoint->longitude,
    0.0,
    &last_waypoint_x,
    &last_waypoint_y,
    &last_waypoint_z);
}

static void
tpo_track_disp(const waypoint *waypointp)
{
  double lat, lon, amt, x, y, z;
  short lat_delta, lon_delta;

  /* fprintf(stderr, "%f/%f\n", waypointp->latitude, waypointp->longitude); */

  /* convert lat/lon position to XYZ meters */
  GPS_Math_WGS84LatLonH_To_XYZ(
    waypointp->latitude,
    waypointp->longitude,
    0.0,
    &x,
    &y,
    &z);

  /* increase the track length by the 3D length of last track segment in feet */
  track_length += METERS_TO_FEET(sqrt(
                                   (x - last_waypoint_x) * (x - last_waypoint_x) +
                                   (y - last_waypoint_y) * (y - last_waypoint_y) +
                                   (z - last_waypoint_z) * (z - last_waypoint_z)));
  last_waypoint_x = x;
  last_waypoint_y = y;
  last_waypoint_z = z;

  /* convert lat/long to NAD27/CONUS datum */
  GPS_Math_WGS84_To_Known_Datum_M(
    waypointp->latitude,
    waypointp->longitude,
    waypointp->altitude,
    &lat,
    &lon,
    &amt,
    78);

  /* swap the sign back *after* the datum conversion */
  lon *= -1.0;

  /* longitude delta from first route point */
  lon_delta = (short)((first_track_waypoint_lon - lon) / output_track_lon_scale);
  gbfputint16(lon_delta, tpo_file_out);

  /* latitude delta from first route point */
  lat_delta = (short)((first_track_waypoint_lat - lat) / output_track_lat_scale);
  gbfputint16(lat_delta, tpo_file_out);

  /*
  fprintf(stderr, "%f %f: %x %x - %f %f %f / %f\n", lon, lat, lon_delta, lat_delta, first_track_waypoint_lat, lat, output_track_lat_scale, (first_track_waypoint_lat - lat) );
  */

}

static void
tpo_track_tlr(const route_head *rte)
{
  unsigned char unknown1[] = { 0x06, 0x00 };

  unsigned char continue_marker[] = { 0x01, 0x80 };
  unsigned char end_marker[] = { 0x00, 0x00 };

  /* pixel to degree scaling factors */
  gbfputdbl(output_track_lon_scale, tpo_file_out);
  gbfputdbl(output_track_lat_scale, tpo_file_out);

  /* 4 bytes: the total length of the route */
  gbfputint32(track_length, tpo_file_out);

  /* 2 unknown bytes */
  gbfwrite(unknown1, 1, sizeof(unknown1), tpo_file_out);

  /* the last track ends with 0x0000 instead of 0x0180 */
  track_out_count++;
  if (track_out_count == track_count()) {
    gbfwrite(end_marker, 1, sizeof(end_marker), tpo_file_out);
  } else {
    gbfwrite(continue_marker, 1, sizeof(continue_marker), tpo_file_out);
  }
}

static void
tpo_wr_init(const char *fname)
{
  if (doing_wpts || doing_rtes) {
    fatal(MYNAME ": this file format only supports tracks, not waypoints or routes.\n");
  }

  tpo_file_out = gbfopen_le(fname, "wb", MYNAME);
  tpo_write_file_header();
}

static void
tpo_wr_deinit(void)
{
  /* the file footer is six bytes of zeroes */
  unsigned char file_footer_bytes[6];
  memset(file_footer_bytes, 0, sizeof(file_footer_bytes));
  gbfwrite(file_footer_bytes, 1, sizeof(file_footer_bytes), tpo_file_out);

  gbfclose(tpo_file_out);
}

static void
tpo_write(void)
{
  unsigned char unknown1[] = { 0xFF, 0xFF, 0x01, 0x00 };

  char* chunk_name = "CTopoRoute";
  int chunk_name_length = strlen(chunk_name);

  /* write the total number of tracks */
  gbfputint16(track_count(), tpo_file_out);

  /* 4 unknown bytes */
  gbfwrite(unknown1, 1, 4, tpo_file_out);

  /* chunk name: "CTopoRoute" */
  gbfputint16(chunk_name_length, tpo_file_out);
  gbfwrite(chunk_name, 1, chunk_name_length, tpo_file_out);

  track_out_count = 0;
  track_disp_all(tpo_track_hdr, tpo_track_tlr, tpo_track_disp);
}

/* TPO 2.x format can read tracks only */
ff_vecs_t tpo2_vecs = {
  ff_type_file,   /* ff_type_internal */
  /*    { ff_cap_none | ff_cap_none, ff_cap_read | ff_cap_write, ff_cap_none | ff_cap_none }, */
  { ff_cap_none, ff_cap_read,  ff_cap_none },
  tpo_rd_init,
  tpo_wr_init,
  tpo_rd_deinit,
  tpo_wr_deinit,
  tpo_read,
  tpo_write,
  NULL,
  tpo2_args,
  CET_CHARSET_ASCII, 0	/* CET-REVIEW */
};

/* TPO 3.x format can read waypoints/tracks/routes */
ff_vecs_t tpo3_vecs = {
  ff_type_file,   /* ff_type_internal */
  { ff_cap_read, ff_cap_read, ff_cap_read },
  tpo_rd_init,
  tpo_wr_init,
  tpo_rd_deinit,
  tpo_wr_deinit,
  tpo_read,
  tpo_write,
  NULL,
  tpo3_args,
  CET_CHARSET_ASCII, 0	/* CET-REVIEW */
};
