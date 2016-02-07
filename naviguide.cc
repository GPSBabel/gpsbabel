/*
    Naviguide Routes


    Copyright (C) 2009 Erez Zuler

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

#include "defs.h"
#include "csv_util.h"
#include "jeeps/gpsmath.h"
#include <QtCore/QDebug>
#include <QtCore/QTextCodec>
#include <math.h>

#define MYNAME        "Naviguide"


/************* Specific Naviguide data formats ****************/

/* Naviguide file header */
typedef struct {
  uint16_t nof_wp;    /* Little endean format */
  unsigned char pad1[6];      /* 0xff, 0xff, 0x01, 0x00, 0x06, 0x00 */
  char signature[9]; /* cWaypoint */
  unsigned char pad2[4];      /* 0x01, 0x00, 0x00, 0x00 */
} ng_file_header_t;

/* Naviguide waypoint/rout data  */
typedef struct {
  unsigned char pad1[8];   /*  0xfe, 0xff, 0xff, 0xff, 0x01, 0x00, 0x00, 0x00 */
  /* coordination are in old israeli grid */
  int32_t East;
  int32_t North;
  unsigned char pad2[2];  /* 0x01, 0x01 */
  uint32_t Alt;
  char CommentLength;
} ng_wp_data_t;

typedef struct {
  unsigned char pad1[2]; /* 0x01, 0x80 */
  uint16_t next_wp;
  unsigned char pad2[2]; /* 0x00, 0x00 */
} ng_next_wp_t;

typedef struct {
  unsigned char chHeaderLen;
  char strName[255];
  ng_wp_data_t wp_data;
} ng_wp_no_comment_t;


/* Global variables */

static gbfile* file_in, *file_out;
static uint16_t nof_wp;
static route_head* rte_head;
static ng_file_header_t ng_file_header;
static ng_wp_no_comment_t WPNC;
static ng_next_wp_t ng_next_wp;
static char strComment[101];

/* Process options */
/* wp - process only waypoints */
/* rte - process as route */
/* wprte - Process waypoints and route */
static char* process = NULL;
static char* reorder = NULL;
static int process_rte = 1;
static int reorder_wp = 0;

static char temp_short_name[5];




/* Forward declarations */
static void ng_read_file_header(void);

static
arglist_t ng_args[] = {
  {
    "output", &process, "'wp' - Create waypoint file , 'rte' - Create route file",
    "rte", ARGTYPE_STRING, ARG_NOMINMAX
  },
  {
    "reorder", &reorder, "'n' - Keep the existing wp name, 'y' - rename waypoints",
    "n", ARGTYPE_STRING, ARG_NOMINMAX
  },

  ARG_TERMINATOR
};

/*===================Utilities ==========================================*/

static void
ng_convert_datum(Waypoint* wpt)
{
  double lat, lon, east, north, alt;

  east = (double) WPNC.wp_data.East;
  north = (double) WPNC.wp_data.North;
  alt = (double) WPNC.wp_data.Alt;

  GPS_Math_ICS_EN_To_WGS84(east, north, &lat, &lon);
  wpt->latitude = lat;
  wpt->longitude = lon;
  wpt->altitude = alt;
}



/*=================== File read/write utilities ==========================================*/

static void
ng_fwrite_wp_data(const QString& s, const QString& d, ng_wp_data_t* wp_data, gbfile* f)
{
  int i;
  char z[50];

  memset(z, 0, 50);
  i = s.length();
  gbfwrite(&i, 1, 1, f);
  gbfwrite(STRFROMUNICODE(s), 1, i, f);

  gbfwrite(&wp_data->pad1[0], 8, 1, f);
  gbfputint32(wp_data->East, f);
  gbfputint32(wp_data->North, f);
  gbfwrite(&wp_data->pad2[0], 2, 1, f);
  gbfputint32(wp_data->Alt, f);

  i = d.length();
  gbfwrite(&i, 1, 1, f);
  gbfwrite(STRFROMUNICODE(d), 1, i, f);
  gbfwrite(z, 44, 1, f);
}

static void
ng_fwrite_next_wp(ng_next_wp_t* nwp, gbfile* f)
{
  gbfwrite(nwp->pad1, 2, 1, f);
  gbfputint16(nwp->next_wp, f);
  gbfwrite(nwp->pad2, 2, 1, f);
}

static void
ng_fread_wp_data(char* d, ng_wp_no_comment_t* wpnc, gbfile* f)
{

  int i;

  gbfread(&wpnc->chHeaderLen ,sizeof(wpnc->chHeaderLen), 1, f);
  gbfread(&wpnc->strName, wpnc->chHeaderLen, 1, f);
  wpnc->strName[wpnc->chHeaderLen] = 0;


  gbfread(&wpnc->wp_data, 8, 1, f);
  wpnc->wp_data.East = gbfgetint32(f);
  wpnc->wp_data.North = gbfgetint32(f);
  gbfread(&wpnc->wp_data.pad2,2, 1, f);
  wpnc->wp_data.Alt = gbfgetint32(f);
  gbfread(&wpnc->wp_data.CommentLength, 1, 1, f);
  i = (int)wpnc->wp_data.CommentLength;


  /* Read the comment field */
  gbfread(d, i + 44, 1, f);

}

static void
ng_fread_next_wp(ng_next_wp_t* nwp, gbfile* f)
{
  gbfread(&nwp->pad1, 2, 1, f);
  nwp->next_wp = gbfgetint16(f);
  gbfread(&nwp->pad2, 2, 1, f);
}

/* =================== Write data functions ====================================*/

static void
ng_fill_header_default(void)
{
  ng_file_header_t default_header = {
    0x00,
    {0xff, 0xff, 0x01, 0x00, 0x09, 0x00},
    {'C', 'W', 'a', 'y', 'P', 'o', 'i', 'n', 't'},
    {0x01, 0x00, 0x00, 0x00},
  };

  ng_file_header =default_header;

}


static void
ng_fill_waypoint_default(void)
{
  ng_wp_data_t default_wp  = {
    {0xfe, 0xff, 0xff, 0xff, 0x01, 0x00, 0x00, 0x00},
    0,
    0,
    {0x01, 0x01},
    0,
    0x00,

  };

  ng_next_wp_t default_ng_next_wp = {
    {0x01, 0x80},
    0,
    {0x00, 0x00},
  };

  WPNC.wp_data = default_wp;
  ng_next_wp = default_ng_next_wp;
}


static void
ng_waypt_rd(const Waypoint* wpt)
{
  char z[50];
  double lat, lon;
  static int current_wp_ix=0;

  memset(z, 0, 50);
  current_wp_ix++;
  ng_fill_waypoint_default();

  if (!GPS_Math_WGS84_To_ICS_EN(wpt->latitude, wpt->longitude, &lon, &lat)) {
    fatal(MYNAME ": Waypoint %d is out of the israeli grid area", current_wp_ix);
  }

  WPNC.wp_data.North = (int32_t)lat;
  WPNC.wp_data.East = (int32_t)lon;

  QString s;
  if (reorder_wp) {
    sprintf(temp_short_name, "A%03d", current_wp_ix);
    s = temp_short_name;
  }

  else {
    s = wpt->shortname;
  }

  ng_fwrite_wp_data(s, wpt->description, &WPNC.wp_data, file_out);


  /* if not Last WP, write the next one index */

  if (nof_wp > current_wp_ix) {
    ng_next_wp.next_wp = current_wp_ix + 1;

    ng_fwrite_next_wp(&ng_next_wp, file_out);

  }
}

static void
header_write(void)
{
  ng_file_header.nof_wp = nof_wp;
  gbfputint16(nof_wp, file_out);
  gbfwrite(&ng_file_header.pad1[0], 19, 1, file_out);

}


static void
data_write(void)
{
  nof_wp = waypt_count();
  if (nof_wp) {
    header_write();
    waypt_disp_all(ng_waypt_rd);
  } else {
    nof_wp = route_waypt_count();
    if (nof_wp) {
      header_write();
      route_disp_all(NULL, NULL, ng_waypt_rd);
    }
  }
}


static void
wr_init(const QString& fname)
{
  file_out = gbfopen_le(fname, "wb", MYNAME);
  ng_fill_header_default();
  if (NULL != reorder)
    if (!case_ignore_strcmp(reorder, "y")) {
      reorder_wp = 1;
    }

}

static void
wr_deinit(void)
{
  gbfclose(file_out);
}

/*=========================== Read data functions ==================================*/

static void
rd_init(const QString& fname)
{
  file_in = gbfopen_le(fname, "rb", MYNAME);

  ng_read_file_header();

  if (NULL != process) {
    if (!case_ignore_strcmp(process, "wp")) {
      process_rte = 0;
    }
    if (!case_ignore_strcmp(process, "rte")) {
      process_rte = 1;
    }
  }


}

static void
rd_deinit(void)
{
  gbfclose(file_in);
  file_in = NULL;
}



static void
ng_read_file_header(void)
{

  nof_wp = gbfgetint16(file_in);
  gbfread(&ng_file_header.pad1[0], 19, 1, file_in);
  ng_file_header.nof_wp = nof_wp;


  if (strncmp("CWayPoint", ng_file_header.signature, 9)) {
    fatal("\nInvalid Naviguide file format\n");
  }


}

static void
data_read(void)
{
  if (process_rte) {
    rte_head = route_head_alloc();
    route_add_head(rte_head);
  }

  for (int n = 0; n < nof_wp; ++n) {

    Waypoint* wpt_tmp = new Waypoint;

    /* Read waypoint data */

    ng_fread_wp_data(strComment, &WPNC, file_in);


    if (n < nof_wp - 1) {
      /*
      	gbfread (&ng_next_wp.pad1[0], 2, 1, file_in);
      	ng_next_wp.next_wp = gbfgetint16 (file_in);
      	gbfread (&ng_next_wp.pad2[0], 2, 1, file_in);
      	*/
      ng_fread_next_wp(&ng_next_wp, file_in);

    }
    /* Clear commas form the comment for CSV file commonality */
    for (unsigned i = 0; i <strlen(strComment); ++i) {
      if (strComment[i] == ',') {
        strComment[i] = ' ';
      }
    }

    /* put the data in the waypoint structure */
    ng_convert_datum(wpt_tmp);
    wpt_tmp->shortname = STRTOUNICODE(WPNC.strName);
    wpt_tmp->description = STRTOUNICODE(strComment);

    if (process_rte) {
      route_add_wpt(rte_head, wpt_tmp);
    } else {
      waypt_add(wpt_tmp);
    }
  }
} /* data_read */



ff_vecs_t ng_vecs = {
  ff_type_file,
  FF_CAP_RW_WPT,
  rd_init,
  wr_init,
  rd_deinit,
  wr_deinit,
  data_read,
  data_write,
  NULL,
  ng_args,
  CET_CHARSET_HEBREW, 0
};
