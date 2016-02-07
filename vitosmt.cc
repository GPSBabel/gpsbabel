/*
    Read Vito Navigator .SMT tracks

    Copyright (C) 2005 Etienne TASSE

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
#include "grtcirc.h"
#include <cmath>
#include <errno.h>

#define MYNAME "vitosmt"

static gbfile*				infile	=NULL;
static gbfile*				ofs	=NULL;
static long				count	=0;

const long				vitosmt_version			=2;
const long				vitosmt_subversion		=1000;
const size_t			vitosmt_headersize		=24;
const size_t			vitosmt_datasize		=64;

static unsigned char*
ReadRecord(gbfile* f, gbsize_t size)
{
  unsigned char* result = (unsigned char*) xmalloc(size);

  gbfread(result, size, 1, f);
  return result;
}

static void
WriteDouble(void* ptr, double d)
{
  unsigned char result[9]="\0\0\0\0\0\0\0\0";
  le_write_double(result,d);
  memcpy(ptr, result, 8);
}


static void
rd_init(const QString& fname)
{
  infile = gbfopen_le(fname, "rb", MYNAME);
}

static void
rd_deinit(void)
{
  gbfclose(infile);
}

static void
vitosmt_read(void)
{
  long			version			=0;
  long			subversion		=0;
  long			check1			=-1;
  long			check2			=-2;
  long			check3			=-3;
  route_head*		route_head		=0;
  Waypoint*		wpt_tmp		=0;
  double			latrad			=0;
  double			lonrad			=0;
  double			elev			=0;
  unsigned char*	timestamp		=0;
  struct tm		tmStruct;
  double			seconds			=0.0;
  double			speed			=0.0;
  double			course			=0.0;
  double			pdop 			=0.0;
  unsigned char	gpsfix			=0;
  unsigned char	gpsvalid		=0;
  unsigned char	gpssats			=0;
  int				serial			=0;


  memset(&tmStruct, 0, sizeof(tmStruct));
  /*
   * 24 bytes header
   */
  version		= gbfgetint32(infile);	/* 2	*/
  subversion	= gbfgetint32(infile);	/* 1000	*/
  count		= gbfgetint32(infile);	/* n	*/
  check1		= gbfgetint32(infile);	/* 0	*/
  check2		= gbfgetint32(infile);	/* not sure */
  (void) check2; // silence warning.
  check3		= gbfgetint32(infile);	/* n	*/

  if (version!=vitosmt_version) {

    fatal("%s (%d) reading file.  Unsupported version %ld.%ld\n",
          MYNAME, __LINE__, version, subversion);
  }

  if (subversion!=vitosmt_subversion) {
    warning("%s (%d) reading file.  Unsafe version %ld.%ld\n",
            MYNAME, __LINE__, version, subversion);
  }

  if ((count!=check3)		||
      (check1!=count-1)	||
      (check3!=count)) {

    fatal("%s (%d) reading file. Invalid file header\n",
          MYNAME, __LINE__);

  }

  while (count) {
    /*
     *	64 bytes of data
     */
    if (gbfeof(infile)||gbferror(infile)) {
      warning("%s (%d) reading file.  Unexpected end of file %s\n",
              MYNAME, __LINE__, strerror(errno));
      break;
    }
#if 0
    fprintf(stderr, "Looptop %d\n", gbftell(infile));
#endif
    latrad		=gbfgetdbl(infile);	/* WGS84 latitude in radians */
    lonrad		=gbfgetdbl(infile);	/* WGS84 longitude in radians */
    elev		=gbfgetdbl(infile);	/* elevation in meters */
#if 0
    fprintf(stderr, "before %d\n", gbftell(infile));
#endif
    timestamp	=ReadRecord(infile,5);	/* UTC time yr/mo/dy/hr/mi */
#if 0
    fprintf(stderr, "%d latrad %f/%f ele %f\n", gbftell(infile),latrad, DEG(latrad), elev);
#endif
    seconds		=gbfgetdbl(infile);	/* seconds */
    speed		=gbfgetdbl(infile);    /* speed in knots */
    course		=gbfgetdbl(infile);	/* course in degrees */
    pdop     	=gbfgetdbl(infile);	/* dilution of precision */
    gpsfix		=gbfgetc(infile);	/* fix type x08,x10, x20  */
    gpsvalid	=gbfgetc(infile);	/* fix is valid */
    gpssats		=gbfgetc(infile);	/* number of sats */

    wpt_tmp = new Waypoint;

    wpt_tmp->latitude	=DEG(latrad);
    wpt_tmp->longitude	=DEG(lonrad);
    wpt_tmp->altitude	=elev;

    tmStruct.tm_year	=timestamp[0]+100;
    tmStruct.tm_mon		=timestamp[1]-1;
    tmStruct.tm_mday	=timestamp[2];
    tmStruct.tm_hour	=timestamp[3];
    tmStruct.tm_min		=timestamp[4];
    tmStruct.tm_sec 	=(int)floor(seconds);
    tmStruct.tm_isdst	=-1;

    double usec = fmod(1000000*seconds+0.5,1000000);
    wpt_tmp->SetCreationTime(mkgmtime(&tmStruct), lround(usec/1000.0));
    wpt_tmp->shortname = QString().sprintf("WP%04d", ++serial);

    WAYPT_SET(wpt_tmp, speed, KNOTS_TO_MPS(speed)); /* meters per second */
    WAYPT_SET(wpt_tmp, course, course);
    wpt_tmp->pdop	= pdop;

    /*
    	GPS Fix data
    */
    if (gpsvalid&0x7) {
      if	(gpsfix==0) {
        wpt_tmp->fix 		=fix_none;
      }
      if	(gpsfix&0x8) {
        wpt_tmp->fix 		=fix_2d;
      } else if	(gpsfix&0x10) {
        wpt_tmp->fix 		=fix_3d;
      } else if	(gpsfix&0x20) {
        wpt_tmp->fix 		=fix_dgps;
      } else {
        wpt_tmp->fix 		=fix_unknown;
      }

      /* <sat> */
      wpt_tmp->sat = gpssats;
    } else {
      wpt_tmp->fix 		=fix_unknown;
    }

    if (doing_wpts) {		/* process as waypoints */
      waypt_add(wpt_tmp);
    } else if (doing_rtes) {	/* process as route */
      if (route_head == NULL)	{
        route_head = route_head_alloc();
        route_add_head(route_head);
      }
      route_add_wpt(route_head, wpt_tmp);
    } else {				/* default track mode */
      if (route_head == NULL)	{
        route_head = route_head_alloc();
        track_add_head(route_head);
      }
      track_add_wpt(route_head, wpt_tmp);
    }

    xfree(timestamp);

    count--;
  }
}

static void
wr_init(const QString& fname)
{
  warning(MYNAME " write: format is experimental and may crash Vito Navigator II.\n");
  ofs = gbfopen_le(fname, "wb", MYNAME);
}

static void
wr_deinit(void)
{
  gbfclose(ofs);

}

static void
vitosmt_waypt_pr(const Waypoint* waypointp)
{
  unsigned char* 	workbuffer		=0;
  size_t			position		=0;
  double			seconds			=0;

  ++count;
  workbuffer = (unsigned char*) xcalloc(vitosmt_datasize,1);

  WriteDouble(&workbuffer[position], RAD(waypointp->latitude));
  position += sizeof(double);
  WriteDouble(&workbuffer[position], RAD(waypointp->longitude));
  position += sizeof(double);
  if (waypointp->altitude-1 > unknown_alt) {
    WriteDouble(&workbuffer[position], waypointp->altitude);
  }
  position += sizeof(double);
  QDate date(waypointp->GetCreationTime().date());
  QTime time(waypointp->GetCreationTime().time());
  workbuffer[position++]	= date.year()-100;
  workbuffer[position++]	= date.month();
  workbuffer[position++]	= date.day();
  workbuffer[position++]	= time.hour();
  workbuffer[position++]	= time.minute();

  WriteDouble(&workbuffer[position], seconds);
  position += sizeof(double);

  /* speed */
  if (waypointp->speed>0) {
    WriteDouble(&workbuffer[position], MPS_TO_MPH(waypointp->speed));
  }
  position += sizeof(double);

  /* course */
  if ((waypointp->course>=-360.0)&&(waypointp->course<=360.0)) {
    WriteDouble(&workbuffer[position], waypointp->course);
  }
  position += sizeof(double);

  /* pdop */
  if (waypointp->pdop>0) {
    WriteDouble(&workbuffer[position], waypointp->pdop);
  }
  position += sizeof(double);


  /* fix type */
  switch (waypointp->fix) {
  case fix_2d:
    workbuffer[position++] = 0x08;
    break;
  case fix_3d:
    workbuffer[position++] = 0x10;
    break;
  case fix_dgps:
    workbuffer[position++] = 0x20;
    break;
  default:
    workbuffer[position++] = 0;
    break;
  }

  /* Assume position is valid */
  workbuffer[position++] = 0x07;

  if ((waypointp->sat>0)&&(waypointp->sat<128)) {
    workbuffer[position++] = waypointp->sat;
  } else {
    workbuffer[position++] = 0;
  }

  (void)gbfwrite(workbuffer,vitosmt_datasize,1,ofs);

  xfree(workbuffer);
}


static void
vitosmt_write(void)
{
  unsigned char* 	workbuffer					=0;
  size_t			position					=0;

  workbuffer = (unsigned char*) xcalloc(vitosmt_headersize,1);

  count = 0;
  position = 0;

  /* leave a spacer for the header */
  memset(workbuffer,0,vitosmt_headersize);
  (void)gbfwrite(workbuffer,vitosmt_headersize,1,ofs);

  if	(doing_wpts) {	/* process as waypoints */
    waypt_disp_all(vitosmt_waypt_pr);
  } else if (doing_rtes) {	/* process as route */
    route_disp_all(NULL, NULL, vitosmt_waypt_pr);
  } else {		/* default track mode */
    track_disp_all(NULL, NULL, vitosmt_waypt_pr);
  }


  /* write the complete the header */
  le_write32(&workbuffer[position],vitosmt_version);
  position += sizeof(uint32_t);
  le_write32(&workbuffer[position],vitosmt_subversion);
  position += sizeof(uint32_t);
  le_write32(&workbuffer[position],count);
  position += sizeof(uint32_t);
  le_write32(&workbuffer[position],0);
  position += sizeof(uint32_t);
  le_write32(&workbuffer[position],count-1);
  position += sizeof(uint32_t);
  le_write32(&workbuffer[position],count);
  position += sizeof(uint32_t);

  gbfrewind(ofs);
  (void)gbfwrite(workbuffer,vitosmt_headersize,1,ofs);

  xfree(workbuffer);
}

ff_vecs_t vitosmt_vecs = {
  ff_type_file,
  FF_CAP_RW_ALL,
  rd_init,
  wr_init,
  rd_deinit,
  wr_deinit,
  vitosmt_read,
  vitosmt_write,
  NULL,
  NULL,
  CET_CHARSET_UTF8, 1	/* do nothing | CET-REVIEW */
};
