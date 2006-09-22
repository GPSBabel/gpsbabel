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

#include <stddef.h>
#include <stdio.h>
#include <string.h>
#include <errno.h>

#define MYNAME "vitosmt"
#include "defs.h"
#include "grtcirc.h"

static FILE				*infile	=0;
static FILE				*ofs	=0;
static long				count	=0;

const long				vitosmt_version			=2;
const long				vitosmt_subversion		=1000;
const size_t			vitosmt_headersize		=24;
const size_t			vitosmt_datasize		=64;
const double mile2km	=1.609344;				/* mile/h to kilometer/h */
const double kts2mps	=0.5144444444444444444;	/* knots to m/s */
const double mph2mps 	=0.447039259;			/* mile/h to m/s     */

static unsigned long
ReadLong(FILE * f)
{
	gbuint32 result = 0;

	fread(&result, sizeof (result), 1, f);
	return le_read32(&result);
}

static double
ReadDouble(FILE * f)
{
	unsigned char buffer[8] = "\0\0\0\0\0\0\0\0";
	fread(buffer, sizeof (buffer), 1, f);
	return le_read_double(buffer );
}


static unsigned char *
ReadRecord(FILE * f,
	   size_t size)
{
	unsigned char *result = (unsigned char *) xmalloc(size);

	fread(result, size, 1, f);
	return result;
}

static void
WriteDouble(void* ptr, double d)
{
  unsigned char result[8]="\0\0\0\0\0\0\0\0";
  le_write_double(result,d);
  memcpy(ptr, result, 8);
}


static void
rd_init(const char *fname)
{
	infile = xfopen(fname, "rb", MYNAME);
}

static void
rd_deinit(void)
{
	fclose(infile);
}

static void
vitosmt_read(void)
{
	long			version			=0;
	long			subversion		=0;
	long			check1			=-1;
	long			check2			=-2;
	long			check3			=-3;
	route_head		*route_head		=0; 
	waypoint		*wpt_tmp		=0;
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
	version		= ReadLong(infile);	/* 2	*/
	subversion	= ReadLong(infile);	/* 1000	*/
	count		= ReadLong(infile);	/* n	*/
	check1		= ReadLong(infile);	/* 0	*/
	check2		= ReadLong(infile);	/* not sure */
	check3		= ReadLong(infile);	/* n	*/

	if (version!=vitosmt_version) {
	
		fatal("%s (%d) reading file.  Unsupported version %ld.%ld\n",
			MYNAME, __LINE__, version, subversion );
	}

	if (subversion!=vitosmt_subversion) {
		warning("%s (%d) reading file.  Unsafe version %ld.%ld\n",
			MYNAME, __LINE__, version, subversion );
	}

	if ((count!=check3)		||
	    (check1!=count-1)	||
		(check3!=count)		) {

		fatal("%s (%d) reading file. Invalid file header\n", 
			MYNAME, __LINE__ );

	}

	while (count) {
		/*
		 *	64 bytes of data	
		 */
		if (feof(infile)||ferror(infile)) 
		{
			warning("%s (%d) reading file.  Unexpected end of file %s\n",
				MYNAME, __LINE__, strerror(errno) );
			break;
		}

		latrad		=ReadDouble(infile);	/* WGS84 latitude in radians */
		lonrad		=ReadDouble(infile);	/* WGS84 longitude in radians */
		elev		=ReadDouble(infile);	/* elevation in meters */
		timestamp	=ReadRecord(infile,5);	/* UTC time yr/mo/dy/hr/mi */
		seconds		=ReadDouble(infile);	/* seconds */
		speed		=ReadDouble(infile);    /* speed in knots */
		course		=ReadDouble(infile);	/* course in degrees */
		pdop     	=ReadDouble(infile);	/* dilution of precision */
		gpsfix		=fgetc(infile);			/* fix type x08,x10, x20  */	
		gpsvalid	=fgetc(infile);			/* fix is valid */
     	gpssats	=fgetc(infile);				/* number of sats */

		wpt_tmp = waypt_new();
		
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

		wpt_tmp->creation_time = mkgmtime(&tmStruct);
		wpt_tmp->centiseconds = fmod(100*seconds+0.5,100);
	
		wpt_tmp->shortname	=xcalloc(16,1);
		snprintf(wpt_tmp->shortname, 15 , "WP%04d", ++serial);

		wpt_tmp->speed	= speed*kts2mps; /* meters per second */
		wpt_tmp->course = course;
		wpt_tmp->pdop	= pdop;

		/* 
			GPS Fix data
		*/
		if (gpsvalid&0x7) {
			if		(gpsfix==0)	
				wpt_tmp->fix 		=fix_none;
			if		(gpsfix&0x8)	
				wpt_tmp->fix 		=fix_2d;
			else if	(gpsfix&0x10)	
				wpt_tmp->fix 		=fix_3d;
			else if	(gpsfix&0x20)	
				wpt_tmp->fix 		=fix_dgps;
			else
				wpt_tmp->fix 		=fix_unknown;
			
			/* <sat> */
			wpt_tmp->sat = gpssats;
		}
		else
			wpt_tmp->fix 		=fix_unknown;

		if (doing_wpts)			/* process as waypoints */
		{
			waypt_add(wpt_tmp);
		} 
		else if (doing_rtes)	/* process as route */
		{
			if (route_head == NULL)	{
				route_head = route_head_alloc();
				route_add_head(route_head);
			}
			route_add_wpt(route_head, wpt_tmp);
		}
		else					/* default track mode */
		{
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
wr_init(const char *fname)
{
	warning(MYNAME " write: format is experimental and may crash Vito Navigator II.\n");
	ofs = xfopen(fname, "wb", MYNAME);
}

static void
wr_deinit(void)
{
	fclose(ofs);

}

static void
vitosmt_waypt_pr(const waypoint *waypointp)
{
	unsigned char *	workbuffer		=0;
	size_t			position		=0;
	struct tm*		tmstructp		=0;
	double			seconds			=0;

	++count;
	workbuffer = xcalloc(vitosmt_datasize,1);

	WriteDouble(&workbuffer[position], RAD(waypointp->latitude) ); 
	position += sizeof(double);
	WriteDouble(&workbuffer[position], RAD(waypointp->longitude) );
	position += sizeof(double);
	if ( waypointp->altitude-1 > unknown_alt)
		WriteDouble(&workbuffer[position], waypointp->altitude );
	position += sizeof(double);

	tmstructp =  gmtime(&waypointp->creation_time);
	seconds = (double) tmstructp->tm_sec + 0.01*waypointp->centiseconds;

	workbuffer[position++]	=tmstructp->tm_year-100;
	workbuffer[position++]	=tmstructp->tm_mon+1;
	workbuffer[position++]	=tmstructp->tm_mday;
	workbuffer[position++]	=tmstructp->tm_hour;
	workbuffer[position++]	=tmstructp->tm_min;
	
	WriteDouble(&workbuffer[position], seconds );
	position += sizeof(double);

	/* speed */
	if (waypointp->speed>0) 
		WriteDouble(&workbuffer[position], waypointp->speed / mph2mps );
	position += sizeof(double);
	
	/* course */
	if ((waypointp->course>=-360.0)&&(waypointp->course<=360.0))
		WriteDouble(&workbuffer[position], waypointp->course );
	position += sizeof(double);

	/* pdop */
	if (waypointp->pdop>0)
		WriteDouble(&workbuffer[position], waypointp->pdop );
	position += sizeof(double);


	/* fix type */
	switch (waypointp->fix)
	{
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
	
	if ((waypointp->sat>0)&&(waypointp->sat<128))
		workbuffer[position++] = waypointp->sat;
	else
		workbuffer[position++] = 0;
	
	if (fwrite(workbuffer,vitosmt_datasize,1,ofs)!=1)
	{
		fatal("%s (%d) writing output file.  Error was '%s'.\n",
			MYNAME, __LINE__, strerror(errno));
	}
	
	xfree(workbuffer);
}


static void
vitosmt_write(void)
{
	time_t now = 0;
	unsigned char *	workbuffer					=0;
	size_t			position					=0;

	workbuffer = xcalloc(vitosmt_headersize,1);

	now = current_time();
	count = 0;
	position = 0;

	/* leave a spacer for the header */
	memset(workbuffer,0,vitosmt_headersize);
	if (fwrite(workbuffer,vitosmt_headersize,1,ofs)!=1)
	{
		fatal("%s (%d) writing output file.  Error was '%s'.\n",
			MYNAME, __LINE__, strerror(errno));
	}

	if 	(doing_wpts)	/* process as waypoints */
	{
		waypt_disp_all(vitosmt_waypt_pr);
	} 
	else if (doing_rtes)	/* process as route */
	{
		route_disp_all(NULL, NULL, vitosmt_waypt_pr);
	}
	else			/* default track mode */
	{
		track_disp_all(NULL, NULL, vitosmt_waypt_pr);
	}


	/* write the complete the header */
	le_write32(&workbuffer[position],vitosmt_version);
	position += sizeof(gbuint32);
	le_write32(&workbuffer[position],vitosmt_subversion);
	position += sizeof(gbuint32);
	le_write32(&workbuffer[position],count);
	position += sizeof(gbuint32);
	le_write32(&workbuffer[position],0);
	position += sizeof(gbuint32);
	le_write32(&workbuffer[position],count-1);
	position += sizeof(gbuint32);
	le_write32(&workbuffer[position],count);
	position += sizeof(gbuint32);

	rewind(ofs);
	if (fwrite(workbuffer,vitosmt_headersize,1,ofs)!=1)
	{
		fatal("%s (%d) writing output file.  Error was '%s'.\n",
			MYNAME, __LINE__, strerror(errno));
	}

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
