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


FILE			*infile	=0;
FILE			*ofs	=0;
gbuint32		count	=0;

#define	VITOSMT_HEADER	24
#define	VITOSMT_DATA	64

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
	double result=0;

	fread(buffer, sizeof (buffer), 1, f);
	le_read64(&result,buffer);
	return result;
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
Skip(FILE * f,
     size_t distance)
{
	fseek(f, distance, SEEK_CUR);
}

void
WriteDouble(void* ptr, double d)
{
  unsigned char result[8]="\0\0\0\0\0\0\0\0";

  le_read64(result, &d);
  memcpy(ptr, result, 8);

  return;
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
	gbuint32 	version			=2;
	gbuint32 	subversion		=1000;
	const size_t 	recsize			=VITOSMT_DATA;
	route_head	*track_head		=0; 
	waypoint	*wpt_tmp		=0;
	double		latrad			=0;
	double		lonrad			=0;
	double		elev			=0;
	unsigned char*	timestamp		=0;
	struct tm	tmStruct		={0,0,0,0,0,0,0,0,0};
	double		seconds			=0.0;
	double		speed			=0.0;
	double		heading			=0.0;
	double		dop			=0.0;
	unsigned char*	unknown			=0;
	char		shortname[10]		="\0";
	char		description[40]		="\0";
	int		serial			=0;

	/* 
	 * 24 bytes header 
	 */
	version = ReadLong(infile);		/* 2	*/
	subversion = ReadLong(infile);		/* 1000	*/
	count = ReadLong(infile);		/* total trackpoints */
	ReadLong(infile);			/* first trackpoint  */
	ReadLong(infile);			/* last trackpoint   */
	ReadLong(infile);			/* total trackpoints */

	while (count) {
		/*
		 *	64 bytes of data	
		 */
		if (feof(infile)||ferror(infile)) 
		{
			fatal("%s reading input file.  Error was '%s'.\n",
				MYNAME, strerror(errno));
		}

		latrad		=ReadDouble(infile);	/* WGS84 latitude in radians */
		lonrad		=ReadDouble(infile);	/* WGS84 longitude in radians */
		elev		=ReadDouble(infile);	/* elevation in meters */
		timestamp	=ReadRecord(infile,5);	/* local time */
		seconds		=ReadDouble(infile);	/* seconds */
		speed		=ReadDouble(infile);    /* possibly ground speed, unknown units*/
		heading		=ReadDouble(infile);	/* heading in degrees */
		dop		=ReadDouble(infile);	/* dilution of precision */
		unknown		=ReadRecord(infile,3); 	/* remainder, unknown fmt */

		wpt_tmp = waypt_new();
		
		wpt_tmp->latitude	=(latrad * 180) / M_PI;
		wpt_tmp->longitude	=(lonrad * 180) / M_PI;
		wpt_tmp->altitude	=elev;

		tmStruct.tm_year	=timestamp[0]+100;
		tmStruct.tm_mon		=timestamp[1]-1;
		tmStruct.tm_mday	=timestamp[2];
		tmStruct.tm_hour	=timestamp[3];
		tmStruct.tm_min		=timestamp[4];
		tmStruct.tm_sec 	=(int)floor(seconds);
		tmStruct.tm_isdst	=-1;

		wpt_tmp->creation_time = mktime(&tmStruct) + get_tz_offset();
		wpt_tmp->centiseconds = fmod(100*seconds+0.5,100);
	
		snprintf(shortname, sizeof shortname-1 , "WP%04d", ++serial);
		snprintf(description, sizeof description-1, 
			"Spd=%.1lf Hdg=%03.0lf DoP=%.1lf Flg=%02x%02x%02x", 
			speed,heading,dop,unknown[0],unknown[1],unknown[2]);
		
		wpt_tmp->shortname = xstrdup(shortname);
		wpt_tmp->wpt_flags.shortname_is_synthetic = 1;
		wpt_tmp->notes = xstrdup(description);

		switch (global_opts.objective)
		{
			case wptdata:
				waypt_add(wpt_tmp);
				break;
			case trkdata:
				if (track_head == NULL) {
					track_head = route_head_alloc();
					track_add_head(track_head);
				}
				route_add_wpt(track_head, wpt_tmp);
				break;
			default:
				fatal(MYNAME ": This file type only supports "
					"waypoint or track (-w or -t) mode.");

				break;
		}

		xfree(timestamp);
		xfree(unknown);

		count--;
	}
}

static void
wr_init(const char *fname)
{
	warning(MYNAME ":format is experimental and may crash Vito Navigator II.\n");
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
	const size_t	recsize				=VITOSMT_DATA;
	unsigned char	workbuffer[VITOSMT_DATA]	="\0";
	size_t		position			=0;
	struct tm*	tmstructp			=0;
	double		seconds				=0;
	double		worknum				=0;

	++count;
	memset(workbuffer,0,recsize);

	WriteDouble(&workbuffer[position], (M_PI*waypointp->latitude)/180 ); 
	position += sizeof(double);
	WriteDouble(&workbuffer[position], (M_PI*waypointp->longitude)/180 );
	position += sizeof(double);
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

	if (fwrite(workbuffer,recsize,1,ofs)!=1)
	{
		fatal("%s writing output file.  Error was '%s'.\n",
			MYNAME, strerror(errno));
	}

}

static void
vitosmt_track_hdr(const route_head *rte)
{
}

static void
vitosmt_track_tlr(const route_head *rte)
{
}

void
vitosmt_write(void)
{
	time_t 		now 				= 0;
	const size_t 	recsize				=VITOSMT_HEADER;
	const gbuint32 	version				=2;
	const gbuint32 	subversion			=1000;
	unsigned char	workbuffer[VITOSMT_HEADER]	="\0";
	size_t		position			=0;

	now = current_time();
	count = 0;
	position = 0;

	/* leave a spacer for the header */
	memset(workbuffer,0,recsize);
	if (fwrite(workbuffer,recsize,1,ofs)!=1)
	{
		fatal("%s writing output file.  Error was '%s'.\n",
			MYNAME, strerror(errno));
	}

	/* dump waypoints and tracks */
	waypt_disp_all(vitosmt_waypt_pr);
	track_disp_all(vitosmt_track_hdr, vitosmt_track_tlr, vitosmt_waypt_pr);

	/* write the complete the header */
	le_write32(&workbuffer[position],version);
	position += sizeof(gbuint32);
	le_write32(&workbuffer[position],subversion);
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
	if (fwrite(workbuffer,recsize,1,ofs)!=1)
	{
		fatal("%s writing output file.  Error was '%s'.\n",
			MYNAME, strerror(errno));
	}


}

ff_vecs_t vitosmt_vecs = {
	ff_type_file,
	{ ff_cap_read | ff_cap_write, ff_cap_read | ff_cap_write, ff_cap_none},
	rd_init,
	wr_init,
	rd_deinit,
	wr_deinit,
	vitosmt_read,
	vitosmt_write,
	NULL, 
	NULL
};
