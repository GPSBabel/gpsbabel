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

#define MYNAME "vitosmt"
#include "defs.h"

FILE			*infile;
static char *  	arg_tzoffset=NULL;

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
	/* unsigned char result[8] = "\0\0\0\0\0\0\0\0"; */
	double result=0;

	fread(&result, sizeof (result), 1, f);
	return result;
}


static unsigned char *
ReadRecord(FILE * f,
	   unsigned long size)
{
	unsigned char *result = (unsigned char *) xmalloc(size);

	fread(result, size, 1, f);
	return result;
}

static void
Skip(FILE * f,
     unsigned long distance)
{
	fseek(f, distance, SEEK_CUR);
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
	unsigned short version			=0;
	unsigned long count				=0;
	const unsigned long recsize		=64;
	unsigned short	stringlen		=0;
	route_head		*track_head		=0; 
	waypoint		*wpt_tmp		=0;
	double			latrad			=0;
	double			lonrad			=0;
	double			elev			=0;
	unsigned char*	timestamp		=0;
	struct tm		tmStruct		={0,0,0,0,0,0,0,0,0};
	double			seconds			=0.0;
	double			speed			=0.0;
	double			heading			=0.0;
	double			dop				=0.0;
	unsigned char*	unknown			=0;
	char			shortname[10]	="\0";
	char			description[40]	="\0";
	int				serial			=0;
	int				iTzoffset		=0;

	/* Compute offset between local time zone and time offset argument */
	if (arg_tzoffset)	{
		iTzoffset = get_tz_offset() - (int)(60*60*atof(arg_tzoffset));
	}

	/* 
	 * 24 bytes header 
	 */
	version = ReadLong(infile);	/* 2	*/
	ReadLong(infile);			/* 1000	*/
	count = ReadLong(infile);	/* 600	*/
	ReadLong(infile);			/* 0	*/
	ReadLong(infile);			/* 599	*/
	ReadLong(infile);			/* 600	*/

	while (count) {
		/*
		 *	64 bytes of data	
		 */
		if (feof(infile)||ferror(infile)) 
		{
			break;
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

		wpt_tmp->creation_time = mktime(&tmStruct) + iTzoffset;
		wpt_tmp->centiseconds = fmod(100*seconds+0.5,100);
	
		snprintf(shortname, sizeof shortname-1 , "WP%04d", ++serial);
		snprintf(description, sizeof description-1, 
			"Spd=%.1lf Hdg=%03.0lf DoP=%4.1lf Flg=%02x%02x%02x", 
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
	fatal(MYNAME ":Not enough information is known about this format to write it.\n");
}

static
arglist_t vitosmt_args[] = {
	{"tzoffset", &arg_tzoffset, "Time zone offset of SMT file (hours)", NULL, ARGTYPE_FLOAT },
	{0, 0, 0, 0, 0}
};

ff_vecs_t vitosmt_vecs = {
	ff_type_file,
	{ ff_cap_read, ff_cap_read, ff_cap_none},
	rd_init,
	wr_init,
	rd_deinit,
	NULL,
	vitosmt_read,
	NULL,
	NULL, 
	vitosmt_args
};
