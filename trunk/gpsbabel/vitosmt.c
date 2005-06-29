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
unsigned long	count	=0;

const size_t vitosmt_headersize		=24;
const size_t vitosmt_datasize		=64;
const double mile2km		=1.609344;		/* mile/h to kilometer/h */
const double mph2mps 		=0.447039259;	/* mile/h to m/s     */

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
	unsigned short version			=0;
	unsigned short	stringlen		=0;
	route_head		*route_head		=0; 
	waypoint		*wpt_tmp		=0;
	double			latrad			=0;
	double			lonrad			=0;
	double			elev			=0;
	unsigned char*	timestamp		=0;
	struct tm		tmStruct		={0,0,0,0,0,0,0,0,0};
	double			seconds			=0.0;
	double			speed			=0.0;
	double			course			=0.0;
	double			pdop 			=0.0;
	unsigned char	gpsfix			=0;
	unsigned char	gpsvalid		=0;
	unsigned char	gpssats			=0;
	int				serial			=0;
	xml_tag *		xml_curr		=0;
	char			buffer[80]		="\0";

		
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
		timestamp	=ReadRecord(infile,5);	/* UTC time yr/mo/dy/hr/mi */
		seconds		=ReadDouble(infile);	/* seconds */
		speed		=ReadDouble(infile);    /* speed in miles per hour */
		course		=ReadDouble(infile);	/* course in degrees */
		pdop     	=ReadDouble(infile);	/* dilution of precision */
		gpsfix		=fgetc(infile);			/* fix type x08,x10, x20  */	
		gpsvalid	=fgetc(infile);			/* fix is valid */
     	gpssats	=fgetc(infile);				/* number of sats */

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

		wpt_tmp->creation_time = mkgmtime(&tmStruct);
		wpt_tmp->centiseconds = fmod(100*seconds+0.5,100);
	
		wpt_tmp->shortname	=xcalloc(16,1);
		snprintf(wpt_tmp->shortname, 15 , "WP%04d", ++serial);

		wpt_tmp->speed	= speed*mph2mps; /* meters per second */
		wpt_tmp->course = course;
		wpt_tmp->pdop	= pdop;

		/* 
			GPS Fix data
		*/
		if (gpsvalid&0x7) {
			
			wpt_tmp->gpx_extras		=(xml_tag *)xcalloc(sizeof(xml_tag),1);
				
			/* <fix> */
			xml_curr				=wpt_tmp->gpx_extras;
			xml_curr->tagname		=xstrdup("fix");
			if		(gpsfix&0x8)	
				xml_curr->cdata		=xstrdup("2d");
			else if	(gpsfix&0x10)	
				xml_curr->cdata		=xstrdup("3d");
			else if	(gpsfix&0x20)	
				xml_curr->cdata		=xstrdup("dgps");
			else
				xml_curr->cdata		=xstrdup("none");
			xml_curr->cdatalen 		=strlen(xml_curr->cdata);
			
			/* <sat> */
			xml_curr->sibling		=(xml_tag *)xcalloc(sizeof(xml_tag),1);
			xml_curr				=xml_curr->sibling;
			xml_curr->tagname		=xstrdup("sat");
			snprintf(buffer, sizeof(buffer), "%d", gpssats);
			xml_curr->cdata			=xstrdup(buffer);
			xml_curr->cdatalen 		=strlen(xml_curr->cdata);
			
		}

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
			route_add_wpt(route_head, wpt_tmp);
		}

		xfree(timestamp);

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

xml_tag *xml_find( xml_tag *cur, char *tagname ) 
{
	xml_tag *result = cur;
	while ( result && case_ignore_strcmp( result->tagname, tagname ))
	{
		result = result->sibling;
	} ;
	return result;
}


static void
vitosmt_waypt_pr(const waypoint *waypointp)
{
	unsigned char *	workbuffer		=0;
	size_t			position		=0;
	struct tm*		tmstructp		=0;
	double			seconds			=0;
	double			worknum			=0;
	xml_tag*		xmltagp			=0;

	++count;
	workbuffer = xcalloc(vitosmt_datasize,1);

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

	/* speed */
	WriteDouble(&workbuffer[position], waypointp->speed / mph2mps );
	position += sizeof(double);
	
	/* course */
	WriteDouble(&workbuffer[position], waypointp->course );
	position += sizeof(double);


	/* pdop */
	WriteDouble(&workbuffer[position], waypointp->pdop );
	position += sizeof(double);


	/* fix type */
	xmltagp = xml_find(waypointp->gpx_extras,"fix");
	if (xmltagp) {
		if		(case_ignore_strcmp(xmltagp->cdata,"dgps"))
			workbuffer[position++] = (unsigned char) 0x20;
		else if	(case_ignore_strcmp(xmltagp->cdata,"3d"))
			workbuffer[position++] = (unsigned char) 0x10;
		else if	(case_ignore_strcmp(xmltagp->cdata,"2d"))
			workbuffer[position++] = (unsigned char) 0x08;
		else
			workbuffer[position++] = (unsigned char) 0;
	}	
	
	workbuffer[position++] = 0x7;
	
	xmltagp = xml_find(waypointp->gpx_extras,"sat");
	if (xmltagp) {
		workbuffer[position++] = (unsigned char) atoi(xmltagp->cdata);
	}	

	
	if (fwrite(workbuffer,vitosmt_datasize,1,ofs)!=1)
	{
		fatal("%s writing output file.  Error was '%s'.\n",
			MYNAME, strerror(errno));
	}
	
	xfree(workbuffer);
}


void
vitosmt_write(void)
{
	time_t now = 0;
	const unsigned short version				=2;
	const unsigned short subversion				=1000;
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
		fatal("%s writing output file.  Error was '%s'.\n",
			MYNAME, strerror(errno));
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
	if (fwrite(workbuffer,vitosmt_headersize,1,ofs)!=1)
	{
		fatal("%s writing output file.  Error was '%s'.\n",
			MYNAME, strerror(errno));
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
	NULL
};
