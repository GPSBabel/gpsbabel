/*
	writegpx.c

	Extract data from MS Streets & Trips .est and Autoroute .axe files in GPX format.

    Copyright (C) 2003 James Sherring, james_sherring@yahoo.com

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


	This app depends on istorage & istorage-make from Pabs (pabs3@zip.to)
	and James Clark's Expat xml parser from http://www.libexpat.org/.

*/
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "st2gpx.h"
#include "gpx.h"
#include "ppinutil.h"
#include "journey.h"
#include "annotations.h"

#define GPX_HEADER1 "<?xml version=\"1.0\" standalone=\"yes\"?>\n<gpx version=\"1.0\" \n  xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\"\n  xmlns=\"http://www.topografix.com/GPX/1/0\"\n  xsi:schemaLocation=\"http://www.topografix.com/GPX/1/0 http://www.topografix.com/GPX/1/0/gpx.xsd\"\n "
#define GPX_HEADER2 " creator=\"st2gpx-james_sherring@yahoo.com\">\n"
#define GPX_THIS_PROGRAM "st2gpx from james_sherring@yahoo.com"
#define GPX_FOOTER "</gpx>"

#define GPX_WPT 0
#define GPX_RTEPT 1
#define GPX_TRKPT 2
char * gpxptypelabel[3] = {"wpt", "rtept", "trkpt"};

FILE* gpx_open_write_file_header(char* gpx_out_file_name)
{
	FILE* gpx_out_file=NULL;

	if (gpx_out_file_name)
	{
		gpx_out_file = fopen(gpx_out_file_name, "w");
		if (gpx_out_file == NULL)
		{
			fprintf(stderr, "Cannot open %s\n", gpx_out_file_name);
			debug_pause();
			exit(1);
		}
	}
	else
		return NULL;

	if (gpx_out_file!=NULL)
	{
		fprintf(gpx_out_file, GPX_HEADER1);
		fprintf(gpx_out_file, GPX_HEADER2);
		if (opts.source_file_name)
			fprintf(gpx_out_file, "\t<desc>This file was created from %s by %s</desc>\n",
					opts.source_file_name, GPX_THIS_PROGRAM);
		// Some more tags we should add here
		//	<name> Descriptive name of the GPX file
		//	<desc> Description of the GPX file
		//	<author> Name of the file's creator
		//	<time> Creation date/time of the GPX file
	}
	return gpx_out_file;
}

void gpx_write_file_trailer(FILE* gpx_out_file)
{
	if (gpx_out_file!=NULL)
	{
		fprintf(gpx_out_file, GPX_FOOTER);
	    fclose(gpx_out_file);
	}
}

void gpx_write_point(FILE* gpx_out_file, struct gpxpt * pt, int pt_type, char* opt_elms)
// FIXEM do <name> from pt, not from opt_elms
// FIXME same for other atts defined in gpxpt
{
	if (gpx_out_file!=NULL)
	{
		if (pt_type == GPX_TRKPT)
			fprintf(gpx_out_file, "\t\t");
		else if (pt_type == GPX_RTEPT)
			fprintf(gpx_out_file, "\t");
		fprintf(gpx_out_file, "\t<%s lat=\"%f\" lon=\"%f\">%s</%s>\n",
				gpxptypelabel[pt_type], pt->lat, pt->lon,
				opt_elms, gpxptypelabel[pt_type]);
	}
}

void gpx_write_jour_point(FILE* gpx_out_file, struct journey * jour, struct jour_rtept * wpt)
{
	struct gpxpt * pt = gpxpt_new();
	char* opt_elms;
	int optlen;
	struct f_jour_pt_head * f_wpt_head;

	if (gpx_out_file==NULL)
		return;

	// FIXME should compare the ppin and wpt coords if both exist
	if (wpt->pushpin == NULL)
	{
		f_wpt_head = (struct f_jour_pt_head *)(jour->buf + (wpt->pthead_os));
		pt->lat=scaled2deg(f_wpt_head->scaled_lat);
		pt->lon=scaled2deg(f_wpt_head->scaled_lon);
		optlen = f_wpt_head->cbtext1 + 60;
		opt_elms = (char*)xmalloc(optlen);
		//FIXME use str2ascii??
		sprintf(opt_elms, "<name><![CDATA[%s]]></name>", wpt->text1);

	}
	else
	{
		optlen = strlen(wpt->pushpin->UdName) + strlen(wpt->pushpin->NoteShort) + 60;
		opt_elms = (char*)xmalloc(optlen);

		sprintf(opt_elms, "<name><![CDATA[%s]]></name><desc><![CDATA[%s]]></desc>",
				wpt->pushpin->UdName, wpt->pushpin->NoteShort);

		pt->lat = wpt->pushpin->lat;
		pt->lon = wpt->pushpin->lon;
	}

	gpx_write_point(gpx_out_file, pt, GPX_RTEPT, opt_elms);

	gpxpt_delete(pt);
	free(opt_elms);
}

void gpx_write_jour_header(FILE* gpx_out_file)
{
	if(gpx_out_file!=NULL)
	{
		fprintf(gpx_out_file, "\t<rte>\n");
		fprintf(gpx_out_file, "\t\t<name>Journey</name>\n");
		fprintf(gpx_out_file, "\t\t<src>Extracted from main Journey</src>\n");
	}
}

void gpx_write_jour_trailer(FILE* gpx_out_file)
{
	if(gpx_out_file!=NULL)
		fprintf(gpx_out_file, "\t</rte>\n");

}

void gpx_write_journey(FILE* gpx_out_file, struct journey * jour)
{
	int j;
	if( (gpx_out_file!=NULL) && (jour!=NULL) && (jour->count_rtepts>0) )
	{
		printf("Writing gpx route for Journey\n");
		gpx_write_jour_header(gpx_out_file);
		for(j=0; j< jour->count_rtepts; j++)
			gpx_write_jour_point(gpx_out_file, jour, jour->rtept_list+j);
		gpx_write_jour_trailer(gpx_out_file);
	}
}

void gpx_write_pushpinlist (FILE* gpx_out_file, struct pushpin_safelist *ppplist)
{
	int i;
	struct gpxpt * pt=NULL;
	char* opt_elms;
	int optlen;

	if ((gpx_out_file==NULL) || (ppplist==NULL))
		return;

	pt = gpxpt_new();

	printf("writting gpx waypoints for %d pushpins\n", ppplist->num_pushpins);

	for (i=0; i<ppplist->num_pushpins; i++)
	{
		if (ppplist->pushpin_list[i]==NULL)
			break;

		optlen = strlen(ppplist->pushpin_list[i]->UdName)
				+ strlen(ppplist->pushpin_list[i]->NoteShort) + 60;
		opt_elms = (char*)xmalloc(optlen);

		sprintf(opt_elms, "<name><![CDATA[%s]]></name><desc><![CDATA[%s]]></desc>",
				ppplist->pushpin_list[i]->UdName, ppplist->pushpin_list[i]->NoteShort);

		pt->lat = ppplist->pushpin_list[i]->lat;
		pt->lon = ppplist->pushpin_list[i]->lon;

		gpx_write_point(gpx_out_file, pt, GPX_WPT, opt_elms);
		free(opt_elms);
	}
	fprintf(gpx_out_file, "\n");
	gpxpt_delete(pt);
}

void gpx_write_annot_rec(FILE* gpx_out_file, const struct annot_rec * rec)
{
	int pt_type;
	int p;
	char opt_elms[200];
	struct gpxpt * pt;

	if ( (gpx_out_file==NULL) || (rec==NULL) )
	{
		return;
	}

	pt_type = GPX_TRKPT ;
	if (opts.use_gpx_route)
		pt_type = GPX_RTEPT;
	switch (rec->type)
	{
	case ANNOT_TYPE_LINE:
		// FIXME lots of format and name info to be added here


		// *******************
		//  rte or trk header
		// *******************

		if(opts.use_gpx_route)
		{
			fprintf(gpx_out_file, "\t<rte>\n");
			fprintf(gpx_out_file, "\t\t<src>Extracted from Annotation %d (%s)</src>\n",
					rec->annot_num, annot_type_name[rec->type]);
			fprintf(gpx_out_file, "\t\t<name>RT%04d</name>\n", rec->annot_num);
		}
		else
		{
			fprintf(gpx_out_file, "\t<trk>\n");
			fprintf(gpx_out_file, "\t\t<name>TK%04d</name>\n", rec->annot_num);
			fprintf(gpx_out_file, "\t\t<src>Extracted from Annotation %d (%s)</src>\n",
					rec->annot_num, annot_type_name[rec->type]);
			fprintf(gpx_out_file, "\t\t<trkseg>\n");
		}

		// *******************
		//  rte or trk points
		// *******************


		for (p=0; p < rec->line_points; p++)
		{
			pt=gpx_get_point(rec->buf + rec->line_offset + 12*p);
			if(pt==NULL)
			{
				printf("got null pt #%p in annotation %d, skipping more points in this annotation\n",
						p, rec->annot_num);
				break;
			}
			if(opts.use_gpx_route)
				sprintf(opt_elms, "<name>rp%04d</name>", p);
			else
				// we need to include a name for trackpoints
				// for them to be recognised by easygps.
				sprintf(opt_elms, "<name>tp%04d</name>", p);
			gpx_write_point(gpx_out_file, pt, pt_type, opt_elms);
			gpxpt_delete(pt);
		}

		// ********************
		//  rte or trk trailer
		// ********************

		if(opts.use_gpx_route)
			fprintf(gpx_out_file, "\t</rte>\n");
		else
			fprintf(gpx_out_file, "\t\t</trkseg>\n\t</trk>\n");

	case ANNOT_TYPE_OVAL:
	case ANNOT_TYPE_TEXT:
	case ANNOT_TYPE_CIRCLE:
	default:
		break;
	}
}

void gpx_write_annotations(FILE* gpx_out_file, struct annotations * annots)
{
	int i;
	if ( (gpx_out_file!=NULL) && (annots!=NULL) )
		for (i=0; i < annots->num_annotations; i++)
		{
			gpx_write_annot_rec(gpx_out_file, annots->annot_list[i]);
		}
}

void gpx_write_all(char* gpx_out_file_name,
				   struct pushpin_safelist *ppplist,
				   struct journey * jour,
				   struct annotations * annots)
{
	FILE* gpx_out_file;
	if ( (ppplist==NULL) && (jour==NULL) && (annots==NULL) )
		return;
	gpx_out_file = gpx_open_write_file_header(gpx_out_file_name);
	gpx_write_pushpinlist(gpx_out_file, ppplist);
	gpx_write_journey(gpx_out_file, jour);
	gpx_write_annotations(gpx_out_file, annots);
	gpx_write_file_trailer(gpx_out_file);
}

