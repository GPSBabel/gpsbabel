/*
	nannol.c

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
#include <memory.h>
#include <math.h>

#include "gpx.h"
#include "st2gpx.h"
#include "annotations.h"

//static int readheight = 0;

struct annot_rec * gpx2annot_line_rec(struct gpxpt** ptlist, int num_pts, int annot, int version)
{
	int i;
	float x;
	float y;
	float z;
	double lat;
	double lon;
	int count_points_offset;
	int line_offset;

	struct annot_rec * rec = annot_rec_new();

	switch (version)
	{
	case 3:
		count_points_offset = 53;
		line_offset = 57;
		rec->buf = (char*)xmalloc(line_offset+12*num_pts);
		// copy the standard annotation header,
		// with appropriate values set
		memcpy(rec->buf, std_annot_linerec_header_v3, LINE_REC_LEN_V3);
		break;
	case 4:
		count_points_offset = 53 +4;
		line_offset = 57 + 4;
		rec->buf = (char*)xmalloc(line_offset+12*num_pts);
		// copy the standard annotation header,
		// with appropriate values set
		memcpy(rec->buf, std_annot_linerec_header_v4, LINE_REC_LEN_V4);
		break;
	default:
		// FIXME version is passed as a param, should be handled there
		printf("Unrecognised record format type %d, unable to process more annotations.\n", rec->type);
		return NULL;
		break;
	}

	// set the annotation (record) number
	memcpy(rec->buf+4, &annot, 4);
	// set the number of points in the line
	memcpy(rec->buf+count_points_offset, &num_pts, 4);

	for (i=0; i<num_pts; i++)
	{
		lat = ptlist[i]->lat;
		lon = ptlist[i]->lon;
		// FIXME how do I specify calc in double but return float?
		x = (float)(cos(lon*M_PI/180)*cos(lat*M_PI/180));
		y = (float)(sin(lon*M_PI/180)*cos(lat*M_PI/180));
		z = (float)sin(lat*M_PI/180);

		// We can do the height calculations also, 
		// and s&t will use this in some calculations, including distance,
		// but there is no way to visualise or edit height in s&t.
		// The example below is for cylindrical height, not radial height.

/*		if (readheight) {
			// 6378137 is earths equatorial radius in meters, 20km less at the poles
			fac=1+(height/6378137);
			x *= fac;
			y *= fac;
			z *= fac;
*/
		memcpy(rec->buf + line_offset + 12*i,     &x, 4);
		memcpy(rec->buf + line_offset + 12*i + 4, &y, 4);
		memcpy(rec->buf + line_offset + 12*i + 8, &z, 4);
	}

	rec->length = line_offset + 12*num_pts;
	rec->annot_num = annot;
	rec->line_points = num_pts;
	rec->line_offset = line_offset;
	rec->type = ANNOT_TYPE_LINE;
	return rec;
}

struct annotations * merge_gpx_annot(struct annotations * annots, struct gpx_data* all_gpx)
{	
	int i;
	int this_annot_num=0;
	int tot_num_annots=0;
	struct gpxrte * this_rte=NULL;
	struct gpxtrk * this_trk=NULL;

	if ( (all_gpx==NULL) || (all_gpx->rte_list_count + all_gpx->trk_list_count==0) )
		return annots;

	if (annots==NULL)
		annots = annotations_new();

	tot_num_annots = annots->num_annotations + all_gpx->rte_list_count 
						+ all_gpx->trk_list_count;

	//printf("reallocing annots, from %d annotations to %d annotations\n", tot_num_annots);

	annots->annot_list = (struct annot_rec**)xrealloc(annots->annot_list, 
											tot_num_annots*sizeof(struct annot_rec*));

	// append the gpx routes as annotation lines
	for (i=0; i < (all_gpx->rte_list_count); i++) 
	{
		this_annot_num = annots->num_annotations  + i;
		this_rte = all_gpx->rte_list[i];
		annots->annot_list[this_annot_num] 
			= gpx2annot_line_rec(this_rte->rtept_list, 
								 this_rte->rtept_list_count, 
								 (annots->max_annot_num)+i+1,
								 annots->version);

	}

	this_rte=NULL;
	annots->num_annotations += all_gpx->rte_list_count;
	annots->max_annot_num   += all_gpx->rte_list_count;

	// append the gpx tracks as annotation lines
	for (i=0; i < all_gpx->trk_list_count; i++)
	{
		this_annot_num = annots->num_annotations + i;
		this_trk = all_gpx->trk_list[i];
		annots->annot_list[this_annot_num] 
			= gpx2annot_line_rec(this_trk->trkpt_list, 
								 this_trk->trkpt_list_count, 
								 (annots->max_annot_num)+i+1,
								 annots->version);
	}
	annots->num_annotations += all_gpx->trk_list_count;
	annots->max_annot_num   += all_gpx->trk_list_count;

	memcpy(annots->header_buf+8, &(annots->num_annotations), 4);

	return annots;
}

void write_annotations(struct annotations * annots, char* annot_out_file_name)
{	// Write annotation file header
	int status;
	int i;
	char eostream[4]={0x00, 0x00, 0x00, 0x00};
	FILE* annot_out_file;

	printf("Writing new annotation stream to %s\n", annot_out_file_name);

	if ((annot_out_file = fopen(annot_out_file_name, "wb")) == NULL) {
           fprintf(stderr, "Cannot open %s\n", annot_out_file_name);
		   fflush(stdout);
			debug_pause();
           exit(1);
	   }

	status = fwrite(annots->header_buf, ANNOT_FILE_HEAD_LEN, 1, annot_out_file);
	if (status != 1)
		printf("expected to write %d, actually wrote %d\n", ANNOT_FILE_HEAD_LEN, status);

	for(i=0; i< annots->num_annotations; i++) 
	{
		status = fwrite(annots->annot_list[i]->buf, annots->annot_list[i]->length, 1, annot_out_file);
		if (status != 1)
			printf("expected to write %d, actually wrote %d\n", annots->annot_list[i]->length, status);
	}

	status = fwrite(eostream, 4, 1, annot_out_file);
	if (status != 1)
		printf("expected to write %d, actually wrote %d\n", 4, status);

    fclose(annot_out_file);
}
