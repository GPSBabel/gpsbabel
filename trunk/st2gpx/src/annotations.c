/*
	annotations.c

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
#include <malloc.h>
#include <memory.h>
#include <math.h>
#include <string.h>

#include "gpx.h"
#include "st2gpx.h"
#include "pushpins.h"

#include "annotations.h"

// std_anotfile_header[8,9] are variable (number of annotations)
char std_annotfile_header[12]    //  num annots
	= {0x34, 0x12, 0x00, 0x2d, 0x03, 0x00, 0x00, 0x00, 0x01, 0x00, 0x00, 0x00};

char std_annot_linerec_header_v3[LINE_REC_LEN_V3] =
//   type                   annot#1               ,
	{0x00, 0x00, 0x00, 0x00, 0x01, 0x00, 0x00, 0x00, // 0x00
//show length,
//   order
//behind roads
	 0x08, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, // 0x08
	 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, // 0x10
	 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, // 0x18
//   blue                     2pt
	 0x12, 0x00, 0x00, 0x00, 0x28, 0x00, 0x00, 0x00, // 0x20
//                         show line
	 0x00, 0x00, 0x00, 0x00, 0x01, 0x00, 0x00, 0x00, // 0x28
	 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, // 0x30
	 0x00};

char std_annot_linerec_header_v4[LINE_REC_LEN_V4] =
//   type                   annot#1               ,
	{0x00, 0x00, 0x00, 0x00, 0x01, 0x00, 0x00, 0x00, // 0x00

	 0x00, 0x00, 0x00, 0x00,

//show length,
//   order
//behind roads
	 0x08, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
	 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
	 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
//   blue                     2pt
	 0x12, 0x00, 0x00, 0x00, 0x28, 0x00, 0x00, 0x00,
//                         show line
	 0x00, 0x00, 0x00, 0x00, 0x01, 0x00, 0x00, 0x00,
	 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
	 0x00};

//struct parameters {
// Also need to include variable text length + line-points with these

// These change with file version, so handle in code
int annot_rec_len[4]={57, 868, 124, 868};
//int annot_head_len[4]={57, 48, 48, 48};

char * annot_type_name[4]={"Line", "Oval", "Textbox", "Circle"};
//} tag_parameters;

// constants for converting from MS Grid & Precision to latitude and longitude
// double magic1=182.044444444444; // = 2^16/360
// double magic2=0x10000;

/*
typedef struct annotationbuf
{
	int annot_num;
	int flags;		// bit fields in first byte
	int unkn1;		// normally 0
	int unkn2;		// ? seems to have byte fields
	int text_len;
	// + 2*text_len bytes of text here
	int fill_color; // seems to have byte fields. First byte is fill, other bytes are strange
	int line_color;
	int line_thickness; // 20 times the point-size
	int arrow_type; 	// 00=none, 01=left, 02=right, 03=both
	int unkn3;
	int joinflag;		// 0 line does not join, 1, joins. Not sure what large balues for non-lines mean - xscale?
	char unkn4;			// y-scale?
	int num_points;		// different meaning for non-lines
} structannotationbuf;
*/


struct annot_rec * annot_rec_new()
{
	struct annot_rec * nw = (struct annot_rec *)xmalloc(sizeof(struct annot_rec));
	nw->type=-1;
	nw->length=0;
	nw->buf=NULL;
	nw->annot_num = -1;
	nw->text_length = 0;
	nw->text = NULL;
	nw->line_points=0;
	nw->line_offset=0;
	return nw;
}

void annot_rec_delete(struct annot_rec * annot_rec)
{
	//int i;
	if (annot_rec==NULL)
		return;
	//for(i=0; i<annot_rec->line_points)
	free(annot_rec->buf);
	free(annot_rec->text);
	free(annot_rec);
}

struct annotations * annotations_new()
{
	struct annotations * nw = (struct annotations*)xmalloc(sizeof(struct annotations));
	nw->annot_list=NULL;
	nw->header_buf=NULL;
	nw->max_annot_num=0;
	nw->num_annotations=0;
	nw->read_recs_ok_flag=0;
	nw->read_tail_ok_flag=0;
	nw->stream_length=0;
	nw->tail_buf_length=0;
	nw->version=0;
	return nw;
}

void annotations_delete(struct annotations * annots)
{
	int i;
	if (annots==NULL)
		return;

	for (i=0; i<annots->num_annotations; i++)
		annot_rec_delete(annots->annot_list[i]);
	free(annots->annot_list);
	free(annots->header_buf);
	free(annots);
}

struct gpxpt* gpx_get_point(char* buf)
// Convert the 12-byte location structure in annotations stream to GPS coordinates.
{
	struct gpxpt * pt = gpxpt_new();
	double x;
	double y;
	double z;

	if (buf==NULL)
	{
		printf("Trying to read line point from NULL\n");
		gpxpt_delete(pt);
		return NULL;
	}

	x= *(float *)(buf);
	y= *(float *)(buf + 4);
	z= *(float *)(buf + 8);

	//printf("gpx_get_point x=%f y=%f z=%f\n",x,y,z);
	pt->lat =  atan2(z,sqrt(pow(x,2)+ pow(y,2)))*180/M_PI;
	pt->lon =  atan2(y,x)*180/M_PI;
	// 6378137 is earths equatorial radius in meters
	// set elevation for curiosity, but dont enable it with pt->use_elevation
	pt->elevation= (sqrt(pow(x,2)+ pow(y,2)+ pow(z,2))-1)*6378137;
	return pt;
}

struct annot_rec * read_annot_rec(FILE* annot_in_file, int version)
{
	int status;
	int i;
	int bit_flags=0;
	int text_offset;
	int text_len_offset;
	int line_offset;
	char* rec_type=NULL;

	struct annot_rec * rec = annot_rec_new();
	int head_len = 0;
	switch (version)
	{
	case 3:
		head_len = ANNOT_REC_HEAD_LEN;
		text_len_offset = ANNOT_RECOS_TEXTLEN;
		text_offset = ANNOT_RECOS_TEXT;
		line_offset = 57;
		break;
	case 4:
		head_len = ANNOT_REC_HEAD_LEN+4;
		text_len_offset = ANNOT_RECOS_TEXTLEN+4;
		text_offset = ANNOT_RECOS_TEXT+4;
		line_offset = 57 + 4;
		break;
	default:
		// FIXME version is passed as a param, should be handled there
		printf("Unrecognised record format type %d, unable to process more annotations.\n", rec->type);
		return NULL;
		break;
	}

	// **********************
	// Read the record header, up to begining of possible text
	// **********************

	rec->buf = (char*)xmalloc(head_len);
	// This read can fail because I have miscalculated size or number of records
	// So exit gracefully
	status = readbytes(annot_in_file, rec->buf, head_len);
	if (status!=head_len)
	{
		//FIXME
//		gpx_write_file_trailer(gpx_out_file);
		// exit(1)
		// should do some cleaning up here
		return rec;
	}

	rec->type = *(int*)(rec->buf+ANNOT_RECOS_TYPE);
	if (rec->type > 3 )
	{
		printf("Unrecognised record type %d, unable to process more annotations\n", rec->type);
		printbuf(rec->buf, head_len);
		return NULL;
	}

	rec->annot_num = *(int*)(rec->buf+ANNOT_RECOS_ANUM);
	// FIXME kludge, fit into the framework
	bit_flags = *(int*)(rec->buf + 8);
	rec->text_length = *(int*)(rec->buf+text_len_offset);
	rec->line_points=0;
	if (rec->type == ANNOT_TYPE_LINE)
		rec->line_points = *(int*)(rec->buf+head_len-4);

	// ***************************
	// Calculate the record length
	// ***************************

	rec->length = annot_rec_len[rec->type] + 2*rec->text_length + 12*rec->line_points;
	if (version > 3)
		rec->length += 4;

	// FIXME This is a kludge
	if (opts.st_version_num>10)
	{
		if ( (rec->type == 1) || (rec->type == 4) )
		{
			printf("Fudge: shortening 336 bytes from oval record length, but I dont know why.\n");
			(rec->length) -= 336;
		}
	}

	if (rec->type == ANNOT_TYPE_LINE)
		rec->line_offset = line_offset;

	if (rec->type<4)
		rec_type =annot_type_name[rec->type];
	else
		rec_type=NULL;

	if (opts.verbose_flag > 4)
		printf("Got annotation id %d, of type %d=%s, text length %d\n",
			rec->annot_num, rec->type, rec_type, rec->text_length);

	// ****************************
	// Read the rest of the record, from begining of possible text
	// ****************************

	rec->buf = (char*)xrealloc(rec->buf, rec->length);
	status = readbytes(annot_in_file, rec->buf+head_len, rec->length - head_len);

	if (status!=(rec->length - head_len))
	{
		printf("Unable to read next part of annotation record - quiting\n");
		// fixme - we should stop processing annots but continue with rest
		debug_pause();
		exit(1);
	}

	if(rec->text_length>0)
	{
		rec->text=(char*)xmalloc(rec->text_length+1);
		rec->text[rec->text_length]=0x0;
		for (i=0;i<rec->text_length;i++)
			rec->text[i]=rec->buf[text_offset + 2*i];
		str2ascii(rec->text);
	}

	if (opts.verbose_flag > 1)
		print_annot_rec(rec);

	if (opts.verbose_flag > 4)
	{
		printf("Recognised record with length %i\n",rec->length);
		printbuf(rec->buf, rec->length);
	}

	return rec;
}

struct annotations * process_annotations_stream(char* annot_in_file_name)
{
	// Read the annotation file header
	int i;
	int status;
	struct annot_rec * rec;
	char* checkEOF;
	int read_tail_buff=1;
	FILE* annot_in_file=NULL;
	struct annotations * annots = annotations_new();

	annots->header_buf=(char*)xmalloc(ANNOT_FILE_HEAD_LEN);

	if ((annot_in_file = fopen(annot_in_file_name, "rb")) == NULL)
	{
		fprintf(stderr, "Quitting because I cannot open %s\n", annot_in_file_name);
		debug_pause();
		exit(1);
	}

	status=readbytes(annot_in_file, annots->header_buf, ANNOT_FILE_HEAD_LEN);
	if (status!=ANNOT_FILE_HEAD_LEN)
	{
		printf("Cant make any more sense of annotations stream, continuing\n");
		return annots;
	}
	annots->stream_length += ANNOT_FILE_HEAD_LEN;

	for (i=1; i<8; i++)
		if ( annots->header_buf[i] != std_annotfile_header[i] )
			printf("Nonstandard annotations file header, header[%i]=0x%x, normal value is 0x%x\n",
					i, annots->header_buf[i], std_annotfile_header[i] );

	annots->version = *(int*)(annots->header_buf+4);
	annots->num_annotations = *(int*)(annots->header_buf+8);

	annots->annot_list = (struct annot_rec **)xmalloc(
								(annots->num_annotations)*sizeof(struct annot_rec*));

	if (opts.verbose_flag > 1)
	{
		printf("Annotations file header, version %d, indicates %d annotations in file\n",
				annots->version, annots->num_annotations);
	}

	if (opts.verbose_flag > 4)
	{
		printf("Dumping file header:\n");
		printbuf(annots->header_buf, ANNOT_FILE_HEAD_LEN);
	}

	/*
	** Read all the annotation records
	*/
	annots->read_recs_ok_flag = 1;
	for (i=0; i < annots->num_annotations; i++)
	{
	    rec = read_annot_rec(annot_in_file, annots->version);

		if (opts.explore_flag)
			explore_annot(rec);
		if (rec==NULL)
		{
			annots->read_recs_ok_flag = 0;
			annots->num_annotations = i-1;
			read_tail_buff=0;
			break;
		}

		annots->annot_list[i] = rec;
		annots->max_annot_num = __max(annots->max_annot_num, rec->annot_num);
		annots->stream_length += rec->length;
	}

	// Check that we are at the end of annotation file
	if (read_tail_buff)
	{
		checkEOF = xmalloc(4);
		status=readbytes(annot_in_file, checkEOF, 4);
		annots->stream_length += status;
		if ( (status!=4) || (checkEOF[0]!=0) || (checkEOF[1]!=0)
				|| (checkEOF[2]!=0) || (checkEOF[3]!=0) || (getc(annot_in_file)!=EOF) )
		{
			fprintf (stderr, "Did not finish reading annotation file at EOF\n");
		}
		else
			annots->read_tail_ok_flag=1;
		free(checkEOF);
	}
	fclose(annot_in_file);
	return annots;
}
