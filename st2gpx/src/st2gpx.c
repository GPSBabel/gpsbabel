/*
	st2gpxmain.c

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


History:

21-10-2003	annotations stream dump
27-10-2003	journey & pushpins using VB module
 8-11-2003	match journey route-points to pushpins,
			integrate parts into singel C/C++ exe (except istorage)
 9-11-2003  fixed invalid gpx

To do:
	Get lat & lon for journey route points not matched to pushpins
	Get point info for non-line annotations, e.g. text boxes.
	Import functionality
	Implement using platform independant libraries.

bugs:
	unable to delete UserData file after accessing with ADO.
*/

#include <stdio.h>
#include <stdlib.h>
#include <malloc.h>
#include <memory.h>
#include <math.h>
#include <string.h>

// #include <getopt.h>
#include "getopt.h"
#include "pushpins.h"

#ifndef M_PI
#define M_PI 3.1415926535
#endif

#define JOUR_FILE_HEAD_LEN 32
#define JOUR_FILE_TAIL_LEN 430
// + text1 +2*text2
#define JOUR_WPTREC_LEN 121

#define JOUR_FILEOS_NUMREC 16

#define JOURWPT_REC_HEAD_LEN 41

#define JOURWPT_RECOS_WPTNUM 0
#define JOURWPT_RECOS_STR1LEN 40
#define JOURWPT_RECOS_STR1 41
// + val(JOURWPT_RECOS_STR1LEN)
#define JOURWPT_RECOS_STR2LEN 44
#define JOURWPT_RECOS_STR2 45

#define ANNOT_FILE_HEAD_LEN 12
// This needs to be long enough to get enough params to decide which record,
// but no biger than the shortest record
#define ANNOT_REC_HEAD_LEN 57 //19

#define ANNOT_TYPE_LINE		0
#define ANNOT_TYPE_OVAL		1
#define ANNOT_TYPE_TEXT		2
#define ANNOT_TYPE_CIRCLE	3

#define REC_CLOSE_LEN 0
// offsets for parameters in annotation recods
#define ANNOT_RECOS_TYPE	0
#define ANNOT_RECOS_ANUM	4
#define ANNOT_RECOS_TEXTLEN	20
#define ANNOT_RECOS_TEXT	24
#define ANNOT_RECOS_LINENUMPOINTS 53
#define ANNOT_RECOS_LINEJOINFLAG 48
#define ANNOT_RECOS_XSCALE	48
#define ANNOT_RECOS_YSCALE	52

#define GPX_HEADER1 "<?xml version=\"1.0\" standalone=\"yes\"?>\n<gpx version=\"1.0\" \n  xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\"\n  xmlns=\"http://www.topografix.com/GPX/1/0\"\n  xsi:schemaLocation=\"http://www.topografix.com/GPX/1/0 http://www.topografix.com/GPX/1/0/gpx.xsd\"\n "
#define GPX_HEADER2 " creator=\"st2gpx-james_sherring@yahoo.com\">\n"
#define GPX_THIS_PROGRAM "st2gpx from james_sherring@yahoo.com"
#define GPX_FOOTER "</gpx>"

#define GPX_WPT 0
#define GPX_RTEPT 1
#define GPX_TRKPT 2

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

typedef struct jourwpt_rec
{
	int jour_num;
	int text1_length;
	char* text1;
	int text2_length;
	char* text2;
	long UdId;
	struct pushpin* pushpin;
	int buf_length;
	char* buf;
} tag_jour_rec;

typedef struct gpx_point
{
	float lat;
	float lon;
	float height;
} tag_gpx_point;

typedef char* annot_file_header;
typedef char* annot_rec_header;
typedef char* annot_buf;

typedef struct annot_rec
{
	int type;
	int length;
	char* buf;
	int annot_num;
	int text_length;
	char* text;
	int line_points;
} tag_annot_rec;

struct point {
	double lon;
	double lat;
} tag_point;

struct ms_point {
	long grid;
	long precision;
} tag_ms_point;


// Also need to include variable text length + line-points with these
int annot_rec_len[4]={57, 868, 124, 868};
int annot_head_len[4]={57, 48, 48, 48};
char* annot_type_name[4]={"Line", "Oval", "Textbox", "Circle"};
static char* gpxptypelabel[] = {"wpt", "rtept", "trkpt"};

// constants for converting from MS Grid & Precision to latitude and longitude
double magic1 = 182.044444444444;
double magic2 = 0x10000;

// Args set by getopt etc

FILE* ppin_file=NULL;
FILE* jour_file=NULL;
FILE* annot_file=NULL;
FILE* gpx_file=NULL;
char* source_file_name=NULL;

/* Flag set by --verbose. */
// 0 - only errors
// 1 also the structured data output, e.g. line point info
// 2 also record types & parameters
// 3 also detailed summary info
// 4 also detailed analysis of headers & record params
// 5 also dump buffers
// 6 lots of debug info
static int verbose_flag = 2;
static int explore_flag = 0;
// force line-type annotations to be exported as GPX routes instead of tracks
static int use_gpx_route = 0;

// std_anotfile_header[8,9] are variable (number of annotations)
char std_annotfile_header[ANNOT_FILE_HEAD_LEN] =
			{0x34, 0x12, 0x00, 0x2d, 0x03, 0x00, 0x00, 0x00, 0x01, 0x00, 0x00, 0x00};

void * xmalloc(size_t size)
{
	void *obj = malloc(size);
	if (!obj)
	{
		fprintf(stderr, "Unable to allocate %d bytes of memory.\n", size);
	}
	return obj;
}

void * xrealloc(void* ptr, size_t size)
{
	void *obj = realloc(ptr, size);
	if (!obj)
	{
		fprintf(stderr, "Unable to (re)allocate %d bytes of memory.\n", size);
	}
	return obj;
}

int fixhex(char c)
{
// gcc (version?) does not print single byte hex vaues properly
// eg 0xe3 prints as 0xFFFFFFE3
// this is only a problem for values above 0x80
// Or maybe that is the proper handling of unsigned?
// Aaaahhh. I should use %u for printing unsigned... but the hex problem is still there
	if(c & 0x80)
	{
		return ((int)c-0xFFFFFF00);
	} else
		return c;
}

void printbuf(char* buf, int len)
{
//	unsigned i;
	int i;
	printf("     0  1  2  3  4  5  6  7  8  9  A  B  C  D  E  F\n");
	printf("    -----------------------------------------------");
	for(i=0; i<len; i++)
	{
		if (((i+1) & 0x0f) == 0x1)
			printf("\n%2x| ",i/16);
		printf("%2x ",fixhex(buf[i]));
	}
	printf("\n\n");
}

void printbufwide(char* buf, int len)
{
	int i;
	for(i=0; i<len; i++)
	{
		printf("%2x ",fixhex(buf[i]));
	}
	printf("\n\n");
}

void printbufhigh(char* buf, int len)
{
	int i;
	for(i=0; i<len; i++)
	{
		printf("%02x %02x\n", i, fixhex(buf[i]));
	}
	printf("\n\n");
}

void printbufasfloat(char* buf, int len)
{
//	unsigned i;
	int i;
//	printf("     0  1  2  3  4  5  6  7  8  9  A  B  C  D  E  F\n");
//	printf("    -----------------------------------------------");
	for(i=0; i<len-3; i++)
	{
		if (((i+1) & 0x03) == 0x1)
			printf("\n%2x| ",i);
		printf("%f  ", *(float*)(buf+i));
		fflush(stdout);
	}
	printf("\n\n");
}

void printfloatbuf(float* fbuf, int len)
{
//	unsigned i;
	int i;
//	printf("     0  1  2  3  4  5  6  7  8  9  A  B  C  D  E  F\n");
//	printf("    -----------------------------------------------");
	for(i=0; i<len; i++)
	{
		if (((i+1) & 0x03) == 0x1)
			printf("\n%2x| ",4*i);
		printf("%f  ", fbuf[i]);
		fflush(stdout);
	}
	printf("\n\n");
}

struct gpx_point gpx_get_point(char* buf)
{
// need to be sure this is done with double precision
	struct gpx_point pt;
	double x;
	double y;
	double z;

	x= *(float *)(buf);
	y= *(float *)(buf + 4);
	z= *(float *)(buf + 8);

	//printf("gpx_get_point x=%f y=%f z=%f\n",x,y,z);
	pt.lat =  atan2(z,sqrt(pow(x,2)+ pow(y,2)))*180/M_PI;
	pt.lon =  atan2(y,x)*180/M_PI;
	// 6378137 is earths equatorial radius in meters
	pt.height = (sqrt(pow(x,2)+ pow(y,2)+ pow(z,2))-1)*6378137;
	return pt;
}

void printpoints(char* buf, int numpts)
{
	int i;
//	int j;
	struct gpx_point pt;
	printf("Latitude  Longitude Height");
	printf("\n");
	printf("--------------------------\n");
	for(i=0; i<numpts; i++)
	{
		pt = gpx_get_point(buf + 12*i);
		printf("%f %f %f \n",pt.lat, pt.lon, pt.height);
	}
	printf("\n\n");
}

void printpointsbuf(char* buf, int len)
{
//	unsigned i;
//	unsigned j;
	int i;
	int j;
	printf("     0  1  2  3  4  5  6  7  8  9  A  B\n");
	printf("    -----------------------------------");
	for(i=0; i<len; i++)
	{
		printf("\n%2x| ",i);
	//	printf("\n");
		for (j=0; j<12; j++)
		{
			printf("%2x ",fixhex(buf[12*i+j]));
			fflush(stdout);
		}
	}
	printf("\n\n");
}

void printbufaspoints(char* buf, int buflen)
{
	int i;
//	int j;
	struct gpx_point pt;
	printf("Latitude  Longitude Height");
	printf("\n");
	printf("--------------------------\n");
	for(i=0; i<buflen-11; i++)
	{
		pt = gpx_get_point(buf + 12*i);
		printf("%#02x %f N %f E %f m \n", i, pt.lat, pt.lon, pt.height);
		fflush(stdout);
	}
	printf("\n\n");
}

void printnzbuf(char* buf, int len)
{
// print non-zero values in the buffer
//	unsigned i;
	int i;
	for(i=0; i<len; i++)
	{
//		if (buf[i] != 0) printf("Buf[%x]=%2x ",fixhex(i), fixhex(buf[i]));
		if (buf[i] != 0) printf("Buf[%x]=%2x ",i, fixhex(buf[i]));
	}
	printf("\n");
}

void printnzhead(struct annot_rec rec)
{
	printnzbuf(rec.buf, annot_head_len[rec.type]);
}

struct point ms2latlong(struct ms_point msp) {

// grid and precision are the high and low words obtained by interleaving the bits
// of latitude and longitude. There is a scale factor, so that grid represents
// latitude and longitude value parts more than ~ 1/182.0444,
// and precision represents the rest of the values.

// So we pull each 2n-th bit out, and shift it n places back to the nth longitudinal bit
// and we pull each 2n-th+1 bit out, and shift it n+1 places to the nth latitudinal bit,

	unsigned long int lat_mask=1;
	unsigned long int lon_mask=2;
	unsigned long int lat_val = 0;
	unsigned long int lon_val = 0;
	struct point p;

	int i;
	for(i=0; i<16; i++) {
		lat_val += (lat_mask & msp.grid) >> i;
		lon_val += (lon_mask & msp.grid) >> (i+1);
		lat_mask <<= 2;
		lon_mask <<= 2;
	}

	//printf("so far, lat_val=%d, lon_val=%d\n",lat_val ,lon_val);

	// Dont divide by magic1 until accounting for precision,
	// because that would introduce rounding errors
	p.lat = (double) lat_val ;//  magic1;
	p.lon = (double) lon_val ;//  magic1;

	//printf("so far, got lat= %f lon= %f\n", p.lat, p.lon);

	lat_mask=1;
	lon_mask=2;
	lat_val=0;
	lon_val=0;

	for (i=0; i<16; i++) {
		lat_val += (lat_mask & msp.precision) >> i;
		lon_val += (lon_mask & msp.precision) >> (i+1);
		lat_mask <<= 2;
		lon_mask <<= 2;
	}

	//printf("so far, lat_val=%d, lon_val=%d\n",lat_val ,lon_val);

	p.lat += (double) lat_val / (magic2);
	p.lon += (double) lon_val / (magic2);

	p.lat *= 1/ (magic1);
	p.lon *= 1/ (magic1);

	//printf("so far, got lat= %f lon= %f\n", p.lat, p.lon);

	if (p.lon > 180) p.lon -= 360;
	if (p.lat > 180) p.lat -= 360;
	return p;
}

void explore_annot(struct annot_rec rec)
{
	char* header;
	char* tail;
	int taillength;

	header = (char*)xmalloc(ANNOT_REC_HEAD_LEN);
	memcpy(header, rec.buf, ANNOT_RECOS_TEXT);
	memcpy(header+ANNOT_RECOS_TEXT+2*rec.text_length, rec.buf,
		ANNOT_REC_HEAD_LEN-ANNOT_RECOS_TEXT);

	tail = rec.buf+ANNOT_REC_HEAD_LEN+2*rec.text_length;
	taillength = rec.length - ANNOT_REC_HEAD_LEN+2*rec.text_length;

	printf("Record buffer\n", annot_type_name[rec.type]);
	printbuf(rec.buf, rec.length);
	printf("Record high buffer\n", annot_type_name[rec.type]);
	printbufhigh(rec.buf, rec.length);
	printf("Record wide header:\n", annot_type_name[rec.type]);
	printbufwide(header, ANNOT_REC_HEAD_LEN);
	printf("Record tail:\n");
	printbuf(tail, taillength);
	printf("Record wide tail:\n");
	printbufwide(tail, taillength);
	printf("Record tail as floats:\n");
	printbufasfloat(tail, taillength);
	printf("Record tail as points:\n");
	printbufaspoints(tail, taillength);
//	if (rec.type == ANNOT_TYPE_TEXT)
//	{
		//printfloatbuf((float*)(tail+3), taillength/4 -1);
//	}
//	else if (rec.type == ANNOT_TYPE_OVAL )
//		printfloatbuf((float*)(tail+3), taillength/4 -1);
//	else if (rec.type == ANNOT_TYPE_CIRCLE ))
//		printfloatbuf((float*)(tail+3), taillength/4 -1);
	free(header);
}

void gpx_open_write_file_header(char* gpx_file_name)
{
	if (gpx_file_name)
	{
		gpx_file = fopen(gpx_file_name, "w");
		if (gpx_file == NULL)
		{
			fprintf(stderr, "Cannot open %s\n", optarg);
			exit(1);
		}
	}
	else
		return;

	if (gpx_file!=NULL)
	{
		fprintf(gpx_file, GPX_HEADER1);
		fprintf(gpx_file, GPX_HEADER2);
		if (source_file_name)
			fprintf(gpx_file, "\t<desc>=\"This file was created from %s by %s\"</desc>\n",
					source_file_name, GPX_THIS_PROGRAM);
		// Some more tags we should add here
		//	<name> Descriptive name of the GPX file
		//	<desc> Description of the GPX file
		//	<author> Name of the file's creator
		//	<time> Creation date/time of the GPX file
	}
}

void gpx_write_file_trailer()
{
	if (gpx_file!=NULL)
	{
		fprintf(gpx_file, GPX_FOOTER);
	    fclose(gpx_file);
	}
}

void gpx_write_point(struct gpx_point pt, int pt_type, char* opt_elms)
{
	if (gpx_file!=NULL)
	{
		if (pt_type == GPX_TRKPT)
			fprintf(gpx_file, "\t\t");
		else if (pt_type == GPX_RTEPT)
			fprintf(gpx_file, "\t");
		fprintf(gpx_file, "\t<%s lat=\"%f\" lon=\"%f\">%s</%s>\n",
				gpxptypelabel[pt_type], pt.lat, pt.lon,
				opt_elms, gpxptypelabel[pt_type]);
	}
}

void gpx_write_jour_point(struct jourwpt_rec * wpt)
{
	struct gpx_point pt;
	char* opt_elms;
	int optlen;

	if (gpx_file==NULL)
		return;

	// For now we only deal with journey points which we matched to a pushpin.
	// Later we need to allow for finding lat & lon in the journey stream itself
	if (!wpt->pushpin)
		return;
	//printf("in gpx_write_jour_point\n");
	//fflush(stdout);

	optlen = strlen(wpt->pushpin->UdName) + strlen(wpt->pushpin->NoteShort) + 40;
	opt_elms = (char*)xmalloc(optlen);

	//printf("doing sprintf(opt_elms\n");
	//fflush(stdout);

	sprintf(opt_elms, "<name>%s</name><cmt>%s</cmt>",
			wpt->pushpin->UdName, wpt->pushpin->NoteShort);

	pt.lat = wpt->pushpin->lat;
	pt.lon = wpt->pushpin->lon;
	pt.height = 0;

	//printf("doing gpx_write_point\n");
	//fflush(stdout);

	gpx_write_point(pt, GPX_RTEPT, opt_elms);

	free(opt_elms);

	//printf("out of gpx_write_jour_point\n");
	//fflush(stdout);

}

void gpx_write_jour_header()
{
	if(gpx_file!=NULL)
	{
		fprintf(gpx_file, "\t<rte>\n");
		fprintf(gpx_file, "\t\t<name>Journey</name>\n");
		fprintf(gpx_file, "\t\t<src>Extracted from main Journey</src>\n");
	}
}

void gpx_write_jour_trailer()
{
	if(gpx_file!=NULL)
		fprintf(gpx_file, "\t</rte>\n");

}

struct pushpin * ppin_by_UdId(int UdId)
{
	int i;

	//printf("looking to match UdId %d to a pushpin\n", UdId);
	//fflush(stdout);

	if (UdId == 0)
		return NULL;

	for (i=0; i< MAX_PUSHPINS; i++)
	{
		//printf("Comparing to pushpin %d\n", i);
		//fflush(stdout);
		if(ppin_list[i] == NULL)
			return NULL;

		//printf(" with UdId %ld ", (ppin_list[i])->UdId);
		//fflush(stdout);
		//printf(" and name %s\n", (ppin_list[i])->UdName);

		if ((ppin_list[i]->UdId) == 0)
			return NULL;

		if (ppin_list[i]->UdId == UdId)
			return ppin_list[i];
	}

	return NULL;
}

void gpx_write_pushpinlist ()
{
	int i;
	struct gpx_point pt;
	char* opt_elms;
	int optlen;

	if (gpx_file==NULL)
		return;

	for (i=0; i<MAX_PUSHPINS; i++)
	{
		if (ppin_list[i]==NULL)
			break;

		optlen = strlen(ppin_list[i]->UdName) + strlen(ppin_list[i]->NoteShort) + 40;
		opt_elms = (char*)xmalloc(optlen);

		sprintf(opt_elms, "<name>%s</name><cmt>%s</cmt>",
				ppin_list[i]->UdName, ppin_list[i]->NoteShort);

		pt.lat = ppin_list[i]->lat;
		pt.lon = ppin_list[i]->lon;
		pt.height = 0;

		gpx_write_point(pt, GPX_WPT, opt_elms);
	}
	fprintf(gpx_file, "\n");
}

void gpx_write_annot_rec(struct annot_rec rec)
{
	int pt_type;
	int p;
	char opt_elms[200];
	struct gpx_point pt;

	if (! gpx_file)
	{
		return;
	}

	pt_type = GPX_TRKPT ;

	// FIXME why do I need to assign memory for this?

	if (use_gpx_route)
		pt_type = GPX_RTEPT;
	switch (rec.type)
	{
		case ANNOT_TYPE_LINE:

		// FIXME lots of format and name info to be added here
		if(use_gpx_route)
		{
			fprintf(gpx_file, "\t<rte>\n");
			fprintf(gpx_file, "\t\t<src>Extracted from Annotation %d (%s)</src>\n",
					rec.annot_num, annot_type_name[rec.type]);
			fprintf(gpx_file, "\t\t<name>RT%04d</name>\n", rec.annot_num);
		}
		else
		{
			fprintf(gpx_file, "\t<trk>\n");
			fprintf(gpx_file, "\t\t<name>TK%04d</name>\n", rec.annot_num);
			fprintf(gpx_file, "\t\t<src>Extracted from Annotation %d (%s)</src>\n",
					rec.annot_num, annot_type_name[rec.type]);
			fprintf(gpx_file, "\t\t<trkseg>\n");
		}

		for (p=0; p < rec.line_points; p++)
		{
			pt=gpx_get_point(rec.buf+annot_head_len[rec.type]+12*p);
			if(use_gpx_route)
				sprintf(opt_elms, "<name>rp%04d</name>", p);
			else
				// we need to include a name for trackpoints
				// for them to be recognised by easygps.
				sprintf(opt_elms, "<name>tp%04d</name>", p);
			gpx_write_point(pt, pt_type, opt_elms);
		}
		if(use_gpx_route)
			fprintf(gpx_file, "\t</rte>\n");
		else
			fprintf(gpx_file, "\t\t</trkseg>\n\t</trk>\n");

		case ANNOT_TYPE_OVAL:
		case ANNOT_TYPE_TEXT:
		case ANNOT_TYPE_CIRCLE:
		default:
			break;
	}
}

int readbytes(FILE* file, char* buf, int bytes2read)
{
	// cant I just do this with fread?
	int i;
//	int status;
/*	int readbyte;
	for (i=0; i<bytes2read; i++)
	{
		readbyte = getc(file);
		if(readbyte==EOF)
		{
           	fprintf(stderr, "Unexpected end of file while reading %i bytes\n",bytes2read);
           	printf("Read %d of a required %d bytes\n", i+1, bytes2read);
           	printf("Dumping the buffer read before unexpected EOF\n");
           	printbuf(buf,i);
           	fflush(stdout);
           	return(1);
           	//exit(1);
		}
		buf[i] = readbyte;
		if (verbose_flag > 5)
		{
			//printf("read byte from file %#x=%u=%c\n", readbyte,readbyte,readbyte);
			//fflush(stdout);

		}
	}
*/
	i = fread(buf, 1, bytes2read, file);
	if (i<bytes2read)
	{
 		if (feof(file))
			printf("Unexpected end of file\n");
		else if (ferror(file))
			perror("Unexpected error while reading from file");
        printf("Read %d of a required %d bytes\n", i+1, bytes2read);
        printf("Dumping the buffer read before unexpected EOF or error\n");
        printbuf(buf,i);
        fflush(stdout);
        return(i);
	}

	if (verbose_flag > 5)
	{
		printf("Readbytes: Read %d bytes from file\n",bytes2read);
		printbuf(buf, bytes2read);
		fflush(stdout);
		if (gpx_file)
			fflush(gpx_file);

	}
	return(i);
}

struct annot_rec read_annot_rec(int annot_num)
{
	// new improved version!

	int status;
	int i;
	struct annot_rec rec;
	rec.buf = (char*)xmalloc(ANNOT_REC_HEAD_LEN);
	// This read can fail because I have miscalculated number of records
	// So exit gracefully
	status = readbytes(annot_file, rec.buf, ANNOT_REC_HEAD_LEN);
	if (status!=ANNOT_REC_HEAD_LEN)
	{
		gpx_write_file_trailer();
		// exit(1)
		// should do some cleaning up here
		return rec;
	}

	rec.type = *(int*)(rec.buf+ANNOT_RECOS_TYPE);
	rec.annot_num = *(int*)(rec.buf+ANNOT_RECOS_ANUM);
	rec.text_length = *(int*)(rec.buf+ANNOT_RECOS_TEXTLEN);
	rec.line_points=0;
	if (rec.type == ANNOT_TYPE_LINE)
		rec.line_points = *(int*)(rec.buf+ANNOT_RECOS_LINENUMPOINTS);
	rec.length = annot_rec_len[rec.type] + 2*rec.text_length + 12*rec.line_points;

	printf("Got annotation %d with internal id %d, of type %s, text length %d, and %d line points\n",
			annot_num+1, rec.annot_num, annot_type_name[rec.type],
			rec.text_length, rec.line_points);

	rec.buf = (char*)xrealloc(rec.buf, rec.length);
	status = readbytes(annot_file, rec.buf+ANNOT_REC_HEAD_LEN, rec.length - ANNOT_REC_HEAD_LEN);
	if (status!=(rec.length - ANNOT_REC_HEAD_LEN))
	{
		gpx_write_file_trailer();
		exit(1);
	}

	if(rec.text_length>0)
	{
		rec.text=(char*)xmalloc(rec.text_length+1);
		rec.text[rec.text_length]=0x0;
		for (i=0;i<rec.text_length;i++)
			rec.text[i]=rec.buf[ANNOT_RECOS_TEXT+ 2*i];
		//if (verbose_flag > 1)

		printf("The sb text in the box is: %s\n", rec.text);
	}

	if (verbose_flag > 4)
	{
		printf("Recognised record with length %i\n",rec.length);
		printbuf(rec.buf, rec.length);
	}

	return rec;
}

void process_annotations_stream(char* annot_file_name)
{
	// Read the annotation file header

	int i;
	int num_annotations;
//	int file_pos = ANNOT_FILE_HEAD_LEN;
	annot_file_header file_header = (annot_file_header)xmalloc(ANNOT_FILE_HEAD_LEN);
	int status;
	struct annot_rec rec;
	char* checkEOF;

	if ((annot_file = fopen(annot_file_name, "rb")) == NULL)
	{
		fprintf(stderr, "Quitting because I cannot open %s\n", annot_file_name);
		exit(1);
	}

	status=readbytes(annot_file, file_header, ANNOT_FILE_HEAD_LEN);
	if (status!=ANNOT_FILE_HEAD_LEN)
	{
		printf("Cant make any more sense of annotations stream, continuing\n");
		return;
	}

    // file_pos is for debugging only

	for (i=1; i<8; i++)
		if ( file_header[i] != std_annotfile_header[i] )
			printf("Nonstandard annotations file header, header[%i]=0x%x, normal value is 0x%x\n",
					i, file_header[i], std_annotfile_header[i] );

	num_annotations = *(int*)(file_header+8);

		if (verbose_flag > 1)
		{
			printf("Annotations file header indicates %i annotations in file\n",
					num_annotations);
		}

	if (verbose_flag > 4)
	{
		printf("Dumping file header:\n");
		printbuf(file_header, ANNOT_FILE_HEAD_LEN);
	}

	/*
	** Read all the annotation records
	*/
	for (i=0; i < num_annotations; i++)
	{
	    rec = read_annot_rec(i);

		if (explore_flag)
			explore_annot(rec);

		gpx_write_annot_rec(rec);

		free(rec.buf);
		if (rec.text_length>0)
			free(rec.text);
	}

	// Check that we are at the end of annotation file

	checkEOF = xmalloc(4);
	status=readbytes(annot_file, checkEOF, 4);
	if ( (status!=4) || (checkEOF[0]!=0) || (checkEOF[1]!=0)
			|| (checkEOF[2]!=0) || (checkEOF[3]!=0) || (getc(annot_file)!=EOF) )
	{
		fprintf (stderr, "Did not finish reading annotation file at EOF\n");
		if (gpx_file)
			fprintf(gpx_file, GPX_FOOTER);
	}
	free(checkEOF);
	fclose(annot_file);
	fflush(stdout);
	fflush(gpx_file);
}

struct jourwpt_rec * read_journey_rec(int jour_num)
{
	int i;
	int status;
	int readlen1 = JOURWPT_REC_HEAD_LEN;
	int readlen2;
	int readlen3;
	char c;
	struct jourwpt_rec * rec = (struct jourwpt_rec *)xmalloc(sizeof(struct jourwpt_rec));

	rec->buf = (char*)xmalloc(readlen1);

	status = readbytes(jour_file, rec->buf, readlen1);
	if (status!=readlen1)
	{
		printf("Unexpected EOF while reading Journey route point %d part 1.\n", jour_num);
		printf("Dumping Journey route point buffer read so far\n");
		printbuf(rec->buf, status);
		//FIXME free rec
		return NULL;
	}

	// FIXME careful here
	c = rec->buf[0];
	rec->UdId = c;

	c = rec->buf[JOURWPT_RECOS_STR1LEN];

	rec->text1_length = (int)c;

	if (verbose_flag > 5)
	{
		printf("Reading Journey routepoint, got buf part1, text1 len = %d\n",
				rec->text1_length);
		printbuf(rec->buf, readlen1);
	}

	readlen2 = rec->text1_length+4;

	rec->buf = (char*)xrealloc(rec->buf, readlen1 + readlen2);
	status = readbytes(jour_file, rec->buf+readlen1, readlen2);
	if (status!=readlen2)
	{
		printf("Unexpected EOF while reading Journey route point %d part 2.\n", jour_num);
		printf("Dumping Journey route point buffer read so far\n");
		printbuf(rec->buf, readlen1+status);
		//FIXME free rec
		return NULL;
	}

	c = rec->buf[JOURWPT_RECOS_STR2LEN + rec->text1_length];
	rec->text2_length = (int)c;
	rec->buf_length = JOUR_WPTREC_LEN + rec->text1_length + 2*rec->text2_length;

	if (verbose_flag > 5)
	{
		printf("got buf part2, text2 len = %d\n", rec->text2_length);
		printbuf(rec->buf, readlen1 + readlen2);
	}
	readlen3 = rec->buf_length  - readlen1 - readlen2;

	rec->buf = (char*)xrealloc(rec->buf, rec->buf_length);
	status = readbytes(jour_file, rec->buf+readlen1+readlen2, readlen3);
	if (status!=readlen3)
	{
		printf("Unexpected EOF while reading Journey route point %d part 3.\n", jour_num);
		printf("Dumping Journey route point buffer read so far\n");
		printbuf(rec->buf, readlen1+readlen2+status);
		//FIXME free rec
		return NULL;
	}

	rec->text1=(char*)xmalloc(rec->text1_length+1);
	rec->text2=(char*)xmalloc(rec->text2_length+1);
	rec->text1[rec->text1_length]=0;
	rec->text2[rec->text2_length]=0;
	memcpy(rec->text1, rec->buf+JOURWPT_RECOS_STR1, rec->text1_length);
	for (i=0; i<rec->text2_length; i++)
		rec->text2[i]=rec->buf[JOURWPT_RECOS_STR2 + rec->text1_length+2*i];

	if (verbose_flag > 3)
		printf("got Journey routepoint UdId %d, text1 '%s', text2 '%s', length %d\n",
			rec->UdId, rec->text1, rec->text2, rec->buf_length);

	rec->pushpin = ppin_by_UdId(rec->UdId);

	if (rec->pushpin != NULL)
	{
		if (verbose_flag > 5)
		{
			printf("Found probable pushpin match\n");
			printf("pushpin name is: %s\n", rec->pushpin->UdName);
			printf("Pushpin short note is: %s\n", rec->pushpin->NoteShort);
			//printf("Pushpin long note is: %s\n", rec->pushpin->NoteLong);
		}

		gpx_write_jour_point(rec);
	}
	else
	{
		printf("*** Warning *** Ignoring route point '%s' without matching pushpin\n",
				rec->text1);
		printf("(yes, I should do someting more clever than this).\n");
		if (rec->UdId)
			printf("There should have been a matching pushin because UdId=%d\n", rec->UdId);
	}
	fflush(stdout);

	if (verbose_flag > 4)
	{
		printf("Dumping the recognised Journey waypoint record\n");
		printbuf(rec->buf, rec->buf_length);
	}
	return rec;
}

void process_journey_stream (char* jour_file_name)
{
	int journey_num_pts;
	int j;
	int readbyte;
	static int verify_eof_flag = 0;
	char* jour_file_header = (char*)xmalloc(JOUR_FILE_HEAD_LEN);
	char* jour_file_trailer;
	int max_read_more = 10000;
	int readmore=0;
	int status;
	char readmorebuf[10000];
	struct jourwpt_rec * rec;

	gpx_write_jour_header();

	printf("Processing Journey stream\n");
	fflush(stdout);
	fflush(gpx_file);


	if ((jour_file = fopen(jour_file_name, "rb")) == NULL)
	{
		fprintf(stderr, "Quitting because I cannot open %s\n", jour_file_name);
		exit(1);
	}

	status = readbytes(jour_file, jour_file_header, JOUR_FILE_HEAD_LEN);
	if (status!=JOUR_FILE_HEAD_LEN)
	{
		printf("Unexpected EOF in the Journey stream\n");
		gpx_write_jour_trailer();
		return;
	}

	journey_num_pts = *(int*)(jour_file_header+JOUR_FILEOS_NUMREC);
	printf("got Journey file header with %d waypoints\n", journey_num_pts);
	if (verbose_flag > 4)
	{
		printbuf(jour_file_header, JOUR_FILE_HEAD_LEN);
	}

	for (j=0; j< journey_num_pts; j++)
	{
		rec = read_journey_rec(j);
		if (rec==NULL)
		{
			printf("Closing stream and skipping rest of Journey stream processing because I read a bad record\n");
			fclose(jour_file);
			printf("Writing gpx route for Journey\n");
			fflush(stdout);

			gpx_write_jour_trailer();

			printf("Finnished closing Journey stream after error.\n");
			return;
		}
	}

	jour_file_trailer = (char*)xmalloc(JOUR_FILE_TAIL_LEN);

	status = readbytes(jour_file, jour_file_trailer, JOUR_FILE_TAIL_LEN);
	if (status!=JOUR_FILE_TAIL_LEN)
	{
		printf("Unexpected EOF while reading journey stream trailer\n");
	}

	if (verbose_flag > 4)
	{
		printf("got Journey file trailer\n");
		printbuf(jour_file_trailer, JOUR_FILE_TAIL_LEN);
	}

	if ((readbyte = getc(jour_file))!=EOF)
	{
		fprintf (stderr, "Did not finish reading journey file at EOF\n");

		do {
			readmorebuf[readmore]=(char)readbyte;
			readmore++;
			if (readmore>max_read_more)
				break;
		} while ((readbyte = getc(jour_file))!=EOF);

		printf("read a further %d bytes past expected eof\n",readmore);
		printbuf(readmorebuf, readmore);
	}

    fclose(jour_file);
	printf("Writing gpx route for Journey\n");
	fflush(stdout);

	gpx_write_jour_trailer();

	printf("Finnished proccessing Journey stream.\n");
	fflush(stdout);
	fflush(gpx_file);
}

void read_ppin_list_csv(char* ppin_file_name)
// for reading my old style csv temp file for ppins
{
	int pinnum;
	int params_read;
	char ppin_line[500+MAX_PPIN_MEMO];
	char ppin_line_strings[500+MAX_PPIN_MEMO];
	struct pushpin * ppin;
	char* status;
	int UdNameLen=0;
	int NoteShortLen=0;
	int NoteLongLen=0;

	printf("Annalysing the pushpin temp file\n");

	for (pinnum=0; pinnum < MAX_PUSHPINS; pinnum++)
		ppin_list[pinnum] = NULL;

	if ((ppin_file = fopen(ppin_file_name, "r")) == NULL)
	{
		fprintf(stderr, "Cannot open %s\n", ppin_file_name);
		exit(1);
	}

	pinnum=0;
	while (!feof( ppin_file))
	{
		//printf("Reading pushpin %d\n", pinnum);
		//fflush(stdout);

		ppin = (struct pushpin *)xmalloc(sizeof(struct pushpin));

		status=fgets(ppin_line, 500+MAX_PPIN_MEMO, ppin_file);

		if (status==NULL)
		{
			printf("oops, trying to read empty line\n", pinnum);
			break;

		}
		else
		{
			//printf("Read ppin temp line: %s\n", ppin_line);
			//fflush(stdout);

			params_read = sscanf(ppin_line, "%ld, %ld, %ld, %d, %d, %d, %[^\n]",
				&(ppin->UdId), &(ppin->Grid), &(ppin->Precision),
				&UdNameLen, &NoteShortLen, &NoteLongLen, ppin_line_strings);

			printf("Got pushpin UdId %ld Grid %d Precision %d ppin_line_strings %s\n",
					ppin->UdId, ppin->Grid, ppin->Precision, ppin_line_strings);
			fflush(stdout);

			if (params_read == 7)
			{
				ppin->UdName = (char*)xmalloc(UdNameLen+1);
				memcpy(ppin->UdName, ppin_line_strings, UdNameLen);
				ppin->UdName[UdNameLen]=0;

				ppin->NoteShort = (char*)xmalloc(NoteShortLen+1);
				memcpy(ppin->NoteShort, ppin_line_strings+UdNameLen, NoteShortLen);
				ppin->NoteShort[NoteShortLen]=0;

//				ppin->NoteLong = (char*)xmalloc(NoteLongLen+1);
//				memcpy(ppin->NoteLong, ppin_line_strings+UdNameLen+NoteShortLen, NoteLongLen);
//				ppin->NoteLong[NoteLongLen]=0;

				//printf("Read %d parameters ok\n", params_read);
				//fflush(stdout);


				ppin_list[pinnum]=ppin;
			//	printf("saving pushpin_linst[%d]: UdId %ld lat %g lon %g UdName %s\n",
			//		pinnum, ppin->UdId, ppin->lat, ppin->lon, ppin->UdName);
			//	fflush(stdout);
				pinnum++;
			}
			else
			{
				printf("freeing this ppin (was# %d) because it did not read properly\n",
						pinnum);
				free(ppin);
				break;
			}
		}
	}

	fclose(ppin_file);
	printf("Read %d pushpins, now writing to gpx\n", pinnum);

}

int check_file_empty(char* filename)
{
	int readchar;
	FILE* file=fopen(filename, "rb");
	// also check if the file exists
	if (file==NULL)
		return 1;
	readchar = fgetc(file);
	if (readchar == EOF)
		return 1;
	else
		return 0;
}

void process_pushpin_file(char* ppin_file_name)
{
	int i=0;
	struct point p;
	struct ms_point msp;

	// dont try and open a empty mdb file
	if (check_file_empty(ppin_file_name))
		return;

	read_ppin_list(ppin_file_name);
	//read_ppin_list_csv(ppin_file_name);

	// set lat & lon for all pushpins
	while(ppin_list[i] && (i<MAX_PUSHPINS))
	{
		msp.grid = ppin_list[i]->Grid;
		msp.precision = ppin_list[i]->Precision;
		p = ms2latlong(msp);

		ppin_list[i]->lat = p.lat;
		ppin_list[i]->lon = p.lon;

	if (verbose_flag > 3)
			printf("Decoded pushpin %d lat %f, lon %f, UdName %s, NoteShort %s\n",
				ppin_list[i]->UdId, ppin_list[i]->lat, ppin_list[i]->lon,
				ppin_list[i]->UdName, ppin_list[i]->NoteShort);

		i++;
	}

	gpx_write_pushpinlist();

	printf("Finished reading pushpins\n");
	fflush(stdout);
	fflush(gpx_file);
}

void show_usage()
{
	printf("st2gpx - Export data from MS Streets & Trips and Autoroute to GPX format\n\n");
	printf("Usage: st2gpx [-hre] [-v verbose-level] [-a annotations-file] [-u userdata-file] ");
	printf("[-j journey-file] [-G gpx-file] stfile\n\n");
	printf("-h : Help (this text)\n");
	printf("-r : Export drawn-lines as routes instead of tracks\n");
	printf("-e : Explore data furhter\n");
	printf("-v [n]              : Set debugging verbosity to 'n' (0-6, default 2)\n");
	printf("-u userdata-file    : Process pushpins in (mdb) file userdata-file\n");
	printf("-j journey-file     : Process Journey in file journey-file\n");
	printf("-a annotations-file : Process Annotations in file annotations-file\n");
	printf("-G gpx-file         : Write output to gpx-file\n");
	exit(0);
}

void xsystem(char* syscmd)
// this is not ANSI
{
	int status;
	printf("%s \n", syscmd);
	_flushall();
	status = system(syscmd);
	if (status)
		fprintf(stderr, "system call returned an error\n");
}

main(int argc, char** argv)
{
	int c;
	static int verify_eof_flag = 0;
	char* jour_file_name=NULL;
	char* annot_file_name=NULL;
	char* ppin_file_name=NULL;
	char* gpx_file_name=NULL;

	char syscmd[1000];
    char cmddrv[_MAX_DRIVE];
    char cmddir[_MAX_DIR];
    char cmdfilename[_MAX_PATH];
    char cmdext[_MAX_EXT];
	char cmdpath[_MAX_PATH];

	int debug_wait_flag=0;

	while (1)
    {
		static struct option long_options[] =
        {
			{"file",    required_argument, 0, 'f'},
			{"outfile",    required_argument, 0, 'F'},
			{"verbose", optional_argument, 0, 'v'},
			{"explore", no_argument,       &explore_flag, 1},
			{"route",    no_argument, &use_gpx_route, 1},
			{0, 0, 0, 0} // what is this for?
        };
		/* getopt_long stores the option index here. */
		int option_index = 0;

		//    c = getopt_long (argc, argv, "v::bqderf:F:",
		//                     long_options, &option_index);
		c = getopt(argc, argv, "hu:j:a:G:rv::ew");

		/* Detect the end of the options. */
		if (c == -1)
			break;

		switch (c)
        {
        case 0:
			/* If this option set a flag, do nothing else now. */
			break;

        case 'h':
			show_usage();
			break;

        case 'u':
			// read a UserData (pushpin) file directly
			free(ppin_file_name);
			ppin_file_name = (char*)xmalloc(strlen(optarg)+1);
			strcpy(ppin_file_name, optarg);
			if (verbose_flag > 1)
				printf("Analysing UserData (pushpin) stream in file %s\n\n", jour_file_name);
           break;

       case 'j':
			// read a Journey file directly
			free(jour_file_name);
			jour_file_name = (char*)xmalloc(strlen(optarg)+1);
			strcpy(jour_file_name, optarg);
			if (verbose_flag > 1)
				printf("Analysing Journey stream in file %s\n\n", jour_file_name);
            break;

        case 'a':
			// read an annotation file directly
			annot_file_name = (char*)xmalloc(strlen(optarg)+1);
			strcpy(annot_file_name, optarg);
			if (verbose_flag > 1)
				printf("Analysing Annotation stream in file %s\n\n", annot_file_name);
            break;

        case 'G':
			gpx_file_name = (char*)xmalloc(200);
			strcpy(gpx_file_name, optarg);
			if (verbose_flag > 1)
				printf("Writing GPX output to file %s\n\n", gpx_file_name);
            break;

        case 'r':
			use_gpx_route=1;
            break;

		case 'v':
			if (optarg==NULL)
				verbose_flag=5;
			else
				verbose_flag = atoi(optarg);
			break;

        case 'e':
			explore_flag = 1;
			break;

        case 'w':
			debug_wait_flag = 1;
			break;

        case '?':
			/* getopt_long already printed an error message. */
			show_usage();
			break;

        default:
			show_usage();
			abort ();
        }
    }

	if (optind == argc-1)
	{
			source_file_name = (char*)xmalloc(strlen(argv[optind])+1);
			strcpy(source_file_name, argv[optind]);
			if (verbose_flag > 1)
				printf("Analysing autoroute file %s\n\n", source_file_name);
			if (ppin_file_name==NULL)
			{
				ppin_file_name = (char*)xmalloc(strlen(source_file_name)+30);
				sprintf(ppin_file_name, "%s.Contents\\UserData.mdb", source_file_name);
			}
			if (jour_file_name==NULL)
			{
				jour_file_name = (char*)xmalloc(strlen(source_file_name)+30);
				sprintf(jour_file_name, "%s.Contents\\Journey", source_file_name);
			}
			if (annot_file_name==NULL)
			{
				annot_file_name = (char*)xmalloc(strlen(source_file_name)+30);
				sprintf(annot_file_name, "%s.Contents\\Annotations", source_file_name);
			}
			if (gpx_file_name==NULL)
			{
				gpx_file_name = (char*)xmalloc(strlen(source_file_name)+30);
				// should remove the est or axe suffix
				sprintf(gpx_file_name, "%s.gpx", source_file_name);
			}
	}
	else if (optind < argc-1)
	{
	    printf("Unrecognised option %s\n", argv[optind+1]);
		show_usage();
    }
	else
	{
		if (verbose_flag > 1)
			printf("Not analysing any core S&T or autoroute file\n");
	}

	if (source_file_name)
	{
		// Open the compound file using the istorage utility
		// I probably should do this in a library and use streams instead of files

		// Find the path for istorage from the path in argv[0]

		// this is not ANSI
		_splitpath(argv[0], cmddrv, cmddir, cmdfilename, cmdext);
		sprintf(cmdpath, "%s%s", cmddrv, cmddir);

		sprintf(syscmd, "%sistorage\\istorage.exe %s", cmdpath, source_file_name);
		xsystem(syscmd);

		// clean up previous copy of this. It should already have been deleted...
		sprintf(syscmd, "del %s.Contents\\UserData.mdb", source_file_name);
		xsystem(syscmd);

		sprintf(syscmd, "rename %s.Contents\\UserData. UserData.mdb", source_file_name);
		xsystem(syscmd);
	}

	// begin processing the files
	gpx_open_write_file_header(gpx_file_name);

	if (ppin_file_name)
		process_pushpin_file(ppin_file_name);
	if (jour_file_name)
		process_journey_stream(jour_file_name);
	if (annot_file_name)
		process_annotations_stream(annot_file_name);

	gpx_write_file_trailer();


	// Clean up the compound file directory
	if (source_file_name)
	{
		sprintf(syscmd, "echo y|del %s.Contents", source_file_name);
		xsystem(syscmd);

	// just for debuging
	if (debug_wait_flag)
	{
		fprintf(stderr, "kill that ado connection!\n");
		fprintf(stderr, "Hit any key to continue\n");
		getchar();
	}

		// how do I do this?
		sprintf(syscmd, "del %s.Contents\\UserData.mdb", source_file_name);
		xsystem(syscmd);

		sprintf(syscmd, "rmdir %s.Contents", source_file_name);
		xsystem(syscmd);
	}

	// just for debuging
	if (debug_wait_flag)
	{
		fprintf(stderr, "Hit any key to continue\n");
		getchar();
	}
}
