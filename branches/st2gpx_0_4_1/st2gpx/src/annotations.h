/*
	annotations.h

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
#ifdef	__cplusplus
extern "C" {
#endif

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

// + 12*NUM POINTS
#define LINE_REC_LEN_V3 57
#define LINE_REC_LEN_V4 61 //???


//#define ANNOT_TYPE_LINE		0
//#define ANNOT_TYPE_OVAL		1
//#define ANNOT_TYPE_TEXT		2
//#define ANNOT_TYPE_CIRCLE	3

/*
typedef struct annotations_file_head
{
	// always 0x2d001234
	unsigned int uiunkn0;
	// autoroute 2001: version=3
	// autoroute 2003: version=4
	unsigned int version;
	// number of annotations in the stream
	unsigned int c_annots;
} tag_annotations_file_head;
		
typedef struct annotation_record_header
{
	// 0 = line
	// 1 = Oval
	// 2 = Textbox/Rectangle
	// 3 = Circle
	unsigned int type;
	// 0x8 = show length
	// 0x10 = order before roads
	// (order defore other objects depends on file order (last highest) and obj type)
	unsigned int bitflags;
	int iunkn0;
	int iunkn1;
	// iunkn2 is only for version 10+ (9+?)
	int iunkn2;
	unsigned int text_len;
	char text[];
	// 0 =black
	// 12=blue
	unsigned int fill_color;
	// 0 = none
	// 1 = fill
	// + some other high-byte flags?
	int fill_flag;
	//  0 = black
	// 12 = blue
	// 0d = yellow
	unsigned int line color;
	// 20*point size 
	unsigned int line_width;
	// 00=none, 
	// 01=left, 
	// 02=right, 
	// 03=both;
	int arrow_type;
	int iunkn3;
	// 01 is a joined line: the first & last points are joined
	// 00 is a line-type
	int closed_flag;
	char cunkn0;
	// number of points in the line
	char c_line_points;
	struct point_rec[];
} tag_annotation_header;

struct annot_line_point
{
	float x;
	float y;
	float z;
}

*/

typedef struct annot_rec
{
	// ANNOT_TYPE_
	int type;
	int length;
	char* buf;
	int annot_num;
	int text_length;
	char* text;
	int line_points;
	// Pointer to the line-points data in buf, 
	// because it moves with different file formats.
	int line_offset;
} tag_annot_rec;

struct annot_rec * annot_rec_new();
void annot_rec_delete(struct annot_rec * annot_rec);

typedef struct annotations
{
	int num_annotations;
	int max_annot_num;
	char* header_buf;
	struct annot_rec ** annot_list;
	int tail_buf_length;
	char read_recs_ok_flag;
	char read_tail_ok_flag;
	int stream_length;
	// This is the annotations version number, as stored in the annotations stream
	int version;
} tag_annotations;

struct annotations * annotations_new();
void annotations_delete(struct annotations * annots);
struct gpxpt* gpx_get_point(char* buf);

extern char std_annotfile_header[ANNOT_FILE_HEAD_LEN];
extern char std_annot_linerec_header_v3[LINE_REC_LEN_V3];
extern char std_annot_linerec_header_v4[LINE_REC_LEN_V4];

//struct parameters {
// Also need to include variable text length + line-points with these
extern char* annot_type_name[4];
extern char* gpxptypelabel[3];
extern char * st_version[];

//} tag_parameters;

struct annotations * process_annotations_stream(char* annot_in_file_name);


#ifdef	__cplusplus
}
#endif
