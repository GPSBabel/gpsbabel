/*
	gpx.h

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

#define GPX_ELEM_TYPE_WPT	1
#define GPX_ELEM_TYPE_RTE	2
#define GPX_ELEM_TYPE_RTEPT	3
#define GPX_ELEM_TYPE_TRK	4
#define GPX_ELEM_TYPE_TRKSEG	5
#define GPX_ELEM_TYPE_TRKPT	6

#define GPX_ELEM_TYPE_NAME	7
#define GPX_ELEM_TYPE_DESC	8
#define GPX_ELEM_TYPE_SRC	9

typedef struct gpxpt
{
	double lat;
	double lon;
	double elevation;
	char use_elevation;
	char * name;
	char * desc;
	// more later
} tag_gpxpt;

struct gpxpt * gpxpt_new();
void gpxpt_delete(struct gpxpt * pt);
struct gpxpt * gpxpt_copy(struct gpxpt * otherpt);

typedef struct gpxrte
{
	char*	name;
	struct gpxpt**	rtept_list;
	int rtept_list_count;
} tag_gpxrte;

struct gpxrte * gpxrte_new();
void gpxrte_delete(struct gpxrte * rte);

typedef struct gpxtrk
{
	struct gpxpt**	trkpt_list;
	int trkpt_list_count;
} tag_gpxtrk;

struct gpxtrk * gpxtrk_new();
void gpxtrk_delete(struct gpxtrk * trk);

typedef struct gpx_data
{
	char* data_source_name;
	struct gpxpt ** wpt_list;
	int wpt_list_count;
	struct gpxrte ** rte_list;
	int rte_list_count;
	struct gpxtrk ** trk_list;
	int trk_list_count;
} tag_gpx_data;

struct gpx_data * gpx_data_new();
void gpx_data_delete(struct gpx_data * data);

struct gpxpt * find_matching_wpt(struct gpx_data * dat, char* name);

struct gpx_data * process_gpx_in_file(char* gpx_in_file_name);
void print_gpx_data(struct gpx_data * all_data);

// FIXME is this the correct way to forward define these?
#ifndef	__cplusplus
extern struct pushpin_safelist;
extern struct journey;
extern struct annotations;
#endif // __cplusplus
				   
void gpx_write_all(char* gpx_out_file_name, 
				   struct pushpin_safelist *ppplist, 
				   struct journey * jour,
				   struct annotations * annots);

#ifdef	__cplusplus
}
#endif
