/*
	readmpst.c

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

#define STATUS_BOF					1
#define STATUS_EOF					2
#define STATUS_FILE_HEADER			3

#define STATUS_WPT_LIST_HEADER		4
#define STATUS_WPT_LIST_BODY		5

#define STATUS_RTE_LIST_HEADER		6
#define STATUS_RTE_ITEM				7
#define STATUS_RTE_PT_LIST_HEADER	8
#define STATUS_RTE_PT_LIST_BODY		9

#define STATUS_TRK_LIST_HEADER		10
#define STATUS_TRK_ITEM				11
#define STATUS_TRK_PT_LIST_HEADER	12
#define STATUS_TRK_PT_LIST_BODY		13

#define MAX_FIELD_WIDTH 50
// I have files with 1300 lines
#define MAX_LINES 10000
//#define MAX_LINES 200

//#define MAX_FIELDS 20;
//char* list_header_fieldnames[MAX_FIELDS];
//char* list_header_values[MAX_FIELDS];
//char* list_fieldnames[MAX_FIELDS];
//char* list_values[MAX_FIELDS];

const char* garmin_16_symbols[16]=
{
		"Waypoint",
		"Residence",
		"Gas Station",
		"Car",
		"Fishing Area",
		"Boat Ramp",
		"Marina",
		"Shipwreck",
		"Exit",
		"Skull and Crossbones",
		"Flag",
		"Campground",
		"Circle with X",
		"Hunting Area",
		"Medical Facility",
		"TrackBack Point"
};

struct gpx_data * read_mpstext(char* mpstxt_file_name)
{
	char line_buf[1001];
	FILE* mpstxt_file = fopen(mpstxt_file_name, "r");
	//int byte_read;
	int status=STATUS_FILE_HEADER;
	int last_status=STATUS_BOF;
	int this_blank=0;
	int last_blank=0;
	char * fgets_status;
	int parsed_vars=0;

	int loop_count=0;

	// being lazy a lot of large variables
	char f_Name[ MAX_FIELD_WIDTH ];
	char f_Desc[ MAX_FIELD_WIDTH ];
	char f_Type[ MAX_FIELD_WIDTH ];
	char f_lat_dir;
	double f_lat;
	char f_lon_dir;
	double f_lon;
	char f_Altitude[ MAX_FIELD_WIDTH ];
	char f_Depth[ MAX_FIELD_WIDTH ];
	char f_Proximity[ MAX_FIELD_WIDTH ];
	char f_DisplayMode[ MAX_FIELD_WIDTH ];
	char f_Color[ MAX_FIELD_WIDTH ];
	char f_Symbol[ MAX_FIELD_WIDTH ];
	char f_Facility[ MAX_FIELD_WIDTH ];
	char f_City[ MAX_FIELD_WIDTH ];
	char f_State[ MAX_FIELD_WIDTH ];
	char f_Country[ MAX_FIELD_WIDTH ];

	char f_Length[ MAX_FIELD_WIDTH ];
	char f_Course[ MAX_FIELD_WIDTH ];
	char Waypoints[ MAX_FIELD_WIDTH ];

	char f_WaypointName[ MAX_FIELD_WIDTH ];
	char f_Distance[ MAX_FIELD_WIDTH ];
	char f_LegLength[ MAX_FIELD_WIDTH ];

	char f_StartTime[ MAX_FIELD_WIDTH ];
	char f_ElapsedTime[ MAX_FIELD_WIDTH ];
	char f_AverageSpeed[ MAX_FIELD_WIDTH ];
	char f_DateTime[ MAX_FIELD_WIDTH ];
	char f_LegTime[ MAX_FIELD_WIDTH ];
	char f_LegSpeed[ MAX_FIELD_WIDTH ];
	char f_LegCourse[ MAX_FIELD_WIDTH ];

	struct gpx_data * dat = gpx_data_new();
	struct gpxpt * this_gpxpt=NULL;
	struct gpxpt * matching_wpt=NULL;
	struct gpxrte * thisrte=NULL;

	dat->data_source_name=(char*)xmalloc(strlen(mpstxt_file_name)+1);
	strcpy(dat->data_source_name, mpstxt_file_name);

	if (mpstxt_file==NULL)
	{
		printf("Error opening MapSource text file %s for input\n", mpstxt_file_name);
		debug_pause();
		exit(1);
	}
	else
		printf("Reading MapSource text file %s\n", mpstxt_file_name);

	while (status!=STATUS_EOF)
	{
		fgets_status = fgets((char*)line_buf, 1000, mpstxt_file);
		if (fgets_status==NULL)
			status=STATUS_EOF;

		//printf("read %d bytes in line: %s\n", bytes_read, line_buf);

		if (strcmp(line_buf,"\n")==0)
			this_blank=1;
		else
			this_blank=0;

		switch(status)
		{
		case STATUS_FILE_HEADER:
			// Just eat all header lines until a this_blank line
			// We should verify this header

			// FIXME We need to check that Grid=Lat/Lon hddd.ddddd° 
			// to be sure we can read the data
			if (this_blank)
				status=STATUS_WPT_LIST_HEADER;
			break;

		case STATUS_WPT_LIST_HEADER:
			// check type & set proper status
			if (this_blank)
				status=STATUS_WPT_LIST_BODY;
			break;

		case STATUS_WPT_LIST_BODY:
			if (this_blank)
			{
				status=STATUS_RTE_LIST_HEADER;
			}
			else
			{
				parsed_vars=sscanf(line_buf, "Waypoint\t%[^\t]\t%[^\t]\t%[^\t]\t%c%lf %c%lf\t%[^\t]\t%[^\t]\t%[^\t]\t%[^\t]\t%[^\t]\t%[^\t]\t%[^\t]\t%[^\t]\t%[^\t]\t%[^\t]",
					&f_Name, &f_Desc, &f_Type,  &f_lat_dir, &f_lat, &f_lon_dir, &f_lon, &f_Altitude,
					&f_Depth, &f_Proximity, &f_DisplayMode, &f_Color, &f_Symbol, &f_Facility,
					&f_City, &f_State, &f_Country);
				if (parsed_vars<4)
				{
					printf("Error parsing line %d as a waypoint. Need at least 4 vars, only got %d\n", 
							loop_count+1, parsed_vars);
					printf("Problem line is: %s\n", line_buf);
				}
				else
				{
					dat->wpt_list=(struct gpxpt **)xrealloc(dat->wpt_list, (dat->wpt_list_count+1)*sizeof(struct gpxpt *));
					this_gpxpt = dat->wpt_list[dat->wpt_list_count] = gpxpt_new();
					
					if (f_lat_dir=='S')
						f_lat=-f_lat;
					else if(f_lat_dir!='N')
						printf("Error parsing line %d as a waypoint. '%c' when expecting 'N' or 'S' for latitdue.\n", f_lat_dir);

					if (f_lon_dir=='W')
						f_lon=-f_lon;
					else if(f_lon_dir!='E')
						printf("Error parsing line %d as a waypoint. '%c' when expecting 'E' or 'W' for longitude.\n", f_lat_dir);
					
					this_gpxpt->lat = f_lat;
					this_gpxpt->lon = f_lon;
					this_gpxpt->name=xmalloc(strlen(f_Name)+1);
					strcpy(this_gpxpt->name, f_Name);
					str2ascii(this_gpxpt->name);
					this_gpxpt->desc=xmalloc(strlen(f_Desc)+1);
					strcpy(this_gpxpt->desc, f_Desc);
					str2ascii(this_gpxpt->desc);
					dat->wpt_list_count++;
				}
			}
			break;

		case STATUS_RTE_LIST_HEADER:
			// check type & set proper status
			if (this_blank && !last_blank)
				status=STATUS_RTE_ITEM;
			break;

		case STATUS_RTE_ITEM:
			if (this_blank)
			{
				if (last_blank)
					status=STATUS_TRK_LIST_HEADER;
				else
					status=STATUS_RTE_PT_LIST_HEADER;
			}
			else
			{
				parsed_vars=sscanf(line_buf, "Route\t%[^\t]\t%[^\t]\t%[^\t]\t%[^\t]",
						&f_Name, &f_Length, &f_Course, &Waypoints);
				if (parsed_vars<1)
				{
					printf("Error parsing line %d as a route. Need at least 1 var, only got %d\n", 
							loop_count+1, parsed_vars);
					printf("Problem line is: %s\n", line_buf);
				}
				else
				{
					dat->rte_list=(struct gpxrte **)xrealloc(dat->rte_list, (dat->rte_list_count+1)*sizeof(struct gpxrte *));
					dat->rte_list[dat->rte_list_count]=gpxrte_new();
					dat->rte_list[dat->rte_list_count]->name = xmalloc(strlen(f_Name)+1);
					str2ascii(f_Name);
					strcpy(dat->rte_list[dat->rte_list_count]->name, f_Name);
					dat->rte_list_count++;
				}
			}
			break;

		case STATUS_RTE_PT_LIST_HEADER:
			if (this_blank)
				status=STATUS_RTE_PT_LIST_BODY;
			break;

		case STATUS_RTE_PT_LIST_BODY:
			if (this_blank)
				//loop back to another new route
				status=STATUS_RTE_ITEM;
			else
			{
				parsed_vars=sscanf(line_buf, "Route Waypoint\t%[^\t]\t%[^\t]\t%[^\t]\t%[^\t]",
					&f_WaypointName, &f_Distance, &f_LegLength, &f_Course);
				if (parsed_vars<1)
				{
					printf("Error parsing line %d as a routepoint. Need at least 1 var, only got %d\n", 
							loop_count+1, parsed_vars);
					printf("Problem line is: %s\n", line_buf);
				}
				else
				{
					matching_wpt=find_matching_wpt(dat, f_WaypointName);
					if (matching_wpt == NULL)
					{
						printf("Ignoring routepoint %s because I cant match it to any waypoints\n",
								f_WaypointName);
					}
					else
					{
						thisrte = dat->rte_list[dat->rte_list_count-1];
						thisrte->rtept_list=(struct gpxpt **)xrealloc(thisrte->rtept_list, 
											(thisrte->rtept_list_count+1)*sizeof(struct gpxpt *));
						this_gpxpt = thisrte->rtept_list[thisrte->rtept_list_count] = gpxpt_copy(matching_wpt);
						thisrte->rtept_list_count++;
					}
				}
			}
			break;

		case STATUS_TRK_LIST_HEADER:
			// check type & set proper status
			if (this_blank && !last_blank)
				status=STATUS_TRK_ITEM;
			break;

		case STATUS_TRK_ITEM:
			if (this_blank)
			{
				if (last_blank)
					status=STATUS_EOF;
				else
					status=STATUS_TRK_PT_LIST_HEADER;
			}
			else
			{
				parsed_vars=sscanf(line_buf, "Track\t%[^\t]\t%[^\t]\t%[^\t]\t%[^\t]\t%[^\t]",
					&f_Name, &f_StartTime, &f_ElapsedTime, &f_Length, &f_AverageSpeed);
				if (parsed_vars<1)
				{
					printf("Error parsing line %d as a Track. Need at least 1 var, only got %d\n", 
							loop_count+1, parsed_vars);
					printf("Problem line is: %s\n", line_buf);
				}
				else
				{
					dat->trk_list=(struct gpxtrk **)xrealloc(dat->trk_list, 
										(dat->trk_list_count+1)*sizeof(struct gpxtrk *));
					dat->trk_list[dat->trk_list_count]=gpxtrk_new();
					//dat->trk_list[dat->trk_list_count]->name = (char*)malloc(strlen(f_Name)+1);
					//strcpy(dat->trk_list[dat->trk_list_count]->name, f_Name);
					dat->trk_list_count++;
				}
			}
			break;

		case STATUS_TRK_PT_LIST_HEADER:
			if (this_blank)
				status=STATUS_TRK_PT_LIST_BODY;
			break;

		case STATUS_TRK_PT_LIST_BODY:
			if (this_blank)
				// loop back for another new track
				status=STATUS_TRK_ITEM;
			else
			{
				parsed_vars=sscanf(line_buf, "Trackpoint\t%c%lf %c%lf\t%[^\t]\t%[^\t]\t%[^\t]\t%[^\t]\t%[^\t]\t%[^\t]",
					&f_lat_dir, &f_lat, &f_lon_dir, &f_lon, &f_DateTime, &f_Altitude,
					&f_Depth, &f_LegLength,	&f_LegTime, &f_LegSpeed, &f_LegCourse);
				if (parsed_vars<4)
				{
					printf("Error parsing line %d as a track-point. Need at least 4 vars, only got %d\n", 
							loop_count+1, parsed_vars);
					printf("Problem line is: %s\n", line_buf);
				}
				else
				{
					struct gpxtrk * thistrk = dat->trk_list[dat->trk_list_count-1];
					thistrk->trkpt_list=(struct gpxpt **)xrealloc(thistrk->trkpt_list, 
											(thistrk->trkpt_list_count+1)*sizeof(struct gpxpt *));
					thistrk->trkpt_list[thistrk->trkpt_list_count]=gpxpt_new();

					if (f_lat_dir=='S')
						f_lat=-f_lat;
					else if(f_lat_dir!='N')
						printf("Error parsing line %d as a waypoint. '%c' when expecting 'N' or 'S' for latitdue.\n", f_lat_dir);

					if (f_lon_dir=='W')
						f_lon=-f_lon;
					else if(f_lon_dir!='E')
						printf("Error parsing line %d as a waypoint. '%c' when expecting 'E' or 'W' for longitude.\n", f_lat_dir);

					thistrk->trkpt_list[thistrk->trkpt_list_count]->lat=f_lat;
					thistrk->trkpt_list[thistrk->trkpt_list_count]->lon=f_lon;
					thistrk->trkpt_list_count++;
				}
			}
			break;

		default:
			{;
			}
		}

		last_status=status;
		last_blank=this_blank;

		loop_count++;
		if (loop_count>MAX_LINES)
		{
			printf("Read %d input lines, which seems rather high\n", loop_count);
			break;
		}

	}
	
	if(opts.verbose_flag>4)
		print_gpx_data(dat);

	return dat;
}
