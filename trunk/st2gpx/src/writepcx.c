/*
	writepcx.c

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
#include <math.h>
#include <memory.h>
#include <string.h>

#include "gpx.h"
#include "st2gpx.h"
#include "ppinutil.h"
#include "journey.h"
#include "annotations.h"

#define GAR_WPT_HEADER "H  IDNT   LATITUDE    LONGITUDE    DATE      TIME     ALT   DESCRIPTION                              PROXIMITY     SYMBOL ;waypts\n"
#define GAR_RTE_HEADER_JOUR "R 01  Streets&Trips Journey\n"
#define GAR_TRK_HEADER "H  LATITUDE    LONGITUDE    DATE      TIME     ALT    ;track\n"

#define GAR_WPT 1
#define GAR_RTE 2
#define GAR_TRK 3

// maximum number of susbstitutions to try get a unique IDENT
#define MAX_IDENT_SUB 10000

char* strpad(char* str, int padlen)
// Right-pad a string with spaces so that strlen(str)=padlen,
// or just truncate str if it is longer that padlen.
// str must have at least padlen+1 bytes allocated.
{
	int i;
	str[padlen]=0;
	for(i=strlen(str); i<padlen; i++)
		str[i]=' ';
	str[padlen]=0;
	//printf("strpad, got:%s\n",str);
	return str;
}

// WPT names (IDENT) need to be 6 characters, uppercase,
// and in [A-Z,0-9,-, ]. I.e. asc 48-57, 65-90, 45, 58
void garmin_ident_crush(char* str)
{
	int i;
	char c;
	strpad(str, 6);
	//convert to upper - cant I just do a (portable) subtraction or something?
	//_strupr(str);
	for(i=0; i<6; i++)
	{
		c=str[i];
		//convert to upper
		// FIXME - cant I do something for accented letters?
		if ((c>96) && (c<123))
			c= str[i]=c-32;
		//convert all non-conforming characters to spaces
		if( !( ((c>47) && (c<59)) || ((c>64) && (c<91)) || (c==45) ))
		{
			str[i]=' ';
		}
	}
	str[6]=0;
}

void comp_mk_uniq_idents(char* ident_array, int ident_to_mk_uniq, int count_idents) 
// compare ident_to_mk_uniq-th ident to all preceding idents,
// and make ident_to_mk_uniq-th ident
{
	int j;
	int k;
	int m;
	char test_ident[7];
	char uniq_part[7];
	int duplicate_flag;

	// compare this ident with all the idents verified as unique
	for(j=0; j<ident_to_mk_uniq; j++)
		if (strcmp(ident_array+7*ident_to_mk_uniq, ident_array+7*j)==0)
		{
			//try to make a new ident, by setting the end to Xk, try k up to 5 digits
			strncpy(test_ident, ident_array+7*ident_to_mk_uniq, 7);
			for (k=0; k<MAX_IDENT_SUB; k++)
			{
				sprintf(uniq_part, "X%d", k);
				strcpy(test_ident+6-strlen(uniq_part), uniq_part);
				if (strlen(test_ident) != 6)
				{
					fprintf(stderr, "Oops, I got my calcs wrong in making unique IDENT, got '%s'\n", test_ident);
					debug_pause();
					exit(1);
				}
				duplicate_flag=0;
				// compare the test_ident to *all* other  idents,
				// so that we dont create a later duplicate
				for (m=0; m<count_idents; m++)
					if ( (m!=ident_to_mk_uniq) && (strcmp(test_ident, ident_array+7*m)==0) )
					{
						duplicate_flag=1;
						break;
					}
				if (duplicate_flag==0)
					break;
			}
			if (k==MAX_IDENT_SUB)
				printf("Panick! **** How could there be %d duplicate IDENTs ?!?\n", MAX_IDENT_SUB);

			printf("Got duplicate Garmin IDENT '%s' for pushpin(%d) and pushpin(%d), so for pushpin(%d) I'm using '%s'\n",
				ident_array+7*j, ident_to_mk_uniq, j, ident_to_mk_uniq, test_ident);

			// ?????
			strncpy(ident_array + 7*ident_to_mk_uniq, test_ident, 7);
		}
}

void make_uniq_idents(struct pushpin_safelist * ppplist, struct journey * jour)
{
	int i;
	int j;
	struct f_jour_pt_head * f_wpt_head=NULL;
	float lat;
	float lon;
	int count_ppins;

	// this is an array of n*6 bytes
	char *ident_array;

	if (jour==NULL)
	{
		printf("Unexpected jour=NULL in make_uniq_idents.\n");
		return;
	}
	if (ppplist==NULL)
	{
		count_ppins=0;
		if(jour->count_rtepts == 0)
			return;
	}
	else
		count_ppins=ppplist->num_pushpins;
	
	ident_array=(char*)xmalloc(7*(count_ppins + jour->count_rtepts));

	// copy ppin names before we start mangling them
	for (i=0; i< (count_ppins); i++)
	{
		if(ppplist->pushpin_list[i]->UdName)
			memcpy(ident_array +7*i, ppplist->pushpin_list[i]->UdName, 6);
		else
			ident_array[7*i]=0;
		//printf("pushpin_list[%d]->UdName:%s\n", i, ppplist->pushpin_list[i]->UdName);
		//printf("ident_array[7*%d]:%s\n", i, ident_array+7*i);
		garmin_ident_crush(ident_array+7*i);
		//printf("Made Ident:%s\n",ident_array+7*i );
	}

	// copy rtept names before we start mangling them
	for (i=0; i< (jour->count_rtepts); i++)
	{
		memcpy(ident_array + 7*(count_ppins) +7*i, 
			   jour->rtept_list[i].text1, 
			   6);
		garmin_ident_crush(ident_array + 7*(count_ppins) + 7*i);
	}

	// compare all fresh ppin idents
	for(i=0; i<(count_ppins); i++)
	{
		// Verify ident_array[7*i] is unique.
		// Create a unique ident if neccesary.
		// Only check against list of already-verified unique
		comp_mk_uniq_idents(ident_array, i, count_ppins + jour->count_rtepts);
		
		strncpy(ppplist->pushpin_list[i]->garmin_ident, ident_array+7*i, 7);
	}

	// compare jour rtept idents to ppin idents + other jour rtept idents
	for(i=0; i<jour->count_rtepts; i++)
	{
		if (jour->rtept_list[i].pushpin != NULL)
		{
			// The rtept has a matching pushpin, so use the pushpin's garmin_ident
			memcpy(jour->rtept_list[i].garmin_ident,
				   jour->rtept_list[i].pushpin->garmin_ident,
				   7);
		}
		else
		{
			f_wpt_head = (struct f_jour_pt_head *)(jour->buf + (jour->rtept_list[i].pthead_os));
			lat=scaled2deg(f_wpt_head->scaled_lat);
			lon=scaled2deg(f_wpt_head->scaled_lon);
			for(j=0; j<count_ppins; j++)
				// Is this ppin closer than 0.00001 deg?
				// This wont work for -180, 180, but thats not realistic or a problem
				// It is questionable if this is even desirable when the ppin name is different,
				// perhaps we want the different name also?
				if( (fabs(lat-ppplist->pushpin_list[j]->lat)<0.00001)
				    && (fabs(lon-ppplist->pushpin_list[j]->lon)<0.00001) )
				{
					// The rtept has a matching pushpin, so use the pushpin's garmin_ident
					memcpy(ident_array + 7*(count_ppins) +7*i,
						   ppplist->pushpin_list[j]->garmin_ident,
						   7);
					// Although the journey stream doesn't associate this pushpin with the route,
					// I have decided to. Again, this is questionable 
					// but not likely to happen anyway.
					jour->rtept_list[i].pushpin = ppplist->pushpin_list[j];
				}
				else
				{
					// no matching wpt for this rtept, so we need to create one.
					// We just create the garmin_ident in the jour-rtept here, 
					// later we will write a corresponding wpt to the pcx file.
					comp_mk_uniq_idents(ident_array, count_ppins + i, count_ppins + jour->count_rtepts);
				}				
		}
		// set the jour-rtept garmin_ident
		memcpy(jour->rtept_list[i].garmin_ident,
			   ident_array + 7*(count_ppins) +7*i,
			   7);
	}
	xfree(ident_array);
}

void gar_write_header(FILE* gar_out_file)
{
	fprintf(gar_out_file, "H  SOFTWARE NAME & VERSION\n");
	fprintf(gar_out_file, "I  PCX5 2.09 output from st2gpx by James Sherring - james_sherring@yahoo.com");
	fprintf(gar_out_file, "\n");
	fprintf(gar_out_file, "H  R DATUM                IDX DA            DF            DX            DY            DZ\n");
	fprintf(gar_out_file, "M  G WGS 84               121 +0.000000e+00 +0.000000e+00 +0.000000e+00 +0.000000e+00 +0.000000e+00\n");
	fprintf(gar_out_file, "\n");
	fprintf(gar_out_file, "H  COORDINATE SYSTEM\n");
	fprintf(gar_out_file, "U  LAT LON DM\n");
	fprintf(gar_out_file, "\n");
}

void pcx5_write_pt(FILE* file, int pt_type, char ident[7], double lat, double lon, char timedate[19],
				   float alt, char desc[41], float proximity,	int symbol)
{
	//char outbuf[118];
	char lat_sym;
	char lon_sym;
	int lat_deg;
	int lon_deg;
	double lat_min;
	double lon_min;

	if (lat<0)
		lat_sym='S';
	else
		lat_sym='N';

	if (lon<0)
		lon_sym='W';
	else
		lon_sym='E';

	lat_deg=(int)floor(fabs(lat));
	lon_deg=(int)floor(fabs(lon));

	lat_min=(fabs(lat)-lat_deg)*60;
	lon_min=(fabs(lon)-lon_deg)*60;

	if ( (pt_type == GAR_WPT) || (pt_type == GAR_RTE) )
		fprintf(file, "W  %6.6s ", ident);
	else if (pt_type == GAR_TRK)
		fprintf(file, "T  ");

	fprintf(file, "%c%02.2d%08.5f %c%03.3d%08.5f", lat_sym, lat_deg, lat_min, lon_sym, lon_deg, lon_min);
	fprintf(file, " %18.18s %5.5s", timedate, "    ");

	if ( (pt_type == GAR_WPT) || (pt_type == GAR_RTE) )
		fprintf(file, " %40.40s %13.13s %1.3d \n", desc, "              ", symbol);
	else if (pt_type == GAR_TRK)
		fprintf(file, "\n");

}

void pcx5_write_ppin(FILE* file, struct pushpin * ppin)
{
	char timedate[19]="";
	float alt=0;
	char desc[41]="";
	float proximity=0;
	// FIXME this should come from the pushpin icon
	int symbol=8; // or 18

	strpad(timedate, 19);

	// Should I create a meaningful note if there is none?
	if(ppin->NoteShort)
		memcpy(desc, ppin->NoteShort, 40);
	else
		desc[0]=0;
	strpad(desc, 40);

	pcx5_write_pt(file, GAR_WPT, ppin->garmin_ident, ppin->lat, ppin->lon, timedate,
		alt, desc, proximity, symbol);
}

void pcx5_write_ppin_list(FILE* file, struct pushpin_safelist * ppplist)
{
	int i;

	if ( (file==NULL) || (ppplist==NULL))
		return;

	fprintf(file, GAR_WPT_HEADER);
	for (i=0; i< (ppplist->num_pushpins); i++)
		pcx5_write_ppin(file, ppplist->pushpin_list[i]);
}

void pcx5_write_jour_pt(FILE* file, struct journey * jour, struct jour_rtept * rtept, int pt_type)
{
	char timedate[19]="";
	float alt=0;
	char desc[40];
	float proximity=0;

	int symbol=8; // or 18
	struct f_jour_pt_head * f_wpt_head;

//	if(pt_type==GAR_WPT)
//	{
		f_wpt_head = (struct f_jour_pt_head *)(jour->buf + (rtept->pthead_os));

		strpad(timedate, 19);

		strcpy(desc, rtept->text1);
		strpad(desc, 40);

		pcx5_write_pt(file, 
					  pt_type, 
					  rtept->garmin_ident, 
					  scaled2deg(f_wpt_head->scaled_lat), 
					  scaled2deg(f_wpt_head->scaled_lon), 
					  timedate,	
					  alt, 
					  desc, 
					  proximity, 
					  symbol);
//	}
//	else if(pt_type==GAR_RTE)
//	{
//		// this is easier that calling pcx5_write_pt
//		fprintf(file, "W  %6.6s \n", rtept->garmin_ident);
//	}
}

void pcx5_write_journey(FILE* file, struct journey * jour, struct pushpin_safelist * ppplist)
{
	int i;

	if( (jour==NULL) || ((jour->count_rtepts) < 1) )
		return;

	// write jour rte-pts w/o ppin as wpts
//	for(i=0; i< jour->count_rtepts; i++)
//		if(jour->rtept_list[i].pushpin == NULL)
//			pcx5_write_jour_pt(file, jour, jour->rtept_list + i, GAR_WPT);

	fprintf(file, "\n");
	fprintf(file, GAR_RTE_HEADER_JOUR);
	fprintf(file, "\n");
	fprintf(file, GAR_WPT_HEADER);

	for(i=0; i< jour->count_rtepts; i++)
		if(jour->rtept_list[i].pushpin)
			pcx5_write_ppin(file, jour->rtept_list[i].pushpin);
		else
			pcx5_write_jour_pt(file, jour, jour->rtept_list+i, GAR_RTE);
}

void pcx5_write_annot_line(FILE* file, struct annot_rec * pannot)
{
	int p;
	char timedate[19]="";
	float alt=0;
	struct gpxpt * pt;

	if (pannot==NULL)
		return;

	fprintf(file, "\n");
	fprintf(file, GAR_TRK_HEADER);

	strpad(timedate, 19);

	for(p=0; p<pannot->line_points; p++)
	{
		pt=gpx_get_point(pannot->buf + pannot->line_offset + 12*p);
		if (pt != NULL)
			pcx5_write_pt(file, GAR_TRK, "", pt->lat, pt->lon, timedate,
						  alt, "", 0, 0);
		else
		{
			printf("Null gpx_pt #%p from annotation %d, skipping more points in this annotation\n",
				    pannot->annot_num);
			break;
		}
		gpxpt_delete(pt);
	}
}

void pcx5_write_annots(FILE* file, struct annotations * pannots)
{
	int i;
	if (pannots==NULL)
		return;

	for(i=0; i < pannots->num_annotations; i++)
		if ( (pannots->annot_list[i]->type) == ANNOT_TYPE_LINE)
			pcx5_write_annot_line(file, pannots->annot_list[i]);

}

void pcx5_export(char* pcx5_out_file_name, struct pushpin_safelist * ppplist, struct journey * jour, struct annotations * pannots)
{
	FILE* pcx5_out_file=NULL;
	struct pushpin_safelist * new_ppplist=NULL;

	if ( (ppplist==NULL) && (jour==NULL) && (pannots==NULL) )
		return;

	// Because Garmin can only accept a nasty 6 char alphanumeric unique ID
	make_uniq_idents(ppplist, jour);

	if (pcx5_out_file_name)
	{
		pcx5_out_file = fopen(pcx5_out_file_name, "w");
		if (pcx5_out_file == NULL)
		{
			fprintf(stderr, "Cannot open %s\n", pcx5_out_file_name);
			debug_pause();
			exit(1);
		}
	}
	else
		return;

	gar_write_header(pcx5_out_file);
	pcx5_write_ppin_list(pcx5_out_file, ppplist);
	pcx5_write_journey(pcx5_out_file, jour, ppplist);
	pcx5_write_annots(pcx5_out_file, pannots);
};
