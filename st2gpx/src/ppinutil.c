/*
	ppinutil.c

	Extract data from MS Streets & Trips .est, Autoroute .axe 
	and Mapoint .ptm files in GPX format.

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
#include "ppinutil.h"

#ifdef EXPLORE
#include "explore.h"
#endif

struct pushpin_safelist * pushpin_safelist_new()
{
	int i;
	struct pushpin_safelist * nw = (struct pushpin_safelist*)xmalloc(sizeof(struct pushpin_safelist));
	nw->pushpin_list=NULL;
	nw->num_pushpins=0;
	for(i=0; i<3; i++)
	{
		nw->UDM_Data_length[i]=0;
		nw->UDM_Data[i]=NULL;
	}
	return nw;
}

void pushpin_safelist_delete(struct pushpin_safelist * ppl)
{
	int i;
	if (ppl==NULL)
		return;

	for (i=0; i<ppl->num_pushpins; i++)
		pushpin_delete(ppl->pushpin_list[i]);

	free(ppl->pushpin_list);

	for(i=0; i<3; i++)
		free(ppl->UDM_Data[i]);

	free(ppl);
}

struct pushpin * pushpin_new()
{
	struct pushpin * nw = (struct pushpin*)xmalloc(sizeof(struct pushpin));
	nw->garmin_ident[0]=0;
	nw->Grid=0;
	nw->Precision=0;
	nw->lat=0;
	nw->lon=0;
	nw->NoteShort=NULL;
	nw->UdName=NULL;
	strcpy(nw->garmin_ident, "      ");
	nw->RenderData=0;
	nw->RenderData2=0;
	nw->url=NULL;
	nw->urlname=NULL;
	return nw;
}

void pushpin_delete(struct pushpin * pp)
{
	if(pp==NULL)
		return;
	free(pp->NoteShort);
	free(pp->UdName);
	free(pp->url);
	free(pp->urlname);
	free(pp);
}

struct point grid2latlon(long grid, long precision)
//struct point ms2latlong(struct ms_point msp)
{
// Convert the 2 long values in UserData stream to GPS coordinates.

// Grid and Precision are the high and low words obtained by interleaving the bits
// of latitude and longitude. There is a scale factor, so that grid represents
// latitude and longitude value parts more than ~ 1/182.0444 = 360/2^16,
// and precision represents the rest of the values.

// So we pull each 2n-th bit out, and shift it n places back to the nth longitudinal bit
// and we pull each 2n-th+1 bit out, and shift it n+1 places to the nth latitudinal bit,

	unsigned long int lat_mask=1;
	unsigned long int lon_mask=2;
	unsigned long int lat_val = 0;
	unsigned long int lon_val = 0;
	struct point p;

	int i;

	if (grid==0x3FFFFFFF)
	{
		printf("Got bad Grid value 0x3FFFFFFF converting pushpin data.\n");
		p.lat=-180;
		p.lon=-180;
		return p;
	}
	for(i=0; i<16; i++)
	{
		lat_val += (lat_mask & grid) >> i;
		lon_val += (lon_mask & grid) >> (i+1);
		lat_mask <<= 2;
		lon_mask <<= 2;
	}


	p.lat = (double) lat_val ;//  magic1;
	p.lon = (double) lon_val ;//  magic1;

	lat_mask=1;
	lon_mask=2;
	lat_val=0;
	lon_val=0;

	for (i=0; i<16; i++)
	{
		lat_val += (lat_mask & precision) >> i;
		lon_val += (lon_mask & precision) >> (i+1);
		lat_mask <<= 2;
		lon_mask <<= 2;
	}

//	p.lat += (double) lat_val / (magic2);
//	p.lon += (double) lon_val / (magic2);
	p.lat += (double) lat_val / (double) (0x10000);
	p.lon += (double) lon_val / (double) (0x10000);

	p.lat *= 360/(double) (0x10000);
	p.lon *= 360/(double) (0x10000);

	if (p.lon > 180) p.lon -= 360;
	if (p.lat > 180) p.lat -= 360;

	if ( (p.lat > 90) || (p.lat < -90) )
	{
		printf("Got bad lat value %f converting pushpin data, setting.\n");
//		p.lat=-180;
//		p.lon=-180;
	}

	return p;
}

struct grid_point latlon2grid(double lat, double lon) {

	struct grid_point msp;
	unsigned long mask=1;
	int i;
	double scaled_lat = (lat/360) * (0x10000);
	double scaled_lon = (lon/360) * (0x10000);
	unsigned long lat_grid_ndx = (unsigned long)floor(scaled_lat);
	unsigned long lon_grid_ndx = (unsigned long)floor(scaled_lon);
	unsigned long lat_prec_ndx = (unsigned long)floor((scaled_lat - lat_grid_ndx)*(0x10000));
	unsigned long lon_prec_ndx = (unsigned long)floor((scaled_lon - lon_grid_ndx)*(0x10000));
	msp.grid=0;
	msp.precision=0;

	for(i=0; i<16; i++) {
		msp.grid += ((mask & lat_grid_ndx) << i);;
		msp.grid += ((mask & lon_grid_ndx) << (i+1));
		mask <<= 1;
	}

	mask=1;
	for(i=0; i<16; i++) {
		msp.precision += ((mask & lat_prec_ndx) << i);
		msp.precision += ((mask & lon_prec_ndx) << (i+1));
		mask <<= 1;
	}
	return msp;
}

struct pushpin * ppin_by_UdId(int UdId, struct pushpin_safelist * ppplist)
{
	int i;

	//printf("looking to match UdId %d to a pushpin\n", UdId);
	//fflush(stdout);

	if (UdId == 0)
		return NULL;

	if (ppplist==NULL)
	{
		printf("Looking for UdId %#x=%d in an empty pushpin list???\n", UdId, UdId);
		return NULL;
	}

	for (i=0; i< ppplist->num_pushpins; i++)
	{
		//printf("Comparing to pushpin %d\n", i);
		//fflush(stdout);
		if(ppplist->pushpin_list[i] == NULL)
			return NULL;

		//printf(" with UdId %ld ", (ppplist->pushpin_list[i])->UdId);
		//fflush(stdout);
		//printf(" and name %s\n", (ppplist->pushpin_list[i])->UdName);

		if ((ppplist->pushpin_list[i]->UdId) == 0)
			return NULL;

		if (ppplist->pushpin_list[i]->UdId == UdId)
			return ppplist->pushpin_list[i];
	}
	return NULL;
}

int check_file_empty(char* filename)
// FIXME there must be a better way to do this
{
	int readchar;
	int retval=0;
	FILE* file = fopen(filename, "rb");
	// also check if the file exists
	if (file==NULL)
		retval= 1;
	else
	{
		readchar = fgetc(file);
		if (readchar == EOF)
			retval=1;
		else
			retval=0;
	}
	if (file)
		fclose(file);
	return retval;
}

struct pushpin_safelist * process_pushpin_file(char* ppin_in_file_name)
{
	int i=0;
	struct point p;
//	struct ms_point msp;
	struct pushpin_safelist * ppplist;

	// dont try and open a empty mdb file
	if (check_file_empty(ppin_in_file_name))
		return NULL;;
	ppplist=read_pushpins(ppin_in_file_name);

	// set lat & lon for all pushpins
	while(ppplist->pushpin_list[i] && (i<ppplist->num_pushpins))
	{
//		msp.grid = ppplist->pushpin_list[i]->Grid;
//		msp.precision = ppplist->pushpin_list[i]->Precision;
//		p = grid2latlon(msp);
		p = grid2latlon(ppplist->pushpin_list[i]->Grid, ppplist->pushpin_list[i]->Precision);

		ppplist->pushpin_list[i]->lat = p.lat;
		ppplist->pushpin_list[i]->lon = p.lon;

	if (opts.verbose_flag > 3)
			printf("Decoded pushpin %d lat %f, lon %f, UdName %s, NoteShort %s\n",
				ppplist->pushpin_list[i]->UdId, ppplist->pushpin_list[i]->lat, ppplist->pushpin_list[i]->lon,
				ppplist->pushpin_list[i]->UdName, ppplist->pushpin_list[i]->NoteShort);

		i++;
	}

	if (opts.verbose_flag>2)
		printf("Finished reading pushpins\n");
	fflush(stdout);

	return ppplist;
}


