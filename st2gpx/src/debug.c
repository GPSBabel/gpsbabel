/*
	debug.c

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
#include <memory.h>
#include <malloc.h>

#include "gpx.h"
#include "st2gpx.h"
#include "getopt.h"
#include "properties.h"
#include "pushpins.h"
#include "ppinutil.h"
#include "annotations.h"
#include "journey.h"

void debug_pause()
{
	char c;
	if (opts.debug_wait_flag)
	{
		fprintf(stderr, "Hit Enter key to continue\n");
		c=getchar();
		// a way to stop pausing
		if (c=='q')
			opts.debug_wait_flag=0;
	}
}

void printbuf(char* buf, int len)
{
	int i;
	unsigned char * ubuf = (unsigned char *)buf;
	printf("     0  1  2  3  4  5  6  7  8  9  A  B  C  D  E  F\n");
	printf("    -----------------------------------------------");
	for(i=0; i<len; i++)
	{
		if (((i+1) & 0x0f) == 0x1)
			printf("\n%2x| ",i/16);
		printf("%2x ", ubuf[i]);
	}
	printf("\n\n");
}


void debug_show_sizes()
{
	printf("sze of struct annot_rec is %d \n", sizeof(struct annot_rec));
	printf("sze of struct annotations is %d \n", sizeof(struct annotations));
	printf("sze of struct dictionary is %d \n", sizeof(struct dictionary));
//	printf("sze of struct FORMATIDOFFSET is %d \n", sizeof(struct FORMATIDOFFSET));
	printf("sze of struct gpx_data is %d \n", sizeof(struct gpx_data));
	printf("sze of struct gpxpt is %d \n", sizeof(struct gpxpt));
	printf("sze of struct gpxrte is %d \n", sizeof(struct gpxrte));
	printf("sze of struct gpxtrk is %d \n", sizeof(struct gpxtrk));
	printf("sze of struct jour_header is %d \n", sizeof(struct f_jour_header));
	printf("sze of struct jour_pt_head is %d \n", sizeof(struct f_jour_pt_head));
	printf("sze of struct jour_pt_mid is %d \n", sizeof(struct f_jour_pt_mid));
	printf("sze of struct jour_pt_tail is %d \n", sizeof(struct f_jour_pt_tail));
	printf("sze of struct jour_rtept_rec is %d \n", sizeof(struct jour_rtept));
//	printf("sze of struct jour_trailer_EUR_8 is %d \n", sizeof(struct jour_opts_EUR_8));
	printf("sze of struct jour_trailer_EUR_8 is %d \n", sizeof(struct f_jour_avoid));
//	printf("sze of struct jour_trailer_EUR_8 is %d \n", sizeof(struct jour_trailer_EUR_8));

	debug_pause();

	printf("sze of struct journey is %d \n", sizeof(struct journey));
	printf("sze of struct grid_point is %d \n", sizeof(struct grid_point));
	printf("sze of struct ole_property is %d \n", sizeof(struct ole_property));
	printf("sze of struct ole_property_set is %d \n", sizeof(struct ole_property_set));
	printf("sze of struct option is %d \n", sizeof(struct option));
	printf("sze of struct point is %d \n", sizeof(struct point));
//	printf("sze of struct PROPERTYIDOFFSET is %d \n", sizeof(struct PROPERTYIDOFFSET));
//	printf("sze of struct PROPERTYSETHEADER is %d \n", sizeof(struct PROPERTYSETHEADER));
	printf("sze of struct pushpin is %d \n", sizeof(struct pushpin));
	printf("sze of struct pushpin_safelist is %d \n", sizeof(struct pushpin_safelist));
	printf("sze of struct pushpinset is %d \n", sizeof(struct pushpinset));
//	printf("sze of struct SERIALIZEDPROPERTYVALUE is %d \n", sizeof(struct SERIALIZEDPROPERTYVALUE));
	printf("sze of struct st2gpx_options is %d \n", sizeof(struct st2gpx_options));
//	printf("sze of struct tagPROPERTYSECTIONHEADER is %d \n", sizeof(struct tagPROPERTYSECTIONHEADER));
}
