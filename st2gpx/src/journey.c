/*
	journey.c

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
#include "ppinutil.h"

#include "journey.h"

float scaled2deg(int scaled_deg)
// there must be a simple single line calc
{
	double x=scaled_deg;
	x = x*360/0x10000;
	x = x/0x10000;
	return (float)x;
}

void jour_rtept_init(struct jour_rtept * nw)
{
//	struct jour_rtept_rec * nw=(struct jour_rtept_rec*)xmalloc(sizeof(struct jour_rtept_rec));
	nw->pthead_os=0;
	nw->text1=NULL;
	nw->ptmid_os=0;
	nw->text2=NULL;
	nw->pttail_os=0;
	// pointer to pushpin owned by pushpinlist
	nw->pushpin=NULL;
	strcpy(nw->garmin_ident,"      ");
}

void jour_rtept_delete(struct jour_rtept * jourpt)
{
	if (jourpt==NULL)
		return;
	xfree(jourpt->text1);
	xfree(jourpt->text2);
	// This is part of an array, so free the array
	//xfree(jourpt);
}

struct journey * journey_new()
{
	struct journey * nw = (struct journey*)xmalloc(sizeof(struct journey));
	nw->buf_len=0;
	nw->buf=NULL;

	nw->rtept_list=NULL;
	nw->jopts_os=0;
	nw->jopts_eur8_os=0;
	nw->jopts_usa8_os=0;
	nw->jopts_usa10_os=0;
	nw->avoid_os_list=NULL;
	nw->trailer_os=0;
	return nw;
}

void journey_delete(struct journey * jour)
{
	int i;
	if (jour==NULL)
		return;
	for(i=0; i< jour->count_rtepts; i++)
		jour_rtept_delete(jour->rtept_list + i);
	xfree(jour->rtept_list);
	xfree(jour->avoid_os_list);
	xfree(jour->buf);
	xfree(jour);
}


struct journey * process_journey_stream (char* jour_in_file_name, 
										 struct pushpin_safelist * ppplist)
{
	int j;
	int readbyte;
	int bytes2read=0;

	int max_read_more = 10000;
	int readmore=0;
	int status;
	char readmorebuf[10000];

	int UdId;
//	double lat;
//	double lon;
//	double x;
//	double y;
//	double z;

	struct journey * jour = journey_new();
//	struct jour_rtept * thispt=NULL;
	FILE* jour_in_file=NULL;
	struct f_jour_pt_head * pt_head=NULL;
	struct f_jour_opts_EUR_8* opts_eur8;
	struct f_jour_opts_EUR_10* opts_eur10;
	struct f_jour_opts_USA_8* opts_usa8;
	struct f_jour_opts_USA_10* opts_usa10;

	printf("Processing Journey stream\n");

	if ((jour_in_file = fopen(jour_in_file_name, "rb")) == NULL)
	{
		fprintf(stderr, "Quitting because I cannot open %s\n", jour_in_file_name);
		exit(1);
	}
	
	bytes2read=sizeof(struct f_jour_header);
	jour->buf=(char*)xmalloc(jour->buf_len+bytes2read);
	status = readbytes(jour_in_file, jour->buf, bytes2read);
	jour->buf_len += bytes2read;
	if (status!=bytes2read)
	{
		printf("Unexpected EOF in the Journey stream header\n");
	    fclose(jour_in_file);
		return jour;
	}
	jour->header_os = 0;
	jour->count_rtepts = ((struct f_jour_header*)(jour->buf + jour->header_os))->cpoints;

	printf("got Journey file header with %d waypoints\n", jour->count_rtepts);
	debug_pause();
	if (opts.explore_flag)
		print_f_jour_header((struct f_jour_header*)(jour->buf + jour->header_os));

	// an array of jour_rtept
	jour->rtept_list = (struct jour_rtept *)xmalloc(
						jour->count_rtepts*sizeof(struct jour_rtept));

	for (j=0; j< jour->count_rtepts; j++)
	{
		jour_rtept_init(jour->rtept_list + j);

		// read pt header
		bytes2read=sizeof(struct f_jour_pt_head);
		jour->buf=(char*)xrealloc(jour->buf, jour->buf_len+bytes2read);
		status = readbytes(jour_in_file, jour->buf + jour->buf_len, bytes2read);
		if (status!=bytes2read)
		{
			printf("Unexpected EOF in the Journey stream, routepoint %d\n", j);
		    fclose(jour_in_file);
			return jour;
		}
		jour->rtept_list[j].pthead_os=jour->buf_len;
		jour->buf_len += bytes2read;
		if (opts.explore_flag)
			print_f_jour_pt_head((struct f_jour_pt_head*)
									(jour->buf + jour->rtept_list[j].pthead_os));

		// read pt text1
		bytes2read=((struct f_jour_pt_head*)
						(jour->buf + jour->rtept_list[j].pthead_os))->cbtext1;
		if (bytes2read)
		{
			jour->buf=(char*)xrealloc(jour->buf, jour->buf_len+bytes2read);
			status = readbytes(jour_in_file, jour->buf + jour->buf_len, bytes2read);
			if (status!=bytes2read)
			{
				printf("Unexpected EOF in the Journey stream, routepoint %d\n", j);
			    fclose(jour_in_file);
				return jour;
			}
			jour->rtept_list[j].text1= (char*)malloc(bytes2read+1);
			memcpy(jour->rtept_list[j].text1, jour->buf + jour->buf_len, bytes2read);
			jour->rtept_list[j].text1[bytes2read]=0;
			jour->buf_len += bytes2read;
		
			str2ascii(jour->rtept_list[j].text1);
		}
		if (opts.explore_flag)
			printf("Got text1 %s\n", jour->rtept_list[j].text1);

		// read pt middle
		bytes2read=sizeof(struct f_jour_pt_mid);
		jour->buf=(char*)xrealloc(jour->buf, jour->buf_len+bytes2read);
		status = readbytes(jour_in_file, jour->buf + jour->buf_len, bytes2read);
		if (status!=bytes2read)
		{
			printf("Unexpected EOF in the Journey stream, routepoint %d\n", j);
		    fclose(jour_in_file);
			return jour;
		}
		jour->rtept_list[j].ptmid_os=jour->buf_len;
		jour->buf_len += bytes2read;
		if (opts.explore_flag)
		{
			printf("dumping jour->rtept_list[%d].ptmid:\n", j);
			printbuf(jour->buf + jour->rtept_list[j].ptmid_os, bytes2read);
		}

		// read pt text2
		bytes2read=2*(((struct f_jour_pt_mid*)(jour->buf+jour->rtept_list[j].ptmid_os))->cbtext2);
		if (bytes2read)
		{
		jour->buf=(char*)xrealloc(jour->buf, jour->buf_len+bytes2read);
			status = readbytes(jour_in_file, jour->buf + jour->buf_len, bytes2read);
			if (status!=bytes2read)
			{
				printf("Unexpected EOF in the Journey stream, routepoint %d\n", j);
			    fclose(jour_in_file);
				return jour;
			}
			jour->rtept_list[j].text2= (char*)malloc(bytes2read+2);
			memcpy(jour->rtept_list[j].text2, jour->buf + jour->buf_len, bytes2read);
			jour->rtept_list[j].text2[bytes2read]=0;
			jour->rtept_list[j].text2[bytes2read+1]=0;
			jour->buf_len += bytes2read;
		}
		if (opts.explore_flag)
			wprintf(L"Got text2 %s\n", jour->rtept_list[j].text2);

		// read pt tail
		bytes2read=sizeof(struct f_jour_pt_tail);
		jour->buf=(char*)xrealloc(jour->buf, jour->buf_len+bytes2read);
		status = readbytes(jour_in_file, jour->buf + jour->buf_len, bytes2read);
		if (status!=bytes2read)
		{
			printf("Unexpected EOF in the Journey stream, routepoint %d\n", j);
		    fclose(jour_in_file);
			return jour;
		}
		jour->rtept_list[j].pttail_os=jour->buf_len;
		jour->buf_len += bytes2read;
		if (opts.explore_flag)
			print_f_jour_pt_tail((struct f_jour_pt_tail*)(jour->buf +jour->rtept_list[j].pttail_os));

		pt_head = (struct f_jour_pt_head*)(jour->buf+jour->rtept_list[j].pthead_os);
//		UdId= ((struct f_jour_pt_head*)(jour->buf+jour->rtept_list[j].pthead_os))->UdId;
		UdId = pt_head->UdId;
		jour->rtept_list[j].pushpin = ppin_by_UdId(UdId, ppplist);

		if (jour->rtept_list[j].pushpin != NULL)
		{
			if (opts.verbose_flag > 5)
			{
				printf("Found matching pushpin:\n");
				printf("Pushpin name is: %s\n", jour->rtept_list[j].pushpin->UdName);
				printf("Pushpin short note is: %s\n", jour->rtept_list[j].pushpin->NoteShort);
			}
/*			if (opts.verbose_flag > 4)
			{
				lat=jour->rtept_list[j].pushpin->lat;
				lon=jour->rtept_list[j].pushpin->lon;

				printf("Matching pushpin Grid %#x=%d, Precision %#x=%d, MOBBId %#x=%d\n", 
						jour->rtept_list[j].pushpin->Grid, 
						jour->rtept_list[j].pushpin->Grid, 
						jour->rtept_list[j].pushpin->Precision, 
						jour->rtept_list[j].pushpin->Precision, 
						jour->rtept_list[j].pushpin->MOBBId, 
						jour->rtept_list[j].pushpin->MOBBId);

				printf("Matching pushpin Lat %f Lon %f\n", 
						jour->rtept_list[j].pushpin->lat, 
						jour->rtept_list[j].pushpin->lon);

				x = cos(lon*M_PI/180)*cos(lat*M_PI/180);
				y = sin(lon*M_PI/180)*cos(lat*M_PI/180);
				z = sin(lat*M_PI/180);

				printf("For matching pushpin X=%f Y=%f Z=%f\n", x,y,z); 
			}
*/
		}
/*		else
		{
			printf("*** Warning *** Ignoring route point '%s' without matching pushpin\n",
					jour->rtept_list[j].text1);
			printf("(yes, I should do someting more clever than this).\n");
			if (UdId)
				printf("There should have been a matching pushin because UdId=%d\n", UdId);
		}
*/
	}

	// read jour generic opts
	bytes2read=sizeof(struct f_jour_opts);
	jour->buf=(char*)xrealloc(jour->buf, jour->buf_len+bytes2read);
	status = readbytes(jour_in_file, jour->buf + jour->buf_len, bytes2read);
	if (status!=bytes2read)
	{
		printf("Unexpected EOF in the Journey stream, options\n");
	    fclose(jour_in_file);
		return jour;
	}
	jour->jopts_os=jour->buf_len;
	jour->buf_len += bytes2read;
	if (opts.explore_flag)
		print_f_jour_opts((struct f_jour_opts*)(jour->buf + jour->jopts_os));

	// read file-version specific journey options
	if( (opts.st_version_num==8) && (opts.isUSA==0))
	{
		bytes2read=sizeof(struct f_jour_opts_EUR_8);
		jour->buf=(char*)xrealloc(jour->buf, jour->buf_len+bytes2read);
		status = readbytes(jour_in_file, jour->buf + jour->buf_len, bytes2read);
		if (status!=bytes2read)
		{
			printf("Unexpected EOF in the Journey stream, options eur8\n");
		    fclose(jour_in_file);
			return jour;
		}
		jour->jopts_eur8_os=jour->buf_len;
		jour->buf_len += bytes2read;
		opts_eur8=(struct f_jour_opts_EUR_8*)(jour->buf + jour->jopts_eur8_os);
		jour->count_avoid_regions = opts_eur8->count_avoid_regions;
		if (opts.explore_flag)
			print_f_jour_opts_EUR_8(opts_eur8);
	}
	else if(    ( (opts.st_version_num==10) && (opts.isUSA==0) )
			 || ( (opts.st_version_num==9)  && (opts.isUSA==1) )
			 || ( (opts.st_version_num==11) && (opts.isUSA==1) ))
	{
		bytes2read=sizeof(struct f_jour_opts_EUR_10);
		jour->buf=(char*)xrealloc(jour->buf, jour->buf_len+bytes2read);
		status = readbytes(jour_in_file, jour->buf + jour->buf_len, bytes2read);
		if (status!=bytes2read)
		{			
			printf("Unexpected EOF in the Journey stream, options eur10\n");
		    fclose(jour_in_file);
			return jour;
		}
		jour->jopts_eur10_os=jour->buf_len;
		jour->buf_len += bytes2read;
		opts_eur10=(struct f_jour_opts_EUR_10*)(jour->buf + jour->jopts_eur10_os);
		jour->count_avoid_regions = opts_eur10->count_avoid_regions;
		if (opts.explore_flag)
			print_f_jour_opts_EUR_10(opts_eur10);
	}
	else if( (opts.st_version_num==8) && (opts.isUSA) )
	{
		bytes2read=sizeof(struct f_jour_opts_USA_8);
		jour->buf=(char*)xrealloc(jour->buf, jour->buf_len+bytes2read);
		status = readbytes(jour_in_file, jour->buf + jour->buf_len, bytes2read);
		if (status!=bytes2read)
		{
			printf("Unexpected EOF in the Journey stream, options usa8\n");
		    fclose(jour_in_file);
			return jour;
		}
		jour->jopts_usa8_os=jour->buf_len;
		jour->buf_len += bytes2read;
		opts_usa8=(struct f_jour_opts_USA_8*)(jour->buf + jour->jopts_usa8_os);
		jour->count_avoid_regions = opts_usa8->count_avoid_regions;
		if (opts.explore_flag)
			print_f_jour_opts_USA_8(opts_usa8);
	}
	else if( (opts.st_version_num==10) && (opts.isUSA) )
	{
		bytes2read=sizeof(struct f_jour_opts_USA_10);
		jour->buf=(char*)xrealloc(jour->buf, jour->buf_len+bytes2read);
		status = readbytes(jour_in_file, jour->buf + jour->buf_len, bytes2read);
		if (status!=bytes2read)
		{
			printf("Unexpected EOF in the Journey stream, options usa10\n");
		    fclose(jour_in_file);
			return jour;
		}
		jour->jopts_usa10_os=jour->buf_len;
		jour->buf_len += bytes2read;
		opts_usa10=(struct f_jour_opts_USA_10*)(jour->buf + jour->jopts_usa10_os);
		jour->count_avoid_regions = opts_usa10->count_avoid_regions;
		if (opts.explore_flag)
			print_f_jour_opts_USA_10(opts_usa10);
	}
	else
		printf("I dont yet understand the structure of the Journey options for this file version.\n");

	if (jour->count_avoid_regions >0)
	{
		printf("Reading %d avoid regions", jour->count_avoid_regions);

		// an array of pointers to f_jour_avoid
		jour->avoid_os_list=(int*)xmalloc((jour->count_avoid_regions)*sizeof(int));

		for (j=0; j< jour->count_avoid_regions; j++)
		{
			// read avoid record
			bytes2read=sizeof(struct f_jour_avoid);
			jour->buf=(char*)xrealloc(jour->buf, jour->buf_len+bytes2read);
			status = readbytes(jour_in_file, jour->buf + jour->buf_len, bytes2read);
			if (status!=bytes2read)
			{
				printf("Unexpected EOF in the Journey stream, avoid record %d\n", j);
				fclose(jour_in_file);
				return jour;
			}
			jour->avoid_os_list[j]= jour->buf_len;
			jour->buf_len += bytes2read;
			if (opts.explore_flag)
				print_f_jour_avoid((struct f_jour_avoid*)(jour->buf + jour->avoid_os_list[j]));
		}
	}

	// read journey tail
	bytes2read=sizeof(struct f_jour_trailer);
	jour->buf=(char*)xrealloc(jour->buf, jour->buf_len+bytes2read);
	status = readbytes(jour_in_file, jour->buf + jour->buf_len, bytes2read);
	if (status!=bytes2read)
	{
		printf("Unexpected EOF in the Journey stream, trailer\n");
	    fclose(jour_in_file);
		return jour;
	}
	jour->trailer_os=jour->buf_len;
	jour->buf_len += bytes2read;
	if (opts.explore_flag)
		print_f_jour_trailer((struct f_jour_trailer*)(jour->buf + jour->trailer_os));

	readmore=0;
	if ((readbyte = getc(jour_in_file))!=EOF)
	{
		fprintf (stderr, "Did not finish reading journey file at EOF\n");

		do {
			readmorebuf[readmore]=(char)readbyte;
			readmore++;
			if (readmore>max_read_more)
				break;
		} while ((readbyte = getc(jour_in_file))!=EOF);

		printf("read a further %d bytes past expected eof\n", readmore);
		printbuf(readmorebuf, readmore);
	}

    fclose(jour_in_file);

	printf("Finished processing Journey stream.\n");
	fflush(stdout);

	return jour;
}
