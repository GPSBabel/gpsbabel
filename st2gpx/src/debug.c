/*
	debug.c

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
	printf("     0  1  2  3  4  5  6  7  8  9  A  B  C  D  E  F\n");
	printf("    -----------------------------------------------");
	for(i=0; i<len; i++)
	{
		if (((i+1) & 0x0f) == 0x1)
			printf("\n%2x| ",i/16);
		printf("%2x ", (unsigned char)buf[i]);
	}
	printf("\n\n");
}

void printpoints(char* buf, int numpts)
{
	int i;
	struct gpxpt* pt;
	printf("Latitude  Longitude Height");
	printf("\n");
	printf("--------------------------\n");
	for(i=0; i<numpts; i++)
	{
		pt = gpx_get_point(buf + 12*i);
		printf("%f %f %f \n",pt->lat, pt->lon, pt->elevation);
		gpxpt_delete(pt);
	}
	printf("\n\n");
}

void explore_annot(struct annot_rec * rec)
{
}

void print_f_jour_header(struct f_jour_header * head)
{
	printf("struct f_jour_header:\n");
	printf("iunkn0 %x=%d\n", head->iunkn0, head->iunkn0);
	printf("iunkn1 %x=%d\n", head->iunkn1, head->iunkn1);
	printf("iunkn2 %x=%d\n", head->iunkn2, head->iunkn2);
	printf("iunkn3 %x=%d\n", head->iunkn3, head->iunkn3);
	printf("cpoints %x=%d\n", head->cpoints, head->cpoints);
	printf("iunkn4 %x=%d\n", head->iunkn4, head->iunkn4);
	printf("iunkn5 %x=%d\n", head->iunkn5, head->iunkn5);
	printf("iunkn6 %x=%d\n", head->iunkn6, head->iunkn6);
}

void print_f_jour_pt_head(struct f_jour_pt_head * pt_head)
{
//	double x;
	printf("struct f_jour_pt_head:\n");
	printf("UdId %d\n", pt_head->UdId);
	printf("stop_dur_secs %d\n",  pt_head->stop_dur_secs);
	printf("iunkn0 %#x=%d\n", pt_head->iunkn0, pt_head->iunkn0);
	printf("sched_arrive_flag %d\n", pt_head->sched_arrive_flag);
	printf("sched_depart_flag %d\n", pt_head->sched_depart_flag);
	printf("arrive_time_secs %d\n", pt_head->arrive_time_secs);
	printf("depart_time_secs %d\n", pt_head->depart_time_secs);
	printf("iunkn1 %#x=%d\n", pt_head->iunkn1, pt_head->iunkn1);
//	x = pt_head->unkn_scaled_lon;
//	x = x*360/0x10000;
//	x = x/0x10000;
	printf("scaled_lon %d gives lon %f\n",
			pt_head->scaled_lon, scaled2deg(pt_head->scaled_lon));
//	x = (pt_head->unkn_scaled_lat*360/0x10000);
//	x = x/0x10000;
	printf("scaled_lat %d gives lat %f\n",
			pt_head->scaled_lat, scaled2deg(pt_head->scaled_lat));
	printf("cbtext1 %d\n",pt_head->cbtext1);
	printbuf((char*)pt_head, sizeof(struct f_jour_pt_head));
}

void print_f_jour_pt_tail(struct f_jour_pt_tail * pt_tail)
{
	printf("struct f_jour_pt_tail: \n");
	printf("route_rd_pref %x\n",pt_tail->route_rd_pref);
	printf("iunkn0 %x=%d\n", pt_tail->iunkn0, pt_tail->iunkn0);
	printf("iunkn1 %x=%d\n", pt_tail->iunkn1, pt_tail->iunkn1);
	printf("iunkn2 %x=%d\n", pt_tail->iunkn2, pt_tail->iunkn2);
	printf("iunkn3 %x=%d\n", pt_tail->iunkn3, pt_tail->iunkn3);
	printf("road_id %x=%d\n", pt_tail->road_id, pt_tail->road_id);
	printf("dist_along_rd_frac %lf \n",  pt_tail->dist_along_rd_frac);

	printf("rd_arrive_sc_lat %d gives lat %f\n",
			pt_tail->rd_arrive_sc_lat, scaled2deg(pt_tail->rd_arrive_sc_lat));

	printf("rd_arrive_sc_lon %d gives lat %f\n",
			pt_tail->rd_arrive_sc_lon, scaled2deg(pt_tail->rd_arrive_sc_lon));

	printf("rd_depart_sc_lat %d gives lat %f\n",
			pt_tail->rd_depart_sc_lat, scaled2deg(pt_tail->rd_depart_sc_lat));

	printf("rd_depart_sc_lon %d gives lat %f\n",
			pt_tail->rd_depart_sc_lon, scaled2deg(pt_tail->rd_depart_sc_lon));

	printf("iunkn8 %x=%d\n", pt_tail->iunkn8, pt_tail->iunkn8);
	printf("iunkn9 %x=%d\n", pt_tail->iunkn9, pt_tail->iunkn9);
	printf("iunkn10 %x=%d\n", pt_tail->iunkn10, pt_tail->iunkn10);
	printf("iunkn11 %x=%d\n", pt_tail->iunkn11, pt_tail->iunkn11);
	printf("iunkn12 %x=%d\n", pt_tail->iunkn12, pt_tail->iunkn12);
	printf("iunkn13 %x=%d\n", pt_tail->iunkn13, pt_tail->iunkn13);
	printf("iunkn14 %x=%d\n", pt_tail->iunkn14, pt_tail->iunkn14);

	printbuf((char*)pt_tail, sizeof(struct f_jour_pt_tail));
}

void print_f_jour_opts(struct f_jour_opts * jopts)
{
	printf("struct f_jour_opts:\n");

	printf("siunkn0 %d\n", jopts->siunkn0);
	printf("funkn0 %f\n", jopts->funkn0);
	printf("toll_rd_pref %lf\n", jopts->toll_rd_pref);
	printf("motorway_pref %lf\n", jopts->motorway_pref);
	printf("major_rd_pref %lf\n", jopts->major_rd_pref);
	printf("minor_rd_pref %lf\n", jopts->minor_rd_pref);
	printf("ferry_pref %lf\n", jopts->ferry_pref);
	printf("iunkn0 %d\n", jopts->iunkn0);
	printf("iunkn1 %d\n", jopts->iunkn1);
	printf("iunkn2 %d\n", jopts->iunkn2);
	printf("fuel_price %lf\n", jopts->fuel_price);
	printf("iunkn3 %d\n", jopts->iunkn3);
	printf("fuel_price_unit %d\n", jopts->fuel_price_unit);
	printf("iunkn4 %d\n", jopts->iunkn4);
	printf("tank_capacity %lf\n", jopts->tank_capacity);
	printf("tank_capacity_units %d\n", jopts->tank_capacity_units);
	printf("tank_start_level %lf\n", jopts->tank_start_level);
	printf("tank_warn_level %lf\n", jopts->tank_warn_level);
	printf("iunkn5 %d\n", jopts->iunkn5);
	printf("fuel_use_city %lf\n", jopts->fuel_use_city);
	printf("fuel_use_city_unit %d\n", jopts->fuel_use_city_unit);
	printf("iunkn6 %d\n", jopts->iunkn6);
	printf("fuel_use_mway %lf\n", jopts->fuel_use_mway);
	printf("fuel_use_mway_unit %d\n", jopts->fuel_use_mway_unit);
	printf("iunkn7 %d\n", jopts->iunkn7);
	printf("mway_speed %f\n", jopts->mway_speed);
	printf("mway_speed_unit %d\n", jopts->mway_speed_unit);
	printf("iunkn9 %d\n", jopts->iunkn9);
	printf("lim_acc_speed %f\n", jopts->lim_acc_speed);
	printf("lim_acc_speed_unit %d\n", jopts->lim_acc_speed_unit);
	printf("iunkn11 %d\n", jopts->iunkn11);
	printf("maj_rd_speed %f\n", jopts->maj_rd_speed);
	printf("maj_rd_speed_unit %d\n", jopts->maj_rd_speed_unit);
	printf("iunkn13 %d\n", jopts->iunkn13);
	printf("min_rd_speed %f\n", jopts->min_rd_speed);
	printf("min_rd_speed_unit %d\n", jopts->min_rd_speed_unit);
	printf("iunkn15 %d\n", jopts->iunkn15);
	printf("street_speed %f\n", jopts->street_speed);
	printf("street_speed_unit %d\n", jopts->street_speed_unit);
	printf("iunkn17 %d\n", jopts->iunkn17);
	printf("iunkn18 %d\n", jopts->iunkn18);
	printf("funkn1 %f\n", jopts->funkn1);
	printf("iunkn19 %d\n", jopts->iunkn19);
	printf("iunkn20 %d\n", jopts->iunkn20);
	printf("iunkn21 %d\n", jopts->iunkn21);
	printf("funkn2 %f\n", jopts->funkn2);
	printf("iunkn22 %d\n", jopts->iunkn22);
	printf("iunkn23 %d\n", jopts->iunkn23);
	printf("iunkn24 %d\n", jopts->iunkn24);
	printf("funkn3 %f\n", jopts->funkn3);
	printf("iunkn25 %d\n", jopts->iunkn25);
	printf("iunkn26 %d\n", jopts->iunkn26);
	printf("iunkn27 %d\n", jopts->iunkn27);
	printf("funkn4 %f\n", jopts->funkn4);
	printf("iunkn28 %d\n", jopts->iunkn28);
	printf("iunkn29 %d\n", jopts->iunkn29);
	printf("iunkn30 %d\n", jopts->iunkn30);
	printf("funkn5 %f\n", jopts->funkn5);
	printf("iunkn31 %d\n", jopts->iunkn31);
	printf("iunkn32 %d\n", jopts->iunkn32);
	printf("iunkn33 %d\n", jopts->iunkn33);
	printf("start_drv_hr %d\n", jopts->start_drv_hr);
	printf("start_drv_min %d\n", jopts->start_drv_min);
	printf("iunkn34 %d\n", jopts->iunkn34);
	printf("end_drv_hr %d\n", jopts->end_drv_hr);
	printf("end_drv_min %d\n", jopts->end_drv_min);
	printf("fuel_warn_flag %d\n", jopts->fuel_warn_flag);
	printf("iunkn35 %d\n", jopts->iunkn35);
	printf("fuel_fixed_rate_flag %d\n", jopts->fuel_fixed_rate_flag);
	printf("iunkn36 %d\n", jopts->iunkn36);
	printf("fuel_cost_dist %d\n", jopts->fuel_cost_dist);
	printf("route_flex_secs %d\n", jopts->route_flex_secs);
	printf("iunkn37 %d\n", jopts->iunkn37);
	printf("rest_flag %d\n", jopts->rest_flag);
	printf("rest_dur_secs %d\n", jopts->rest_dur_secs);
	printf("rest_interval_secs %d\n", jopts->rest_interval_secs);

	printbuf((char*)jopts, sizeof(struct f_jour_opts));
	printf("\n");
}

void print_f_jour_opts_EUR_8(struct f_jour_opts_EUR_8 * jopts)
{
	printf("struct f_jour_opts_EUR_8:\n");

	printf("iunkn38 %d\n", jopts->iunkn38);
	printf("iunkn39 %d\n", jopts->iunkn39);
	printf("iunkn40 %d\n", jopts->iunkn40);
	printf("iunkn41 %d\n", jopts->iunkn41);
	printf("iunkn42 %d\n", jopts->iunkn42);
	printf("siunkn1 %d\n", jopts->siunkn1);
	printf("route_show_tm %d\n", jopts->route_show_tm);
	printf("route_show_dist %d\n", jopts->route_show_dist);
	printf("route_show_inst %d\n", jopts->route_show_inst);
	printf("route_show_for %d\n", jopts->route_show_for);
	printf("route_show_to %d\n", jopts->route_show_to);
	printf("route_show_font_size %d\n", jopts->route_show_font_size);
	printf("iunkn43 %d\n", jopts->iunkn43);
	printf("iunkn44 %d\n", jopts->iunkn44);
	printf("count_avoid_regions %d\n", jopts->count_avoid_regions);

	printbuf((char*)jopts, sizeof(struct f_jour_opts_EUR_8));
	printf("\n");
}

void print_f_jour_opts_EUR_10(struct f_jour_opts_EUR_10 * jopts)
{
	printf("struct f_jour_opts_EUR_10:\n");

	printf("iunkn38 %d\n", jopts->iunkn38);
	printf("iunkn39 %d\n", jopts->iunkn39);
	printf("iunkn40 %d\n", jopts->iunkn40);
	printf("iunkn41 %d\n", jopts->iunkn41);
	printf("iunkn41 %d\n", jopts->iunkn42);
	printf("count_avoid_regions %d\n", jopts->count_avoid_regions);

	printbuf((char*)jopts, sizeof(struct f_jour_opts_EUR_10));
	printf("\n");
}

void print_f_jour_opts_USA_8(struct f_jour_opts_USA_8 * jopts)
{
	printf("struct f_jour_opts_USA_8:\n");

	printf("iunkn38 %d\n", jopts->iunkn38);
	printf("iunkn39 %d\n", jopts->iunkn39);
	printf("iunkn40 %d\n", jopts->iunkn40);
	printf("iunkn41 %d\n", jopts->iunkn41);
	printf("iunkn42 %d\n", jopts->iunkn42);
	printf("siunkn1 %d\n", jopts->siunkn1);
	printf("iunkn43 %d\n", jopts->iunkn43);
	printf("iunkn44 %d\n", jopts->iunkn44);
	printf("iunkn45 %d\n", jopts->iunkn45);
	printf("iunkn46 %d\n", jopts->iunkn46);
	printf("iunkn47 %d\n", jopts->iunkn47);
	printf("iunkn48 %d\n", jopts->iunkn48);
	printf("iunkn49 %d\n", jopts->iunkn49);
	printf("count_avoid_regions %d\n", jopts->count_avoid_regions);

	printbuf((char*)jopts, sizeof(struct f_jour_opts_USA_8));
	printf("\n");
}

void print_f_jour_opts_USA_10(struct f_jour_opts_USA_10 * jopts)
{
	printf("struct f_jour_opts_USA_10:\n");

	printf("iunkn38 %d\n", jopts->iunkn38);
	printf("iunkn39 %d\n", jopts->iunkn39);
	printf("iunkn40 %d\n", jopts->iunkn40);
	printf("iunkn41 %d\n", jopts->iunkn41);
	printf("iunkn42 %d\n", jopts->iunkn42);
	printf("siunkn1 %d\n", jopts->siunkn1);
	printf("iunkn43 %d\n", jopts->iunkn43);
	printf("iunkn44 %d\n", jopts->iunkn44);
	printf("count_avoid_regions %d\n", jopts->count_avoid_regions);

	printbuf((char*)jopts, sizeof(struct f_jour_opts_USA_10));
	printf("\n");
}

void print_f_jour_avoid(struct f_jour_avoid * avoid)
{
	struct gpxpt * gpt = gpx_get_point((char*)avoid);
	struct point pt;
	printf("struct f_jour_avoid:\n");

	printf("x %f\n", avoid->x);
	printf("y %f\n", avoid->y);
	printf("z %f\n", avoid->z);
	printf("iunkn0 %d\n", avoid->iunkn0);
	printf("iunkn1 %d\n", avoid->iunkn1);
	printf("iunkn2 %d\n", avoid->iunkn2);
	printf("iunkn3 %d\n", avoid->iunkn3);
	printf("iunkn4 %d\n", avoid->iunkn4);
	printf("iunkn5 %d\n", avoid->iunkn5);
	printf("annot_num %d\n", avoid->annot_num);
	printf("x,y,z give lat %f and lon %f\n", gpt->lat, gpt->lon);

	pt = grid2latlon(avoid->iunkn1, avoid->iunkn2);
	printf("iunkn1, lunkn2 as grid, precision give lat %f and lon %f\n", pt.lat, pt.lon);
	pt = grid2latlon(avoid->iunkn2, avoid->iunkn3);
	printf("iunkn2, lunkn3 as grid, precision give lat %f and lon %f\n", pt.lat, pt.lon);
	pt = grid2latlon(avoid->iunkn3, avoid->iunkn4);
	printf("iunkn3, lunkn4 as grid, precision give lat %f and lon %f\n", pt.lat, pt.lon);

	printbuf((char*)avoid, sizeof(struct f_jour_avoid));
	printf("\n");
}

void print_f_jour_trailer(struct f_jour_trailer * trailer)
{
	printf("struct f_jour_trailer:\n");

	printf("iunkn0 %d\n", trailer->iunkn0);
	printf("iunkn1 %d\n", trailer->iunkn1);

	printbuf((char*)trailer, sizeof(struct f_jour_trailer));
	printf("\n");
}

void print_annot_rec(struct annot_rec * rec)
{
	int bit_flags = *(int*)(rec->buf + 8);
	char* rec_type = NULL;

	if (rec->type<4)
		rec_type =annot_type_name[rec->type];

	printf("Got annotation id %d, of type %s, %d line points",
			rec->annot_num, rec_type, rec->line_points);
	if(rec->text !=NULL)
		printf(" and text '%s'", rec->text);
	printf("\n");
	if (opts.verbose_flag > 4)
		printf("(type=%d) text length %d, bitflags %#x buf length %d\n",
				rec->type, rec->text_length, bit_flags, rec->length);
}

void print_annotations(struct annotations * annots)
{
	int i;
	// This is only the main stuff
	printf("Annotations list, version=%d, num_annotations=%d, max_annot_num=%d, stream_length=%d\n",
		   annots->version, annots->num_annotations,
		   annots->max_annot_num, annots->stream_length);

	for(i=0; i<annots->num_annotations; i++)
		print_annot_rec(annots->annot_list[i]);
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
