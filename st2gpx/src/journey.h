/*
	journey.h

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
#ifdef	__cplusplus
extern "C" {
#endif

#define JOUR_FILE_HEAD_LEN 32
#define JOUR_FILE_TAIL_LEN_EUR_V8 430
#define JOUR_FILE_TAIL_LEN_USA_V8 426
#define JOUR_FILE_TAIL_LEN_USA_V10 396

// + text1 +2*text2
#define JOUR_WPTREC_LEN 121

#define JOUR_FILE_OS_NUMREC 16

#define JOURWPT_REC_HEAD_LEN 41

#define JOURWPT_RECOS_WPTNUM 0
#define JOURWPT_RECOS_STR1LEN 40
#define JOURWPT_RECOS_STR1 41
// + val(JOURWPT_RECOS_STR1LEN)
#define JOURWPT_RECOS_STR2LEN 44
#define JOURWPT_RECOS_STR2 45

// Still to test in Journey:
// * points located by address Vs by mouse Vs by pushpin
// * avoid areas
// * drag route
struct f_jour_header
{
	int	iunkn0;
	int	iunkn1;
	int	iunkn2;
	int	iunkn3;
	unsigned int cpoints;
	int	iunkn4;
	int	iunkn5;
	int	iunkn6;
};

struct f_jour_pt_head
{
	// DB prim key for pushpin
	// 0 if no pushpin
	int UdId;
	int	stop_dur_secs;
	int	iunkn0;
	int sched_arrive_flag;
	// This may have more meaning,
	// it changes with small changes in route.
	// Perhaps just small changes in route time?
	int	sched_depart_flag;
	int	arrive_time_secs;
	int	depart_time_secs;
	int	iunkn1;
	// The coords for the actual journey point, not neccessarily on a road.
	//ie =lat/360*2^32
	int scaled_lon;
	int scaled_lat;
	unsigned char cbtext1;
};

// then jour_pt_head.cbtext1 bytes of text

struct f_jour_pt_mid
{
	// this just seems to be a const FF FE FF
	char mid_tag[3];
	unsigned char cbtext2;
};

// then 2*jour_pt_mid.cbtext2 bytes of wide text

struct f_jour_pt_tail
{
	// 0=fastest
	// 1=shortest
	// 2=prefered
	// (from previous jour_pt)
	int	route_rd_pref; // =0
	// these seem const
	int iunkn0; // = 1
	int iunkn1; // = 4
	int iunkn2; // =0
	int iunkn3; // =1
	int road_id;
	double dist_along_rd_frac;
	// The coords for the arrival point on the road
	//ie =lat/360*2^32
	int rd_arrive_sc_lat;
	int rd_arrive_sc_lon;
	// The coords for the departure point on the road
	int rd_depart_sc_lat;
	int rd_depart_sc_lon;
	int	iunkn8;  // 4 
	int	iunkn9;  // 1 
	int	iunkn10; // 1 
	int	iunkn11; // 6 
	int	iunkn12; // 1
	int	iunkn13; // 1
	// ? size ?
	int	iunkn14; // 30
} ;

struct f_jour_opts
{
	// @00
	short int	siunkn0;
	float	funkn0;		
	double	toll_rd_pref;
	double	motorway_pref;	// ok
	// @16
	double	major_rd_pref; 
	double	minor_rd_pref;
	// @26
	double	ferry_pref;
	int		iunkn0;
	// @32
	int		iunkn1;
	int		iunkn2;
	double	fuel_price;			// ok
	// @42
	// 3 = per US Gallon
	int		iunkn3;
	int		fuel_price_unit;
	int		iunkn4;
	double	tank_capacity;		// ok
	// @56
	// 2 = liters
	int		tank_capacity_units;
	double	tank_start_level;	// ok
	// @62
	double	tank_warn_level;	// ok
	int		iunkn5;	
	double	fuel_use_city;		// ok
	// @76
	// 3 = liters/100k
	int		fuel_use_city_unit;
	int		iunkn6;
	double	fuel_use_mway;		//ok
	// @86
	// 3 = liters/100k
	int		fuel_use_mway_unit;
	int		iunkn7;
	double	mway_speed;			//ok
	// @96
	int		mway_speed_unit;
	int		iunkn9;
	double	lim_acc_speed;		//ok
	// @a6
	int		lim_acc_speed_unit;
	int		iunkn11;
	double	maj_rd_speed;		//ok
	// @b6
	int		maj_rd_speed_unit;
	int		iunkn13;
	double	min_rd_speed;		//ok
	// @c6
	int		min_rd_speed_unit;
	int		iunkn15;
	double	street_speed;		//ok
	// @d6
	int		street_speed_unit;
	int		iunkn17;
	int		iunkn18;
	float	funkn1;
	// @e6
	int		iunkn19;
	int		iunkn20;
	int		iunkn21;
	float	funkn2;
	// @f6
	int		iunkn22;
	int		iunkn23;
	int		iunkn24;
	float	funkn3;
	// @106
	int		iunkn25;
	int		iunkn26;
	int		iunkn27;
	float	funkn4;
	// @116
	int		iunkn28;
	int		iunkn29;
	int		iunkn30;
	float	funkn5;
	// @126
	int		iunkn31;
	int		iunkn32;
	int		iunkn33;
	int		start_drv_hr;		//ok
	// @136
	int		start_drv_min;		//ok
	int		iunkn34;
	int		end_drv_hr;			//ok
	int		end_drv_min;		//ok
	// @146
	int		fuel_warn_flag;		
	int		iunkn35;
	int		fuel_fixed_rate_flag;
	int		iunkn36;
	// @156
	int		fuel_cost_dist;	
	int		route_flex_secs;	//ok
	int		iunkn37;
	int		rest_flag;
	// @166
	int		rest_dur_secs;
	int		rest_interval_secs;
} ;

// 56 bytes
 struct f_jour_opts_EUR_8
{
	int		iunkn38;
	int		iunkn39;
	// @176
	int		iunkn40;
	int		iunkn41;
	int		iunkn42;
	short int	siunkn1;
	// @184
	int		route_show_tm;
	int		route_show_dist;
	int		route_show_inst;
	// @190
	int		route_show_for;
	int		route_show_to;
	int		route_show_font_size;
	int		iunkn43;
	// @1a0
	// not in usa version8
	int		iunkn44;
	unsigned short int count_avoid_regions;
} ;

// 22 bytes
// Also works for USA9 and USA11 - and EUR9, EUR11
struct f_jour_opts_EUR_10
{
	int		iunkn38;
	int		iunkn39;
	// @176
	int		iunkn40;
	int		iunkn41;
	int		iunkn42;

	unsigned short int count_avoid_regions;
};

// 52 bytes
struct f_jour_opts_USA_8
{
	int		iunkn38;
	int		iunkn39;
	// @176
	int		iunkn40;
	int		iunkn41;
	int		iunkn42;
	short int	siunkn1;
	// @184
	int		iunkn43;
	int		iunkn44;
	int		iunkn45;
	// @190
	int		iunkn46;
	int		iunkn47;
	int		iunkn48;
	int		iunkn49;
	// @1a0
	unsigned short int count_avoid_regions;
};

// 32 bytes
struct f_jour_opts_USA_10
{
	int		iunkn38;
	int		iunkn39;
	// @176
	int		iunkn40;
	int		iunkn41;
	int		iunkn42;
	short int	siunkn1;
	// @184
	int		iunkn43;
	int		iunkn44;
	unsigned short int count_avoid_regions;
};


struct f_jour_avoid
{
	float	x;
	float	y;
	float	z;
	int		iunkn0;
	int		iunkn1;
	int		iunkn2;
	int		iunkn3;
	int		iunkn4;
	int		iunkn5;
	// avoid num? annot?
	int		annot_num;
};


struct f_jour_trailer
{
	int		iunkn0; // =0x65
	int		iunkn1;
};

struct jour_rtept
{
	int pthead_os;
	// mem owned by this struct, as buf does not have terminating null
	char* text1;
	int ptmid_os;
	// mem owned by this struct, as buf does not have terminating null
	char* text2;
	int pttail_os;
	// pointer to pushpin owned by pushpinlist
	struct pushpin* pushpin;
	char garmin_ident[7];
};

struct jour_rtept_rec * jour_rtept_rec_new();
void jour_rtept_rec_delete(struct jour_rtept_rec * jourpt_rec);

struct journey
{
	int buf_len;
	char* buf;
	// We need to use nasty unsigned shorts if we compare to ->header->cpoints
	int count_rtepts;
	// a convenience for looking in multiple version opts 
	int count_avoid_regions;
	int header_os;
	// an array of jour_rtept
	struct jour_rtept * rtept_list;
	int jopts_os;
	int jopts_eur8_os;
	int jopts_eur10_os;
	int jopts_usa8_os;
	int jopts_usa10_os;
	// an array of ofsets to f_jour_avoid
	int * avoid_os_list;
	int trailer_os;
};

float scaled2deg(int scaled_deg);

struct journey * journey_new();
void journey_delete(struct journey * jour);

struct journey * process_journey_stream (char* jour_in_file_name, struct pushpin_safelist * ppplist);


#ifdef	__cplusplus
}
#endif
