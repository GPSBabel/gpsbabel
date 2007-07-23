/*
    Definitions for QuoVadis files.

    Copyright (C) 2002 Robert Lipe, robertlipe@usa.net

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

 */
#ifndef __quovadis_h__
#define __quovadis_h__

#include "defs.h"
#include "pdbfile.h"

#define MYNAME "QuoVadis"
#define MYTYPE  0x51564D52	/* QVMR */
#define MYCREATOR 0x51554F56 	/* QUOV */

#define MAXCHUNKSIZE	0x10000	/* 64k Chunk max */

struct record {
    char		name[32];
    pdb_32		longitude; /* Scaled. (degrees + 180.0) * 1e6 */
    pdb_32		latitude; /* Scaled. (degrees + 90.0) * 1e6 */
    pdb_16		icon_bitmap; /* Valid range: 0 - 47 */
    pdb_32		note_id; /* Global ID of note in NotesDB. Not
				    supported yet */
    char		name_scale; /* Map scale to display the
				       name. Valid range 15-26. 15 =>
				       18.12 miles, 26 => 46 feet */
    char		icon_scale; /* As above. */
    unsigned char	reserved[8];
};

struct qv_icon_mapping {
    const geocache_type		gc_type;
    const int			bitmap_id;
};

/* Icon Types */
#define QUESTION_ICON		0
#define RESTARAUNT_ICON		1
#define BAR_ICON		2
#define HOTEL_ICON		3
#define PHONE_ICON		4
#define HOSPITAL_ICON		5
#define CHURCH_ICON		6
#define AIRPORT_ICON		7
#define TRAIN_ICON		8
#define BUS_ICON		9
#define CAR_ICON		10
#define WRENCH_ICON		11
#define GASOLINE_ICON		12
#define PARKING_ICON		13
#define MAIL_ICON		14
#define MAILBOX_ICON		15
#define CLOCK_ICON		16
#define FLAG_ICON		17
#define LIBRARY_ICON		18
#define BUILDINGS_ICON		19
#define RELIGION_ICON		20
#define DOCUMENT_ICON		21
#define ANIMAL_ICON		22
#define CAMERA_ICON		23
#define MOVIE_ICON		24
#define MUSIC_ICON		25
#define MEETING_ICON		26
#define RESTROOM_ICON		27
#define FAMILY_ICON		28
#define TELEVISION_ICON		29
#define SAILING_ICON		30
#define GOLF_ICON		31
#define WORKOUT_ICON		32
#define CYCLING_ICON		33
#define JOGGING_ICON		34
#define CAMPING_ICON		35
#define WATER_ICON		36
#define FOREST_ICON		37
#define GIFTSHOP_ICON		38
#define BABY_ICON		39
#define LOVE_ICON		40
#define CEMETARY_ICON		41
#define COMPUTER_ICON		42
#define MONEY_ICON		43
#define LEFT_ARROW_ICON		44
#define RIGHT_ARROW_ICON	45
#define DOWN_ARROW_ICON		46
#define UP_ARROW_ICON		47

/* Scale Values */
#define SCALE_18_12MI	15
#define SCALE_9_06MI	16
#define SCALE_4_53MI	17
#define SCALE_2_26MI	18
#define SCALE_1_13MI	19
#define SCALE_0_56MI	20
#define SCALE_0_28MU	21
#define SCALE_747FT	22
#define SCALE_373FT	23
#define SCALE_186FT	24
#define SCALE_93FT	25
#define SCALE_46FT	26

#define MAXRECORDS	(MAXCHUNKSIZE / sizeof(struct record))
/*#define MAXRECORDS	100*/
#define DEFAULT_ICON_BITMAP	QUESTION_ICON
#define DEFAULT_NAME_SCALE	SCALE_2_26MI
#define DEFAULT_ICON_SCALE	SCALE_18_12MI

#endif
