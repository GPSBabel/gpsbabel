/*
    Garmin icon tables
    Based on information provided by Ian Cowley, Sigurd Humerfelt,
	and Garmin MapSource

    Copyright (C) 2003 Robert Lipe, robertlipe@usa.net

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

typedef struct icon_mapping {
	const int mpssymnum;
	const int pcxsymnum;
	const char *icon;
} icon_mapping_t;

typedef enum {MAPSOURCE, PCX, GARMIN_SERIAL} garmin_formats_e;

extern const 
char *mps_find_desc_from_icon_number(const int icon, 
	garmin_formats_e garmin_format);
extern int
mps_find_icon_number_from_desc(const char *desc, 
	garmin_formats_e garmin_format);

extern icon_mapping_t garmin_icon_table[];
