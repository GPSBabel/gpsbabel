/*
    Display scaled distances in 'local' units.
 
    Copyright (C) 2006 Robert Lipe, robertlipe@usa.net

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

#include "defs.h"

static int units = units_statute;

int 
fmt_setunits(fmt_units u)
{
	switch (u) {
	case units_statute:
	case units_metric:
	case units_nautical:
	case units_aviation:
		units = u;
		return 0;
	default:
		return 1;
	}
}

double
fmt_distance(const double distance_meters, char **tag)
{
	double d;

	switch (units) {
	case units_statute: 
		d = METERS_TO_FEET(distance_meters);
		if (d < 5280) {
			*tag = "ft";
		} else  {
			d = METERS_TO_MILES(distance_meters);
			*tag = "mi";
		}
		break;
	case units_nautical: 
	case units_aviation: 
		d = METERS_TO_NMILES(distance_meters);
		*tag = "NM";
		break;
	case units_metric:
		d = distance_meters;
		if (d < 1000) {
			*tag = "meters";
		} else {
			d = d / (double) 1000.0;
			*tag = "km";
		}
		break;
		
	default: 
		fatal("not done yet");
		break;
	}

	return d;
}

double
fmt_altitude(const double distance_meters, char **tag)
{
	double d;

	switch (units) {
	case units_statute: 
	case units_aviation: 
		d = METERS_TO_FEET(distance_meters);
		*tag = "ft";
		break;
	case units_nautical: 
		d = METERS_TO_NMILES(distance_meters);
		*tag = "NM";
		break;
	case units_metric:
		d = distance_meters;
		*tag = "meters";
		break;
		
	default: 
		fatal("not done yet");
		break;
	}

	return d;
}

double
fmt_speed(const double distance_meters_sec, char **tag)
{
	double d;

	switch (units) {
	case units_statute:
		d = METERS_TO_MILES(distance_meters_sec) * SECONDS_PER_HOUR ;
		*tag = "mph";
		break;
	case units_nautical:
	case units_aviation:
		d = METERS_TO_NMILES(distance_meters_sec) * SECONDS_PER_HOUR ;
		*tag = "knts";
		break;
	case units_metric:
		d = distance_meters_sec * SECONDS_PER_HOUR;
		*tag = "meters/hour";
		if (d > 1000.0) {
			d /= 1000.0;
			*tag = "km/hour";
		}
		break;
	default: fatal("not done yet");

	}
	return d;
}
