/*
    Great Circle utility functions

    Copyright (C) 2002-2014 Robert Lipe, robertlipe+source@gpsbabel.org

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

#ifndef GRTCIRC_H
#define GRTCIRC_H

double gcdist(double lat1, double lon1, double lat2, double lon2);
double heading(double lat1, double lon1, double lat2, double lon2);
double heading_true_degrees(double lat1, double lon1, double lat2, double lon2);

double linedistprj(double lat1, double lon1,
                   double lat2, double lon2,
                   double lat3, double lon3,
                   double* prjlat, double* prjlon,
                   double* frac);

double linedist(double lat1, double lon1,
                double lat2, double lon2,
                double lat3, double lon3);

double radtometers(double rads);
double radtomiles(double rads);

void linepart(double lat1, double lon1,
              double lat2, double lon2,
              double frac,
              double* reslat, double* reslon);

/* Degrees to radians */
#define DEG(x) ((x)*180.0/M_PI)

/* Radians to degrees */
#define RAD(x) ((x)*M_PI/180.0)

#endif
