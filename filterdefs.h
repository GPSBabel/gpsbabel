/*
    Filter definitions.

    Copyright (C) 2005-2014 Robert Lipe, robertlipe+source@gpsbabel.org

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
    Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.

 */

#ifndef FILTERDEFS_H_INCLUDED_
#define FILTERDEFS_H_INCLUDED_

#include "defs.h"


// forward declare filter to avoid exposing global_waypoint_list by filter.h
class Filter;

Filter* find_filter_vec(const QString&);
void free_filter_vec(Filter*);
void disp_filters(int version);
void disp_filter_vec(const QString& vecname);
void disp_filter_vecs();
void init_filter_vecs();
void exit_filter_vecs();
bool validate_filters();

#endif // FILTERDEFS_H_INCLUDED_
