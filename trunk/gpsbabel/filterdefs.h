/*
    Filter definitions.

    Copyright (C) 2005  Robert Lipe, robertlipe@usa.net

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



/*
 * Filters can do some things that modules really shouldn't do.
 * This is our (weak) attempt to make that distinction.
 */

extern queue waypt_head;
#if NEWQ
extern QList<waypoint*> waypt_list;
#endif

typedef struct filter_vecs {
  filter_init f_init;
  filter_process f_process;
  filter_deinit f_deinit;
  filter_exit f_exit;
  arglist_t* args;
} filter_vecs_t;

filter_vecs_t* find_filter_vec(char* const, char**);
void free_filter_vec(filter_vecs_t*);
void disp_filters(int version);
void disp_filter(const char* vecname);
void disp_filter_vec(const char* vecname);
void disp_filter_vecs(void);
void init_filter_vecs(void);
void exit_filter_vecs(void);

