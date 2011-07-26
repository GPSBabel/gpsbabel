/*

    GPSBabel session (format session) management
    Copyright (C) 2008 Olaf Klein, o.b.klein@gpsbabel.org

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

#ifndef SESSION_H
#define SESSION_H

typedef struct {
  queue Q;
  int id;
  char* name;
} category_t;

typedef struct {
  queue Q;
  int nr;
  const char* name;		/* in normal case the name of a format */
  char* filename;			/* used file within format */
  int category_ct;
  int unknown_category_ct;	/* added without id */
  queue category_list;
} session_t;

void session_init(void);
void session_exit(void);

void start_session(const char* name, const char* filename);
session_t* curr_session(void);

/* in work
int session_add_category(const char *name, const int id);
*/

#endif
