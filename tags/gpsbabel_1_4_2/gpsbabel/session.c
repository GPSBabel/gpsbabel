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

#include "defs.h"
#include "session.h"

static queue session_list;
static int session_ct;

static void session_free(session_t *s);

void
session_init(void)
{
	QUEUE_INIT(&session_list);
	session_ct = 0;
}

void
session_exit(void)
{
	queue *elem, *tmp;
	
	QUEUE_FOR_EACH(&session_list, elem, tmp) {
		session_t *s = (session_t *)elem;
		dequeue(&s->Q);
		session_free(s);
	}
}

void
start_session(const char *name, const char *filename)
{
	session_t *s;
	
	if (session_ct == 0) QUEUE_INIT(&session_list);
	session_ct++;

	s = (session_t*) xcalloc(1, sizeof(*s));
	ENQUEUE_TAIL(&session_list, &s->Q);
	QUEUE_INIT(&s->category_list);
	s->nr = session_ct;
	s->name = name;
	s->filename = xstrdup(filename);
}

session_t *
curr_session(void)
{
	return (session_t *) session_list.prev;
}

/* in work 

int
session_add_category(const char *name, const int id)
{
	queue *elem, *tmp;
	session_t *s;
	category_t *c;

	s = curr_session();
	
	QUEUE_FOR_EACH(&s->category_list, elem, tmp) {
		c = (category_t *) elem;
		if (case_ignore_strcmp(c->name, name) == 0) {
			if (id >= 0) c->id = id;
			return c->id;
		}
		
	}
	
	c = xmalloc(sizeof(*c));
	c->name = xstrdup(name);
	if (id < 0) c->id = -(++s->unknown_category_ct);
	else c->id = id;
	
	s->category_ct++;
	ENQUEUE_TAIL(&s->category_list, &c->Q);
	
	return c->id;
}
*/

/* non public functions */

static void
session_free(session_t *s)
{
	queue *elem, *tmp;
	QUEUE_FOR_EACH(&s->category_list, elem, tmp) {
		category_t *c = (category_t *) elem;
		dequeue(&c->Q);
		xfree(c);
	}
	xfree(s->filename);
	xfree(s);
}
