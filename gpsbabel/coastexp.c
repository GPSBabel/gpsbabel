/*
    Copyright (C) 2004 Justin Broughton, justinbr@earthlink.net

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
#if !NO_EXPAT
#include <expat.h>
static XML_Parser psr;
#endif

FILE *fd;
FILE *ofd;

#define MYNAME "coastexp"
#define MY_CBUF 4096

#if NO_EXPAT
void
ce_rd_init(const char *fname)
{
	fatal(MYNAME ": This build excluded CoastalExplorer support because expat was not installed.\n");
}

void
ce_read(void)
{
}
#else

static char *element; // Current element being parsed
static char *cdatastr; // Current XML character data being built up (until a <lf>)

/* CE-specific mark structure - used for both route marks and standalone marks */
struct CE_MARK {
	struct CE_MARK *next; 	// pointer to next mark in list
	char *id;		// CE's mark ID (of the form "{<guid>}")
	waypoint *wp;		// GPSBabel waypoint
	int used;		// Is this mark used in a route or not?
};
typedef struct CE_MARK ce_mark;

/* CE-specific route structure */
struct CE_ROUTE {
	struct CE_ROUTE *next;	// pointer to next route in list
	char *id;		// CE's route ID (of the form "{<guid>}")
	route_head *r;		// GPSBabel route header
	ce_mark *marks;		// list of CE marks in this route
};
typedef struct CE_ROUTE ce_route;

static ce_route *routes = NULL;		// List of routes currently found
static ce_route *currentRoute = NULL;	// Current route being processed
static ce_mark *marks = NULL;		// List of stand-alone marks currently found
static ce_mark *currentMark = NULL;	// Current mark being processed
static int inRoute = 0;			// Are we processing a route?
static int inMark = 0;			// Are we processing a mark?

/* Add a route to the list of routes */
static void
add_route(ce_route *route)
{
	if (routes == NULL) {
		routes = route;
	} else {
		ce_route *curr = routes;
		while (curr->next != NULL)
			curr = curr->next;
		curr->next = route;
	}
}

/* Add a mark to the list of stand-alone marks */
static void
add_mark(ce_mark *mark)
{
	if (marks == NULL) {
		marks = mark;
	} else {
		ce_mark *curr = marks;
		while (curr->next != NULL)
			curr = curr->next;
		curr->next = mark;
	}
}

/* Add a mark to the specified route */
static void
add_mark_to_route(ce_route *route, ce_mark *mark)
{
	if (route->marks == NULL) {
		route->marks = mark;
	} else {
		ce_mark *curr = route->marks;
		while (curr->next != NULL)
			curr = curr->next;
		curr->next = mark;
	}
}

/* Start processing an XML item */
static void
ce_start(void *data, const char *el, const char **attr)
{
	element = xstrdup(el);
	if (0 == strcmp(el, "Route")) {
		inRoute = 1;
		const char **ap;
		for (ap = attr; *ap; ap+=2) {
			if (0 == strcmp(ap[0], "id")) {
				// Create a CE route object and add it to the list of routes
				currentRoute = (ce_route *) xcalloc(sizeof (ce_route), 1);
				currentRoute->next = NULL;
				currentRoute->id=xstrdup(ap[1]);
				currentRoute->r = route_head_alloc();
				currentRoute->marks = NULL;
				add_route(currentRoute);
			}
		}
	} else if (0 == strcmp(el, "Mark")) {
		inMark = 1;
		const char **ap;
		for (ap = attr; *ap; ap+=2) {
			if (0 == strcmp(ap[0], "id")) {
				// Create a CE mark object and add it to the list of stand-alone marks
				currentMark = (ce_mark *) xcalloc(sizeof (ce_mark), 1);
				currentMark->next = NULL;
				currentMark->id=xstrdup(ap[1]);
				currentMark->wp = NULL;
				currentMark->used = 0;
				add_mark(currentMark);
			}
		}
	}
}

/* Finish processing an XML item */
static void
ce_end(void *data, const char *el)
{
	if (0 == strcmp(el, "Route"))
		inRoute = 0;
	else if (0 == strcmp(el, "Mark"))
		inMark = 0;
}

/* Process some XML character data for the current item */
static void
ce_cdata(void *dta, const XML_Char *s, int len)
{
	if (*s != '\n') {
		// We buffer up characters in 'cdatastr' until a single <lf> is received
		if ((strlen(cdatastr) + len) > MY_CBUF) {
			printf("Buffer overflow - line too long!");
			exit(-1);
		}
		char *edatastr = cdatastr+strlen(cdatastr);
		memcpy(edatastr, s, len);
		edatastr[len] = '\0';
	} else {
		// Now process what we have in 'cdatastr'
		s = cdatastr;
		while (*s != '\0' && (*s == '\b' || *s == '\t'))
			s++;
		if (strlen(s) <= 0)
			return;
		if (0 == strcmp(element, "Marks")) {
			if (inRoute == 1) {
				// We are processing the marks in a route so create a CE mark object
				// and add it to the current route
				ce_mark *mark = (ce_mark *) xcalloc(sizeof (ce_mark), 1);
				mark->next = NULL;
				mark->id = xstrdup(s);
				mark->wp = NULL;
				add_mark_to_route(currentRoute, mark);
			}
		} else if (0 == strcmp(element, "Position")) {
			if (inMark == 1) {
				// We are processing a standalone mark so read the lat/long position
				// and create a waypoint to add to the current mark
				char *position = xstrdup(s);
				char *lat = position;
				char *latNorS = position;
				while (*latNorS != ' ')
					latNorS++;
				*latNorS++ = '\0';
				char *lng = latNorS;
				lng++; lng++;
				char *longEorW = lng;
				while (*longEorW != ' ')
					longEorW++;
				*longEorW++ = '\0';
				currentMark->wp = waypt_new();
				currentMark->wp->latitude = atof(lat);
				if (*latNorS == 'S')
					currentMark->wp->latitude = -currentMark->wp->latitude;
				currentMark->wp->longitude = atof(lng);
				if (*longEorW == 'W')
					currentMark->wp->longitude = -currentMark->wp->longitude;
			}
		} else if (0 == strcmp(element, "Name")) {
			// Names we care about may be either for routes or marks
			char *name = xstrdup(s);
			if (inMark == 1)
				currentMark->wp->shortname = name;
			else if (inRoute == 1)
				currentRoute->r->rte_name = name;
		} else if (0 == strcmp(element, "Description")) {
			// Descriptions we care about may be either for routes or marks
			char *desc = xstrdup(s);
			if (inMark == 1)
				currentMark->wp->description = desc;
			else if (inRoute == 1)
				currentRoute->r->rte_desc = desc;
		}

		// Start building a new string since we are done with this one
		cdatastr[0] = '\0';
	}
}

/* Set up reading the CE input file */
void
ce_rd_init(const char *fname)
{
	fd = xfopen(fname, "r", MYNAME);

	psr = XML_ParserCreate(NULL);
	if (!psr) {
		fatal(MYNAME ":Cannot create XML parser\n");
	}

	XML_SetElementHandler(psr, ce_start, ce_end);
	cdatastr = xcalloc(MY_CBUF,1);
	XML_SetCharacterDataHandler(psr, ce_cdata);
}

/* Parse the input file */
void
ce_read(void)
{
	int len;
	char buf[MY_CBUF];

	while ((len = fread(buf, 1, sizeof(buf), fd))) {
		if (!XML_Parse(psr, buf, len, feof(fd))) {
			fatal(MYNAME ":Parse error at %d: %s\n",
				XML_GetCurrentLineNumber(psr),
				XML_ErrorString(XML_GetErrorCode(psr)));
		}
	}

	XML_ParserFree(psr);
}

#endif

/* Fix waypoints in route marks from the standalone marks */
void
ce_fix_waypoints(void)
{
	ce_route *route = routes;
	while (route != NULL) {
		ce_mark *mark = route->marks;
		while (mark != NULL) {
			ce_mark *mark2 = marks;
			while (mark2 != NULL) {
				if (0 == strcmp(mark->id, mark2->id)) {
					mark->wp = mark2->wp;
					mark2->used = 1;
					break;
				}
				mark2 = mark2->next;
			}
			mark = mark->next;
		}
		route = route->next;
	}
}

/* Check route name and if NULL assign a name */
void
ce_check_route_names(void)
{
	ce_route *route = routes;
	while (route != NULL) {
		if (route->r->rte_name == NULL)	{
			cdatastr = '\0';
			strcat(cdatastr, marks->wp->shortname);
			strcat(cdatastr, "->");
			ce_mark *mark = route->marks;
			while (mark->next != NULL)
				mark = mark->next;
			strcat(cdatastr, mark->wp->shortname);
			route->r->rte_name = xstrdup(cdatastr);
		}
		route = route->next;
	}
}

/* Remove marks used in routes */
void
ce_remove_unused_marks(void)
{
	ce_mark *mark = marks;
	ce_mark *prev_mark = NULL;
	while (mark != NULL) {
		if (mark->used == 1) {
			if (prev_mark == NULL)
				marks = mark = mark->next;
			else
				prev_mark->next = mark = mark->next;
			continue;
		}
		prev_mark = mark;
		mark = mark->next;
	}
}

/* Print out results */
void
ce_print_results(void)
{
	ce_route *curr_route = routes;
	while (curr_route != NULL) {
		printf("Route name=%s id=%s\n", curr_route->r->rte_name, curr_route->id);
		ce_mark *curr_mark = curr_route->marks;
		while (curr_mark != NULL) {
			if (curr_mark->wp == NULL)
				printf("  null\n");
			else
				printf("  %s (%f, %f)\n", curr_mark->wp->shortname, curr_mark->wp->latitude, curr_mark->wp->longitude);
			curr_mark = curr_mark->next;
		}
		curr_route = curr_route->next;
	}
	ce_mark *curr_mark = marks;
	while (curr_mark != NULL) {
		printf("Mark name=%s id=%s ", curr_mark->wp->shortname, curr_mark->id);
		if (curr_mark->wp == NULL)
			printf("(null)\n");
		else
			printf("(%f, %f)\n", curr_mark->wp->latitude, curr_mark->wp->longitude);
		curr_mark = curr_mark->next;
	}
}

/* Finish reading the input file */
void
ce_rd_deinit(void)
{
	ce_fix_waypoints();
	ce_check_route_names();
	ce_remove_unused_marks();
	// ce_print_results();

	// Add routes to GPSBabel
	ce_route *route = routes;
	while (route != NULL) {
		route_add_head(route->r);
		ce_mark *mark = route->marks;
		while (mark->next != NULL) {
			route_add_wpt(route->r, mark->wp);
			mark = mark->next;
		}
		route = route->next;
	}

	// Add (unused) marks to GPSBabel
	ce_mark *mark = marks;
	while (mark != NULL) {
		waypt_add(mark->wp);
		mark = mark->next;
	}

	fclose(fd);
}

void
ce_wr_init(const char *fname)
{
	fatal(MYNAME ": Does not support writing CoastalExplorer files.\n");
	ofd = xfopen(fname, "w", MYNAME);
}

void
ce_wr_deinit(void)
{
	fclose(ofd);
}

void
ce_write(void)
{
}

ff_vecs_t coastexp_vecs = {
	ff_type_file,
	ce_rd_init,
	ce_wr_init,
	ce_rd_deinit,
	ce_wr_deinit,
	ce_read,
	ce_write,
	NULL,
	NULL,
};
