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
#include <time.h>

#include "uuid.h"

FILE *fd;
FILE *ofd;

#define MYNAME "coastexp"
#define MY_CBUF 4096
#define MY_TBUF 64
#define MY_UBUF 128

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

static void *mkshort_handle; // short-name handle
static char *element; // Current element being parsed
static char *cdatastr; // Current XML character data being built up (until a <lf>)

/* CE-specific mark structure - used for both route marks and standalone marks */
struct CE_MARK {
	queue Q;
	char *id;		// CE's mark ID (of the form "{<guid>}")
	char *created;	// CE's creation time (of the form "<YYYY><MM><DD>T<HH><MM><SS>Z")
	waypoint *wp;	// GPSBabel waypoint
	int used;		// Is this mark used in a route or not?
};
typedef struct CE_MARK ce_mark;

/* CE-specific route structure */
struct CE_ROUTE {
	queue Q;
	char *id;			// CE's route ID (of the form "{<guid>}")
	route_head *r;		// GPSBabel route header
	queue ce_mark_head;	// list of CE marks in this route
};
typedef struct CE_ROUTE ce_route;

static queue ce_route_head;				// List of routes currently found
static ce_route *currentRoute = NULL;	// Current route being processed
static queue ce_mark_head;				// List of stand-alone marks currently found
static ce_mark *currentMark = NULL;		// Current mark being processed
static char *time_buffer = NULL;		// Time buffer for processing times
static char *uuid_buffer = NULL;		// UUID buffer for processing uuid's
static int inRoute = 0;					// Are we processing a route?
static int inMark = 0;					// Are we processing a mark?

/* Add a route to the list of routes */
static void
add_route(ce_route *route)
{
	ENQUEUE_TAIL(&ce_route_head, &route->Q);
}

/* Add a mark to the list of stand-alone marks */
static void
add_mark(ce_mark *mark)
{
	ENQUEUE_TAIL(&ce_mark_head, &mark->Q);
}

/* Add a mark to the specified route */
static void
add_mark_to_route(ce_route *route, ce_mark *mark)
{
	ENQUEUE_TAIL(&route->ce_mark_head, &mark->Q);
}

/* Free a mark */
static void
free_mark(ce_mark *mark)
{
	xfree(mark->id);
	if (mark->created)
		xfree(mark->created);
	xfree(mark);
}

/* Free a route */
static void
free_route(ce_route *route)
{
	queue *elem, *tmp;
	QUEUE_FOR_EACH(&route->ce_mark_head, elem, tmp) {
		ce_mark *mark = (ce_mark *) elem;
		free_mark(mark);
	}
	xfree(route->id);
	xfree(route);
	// Don't free the waypoint since this is done elsewhere
}

/* Start processing an XML item */
static void
ce_start(void *data, const char *el, const char **attr)
{
	const char **ap;
	strcpy(element, el);
	if (0 == strcmp(el, "Route")) {
		inRoute = 1;
		for (ap = attr; *ap; ap+=2) {
			if (0 == strcmp(ap[0], "id")) {
				// Create a CE route object and add it to the list of routes
				currentRoute = (ce_route *) xcalloc(sizeof (ce_route), 1);
				currentRoute->id=xstrdup(ap[1]);
				currentRoute->r = route_head_alloc();
				QUEUE_INIT(&currentRoute->ce_mark_head);
				add_route(currentRoute);
			}
		}
	} else if (0 == strcmp(el, "Mark")) {
		inMark = 1;
		currentMark = (ce_mark *) xcalloc(sizeof (ce_mark), 1);
		currentMark->wp = NULL;
		currentMark->used = 0;
		add_mark(currentMark);
		for (ap = attr; *ap; ap+=2) {
			if (0 == strcmp(ap[0], "id")) {
				// Create a CE mark object and add it to the list of stand-alone marks
				currentMark->id = xstrdup(ap[1]);
			}
			else if (0 == strcmp(ap[0], "created")) {
				currentMark->created = xstrdup(ap[1]);
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
		char *edatastr;
		// We buffer up characters in 'cdatastr' until a single <lf> is received
		if ((strlen(cdatastr) + len) > MY_CBUF) {
			printf("Buffer overflow - line too long!");
			exit(-1);
		}
		edatastr = cdatastr+strlen(cdatastr);
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
			if (inRoute) {
				// We are processing the marks in a route so create a CE mark object
				// and add it to the current route
				ce_mark *mark = (ce_mark *) xcalloc(sizeof (ce_mark), 1);
				mark->id = xstrdup(s);
				mark->created = NULL;
				mark->wp = NULL;
				add_mark_to_route(currentRoute, mark);
			}
		} else if (0 == strcmp(element, "Position")) {
			if (inMark) {
				// We are processing a standalone mark so read the lat/long position
				// and create a waypoint to add to the current mark
				char *position = xstrdup(s);
				char *lat = position;
				char *latNorS = position;
				char *lng;
				char *longEorW;
				while (*latNorS != ' ')
					latNorS++;
				*latNorS++ = '\0';
				lng = latNorS;
				lng++; lng++;
				longEorW = lng;
				while (*longEorW != ' ')
					longEorW++;
				*longEorW++ = '\0';
				if (!currentMark->wp)
					currentMark->wp = waypt_new();
				currentMark->wp->latitude = atof(lat);
				if (*latNorS == 'S')
					currentMark->wp->latitude = -currentMark->wp->latitude;
				currentMark->wp->longitude = atof(lng);
				if (*longEorW == 'W')
					currentMark->wp->longitude = -currentMark->wp->longitude;
				xfree(position);
			}
		} else if (0 == strcmp(element, "Name")) {
			// Names we care about may be either for routes or marks
			if (inMark)
			{
				if (!currentMark->wp)
					currentMark->wp = waypt_new();
				currentMark->wp->shortname = xstrdup(s);

				// Also set the creation time
				if (currentMark->created)
				{
					struct tm t;
					char yearString[5], monthString[3], dayString[3], hourString[3], minString[3], secString[3];
					memset(&t, 0, sizeof(struct tm));
					strncpy(yearString, currentMark->created, 4);
					yearString[4] = '\0';
					t.tm_year = atoi(yearString) - 1900;
					strncpy(monthString, currentMark->created+4, 2);
					monthString[3] = '\0';
					t.tm_mon = atoi(monthString) - 1;
					strncpy(dayString, currentMark->created+6, 2);
					dayString[3] = '\0';
					t.tm_mday = atoi(dayString);
					strncpy(hourString, currentMark->created+9, 2);
					hourString[3] = '\0';
					t.tm_hour = atoi(hourString);
					strncpy(minString, currentMark->created+11, 2);
					minString[3] = '\0';
					t.tm_min = atoi(minString);
					strncpy(secString, currentMark->created+13, 2);
					secString[3] = '\0';
					t.tm_sec = atoi(secString);
					currentMark->wp->creation_time = mktime(&t);
				}
			}
			else if (inRoute)
				currentRoute->r->rte_name = xstrdup(s);
		} else if (0 == strcmp(element, "Description")) {
			// Descriptions we care about may be either for routes or marks
			char *desc = xstrdup(s);
			if (inMark)
			{
				if (!currentMark->wp)
					currentMark->wp = waypt_new();
				currentMark->wp->description = desc;
			}
			else if (inRoute)
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
	QUEUE_INIT(&ce_route_head);
	QUEUE_INIT(&ce_mark_head);

	psr = XML_ParserCreate(NULL);
	if (!psr) {
		fatal(MYNAME ":Cannot create XML parser\n");
	}

	XML_SetElementHandler(psr, ce_start, ce_end);
	cdatastr = xcalloc(MY_CBUF,1);
	element = xcalloc(MY_CBUF,1);
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
	queue *elem, *tmp;
	QUEUE_FOR_EACH(&ce_route_head, elem, tmp) {
		ce_route *route = (ce_route *) elem;
		queue *elem2, *tmp2;
		QUEUE_FOR_EACH(&route->ce_mark_head, elem2, tmp2) {
			ce_mark *mark = (ce_mark *) elem2;
			queue *elem3, *tmp3;
			QUEUE_FOR_EACH(&ce_mark_head, elem3, tmp3) {
				ce_mark *mark2 = (ce_mark *) elem3;
				if (0 == strcmp(mark->id, mark2->id)) {
					mark->wp = waypt_dupe(mark2->wp);
					mark2->used = 1;
					break;
				}
			}
		}
	}
}

/* Check route name and if NULL assign a name */
void
ce_check_route_names(void)
{
	queue *elem, *tmp;
	QUEUE_FOR_EACH(&ce_route_head, elem, tmp) {
		ce_route *route = (ce_route *) elem;
		if (route->r->rte_name == NULL)	{
			*cdatastr = '\0';
			strcat(cdatastr, ((ce_mark *) QUEUE_FIRST(&route->ce_mark_head))->wp->shortname);
			strcat(cdatastr, "->");
			strcat(cdatastr, ((ce_mark *) QUEUE_LAST(&route->ce_mark_head))->wp->shortname);
			route->r->rte_name = xstrdup(cdatastr);
		}
	}
}

/* Remove marks used in routes */
void
ce_remove_unused_marks(void)
{
	queue *elem, *tmp;
	QUEUE_FOR_EACH(&ce_mark_head, elem, tmp) {
		ce_mark *mark = (ce_mark *) elem;
		if (mark->used)
		{
			dequeue(elem);
			if (mark->wp)
				waypt_free(mark->wp);
			free_mark(mark);
		}
	}
}

/* Print out results */
void
ce_print_results(void)
{
	queue *elem, *tmp;
	QUEUE_FOR_EACH(&ce_route_head, elem, tmp) {
		queue *elem2, *tmp2;
		ce_route *route = (ce_route *) elem;
		printf("Route name=%s id=%s\n", route->r->rte_name, route->id);
		QUEUE_FOR_EACH(&route->ce_mark_head, elem2, tmp2) {
			ce_mark *mark = (ce_mark *) elem2;
			if (mark->wp == NULL)
				printf("  null\n");
			else
				printf("  %s (%f, %f)\n", mark->wp->shortname, mark->wp->latitude, mark->wp->longitude);
		}
	}

	QUEUE_FOR_EACH(&ce_mark_head, elem, tmp) {
		ce_mark *mark = (ce_mark *) elem;
		printf("Mark name=%s id=%s ", mark->wp->shortname, mark->id);
		if (mark->wp == NULL)
			printf("(null)\n");
		else
			printf("(%f, %f)\n", mark->wp->latitude, mark->wp->longitude);
	}
}

/* Finish reading the input file */
void
ce_rd_deinit(void)
{
	queue *elem, *tmp;

	ce_fix_waypoints();
	ce_check_route_names();
	ce_remove_unused_marks();
	// ce_print_results();

	// Add routes to GPSBabel
	QUEUE_FOR_EACH(&ce_route_head, elem, tmp) {
		ce_route *route = (ce_route *) elem;
		queue *elem2, *tmp2;
		route_add_head(route->r);
		QUEUE_FOR_EACH(&route->ce_mark_head, elem2, tmp2) {
			ce_mark *mark = (ce_mark *) elem2;
			if (mark->wp)
				route_add_wpt(route->r, mark->wp);
			else
				printf("Undefined mark: %s\n", mark->id);
		}
		free_route(route);
	}

	// Add (unused) marks to GPSBabel
	QUEUE_FOR_EACH(&ce_mark_head, elem, tmp) {
		ce_mark *mark = (ce_mark *) elem;
		waypt_add(mark->wp);
		free_mark(mark);
	}

	fclose(fd);
	xfree(element);
	xfree(cdatastr);
}

void
ce_wr_init(const char *fname)
{
	mkshort_handle = mkshort_new_handle();
	QUEUE_INIT(&ce_mark_head);
	time_buffer = xcalloc(MY_TBUF,1);
	uuid_buffer = xcalloc(MY_UBUF,1);

	ofd = xfopen(fname, "w", MYNAME);
}

void
ce_wr_deinit(void)
{
	fclose(ofd);
	mkshort_del_handle(mkshort_handle);
	xfree(time_buffer);
	xfree(uuid_buffer);
}

static char *
ce_gen_creation_time(time_t tm)
{
	struct tm *t = localtime(&tm);
	sprintf(time_buffer, "%04d%02d%02dT%02d%02d%02dZ", t->tm_year+1900, t->tm_mon+1, t->tm_mday, t->tm_hour, t->tm_min, t->tm_sec);
	return time_buffer;
}

static char *
ce_gen_current_time(void)
{
	return ce_gen_creation_time(current_time());
}

static char *
ce_gen_uuid(void)
{
	uuid_t uu;
	uuid_generate(uu);
	sprintf(uuid_buffer, "%02x%02x%02x%02x-%02x%02x-%02x%02x-%02x%02x-%02x%02x%02x%02x%02x%02x",
		uu[0], uu[1], uu[2], uu[3], uu[4], uu[5], uu[6], uu[7],
		uu[8], uu[9], uu[10], uu[11], uu[12], uu[13], uu[14], uu[15]);
	return uuid_buffer;
}

static void
ce_route_hdr(const route_head *rte)
{
	fprintf(ofd, "\t<Route created=\"%s\" id=\"{%s}\">\n", ce_gen_current_time(), ce_gen_uuid());
	fprintf(ofd, "\t\t<Marks>\n");
}

static void
ce_route_disp(const waypoint *waypointp)
{
	char *uuid = ce_gen_uuid();
	char *id = xcalloc(strlen(uuid)+3, 1);
	sprintf(id, "{%s}", uuid);
	currentMark = (ce_mark *) xcalloc(sizeof (ce_mark), 1);
	currentMark->id = id;
	currentMark->wp = (waypoint *) waypointp;
	ENQUEUE_TAIL(&ce_mark_head, &currentMark->Q);
	fprintf(ofd, "\t\t\t%s\n", id);
}

static void
ce_route_tlr(const route_head *rte)
{
	fprintf(ofd, "\t\t</Marks>\n");
	if (rte->rte_name)
		fprintf(ofd, "\t\t<Name>%s</Name>\n", rte->rte_name);
	fprintf(ofd, "\t</Route>\n");
}

static void
ce_waypt_pr(const waypoint *waypointp)
{
}

static void
ce_mark_pr(void)
{
	queue *elem, *tmp;
	QUEUE_FOR_EACH(&ce_mark_head, elem, tmp) {
		ce_mark *mark = (ce_mark *) elem;
		double latitude = mark->wp->latitude;
		char NorS = 'N';
		char EorW = 'E';
		double longitude = mark->wp->longitude;
		fprintf(ofd, "\t<Mark created=\"%s\" id=\"%s\">\n", ce_gen_creation_time(mark->wp->creation_time), mark->id);
		if (latitude < 0) {
			latitude = -latitude;
			NorS = 'S';
		}
		if (longitude < 0) {
			longitude = -longitude;
			EorW = 'W';
		}
		fprintf(ofd, "\t\t<Position>%3.6f %c %3.6f %c</Position>\n", latitude, NorS, longitude, EorW);
		if (mark->wp->shortname)
			fprintf(ofd, "\t\t<Name>%s</Name>\n", mark->wp->shortname);
		fprintf(ofd, "\t</Mark>\n");
		free_mark(mark);
	}
}

void
ce_write(void)
{
	setshort_whitespace_ok(mkshort_handle, 0);
	setshort_length(mkshort_handle, 32);

	fprintf(ofd, "<?xml version=\"1.0\"?>\n");
	fprintf(ofd, "<NavObjectCollection created=\"%s\"\n", ce_gen_current_time());
	fprintf(ofd, "\t<Name>Navigation Objects</Name>\n");

	route_disp_all(ce_route_hdr, ce_route_tlr, ce_route_disp);
	ce_mark_pr();
	waypt_disp_all(ce_waypt_pr);

	fprintf(ofd, "</NavObjectCollection>\n");
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
