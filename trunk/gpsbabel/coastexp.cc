/*
    Copyright (C) 2004 Justin Broughton, justinbr@earthlink.net
    Copyright (C) 2007 Robert Lipe, robertlipe@gpsbabel.org

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
#include "xmlgeneric.h"
#if HAVE_LIBEXPAT
#  include <expat.h>
static XML_Parser psr;
#endif

#include <time.h>
#include "cet_util.h"

#include "uuid.h"

static gbfile* fd, *ofd;

#define MYNAME "coastexp"
#define MY_CBUF 4096
#define MY_UBUF 128
#define MY_TBUF 64
#define MY_XBUF 128


static char* element; // Current element being parsed
static char* cdatastr; // Current XML character data being built up (until a <lf>)

/* CE-specific mark structure - used for both route marks and standalone marks */
struct CE_MARK {
  queue Q;
  char* id;		// CE's mark ID (of the form "{<guid>}")
  char* created;	// CE's creation time (of the form "<YYYY><MM><DD>T<HH><MM><SS>Z")
  waypoint* wp;	// GPSBabel waypoint
  int used;		// Is this mark used in a route or not?
};
typedef struct CE_MARK ce_mark;

/* CE-specific route structure */
struct CE_ROUTE {
  queue Q;
  char* id;			// CE's route ID (of the form "{<guid>}")
  route_head* r;		// GPSBabel route header
  queue ce_mark_head;	// list of CE marks in this route
};
typedef struct CE_ROUTE ce_route;

static queue ce_route_head;				// List of routes currently found
static ce_route* currentRoute = NULL;	// Current route being processed
static queue ce_mark_head;				// List of stand-alone marks currently found
static ce_mark* currentMark = NULL;		// Current mark being processed
static char* time_buffer = NULL;		// Time buffer for processing times
static char* uuid_buffer = NULL;		// UUID buffer for processing uuid's
static char* xml_buffer = NULL;			// XML buffer for processing XML strings
static int inRoute = 0;					// Are we processing a route?
static int inMark = 0;					// Are we processing a mark?

/* Add a route to the list of routes */
static void
ce_add_route(ce_route* route)
{
  ENQUEUE_TAIL(&ce_route_head, &route->Q);
}

/* Add a mark to the list of stand-alone marks */
static void
ce_add_mark(ce_mark* mark)
{
  ENQUEUE_TAIL(&ce_mark_head, &mark->Q);
}

/* Add a mark to the specified route */
static void
ce_add_mark_to_route(ce_route* route, ce_mark* mark)
{
  ENQUEUE_TAIL(&route->ce_mark_head, &mark->Q);
}

/* Free a mark */
static void
ce_free_mark(ce_mark* mark)
{
  dequeue(&mark->Q);
  if (mark->id) {
    xfree(mark->id);
  }
  if (mark->created) {
    xfree(mark->created);
  }
  xfree(mark);
}

/* Free a route */
static void
ce_free_route(ce_route* route)
{
  queue* elem, *tmp;
  QUEUE_FOR_EACH(&route->ce_mark_head, elem, tmp) {
    ce_mark* mark = (ce_mark*) elem;
    ce_free_mark(mark);
  }
  xfree(route->id);
  xfree(route);
  // Don't free the waypoint since this is done elsewhere
}

/* Allocate a mark */
static ce_mark*
ce_alloc_mark(const waypoint* wpt, const char* id)
{
  ce_mark* res = (ce_mark*) xcalloc(sizeof(ce_mark), 1);
  res->id = (char*) id;
  res->wp = (waypoint*) wpt;
  return res;
}

#if !HAVE_LIBEXPAT
void
ce_rd_init(const char* fname)
{
  fatal(MYNAME ": This build excluded CoastalExplorer support because expat was not installed.\n");
}

void
ce_read(void)
{
}
#else

/* Start processing an XML item */
static void
ce_start(void* data, const XML_Char* xml_el, const XML_Char** xml_attr)
{
  const char* el = xml_convert_to_char_string(xml_el);
  const char** ap;
  const char** attr;

  attr = xml_convert_attrs_to_char_string(xml_attr);
  strcpy(element, el);
  if (0 == strcmp(el, "Route")) {
    inRoute = 1;
    for (ap = attr; *ap; ap+=2) {
      if (0 == strcmp(ap[0], "id")) {
        // Create a CE route object and add it to the list of routes
        currentRoute = (ce_route*) xcalloc(sizeof(ce_route), 1);
        currentRoute->id=xstrdup(ap[1]);
        currentRoute->r = route_head_alloc();
        QUEUE_INIT(&currentRoute->ce_mark_head);
        ce_add_route(currentRoute);
      }
    }
  } else if (0 == strcmp(el, "Mark")) {
    inMark = 1;
    currentMark = ce_alloc_mark(NULL, NULL);
    ce_add_mark(currentMark);
    for (ap = attr; *ap; ap+=2) {
      if (0 == strcmp(ap[0], "id")) {
        // Create a CE mark object and add it to the list of stand-alone marks
        currentMark->id = xstrdup(ap[1]);
      } else if (0 == strcmp(ap[0], "created")) {
        currentMark->created = xstrdup(ap[1]);
      }
    }
  }
  xml_free_converted_string(el);
  xml_free_converted_attrs(attr);
}

/* Finish processing an XML item */
static void
ce_end(void* data, const XML_Char* xml_el)
{
  const char* el = xml_convert_to_char_string(xml_el);
  if (0 == strcmp(el, "Route")) {
    inRoute = 0; /* ??? */
  } else if (0 == strcmp(el, "Mark")) {
    inMark = 0;
  }
  xml_free_converted_string(el);
}

/* Process some XML character data for the current item */
static void
ce_cdata(void* dta, const XML_Char* xml_s, int len)
{
  const char* origs = xml_convert_to_char_string_n(xml_s, &len);
  const char* s = origs;
  if (*s != '\n') {
    char* edatastr;
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
    while (*s != '\0' && (*s == '\b' || *s == '\t')) {
      s++;
    }
    if (strlen(s) <= 0) {
      return;
    }
    if (0 == strcmp(element, "Marks")) {
      if (inRoute) {
        // We are processing the marks in a route so create a CE mark object
        // and add it to the current route
        ce_mark* mark = (ce_mark*) ce_alloc_mark(NULL, xstrdup(s));
        ce_add_mark_to_route(currentRoute, mark);
      }
    } else if (0 == strcmp(element, "Position")) {
      if (inMark) {
        // We are processing a standalone mark so read the lat/long position
        // and create a waypoint to add to the current mark
        char* position = xstrdup(s);
        char* lat = position;
        char* latNorS = position;
        char* lng;
        char* longEorW;
        while (*latNorS != ' ') {
          latNorS++;
        }
        *latNorS++ = '\0';
        lng = latNorS;
        lng++;
        lng++;
        longEorW = lng;
        while (*longEorW != ' ') {
          longEorW++;
        }
        *longEorW++ = '\0';
        if (!currentMark->wp) {
          currentMark->wp = waypt_new();
        }
        currentMark->wp->latitude = atof(lat);
        if (*latNorS == 'S') {
          currentMark->wp->latitude = -currentMark->wp->latitude;
        }
        currentMark->wp->longitude = atof(lng);
        if (*longEorW == 'W') {
          currentMark->wp->longitude = -currentMark->wp->longitude;
        }
        xfree(position);
      }
    } else if (0 == strcmp(element, "Name")) {
      // Names we care about may be either for routes or marks
      if (inMark) {
        if (!currentMark->wp) {
          currentMark->wp = waypt_new();
        }
        currentMark->wp->shortname = xstrdup(s);

        // Also set the creation time
        if (currentMark->created) {
          struct tm t;
          char yearString[5], monthString[3], dayString[3], hourString[3], minString[3], secString[3];
          memset(&t, 0, sizeof(struct tm));
          strncpy(yearString, currentMark->created, 4);
          yearString[4] = '\0';
          t.tm_year = atoi(yearString) - 1900;
          strncpy(monthString, currentMark->created+4, 2);
          monthString[2] = '\0';
          t.tm_mon = atoi(monthString) - 1;
          strncpy(dayString, currentMark->created+6, 2);
          dayString[2] = '\0';
          t.tm_mday = atoi(dayString);
          strncpy(hourString, currentMark->created+9, 2);
          hourString[2] = '\0';
          t.tm_hour = atoi(hourString);
          strncpy(minString, currentMark->created+11, 2);
          minString[2] = '\0';
          t.tm_min = atoi(minString);
          strncpy(secString, currentMark->created+13, 2);
          secString[2] = '\0';
          t.tm_sec = atoi(secString);
          currentMark->wp->SetCreationTime(mkgmtime(&t));
        }
      } else if (inRoute) {
        currentRoute->r->rte_name = xstrdup(s);
      }
    } else if (0 == strcmp(element, "Description")) {
      // Descriptions we care about may be either for routes or marks
      char* desc = xstrdup(s);
      if (inMark) {
        if (!currentMark->wp) {
          currentMark->wp = waypt_new();
        }
        currentMark->wp->description = desc;
      } else if (inRoute) {
        currentRoute->r->rte_desc = desc;
      }
    }

    // Start building a new string since we are done with this one
    cdatastr[0] = '\0';
  }

  xml_free_converted_string(origs);
}

/* Set up reading the CE input file */
void
ce_rd_init(const char* fname)
{
  fd = gbfopen(fname, "r", MYNAME);
  QUEUE_INIT(&ce_route_head);
  QUEUE_INIT(&ce_mark_head);

  psr = XML_ParserCreate(NULL);
  if (!psr) {
    fatal(MYNAME ":Cannot create XML parser\n");
  }

  XML_SetUnknownEncodingHandler(psr, cet_lib_expat_UnknownEncodingHandler, NULL);
  XML_SetElementHandler(psr, ce_start, ce_end);
  cdatastr = (char*) xcalloc(MY_CBUF,1);
  element = (char*) xcalloc(MY_CBUF,1);
  XML_SetCharacterDataHandler(psr, ce_cdata);
}

/* Parse the input file */
void
ce_read(void)
{
  int len;
  char buf[MY_CBUF + 1];

  while ((len = gbfread(buf, 1, sizeof(buf) - 1, fd))) {
    buf[len] = '\0';
    if (!XML_Parse(psr, buf, len, gbfeof(fd))) {
      fatal(MYNAME ":Parse error at %d: %s\n",
            (int) XML_GetCurrentLineNumber(psr),
            XML_ErrorString(XML_GetErrorCode(psr)));
    }
  }

  XML_ParserFree(psr);
}

#endif

/* Fix waypoints in route marks from the standalone marks */
void
ce_fix_route_mark_waypoints(void)
{
  queue* elem, *tmp;
  QUEUE_FOR_EACH(&ce_route_head, elem, tmp) {
    ce_route* route = (ce_route*) elem;
    queue* elem2, *tmp2;
    QUEUE_FOR_EACH(&route->ce_mark_head, elem2, tmp2) {
      ce_mark* mark = (ce_mark*) elem2;
      queue* elem3, *tmp3;
      QUEUE_FOR_EACH(&ce_mark_head, elem3, tmp3) {
        ce_mark* mark2 = (ce_mark*) elem3;
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
  queue* elem, *tmp;
  QUEUE_FOR_EACH(&ce_route_head, elem, tmp) {
    ce_route* route = (ce_route*) elem;
    if (route->r->rte_name == NULL)	{
      *cdatastr = '\0';
      if (!QUEUE_EMPTY(&route->ce_mark_head)) {
        strcat(cdatastr, ((ce_mark*) QUEUE_FIRST(&route->ce_mark_head))->wp->shortname);
        strcat(cdatastr, "->");
        strcat(cdatastr, ((ce_mark*) QUEUE_LAST(&route->ce_mark_head))->wp->shortname);
      }
      route->r->rte_name = xstrdup(cdatastr);
    }
  }
}

/* Remove marks used in routes */
void
ce_remove_used_marks(void)
{
  queue* elem, *tmp;
  QUEUE_FOR_EACH(&ce_mark_head, elem, tmp) {
    ce_mark* mark = (ce_mark*) elem;
    if (mark->used) {
      if (mark->wp) {
        waypt_free(mark->wp);
      }
      ce_free_mark(mark);
    }
  }
}

/* Print out results */
void
ce_print_results(void)
{
  queue* elem, *tmp;
  QUEUE_FOR_EACH(&ce_route_head, elem, tmp) {
    queue* elem2, *tmp2;
    ce_route* route = (ce_route*) elem;
    printf("Route name=%s id=%s\n", route->r->rte_name, route->id);
    QUEUE_FOR_EACH(&route->ce_mark_head, elem2, tmp2) {
      ce_mark* mark = (ce_mark*) elem2;
      if (mark->wp == NULL) {
        printf("  null\n");
      } else {
        printf("  %s (%f, %f)\n", mark->wp->shortname, mark->wp->latitude, mark->wp->longitude);
      }
    }
  }

  QUEUE_FOR_EACH(&ce_mark_head, elem, tmp) {
    ce_mark* mark = (ce_mark*) elem;
    printf("Mark name=%s id=%s ", mark->wp->shortname, mark->id);
    if (mark->wp == NULL) {
      printf("(null)\n");
    } else {
      printf("(%f, %f)\n", mark->wp->latitude, mark->wp->longitude);
    }
  }
}

/* Finish reading the input file */
void
ce_rd_deinit(void)
{
  /* If doing routes, we create GPSBabel route structures and waypoint structures for
     any standalone waypoints.
     If doing waypoints, we create only waypoint structures for both route waypoints and
     standalone waypoints.
  */
  queue* elem, *tmp;

  ce_fix_route_mark_waypoints();
  ce_check_route_names();
  ce_remove_used_marks();

  // Log results
  if (global_opts.debug_level > 1) {
    ce_print_results();
  }

  // Add routes to GPSBabel
  QUEUE_FOR_EACH(&ce_route_head, elem, tmp) {
    ce_route* route = (ce_route*) elem;
    queue* elem2, *tmp2;
    route_add_head(route->r);
    QUEUE_FOR_EACH(&route->ce_mark_head, elem2, tmp2) {
      ce_mark* mark = (ce_mark*) elem2;
      if (mark->wp) {
        route_add_wpt(route->r, mark->wp);
      } else {
        printf("Undefined mark: %s\n", mark->id);
      }
    }
    ce_free_route(route);
  }

  // Add (unused) marks to GPSBabel
  QUEUE_FOR_EACH(&ce_mark_head, elem, tmp) {
    ce_mark* mark = (ce_mark*) elem;
    waypt_add(mark->wp);
    ce_free_mark(mark);
  }

  gbfclose(fd);
  xfree(element);
  xfree(cdatastr);
}

/* Setup for writing */
void
ce_wr_init(const char* fname)
{
  QUEUE_INIT(&ce_mark_head);

  // Alloocate all buffers used for writing
  time_buffer = (char*) xcalloc(MY_TBUF,1);
  uuid_buffer = (char*) xcalloc(MY_UBUF,1);
  xml_buffer = (char*) xcalloc(MY_XBUF, 1);

  ofd = gbfopen(fname, "w", MYNAME);
  srand(gpsbabel_now);
}

void
ce_wr_deinit(void)
{
  gbfclose(ofd);

  // Free the buffers used for writing
  xfree(time_buffer);
  xfree(uuid_buffer);
  xfree(xml_buffer);
}

/* Generate a CE-style creation time based on supplied time */
static char*
ce_gen_creation_time(time_t tm)
{
  QDateTime qtm;
  qtm = QDateTime::fromTime_t(tm);
  xml_fill_in_time(time_buffer, qtm, XML_SHORT_TIME);
  return time_buffer;
}

/* Generate a CE-style creation time based on current time */
static char*
ce_gen_current_time(void)
{
  return ce_gen_creation_time(current_time());
}

/* Generate a UUID (has same format as Microsoft registry GUIDs */
static char*
ce_gen_uuid(void)
{
  uuid_t uu;

  memset(&uu, 0, sizeof(uu));
  gb_uuid_generate(uu);
  sprintf(uuid_buffer, "%02x%02x%02x%02x-%02x%02x-%02x%02x-%02x%02x-%02x%02x%02x%02x%02x%02x",
          uu[0], uu[1], uu[2], uu[3], uu[4], uu[5], uu[6], uu[7],
          uu[8], uu[9], uu[10], uu[11], uu[12], uu[13], uu[14], uu[15]);
  return uuid_buffer;
}

/* Generate route header XML */
static void
ce_route_hdr(const route_head* rte)
{
  sprintf(xml_buffer, "{%s}", ce_gen_uuid());
  write_xml_entity_begin2(ofd, "\t", "Route", "created", ce_gen_current_time(), "id", xml_buffer);
  write_xml_entity_begin0(ofd, "\t\t", "Marks");
}

/* Generate route body XML */
static void
ce_route_disp(const waypoint* waypointp)
{
  char* uuid = ce_gen_uuid();
  char* id = (char*) xcalloc(strlen(uuid)+3, 1);

  sprintf(id, "{%s}", uuid);
  currentMark = ce_alloc_mark(waypointp, id);
  ENQUEUE_TAIL(&ce_mark_head, &currentMark->Q);

  gbfprintf(ofd, "\t\t\t%s\n", id); // CE's departure from XML standard!
}

/* Generate route trailer XML */
static void
ce_route_tlr(const route_head* rte)
{
  write_xml_entity_end(ofd, "\t\t", "Marks");
  write_optional_xml_entity(ofd, "\t\t", "Name", rte->rte_name);
  write_xml_entity_end(ofd, "\t", "Route");
}

/* Generate waypoint body XML */
static void
ce_waypt_pr(const waypoint* wp)
{
  double latitude = wp->latitude;
  char NorS = 'N';
  char EorW = 'E';
  double longitude = wp->longitude;

  if (latitude < 0) {
    latitude = -latitude;
    NorS = 'S';
  }
  if (longitude < 0) {
    longitude = -longitude;
    EorW = 'W';
  }
  sprintf(xml_buffer, "%3.6f %c %3.6f %c", latitude, NorS, longitude, EorW);
  write_xml_entity(ofd, "\t\t", "Position", xml_buffer);
  write_optional_xml_entity(ofd, "\t\t", "Name", wp->shortname);
  if (wp->description && wp->shortname &&
      strcmp(wp->description, wp->shortname)) {
    write_optional_xml_entity(ofd, "\t\t", "Description", wp->description);
  }
}

static char*
ce_find_uuid(const waypoint* wpt)
{
  queue* elem, *tmp;

  QUEUE_FOR_EACH(&ce_mark_head, elem, tmp) {
    ce_mark* mark = (ce_mark*) elem;
    if (mark->wp == wpt) {
      return mark->id;
    }
  }
  return NULL;
}

static waypoint*
ce_find_wpt(const waypoint* wpt)
{
  queue* elem, *tmp;

  QUEUE_FOR_EACH(&ce_mark_head, elem, tmp) {
    ce_mark* mark = (ce_mark*) elem;
    if ((mark->wp->shortname == wpt->shortname) &&
        (mark->wp->latitude == wpt->latitude) &&
        (mark->wp->longitude == wpt->longitude)) {
      return mark->wp;
    }
  }
  return NULL;
}

/* Generate a mark XML; look for created id's */
static void
ce_mark_pr(const waypoint* wp)
{
  char* id;

  if (inRoute) {
    id = ce_find_uuid(wp);
    if (id == NULL) {
      sprintf(xml_buffer, "{%s}", ce_gen_uuid());
      id = xml_buffer;
    }
  }
  /* Have we seen and written the (nearly) same waypoint ? */
  else if (ce_find_wpt(wp) != NULL) {
    return;
  } else {
    ce_mark* mark = ce_alloc_mark(wp, NULL);
    ENQUEUE_TAIL(&ce_mark_head, &mark->Q);
    sprintf(xml_buffer, "{%s}", ce_gen_uuid());
    id = xml_buffer;
  }
  write_xml_entity_begin2(ofd, "\t", "Mark",
                          "created", ce_gen_creation_time(wp->GetCreationTime()),
                          "id", id);
  ce_waypt_pr(wp);
  write_xml_entity_end(ofd, "\t", "Mark");
}

/* Generate all route marks */
static void
ce_marks_pr(void)
{
  queue* elem, *tmp;

  QUEUE_FOR_EACH(&ce_mark_head, elem, tmp) {
    ce_mark* mark = (ce_mark*) elem;
    ce_mark_pr(mark->wp);
  }
}

/* Release all generated marks */
static void
ce_marks_flush_all(void)
{
  queue* elem, *tmp;

  QUEUE_FOR_EACH(&ce_mark_head, elem, tmp) {
    ce_mark* mark = (ce_mark*) elem;
    ce_free_mark(mark);
  }
}

/* Write all routes and marks */
void
ce_write(void)
{
  /* If doing routes, we write out the routes and all the standalone waypoints.
     If doing waypoints, we write out the route waypoints (without the routes) and
     the standalone waypoints.
  */
  time_t now = 0;
  now = current_time();

  write_xml_header(ofd);
  write_xml_entity_begin1(ofd, "", "NavObjectCollection", "created",
                          ce_gen_current_time());
  write_xml_entity(ofd, "\t", "Name", "Navigation Objects");

  inRoute = 1;
  route_disp_all(ce_route_hdr, ce_route_tlr, ce_route_disp);
  ce_marks_pr();
  inRoute = 0;

  waypt_disp_all(ce_mark_pr);
  ce_marks_flush_all();

  write_xml_entity_end(ofd, "", "NavObjectCollection");
}

ff_vecs_t coastexp_vecs = {
  ff_type_file,
  { (ff_cap)(ff_cap_read|ff_cap_write), ff_cap_none, (ff_cap)(ff_cap_read|ff_cap_write) },
  ce_rd_init,
  ce_wr_init,
  ce_rd_deinit,
  ce_wr_deinit,
  ce_read,
  ce_write,
  NULL,
  NULL,
  CET_CHARSET_ASCII, 0	/* CET-REVIEW */
};
