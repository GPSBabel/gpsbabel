/*
    Radius Filter

    Copyright (C) 2002 Robert Lipe, robertlipe@usa.net

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
#include "filterdefs.h"
#include "grtcirc.h"

#if FILTERS_ENABLED

#ifndef M_PI
#  define M_PI 3.14159265358979323846
#endif

static double pos_dist;
static char *distopt = NULL;
static char *latopt = NULL;
static char *lonopt = NULL;
static char *exclopt = NULL;
static char *nosort = NULL;
static char *maxctarg = NULL;
static char *routename = NULL;
static int maxct;

static waypoint * home_pos;

typedef struct {
  double distance;
} extra_data;

static
arglist_t radius_args[] = {
  {
    "lat", &latopt,       "Latitude for center point (D.DDDDD)",
    NULL, ARGTYPE_FLOAT | ARGTYPE_REQUIRED, ARG_NOMINMAX
  },
  {
    "lon", &lonopt,       "Longitude for center point (D.DDDDD)",
    NULL, ARGTYPE_FLOAT | ARGTYPE_REQUIRED, ARG_NOMINMAX
  },
  {
    "distance", &distopt, "Maximum distance from center",
    NULL, ARGTYPE_FLOAT | ARGTYPE_REQUIRED, ARG_NOMINMAX
  },
  {
    "exclude", &exclopt,  "Exclude points close to center",
    NULL, ARGTYPE_BOOL, ARG_NOMINMAX
  },
  {
    "nosort", &nosort,    "Inhibit sort by distance to center",
    NULL, ARGTYPE_BOOL, ARG_NOMINMAX
  },
  {
    "maxcount", &maxctarg,"Output no more than this number of points",
    NULL, ARGTYPE_INT, "1", NULL
  },
  {
    "asroute", &routename,"Put resulting waypoints in route of this name",
    NULL, ARGTYPE_STRING, NULL, NULL
  },
  ARG_TERMINATOR
};

static double
gc_distance(double lat1, double lon1, double lat2, double lon2)
{
  return gcdist(
           RAD(lat1),
           RAD(lon1),
           RAD(lat2),
           RAD(lon2)
         );
}

static int
dist_comp(const void * a, const void * b)
{
  const waypoint *x1 = *(waypoint **)a;
  const waypoint *x2 = *(waypoint **)b;
  extra_data *x1e = (extra_data *) x1->extra_data;
  extra_data *x2e = (extra_data *) x2->extra_data;

  if (x1e->distance > x2e->distance) {
    return 1;
  }
  if (x1e->distance < x2e->distance) {
    return -1;
  }
  return 0;

}

void
radius_process(void)
{
  queue * elem, * tmp;
  waypoint * waypointp;
  double dist;
  waypoint ** comp;
  int i, wc;
  queue temp_head;
  route_head *rte_head = NULL;

  QUEUE_FOR_EACH(&waypt_head, elem, tmp) {
    extra_data *ed;

    waypointp = (waypoint *)elem;
    dist = gc_distance(waypointp->latitude,
                       waypointp->longitude,
                       home_pos->latitude,
                       home_pos->longitude);

    /* convert radians to float point statute miles */
    dist = radtomiles(dist);

    if ((dist >= pos_dist) == (exclopt == NULL)) {
      waypt_del(waypointp);
      waypt_free(waypointp);
      continue;
    }

    ed = (extra_data *) xcalloc(1, sizeof(*ed));
    ed->distance = dist;
    waypointp->extra_data = ed;
  }

  wc = waypt_count();
  QUEUE_INIT(&temp_head);

  comp = (waypoint **) xcalloc(wc, sizeof(*comp));

  i = 0;

  /*
   * Create an array of remaining waypoints, popping them off the
   * master queue as we go.   This gives us something reasonable
   * for qsort.
   */

  QUEUE_FOR_EACH(&waypt_head, elem, tmp) {
    waypoint *wp = (waypoint *) elem;
    comp[i] = wp;
    waypt_del(wp);
    i++;
  }

  if (!nosort) {
    qsort(comp, wc, sizeof(waypoint *), dist_comp);
  }

  if (routename) {
    rte_head = route_head_alloc();
    rte_head->rte_name = xstrdup(routename);
    route_add_head(rte_head);
  }

  /*
   * The comp array is now sorted by distance.   As we run through it,
   * we push them back onto the master wp list, letting us pass them
   * on through in the modified order.
   */
  for (i = 0; i < wc; i++) {
    waypoint * wp = comp[i];

    xfree(wp->extra_data);
    wp->extra_data = NULL;

    if (maxctarg && i >= maxct) {
      continue;
    }
    if (routename) {
      route_add_wpt(rte_head, wp);
    } else {
      waypt_add(wp);
    }
  }

  xfree(comp);
}

void
radius_init(const char *args)
{
  char *fm;

  pos_dist = 0;

  if (distopt) {
    pos_dist = strtod(distopt, &fm);

    if ((*fm == 'k') || (*fm == 'K')) {
      /* distance is kilometers, convert to feet */
      pos_dist *= .6214;
    }
  }

  if (maxctarg) {
    maxct = atoi(maxctarg);
  } else {
    maxct = 0;
  }

  home_pos = (waypoint *) xcalloc(sizeof(*home_pos), 1);

  if (latopt) {
    home_pos->latitude = atof(latopt);
  }
  if (lonopt) {
    home_pos->longitude = atof(lonopt);
  }
}

void
radius_deinit(void)
{
  if (home_pos) {
    xfree(home_pos);
  }
}

filter_vecs_t radius_vecs = {
  radius_init,
  radius_process,
  radius_deinit,
  NULL,
  radius_args
};
#endif // FILTERS_ENABLED
