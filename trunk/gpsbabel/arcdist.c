/*
    Distance from point to arc filter

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
#define MYNAME "Arc filter"

static double pos_dist;
static char* distopt = NULL;
static char* arcfileopt = NULL;
static char* exclopt = NULL;
static char* ptsopt = NULL;

typedef struct {
  double distance;
} extra_data;

static
arglist_t arcdist_args[] = {
  {
    "file", &arcfileopt,  "File containing vertices of arc",
    NULL, ARGTYPE_FILE | ARGTYPE_REQUIRED, ARG_NOMINMAX
  },
  {
    "distance", &distopt, "Maximum distance from arc",
    NULL, ARGTYPE_FLOAT | ARGTYPE_REQUIRED, ARG_NOMINMAX
  },
  {
    "exclude", &exclopt, "Exclude points close to the arc", NULL,
    ARGTYPE_BOOL, ARG_NOMINMAX
  },
  {
    "points", &ptsopt, "Use distance from vertices not lines",
    NULL, ARGTYPE_BOOL, ARG_NOMINMAX
  },
  ARG_TERMINATOR
};

#define BADVAL 999999

void
arcdist_process(void)
{
  queue* elem, * tmp;
  waypoint* waypointp;
  double dist;
  extra_data* ed;
  double lat1, lon1, lat2, lon2;
  int fileline = 0;
  char* line;
  gbfile* file_in;

  file_in = gbfopen(arcfileopt, "r", MYNAME);

  lat1 = lon1 = lat2 = lon2 = BADVAL;
  while ((line = gbfgetstr(file_in))) {
    char* pound = NULL;
    int argsfound = 0;

    fileline++;

    pound = strchr(line, '#');
    if (pound) {
      if (0 == strncmp(pound, "#break", 6)) {
        lat1 = lon1 = BADVAL;
      }
      *pound = '\0';
    }

    lat2 = lon2 = BADVAL;
    argsfound = sscanf(line, "%lf %lf", &lat2, &lon2);

    if (argsfound != 2 && strspn(line, " \t\n") < strlen(line)) {
      warning(MYNAME ": Warning: Arc file contains unusable vertex on line %d.\n", fileline);
    } else if (lat2 != BADVAL && lon2 != BADVAL &&
               (ptsopt || (lat1 != BADVAL && lon1 != BADVAL))) {
      QUEUE_FOR_EACH(&waypt_head, elem, tmp) {

        waypointp = (waypoint*)elem;
        if (waypointp->extra_data) {
          ed = (extra_data*) waypointp->extra_data;
        } else {
          ed = (extra_data*) xcalloc(1, sizeof(*ed));
          ed->distance = BADVAL;
        }
        if (ed->distance == BADVAL || ed->distance >= pos_dist) {
          if (ptsopt) {
            dist = gcdist(RAD(lat2), RAD(lon2),
                          RAD(waypointp->latitude),
                          RAD(waypointp->longitude));
          } else {
            dist = linedist(lat1, lon1, lat2, lon2,
                            waypointp->latitude,
                            waypointp->longitude);
          }

          /* convert radians to float point statute miles */
          dist = radtomiles(dist);

          if (ed->distance > dist) {
            ed->distance = dist;
          }
          waypointp->extra_data = ed;
        }
      }
    }
    lat1 = lat2;
    lon1 = lon2;
  }

  gbfclose(file_in);

  QUEUE_FOR_EACH(&waypt_head, elem, tmp) {
    waypoint* wp = (waypoint*) elem;
    ed = (extra_data*) wp->extra_data;
    wp->extra_data = NULL;
    if (ed) {
      if ((ed->distance >= pos_dist) == (exclopt == NULL)) {
        waypt_del(wp);
        waypt_free(wp);
      }
      xfree(ed);
    }
  }
}

void
arcdist_init(const char* args)
{
  char* fm;

  pos_dist = 0;

  if (distopt) {
    pos_dist = strtod(distopt, &fm);

    if ((*fm == 'k') || (*fm == 'K')) {
      /* distance is kilometers, convert to feet */
      pos_dist *= .6214;
    }
  }
}

void
arcdist_deinit(void)
{
  /* do nothing */
}

filter_vecs_t arcdist_vecs = {
  arcdist_init,
  arcdist_process,
  arcdist_deinit,
  NULL,
  arcdist_args
};
#endif // FILTERS_ENABLED
